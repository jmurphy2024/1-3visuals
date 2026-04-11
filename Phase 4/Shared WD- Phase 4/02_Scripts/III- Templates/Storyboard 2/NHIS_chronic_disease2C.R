# ==============================================================================
# SCRIPT: NHIS_chronic_disease_2C.R
# Purpose: Full 3-Country Chronic Disease Prevalence using NHIS 2020
# Visual: Minimalist Skyline 2C (No grids, strict min/max y-axis, robust ntile)
# ==============================================================================
rm(list = ls()); gc()
options(expressions = 500000)

library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tidyr); library(stringr)
library(gridExtra); library(grid); library(scales); library(Hmisc)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))

apply_standard_theme <- function(p, r_margin = 30) {
  p_updated <- p +
    theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      legend.position   = "none", 
      panel.grid        = element_blank(), 
      axis.text.x       = element_blank(), 
      axis.text.y       = element_text(color = "black", size = 11, family = "serif", margin = margin(r = 5)), 
      axis.line.x       = element_line(color = "black", linewidth = 1.2), 
      axis.line.y       = element_line(color = "black", linewidth = 1.2), 
      axis.title        = element_blank(), 
      plot.title        = element_blank(), 
      plot.caption      = element_blank(), 
      plot.margin       = margin(t = 30, r = r_margin, b = 30, l = 20),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA)
    )
  return(p_updated)
}

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found.")
cutoffs <- readRDS(cutoffs_path)

# 2. DATA ACQUISITION & PIPELINE
# ------------------------------------------------------------------------------
USER_SAMPLE <- "ih2020" 
VARS_NEEDED <- c("SAMPWEIGHT", "INCFAM07ON", "REGION", "AGE", "DIABETICEV", "CHEARTDIEV", "CANCEREV", "HYPERTENEV")

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", USER_SAMPLE)
TARGET_FILE <- file.path(TARGET_DIR, "raw_data.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS NHIS 2020 API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "nhis", samples = USER_SAMPLE, variables = VARS_NEEDED, description = "Three Countries NHIS 2020 Full Spectrum"
  )
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  message("--- Loading existing NHIS 2020 data ---")
  raw_data <- readRDS(TARGET_FILE)
}

message("Applying normalization and pillar logic...")

region_rpp_lookup <- tibble(REGION_ID = c(1, 2, 3, 4), REG_RPP = c(105.2, 92.8, 95.4, 104.1))
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2020, base_year = 2023)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  left_join(region_rpp_lookup, by = c("REGION" = "REGION_ID")) %>%
  mutate(
    PERWT = as.numeric(SAMPWEIGHT),
    raw_dollars = case_when(
      INCFAM07ON == 11 ~ 17500,  # $0 - $34,999
      INCFAM07ON == 12 ~ 42500,  # $35,000 - $49,999
      INCFAM07ON == 22 ~ 62500,  # $50,000 - $74,999
      INCFAM07ON == 23 ~ 87500,  # $75,000 - $99,999
      INCFAM07ON == 24 ~ 150000, # $100,000+
      TRUE ~ NA_real_            
    ),
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)),
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    ),
    target_indicator = if_else(DIABETICEV == 2 | CHEARTDIEV == 2 | CANCEREV == 2 | HYPERTENEV == 2, 1, 0)
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, AGE >= 18, !is.na(REAL_INCOME), !is.na(Country))

# 3. SUMMARY STATISTICS
# ------------------------------------------------------------------------------
message("\n=== CHRONIC DISEASE PREVALENCE SUMMARY ===")
overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile", Total_Population = sum(PERWT, na.rm = TRUE),
    `Chronic Disease Prevalence (%)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  )

prepared_data_q <- prepared_data %>%
  group_by(Country) %>%
  mutate(
    q25 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.25, na.rm = TRUE),
    q50 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.50, na.rm = TRUE),
    q75 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.75, na.rm = TRUE),
    Quartile = case_when(REAL_INCOME <= q25 ~ "Q1", REAL_INCOME <= q50 ~ "Q2", REAL_INCOME <= q75 ~ "Q3", TRUE ~ "Q4")
  ) %>% ungroup()

quartile_stats <- prepared_data_q %>%
  group_by(Country, Quartile) %>%
  summarise(
    Subgroup = first(Quartile), Total_Population = sum(PERWT, na.rm = TRUE),
    `Chronic Disease Prevalence (%)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  ) %>% select(-Quartile)

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(Total_Population = scales::comma(Total_Population))

print(as.data.frame(macro_summary))
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)
table_grob <- tableGrob(macro_summary, rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)), colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))))

# 4. VISUALIZATION
# ------------------------------------------------------------------------------
message("\nGenerating Minimalist Skyline Plot...")

viz_data_single <- prepared_data %>%
  group_by(Country) %>% mutate(decile = ntile(REAL_INCOME, 10)) %>% group_by(Country, decile) %>%
  summarise(avg_val = weighted.mean(target_indicator, w = PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third")), x_id = row_number())

p_chart <- ggplot(viz_data_single, aes(x = x_id, y = avg_val, color = Country)) +
  geom_line(aes(group = 1), linewidth = 3, alpha = 0.2) + geom_line(aes(group = 1), linewidth = 1.2, linejoin = "round") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, NA), breaks = function(x) c(0, max(x, na.rm = TRUE)), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.05))) + labs(x = NULL, y = NULL)

p_chart <- apply_standard_theme(p_chart, r_margin = 30)

# Render & Save
grid.newpage(); grid.draw(table_grob)
print(p_chart)

ggsave(here::here("03_output", "visualizations_final", "NHIS_2020_Chronic_Disease_Summary_Table_2C.png"), table_grob, width = 7.5, height = 5, bg = "white", dpi = 300)
ggsave(here::here("03_output", "visualizations_final", "NHIS_2020_Chronic_Disease_Skyline_2C.png"), p_chart, width = 10, height = 7, dpi = 300, bg = "white")
message("Processing & Visualizations Complete!")