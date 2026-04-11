# ==============================================================================
# SCRIPT: NCVS_prevalence_2C.R
# Purpose: Generate 3-Country Skyline for Crime PREVALENCE (% of people victimized)
# Visual: Minimalist Skyline 2C (No grids, strict min/max y-axis, robust ntile)
# ==============================================================================
rm(list = ls()); gc()
options(expressions = 500000)

library(dplyr); library(here); library(stringr); library(tidyr); library(ggplot2)
library(gridExtra); library(grid); library(scales); library(Hmisc)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
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
set.seed(123) 

extract_code <- function(x) { as.numeric(str_extract(as.character(x), "\\d+")) }

# 2. DATA ACQUISITION & PIPELINE
# ------------------------------------------------------------------------------
HH_FILE   <- here::here("01_data", "raw", "NCVS", "ncvs_household_2023.rda")
PER_FILE  <- here::here("01_data", "raw", "NCVS", "ncvs_person_2023.rda")
INC_FILE  <- here::here("01_data", "raw", "NCVS", "ncvs_extract_2023.rda")

if (!file.exists(HH_FILE) | !file.exists(PER_FILE) | !file.exists(INC_FILE)) {
  stop("NCVS .rda files not found. Please verify the exact filenames.")
}

message("--- Loading NCVS RDA Files ---")
hh_obj_name  <- load(HH_FILE); per_obj_name <- load(PER_FILE); inc_obj_name <- load(INC_FILE)
ds2_raw <- get(hh_obj_name[1]); ds3_raw <- get(per_obj_name[1]); ds5_raw <- get(inc_obj_name[1])
rm(list = c(hh_obj_name, per_obj_name, inc_obj_name)); gc()

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023) 

ds2_unique <- ds2_raw %>% group_by(IDHH) %>% slice(1) %>% ungroup() 
ds3_unique <- ds3_raw %>% group_by(IDHH, IDPER) %>% slice(1) %>% ungroup() 

prepared_data <- ds3_unique %>%
  left_join(ds2_unique %>% select(IDHH, V2026, V2127B), by = "IDHH") %>% 
  left_join(ds5_raw %>% select(IDHH, IDPER, V4529), by = c("IDHH", "IDPER"), relationship = "one-to-many") %>% 
  mutate(
    income_code   = extract_code(V2026), 
    PERWT         = as.numeric(as.character(WGTPERCY)), 
    crime_code    = extract_code(V4529), 
    MAPPED_REGION = as.numeric(V2127B), 
    Violent_Crime = if_else(!is.na(crime_code) & crime_code >= 1 & crime_code <= 20, 1, 0), 
    Property_Crime= if_else(!is.na(crime_code) & crime_code >= 31 & crime_code <= 59, 1, 0) 
  ) %>%
  group_by(IDHH, IDPER) %>%
  summarise(
    PERWT         = first(PERWT), 
    income_code   = first(income_code), 
    MAPPED_REGION = first(MAPPED_REGION), 
    Violent_Crime = max(Violent_Crime, na.rm = TRUE), # Max for Prevalence (0 or 1)
    Property_Crime= max(Property_Crime, na.rm = TRUE), 
    .groups       = "drop" 
  ) %>%
  filter(!is.na(income_code) & income_code < 99 & PERWT > 0) %>% 
  mutate(
    raw_dollars = case_when(
      income_code == 1 ~ runif(n(),0,4999),       income_code == 2 ~ runif(n(),5000,9999),
      income_code == 3 ~ runif(n(),10000,14999),  income_code == 4 ~ runif(n(),15000,24999),
      income_code == 5 ~ runif(n(),25000,34999),  income_code == 6 ~ runif(n(),35000,49999),
      income_code == 7 ~ runif(n(),50000,74999),  income_code == 17 ~ runif(n(),75000,250000), 
      TRUE             ~ runif(n(),35000,49999) 
    ),
    REAL_INCOME = raw_dollars * INFLATION_ADJ * get_regional_rpp_multiplier(MAPPED_REGION), 
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third", 
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third", 
      TRUE ~ "Top Third" 
    )
  ) %>%
  filter(!is.na(REAL_INCOME), !is.na(Country)) %>%
  rename(`Property Crime` = Property_Crime, `Violent Crime` = Violent_Crime)

# 3. SUMMARY STATISTICS
# ------------------------------------------------------------------------------
message("\n=== NCVS PREVALENCE SUMMARY (%) ===")
overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile", Total_Population = sum(PERWT, na.rm = TRUE),
    `Property Crime (%)` = round((sum(`Property Crime` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Violent Crime (%)`  = round((sum(`Violent Crime` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
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
    `Property Crime (%)` = round((sum(`Property Crime` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Violent Crime (%)`  = round((sum(`Violent Crime` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
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
message("\nGenerating Minimalist Skyline Plots...")

viz_data_multi <- prepared_data %>%
  group_by(Country) %>% mutate(decile = ntile(REAL_INCOME, 10)) %>% group_by(Country, decile) %>%
  summarise(
    `Property Crime` = weighted.mean(`Property Crime`, w = PERWT, na.rm = TRUE),
    `Violent Crime`  = weighted.mean(`Violent Crime`, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Property Crime", "Violent Crime"), names_to = "Variable", values_to = "val") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Variable, Country, decile) %>% group_by(Variable) %>% mutate(x_id = row_number()) %>% ungroup()

p_chart <- ggplot(viz_data_multi, aes(x = x_id, y = val, group = Variable)) +
  geom_line(aes(color = Country), linewidth = 3, alpha = 0.2) + geom_line(aes(color = Country), linewidth = 1.2, linejoin = "round") +
  geom_text(data = viz_data_multi %>% filter(x_id == 30), aes(label = Variable), hjust = -0.1, size = 4, fontface = "bold", color = "black", family = "serif") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1), limits = c(0, NA), breaks = function(x) c(0, max(x, na.rm = TRUE)), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.35))) + coord_cartesian(clip = "off") + labs(x = NULL, y = NULL)

p_chart <- apply_standard_theme(p_chart, r_margin = 160) 

# Render & Save
grid.newpage(); grid.draw(table_grob)
print(p_chart)

ggsave(here::here("03_output", "visualizations_final", "NCVS_Prevalence_Summary_Table_2C.png"), table_grob, width = 10, height = 5, bg = "white", dpi = 300)
ggsave(here::here("03_output", "visualizations_final", "NCVS_Prevalence_Skyline_2C.png"), p_chart, width = 10, height = 7, dpi = 300, bg = "white")
message("Processing & Visualizations Complete!")