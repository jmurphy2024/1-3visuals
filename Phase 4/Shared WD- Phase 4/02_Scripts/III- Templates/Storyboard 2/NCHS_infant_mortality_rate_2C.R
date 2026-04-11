# ==============================================================================
# SCRIPT: NCHS_infant_mortality_rate_2C.R
# Purpose: Generate 3-Country Skyline for Infant Mortality
# Logic:   CDC WONDER Linked Births (2017-2023) + Census ACS Income by County
# Logic Check: Explicitly removes negative income (HHINCOME >= 0), Person-Weighted
# Visual:  Minimalist Skyline (No titles, no gridlines, no dots, strict min/max y-axis)
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit for heavy ggplot rendering
options(expressions = 500000)

library(tidycensus); library(purrr); library(dplyr); library(here); library(scales)
library(data.table); library(stringr); library(tidyr); library(ggplot2)
library(gridExtra); library(grid); library(Hmisc) 

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2C.R"))

# ==============================================================================
# VISUAL THEME STANDARDIZATION (MINIMALIST)
# ==============================================================================
apply_standard_theme <- function(p) {
  p_updated <- p +
    theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      legend.position   = "none", # Removed legend to match the clean image style
      panel.grid        = element_blank(), # Removed all gridlines
      axis.text.x       = element_blank(), # Removed X-axis text
      axis.text.y       = element_text(color = "black", size = 11, family = "serif", margin = margin(r = 5)), 
      axis.line.x       = element_line(color = "black", linewidth = 1.2), # Bolded axis lines
      axis.line.y       = element_line(color = "black", linewidth = 1.2), 
      axis.title        = element_blank(), # Removed axis titles
      plot.title        = element_blank(), # Removed plot title
      plot.caption      = element_blank(), # Removed plot caption
      plot.margin       = margin(t = 30, r = 30, b = 30, l = 20),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA)
    )
  return(p_updated)
}

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT
# ------------------------------------------------------------------------------
TARGET_YEAR <- 2022 
TARGET_DIR  <- here::here("01_data", "raw", "NCHS_CDC")
NCHS_FILE   <- here::here("Linked Birth  Infant Death Records, 2017-2023 Expanded.csv") 
TARGET_FILE <- file.path(TARGET_DIR, "nchs_acs_infant_mortality_raw_v2.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering Census API & Loading NCHS Extracts ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  nchs_data <- read.csv(NCHS_FILE, check.names = FALSE)
  acs_income <- purrr::map_dfr(c(state.abb, "DC"), ~get_acs(
    geography = "county", variables = "B19013_001", state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE
  ))
  saveRDS(list(nchs = nchs_data, acs = acs_income), TARGET_FILE)
} else {
  message("--- Loading existing API & Extract data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 3. DATA ENGINEERING & CLEANING
# ------------------------------------------------------------------------------
message("Standardizing income and dropping negative values...")
dt_acs <- as.data.table(raw_data$acs)[, .(fips = GEOID, est_median_hh_inc = estimate)]

# CORE LOGIC: Set 9999999 to NA and filter for non-negative income
dt_acs[est_median_hh_inc == 9999999, est_median_hh_inc := NA_real_]
dt_acs <- dt_acs[est_median_hh_inc >= 0] 

dt_nchs <- as.data.table(raw_data$nchs)[!is.na(`County Code`) & `County Code` != "", .(
  fips   = sprintf("%05d", as.numeric(`County Code`)), 
  deaths = as.numeric(Deaths),
  births = as.numeric(Births)
)][!endsWith(fips, "999") & births > 0]

dt_nchs[, infant_mort_rate := deaths / births]
dt_linked <- merge(dt_nchs, dt_acs, by = "fips", all.x = FALSE) 

# 4. NORMALIZATION & PILLAR LOGIC
# ------------------------------------------------------------------------------
message("Applying normalization and dynamic cutoffs...")

state_rpp_lookup <- tibble::tibble(STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56), STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6))
INFLATION_ADJ <- get_inflation_multiplier(data_year = TARGET_YEAR, base_year = 2023)

prepared_data <- as_tibble(dt_linked) %>%
  mutate(STATEFIP = as.numeric(substr(fips, 1, 2))) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    PERWT            = births,
    target_indicator = infant_mort_rate,
    REAL_INCOME      = (as.numeric(est_median_hh_inc) * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    Country          = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(REAL_INCOME), REAL_INCOME >= 0)

# SMOOTHING TRIM: Remove extreme 1% outliers for rate clarity
prepared_data <- prepared_data %>%
  filter(target_indicator >= quantile(target_indicator, 0.01, na.rm = TRUE),
         target_indicator <= quantile(target_indicator, 0.99, na.rm = TRUE))

# ==============================================================================
# 5. SUMMARY STATISTICS (TERCILES & QUARTILES WITHIN)
# ==============================================================================
message("\n=== INFANT MORTALITY SUMMARY (PER 1,000 BIRTHS) ===")

overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Births = sum(PERWT, na.rm = TRUE),
    `Infant Mortality (per 1,000)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Births) * 1000, 2),
    .groups = "drop"
  )

prepared_data_q <- prepared_data %>%
  group_by(Country) %>%
  mutate(
    q25 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.25, na.rm = TRUE),
    q50 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.50, na.rm = TRUE),
    q75 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.75, na.rm = TRUE),
    Quartile = case_when(
      REAL_INCOME <= q25 ~ "Q1 (Bottom 25%)",
      REAL_INCOME > q25 & REAL_INCOME <= q50 ~ "Q2",
      REAL_INCOME > q50 & REAL_INCOME <= q75 ~ "Q3",
      TRUE ~ "Q4 (Top 25%)"
    )
  ) %>%
  ungroup()

quartile_stats <- prepared_data_q %>%
  group_by(Country, Quartile) %>%
  summarise(
    Subgroup = first(Quartile),
    Total_Births = sum(PERWT, na.rm = TRUE),
    `Infant Mortality (per 1,000)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Births) * 1000, 2),
    .groups = "drop"
  ) %>%
  select(-Quartile)

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(Total_Births = scales::comma(Total_Births))

print(as.data.frame(macro_summary))

# ==============================================================================
# 6. VISUALIZATION EXECUTION (Minimalist Format)
# ==============================================================================
message("\nGenerating Minimalist Skyline Plot...")
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

summary_plot_data <- prepared_data %>%
  group_by(Country) %>%
  mutate(decile = ntile(REAL_INCOME, 10)) %>%
  group_by(Country, decile) %>%
  summarise(avg_val = weighted.mean(target_indicator, w = PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third")), x_id = row_number())

p_chart <- ggplot(summary_plot_data, aes(x = x_id, y = avg_val, color = Country)) +
  geom_line(aes(group = 1), linewidth = 3, alpha = 0.2) + 
  geom_line(aes(group = 1), linewidth = 1, linejoin = "round") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1), 
    limits = c(0, NA), 
    breaks = function(x) c(0, max(x, na.rm = TRUE)), # Force breaks to only be 0 and the actual max value
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = NULL)

p_chart <- apply_standard_theme(p_chart)

# --- RSTUDIO DIRECT OUTPUTS ---
# Show summary table in RStudio plot window
table_grob <- tableGrob(macro_summary, rows = NULL,
                        theme = ttheme_default(core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)),
                                               colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))))
grid.newpage()
grid.draw(table_grob)

# Render the Skyline plot in RStudio plot window
print(p_chart)

ggsave(here::here("03_output", "visualizations_final", "NCHS_Infant_Mortality_Summary_Table_2C.png"), table_grob, width = 7.5, height = 5, bg = "white")
ggsave(here::here("03_output", "visualizations_final", "NCHS_Infant_Mortality_Skyline_2C.png"), p_chart, width = 10, height = 7, dpi = 300, bg = "white")

message("Processing & Visualizations Complete!")