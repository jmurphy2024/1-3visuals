# ==============================================================================
# SCRIPT: ACS_health_insurance_2D.R
# Purpose: Generate 3-Country Skyline for Private vs Public Health Coverage
# Definition: Baseline percentage of working-age adults (18-64) with private, 
#             public, or no health insurance coverage.
# Engine:  Minimalist 2D (LOESS Smoothed Trend Lines)
# ==============================================================================
rm(list = ls()); gc()
options(expressions = 500000)

library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tibble)
library(data.table); library(scales); library(stringr); library(tidyr)
library(gridExtra); library(grid); library(Hmisc)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2D.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 
VARS_NEEDED <- c("SERIAL", "PERWT", "HHINCOME", "STATEFIP", "AGE", "HCOVANY", "HCOVPUB", "HCOVPRIV")
TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_health_macro_native_hh")
TARGET_FILE <- file.path(TARGET_DIR, "acs_health_macro_native_hh.rds")

if (!file.exists(TARGET_FILE)) {
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", 
    description = "Three Countries ACS Private vs Public Coverage (Native HHINCOME)",
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED
  )
  submitted <- submit_extract(extract_def)
  ready <- wait_for_extract(submitted)
  files <- download_extract(ready, download_dir = TARGET_DIR, overwrite = TRUE)
  data <- read_ipums_micro(read_ipums_ddi(files[grep("\\.xml$", files)]), verbose = FALSE)
  saveRDS(data, TARGET_FILE)
} else {
  data <- readRDS(TARGET_FILE)
}

# 3. DATA ENGINEERING & CLEANING
# ------------------------------------------------------------------------------
dt_raw <- as.data.table(data)
dt_raw[, HHINCOME_clean := as.numeric(HHINCOME)]
dt_raw[HHINCOME_clean == 9999999, HHINCOME_clean := NA_real_]
dt_filtered <- dt_raw[HHINCOME_clean >= 0]

# 4. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

prepared_data <- as_tibble(dt_filtered) %>%
  mutate(
    PERWT = as.numeric(PERWT),
    `Uninsured`        = if_else(as.numeric(HCOVANY) == 1, 1, 0),
    `Public Coverage`  = if_else(as.numeric(HCOVPUB) == 2, 1, 0),
    `Private Coverage` = if_else(as.numeric(HCOVPRIV) == 2, 1, 0),
    Health_Uninsured_Composite = `Uninsured`,
    REAL_INCOME = HHINCOME_clean * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    decile = ntile(REAL_INCOME, 10),
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), as.numeric(AGE) >= 18, as.numeric(AGE) <= 64)

# 5. SUMMARY STATISTICS (TERCILES & QUARTILES WITHIN)
# ------------------------------------------------------------------------------
message("\n=== HEALTHCARE ACCESS SUMMARY ===")
overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Uninsured (%)` = round((sum(Health_Uninsured_Composite * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
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
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Uninsured (%)` = round((sum(Health_Uninsured_Composite * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  ) %>%
  select(-Quartile)

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(Total_Population = scales::comma(Total_Population))

print(as.data.frame(macro_summary))

dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)
table_grob <- tableGrob(macro_summary, rows = NULL, theme = ttheme_default(
  core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)), 
  colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))
))
ggsave(here::here("03_output", "visualizations_final", "ACS_health_Summary_Table_2D.png"), table_grob, width = 10, height = 6, bg = "white", dpi = 300)

# 6. VISUALIZATION EXECUTION (Minimalist 2D Format)
# ------------------------------------------------------------------------------
message("\nGenerating Minimalist Skyline Plots...")

plot_data <- prepared_data %>% rename(`Public Coverage\n\n` = `Public Coverage`)

p_pillars <- plot_economic_skyline_multi_2C(
  data           = plot_data, 
  indicator_vars = c("Private Coverage", "Public Coverage\n\n", "Uninsured"), 
  weight_var     = "PERWT"
)
ggsave(here::here("03_output", "visualizations_final", "ACS_health_private_vs_public_pillars_2D.png"), p_pillars, width = 10, height = 7, dpi = 300, bg = "white")

p_index <- plot_economic_skyline_2C(
  data          = prepared_data, 
  indicator_var = "Health_Uninsured_Composite", 
  weight_var    = "PERWT"
)
ggsave(here::here("03_output", "visualizations_final", "ACS_health_uninsured_composite_2D.png"), p_index, width = 10, height = 7, dpi = 300, bg = "white")

message("Processing & Visualizations Complete!")
print(p_index)