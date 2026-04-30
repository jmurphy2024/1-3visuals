# ==============================================================================
# SCRIPT: ACS_housing_burden_2D.R
# Purpose: Generate 3-Country Skyline for Housing Cost Burden (Mortgage + Rent)
# Definition: Working-age adults spending >30%, >50%, and >70% of total income 
#             on shelter.
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
VARS_NEEDED <- c("SERIAL", "PERWT", "HHINCOME", "STATEFIP", "AGE", "OWNERSHP", "RENTGRS", "OWNCOST")
TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_housing_burden")
TARGET_FILE <- file.path(TARGET_DIR, "acs_housing_burden_raw.rds")

if (!file.exists(TARGET_FILE)) {
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", 
    description = "Three Countries ACS Housing Cost Burden (Working-Age, Native HHINCOME)",
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED
  )
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  raw_data <- readRDS(TARGET_FILE)
}

# 3. DATA ENGINEERING & CLEANING
# ------------------------------------------------------------------------------
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

dt_raw[, HHINCOME_clean := as.numeric(HHINCOME)]
dt_raw[HHINCOME_clean == 9999999, HHINCOME_clean := NA_real_]
dt_filtered <- dt_raw[HHINCOME_clean >= 0] 

# 4. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

prepared_data <- as_tibble(dt_filtered) %>%
  mutate(
    PERWT = as.numeric(PERWT),
    monthly_cost = case_when(
      as.numeric(OWNERSHP) == 1 & as.numeric(OWNCOST) > 0 & as.numeric(OWNCOST) < 99999 ~ as.numeric(OWNCOST),
      as.numeric(OWNERSHP) == 2 & as.numeric(RENTGRS) > 0 & as.numeric(RENTGRS) < 9999 ~ as.numeric(RENTGRS),
      TRUE ~ NA_real_
    ),
    annual_cost  = monthly_cost * 12,
    burden_ratio = case_when(
      !is.na(annual_cost) & HHINCOME_clean > 0 ~ annual_cost / HHINCOME_clean,
      !is.na(annual_cost) & HHINCOME_clean == 0 & annual_cost > 0 ~ 1.0, 
      TRUE ~ NA_real_
    ),
    
    `Cost Burdened (>30%)`     = if_else(!is.na(burden_ratio) & burden_ratio > 0.30, 1, 0),
    `Severely Burdened (>50%)` = if_else(!is.na(burden_ratio) & burden_ratio > 0.50, 1, 0),
    `Extremely Burdened (>70%)`= if_else(!is.na(burden_ratio) & burden_ratio > 0.70, 1, 0),
    Burden_Composite = `Cost Burdened (>30%)`,
    
    REAL_INCOME = HHINCOME_clean * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    decile = ntile(REAL_INCOME, 10),
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), 
         !is.na(burden_ratio), as.numeric(AGE) >= 18, as.numeric(AGE) <= 64)

# 5. SUMMARY STATISTICS (TERCILES & QUARTILES WITHIN)
# ------------------------------------------------------------------------------
message("\n=== HOUSING COST BURDEN SUMMARY ===")
overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Cost Burdened (>30%) (%)` = round((sum(Burden_Composite * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
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
    `Cost Burdened (>30%) (%)` = round((sum(Burden_Composite * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
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
ggsave(here::here("03_output", "visualizations_final", "ACS_Housing_Burden_Summary_Table_2D.png"), table_grob, width = 10, height = 6, bg = "white", dpi = 300)

# 6. VISUALIZATION EXECUTION (Minimalist 2D Format)
# ------------------------------------------------------------------------------
message("\nGenerating Minimalist Skyline Plots...")

p_pillars <- plot_economic_skyline_multi_2C(
  data           = prepared_data, 
  indicator_vars = c("Cost Burdened (>30%)", "Severely Burdened (>50%)", "Extremely Burdened (>70%)"), 
  weight_var     = "PERWT"
)
ggsave(here::here("03_output", "visualizations_final", "ACS_housing_burden_pillars_2D.png"), p_pillars, width = 10, height = 7, dpi = 300, bg = "white")

p_index <- plot_economic_skyline_2C(
  data          = prepared_data, 
  indicator_var = "Burden_Composite", 
  weight_var    = "PERWT"
)
ggsave(here::here("03_output", "visualizations_final", "ACS_housing_burden_composite_2D.png"), p_index, width = 10, height = 7, dpi = 300, bg = "white")

message("Processing & Visualizations Complete!")

print(p_index)