# ==============================================================================
# SCRIPT: ACS_housing_burden_working_age.R
# Purpose: Generate 3-Country Skyline for Housing Cost Burden (Mortgage + Rent)
# Logic:   ACS Microdata + Exact Monthly Costs + Working-Age (18-64)
# Engine:  data.table for high-speed 15M+ row aggregation
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tibble)
library(data.table); library(scales); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT (2023 ACS)
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 
VARS_NEEDED <- c("SERIAL", "PERWT", "INCTOT", "STATEFIP", "AGE", "OWNERSHP", "RENTGRS", "OWNCOST")

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_housing_burden")
TARGET_FILE <- file.path(TARGET_DIR, "acs_housing_burden_raw.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", samples = USER_SAMPLE, variables = VARS_NEEDED,
    description = "Three Countries ACS Housing Cost Burden (Working-Age)"
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

# 3. CONSTRUCT TRUE HOUSEHOLD INCOME
# ------------------------------------------------------------------------------
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

# ACS uses 9999999 for Missing/NIU INCTOT 
hh_aggregated <- as_tibble(dt_raw[INCTOT != 9999999, .(AGGREGATED_HHINCOME = sum(INCTOT, na.rm = TRUE)), by = SERIAL])
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

# 4. NORMALIZATION & BURDEN MATH
# ------------------------------------------------------------------------------
prepared_data <- as_tibble(dt_raw) %>%
  left_join(hh_aggregated, by = "SERIAL") %>%
  mutate(
    PERWT = as.numeric(PERWT),
    
    # Isolate true monthly cost depending on if they Rent (2) or Own (1)
    # (Excludes NIU/Missing codes for both variables)
    monthly_cost = case_when(
      as.numeric(OWNERSHP) == 1 & as.numeric(OWNCOST) > 0 & as.numeric(OWNCOST) < 99999 ~ as.numeric(OWNCOST),
      as.numeric(OWNERSHP) == 2 & as.numeric(RENTGRS) > 0 & as.numeric(RENTGRS) < 9999 ~ as.numeric(RENTGRS),
      TRUE ~ NA_real_
    ),
    
    # Mathematically calculate the HUD Burden Ratio
    # (If household income is 0 but they pay housing costs, they are mathematically >100% burdened)
    annual_cost  = monthly_cost * 12,
    burden_ratio = case_when(
      !is.na(annual_cost) & AGGREGATED_HHINCOME > 0 ~ annual_cost / AGGREGATED_HHINCOME,
      !is.na(annual_cost) & AGGREGATED_HHINCOME <= 0 & annual_cost > 0 ~ 1.0, 
      TRUE ~ NA_real_
    ),
    
    # --- PILLAR LOGIC (THE 3 DEGREES OF HARDSHIP) ---
    `Cost Burdened (>30%)`     = if_else(!is.na(burden_ratio) & burden_ratio > 0.30, 1, 0),
    `Severely Burdened (>50%)` = if_else(!is.na(burden_ratio) & burden_ratio > 0.50, 1, 0),
    `Extremely Burdened (>70%)`= if_else(!is.na(burden_ratio) & burden_ratio > 0.70, 1, 0),
    
    # Composite Target: Anyone who meets the baseline HUD definition of Burdened (>30%)
    Burden_Composite = `Cost Burdened (>30%)`,
    
    # --- ONE-STEP REAL INCOME NORMALIZATION ---
    REAL_INCOME = AGGREGATED_HHINCOME * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # UNIVERSE FILTER: Working-age adults (18-64) with valid housing cost data
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), 
         !is.na(burden_ratio), as.numeric(AGE) >= 18, as.numeric(AGE) <= 64)

# 5. SUMMARY STATISTICS
# ------------------------------------------------------------------------------
message("\n=== HOUSING COST BURDEN SUMMARY (WORKING AGE) ===")
print(as.data.frame(get_country_summary(prepared_data, "Burden_Composite", "PERWT")))

# 6. VISUALIZATION
# ------------------------------------------------------------------------------
plot_caption <- paste0(
  "The Architecture of Housing Hardship (Working-Age Adults Only, 18-64):\n",
  "Calculated natively using exact monthly costs for Gross Rent (RENTGRS) and Selected Monthly Owner Costs (OWNCOST). ",
  "The pillars map the official HUD thresholds of unaffordability, demonstrating the percentage of working-age adults ",
  "spending more than 30%, 50%, and 70% of their total household income purely to maintain shelter."
)

p_pillars <- plot_economic_skyline_2(
  data           = prepared_data, 
  indicator_vars = c("Cost Burdened (>30%)", "Severely Burdened (>50%)", "Extremely Burdened (>70%)"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Experiencing Cost Burden (%)",
  plot_title     = "ACS_housing_burden_working_age_pillars",
  caption_text   = stringr::str_wrap(plot_caption, width = 130)
)
print(p_pillars)

composite_caption <- paste0(
  "Note: The Housing Burden Index represents the baseline percentage of working-age adults (18-64) who are 'Cost Burdened' ",
  "by official HUD definitions (spending >30% of total household income on rent or mortgage + utilities)."
)

p_index <- plot_economic_skyline(
  data          = prepared_data, 
  indicator_var = "Burden_Composite", 
  weight_var    = "PERWT", 
  y_axis_label  = "Overall Housing Cost Burden (>30%)",
  plot_title    = "ACS_housing_burden_working_age_composite",
  caption_text  = stringr::str_wrap(composite_caption, width = 130)
)
print(p_index)