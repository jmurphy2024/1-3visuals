# ==== 0. ABOUT ====
## Script: II-C_Border_Setup_Adjusted.R
## Purpose: Definitive Household income borders for the 1/3 Country Project.
## Logic: Household Universe (PERNUM 1) | 5-Year Normalization (ADJUST)

# Clear environment
rm(list = ls()); gc() 

# ==== 1. PROJECT SETUP ====
library(ipumsr); library(dplyr); library(readr); library(srvyr)
library(survey); library(rlang); library(tibble); library(here); library(scales) 

# ====== 1.2. Parameters & File Paths ======
ACS_SAMPLE_ID  <- "us2023b" # 2019-2023 5-Year Sample
RAW_INCOME_VAR <- "HHINCOME"
ADJUSTED_VAR   <- "REAL_INCOME" 
HH_WEIGHT_VAR  <- "HHWT"

processed_data_dir  <- here::here("01_data", "processed")
income_borders_file <- file.path(processed_data_dir, "within_tercile_quantile_borders_2023.csv")
main_cutoffs_file   <- file.path(processed_data_dir, "main_tercile_cutoffs_2023.rds")

# ==== 2. DATA ACQUISITION ====
# ADJUST handles 5-year price normalization.
# PERNUM is required to ensure a Household-level denominator.
border_vars <- c(RAW_INCOME_VAR, HH_WEIGHT_VAR, "STATEFIP", "PUMA", "ADJUST", "PERNUM")

minimal_extract_def <- define_extract_micro(
  collection = "usa", 
  description = "2023 5-Year Household Borders", 
  samples = ACS_SAMPLE_ID, 
  variables = border_vars
)

message("Submitting 5-year extract...")
submitted_extract <- submit_extract(minimal_extract_def)
ready_extract     <- wait_for_extract(submitted_extract)
minimal_files     <- download_extract(ready_extract, download_dir = here::here("01_data", "raw"), overwrite = TRUE)

# ==== 2.3. Load Data with ipumsr cleaning ====
ddi_file   <- minimal_files[grep("\\.xml$", minimal_files)]

# lbl_clean() removes the 'labelled' class that causes math errors.
ipums_data <- read_ipums_micro(ddi_file, verbose = FALSE) %>% 
  lbl_clean()
# ==== 3. NORMALIZATION & DENOMINATOR CLEANING ====

# 3.1. Reference Tables
crosswalk_clean <- read_csv(here::here("01_data", "raw", "crosswalks", "MSA_PUMA_crosswalk.csv"), show_col_types = FALSE) %>%
  rename(STATEFIP = `State FIPS Code`, PUMA = `PUMA Code`) %>%
  mutate(across(c(STATEFIP, PUMA), as.numeric)) %>% # FIX: Numeric join types
  group_by(STATEFIP, PUMA) %>% slice_max(order_by = `Percent PUMA Population`, n = 1, with_ties = FALSE) %>% ungroup()

state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

# ==== 3.2. Apply Logic: Householders Only & ADJUST normalization ====

ipums_data_adjusted <- ipums_data %>%
  # UNIVERSE: PERNUM 1 ensures one record per household.
  filter(PERNUM == 1) %>% 
  mutate(
    # 1. Force numeric types for geographic joins
    across(c(STATEFIP, PUMA), as.numeric),
    
    # 2. Clean Income: 9999999 is N/A. Keep 0 for population-based denominator.
    income_raw = as.numeric(HHINCOME),
    income_cleaned = if_else(income_raw >= 9999998 | income_raw < 0, NA_real_, income_raw),
    
    # 3. ADJUST Normalization (6 implied decimals)
    # In us2023b, ADJUST usually looks like 1012345 (representing 1.012345).
    # If the raw value is already small (e.g., 1.01), DO NOT divide by 1,000,000 again.
    adj_factor = if_else(as.numeric(ADJUST) > 100, as.numeric(ADJUST) / 1000000, as.numeric(ADJUST))
  ) %>%
  left_join(crosswalk_clean, by = c("STATEFIP", "PUMA")) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>% 
  mutate(
    # 4. Final Real Income Calculation
    # Normalizes 5-year data into constant 2023 calendar-year purchasing power.
    REAL_INCOME = (income_cleaned * adj_factor) * (100 / coalesce(STATE_RPP, 100))
  ) %>%
  filter(!is.na(REAL_INCOME), !is.na(HHWT), HHWT > 0)

# ==== CRITICAL DIAGNOSTIC ====
# This will tell us if the raw data is the problem.
message("--- DATA UNIT CHECK ---")
message(paste("Raw HHINCOME Median:", median(ipums_data$HHINCOME, na.rm=T)))
message(paste("ADJUST Factor Median:", median(ipums_data_adjusted$adj_factor, na.rm=T)))
message(paste("Final REAL_INCOME Median:", median(ipums_data_adjusted$REAL_INCOME, na.rm=T)))
# ==== 4. CALCULATE HOUSEHOLD BORDERS ====

# 4.1. Strict Cleaning with Verification
# UNIVERSE/DENOMINATOR: PERNUM 1 ensures one record per household.
# We force numeric conversion one last time to ensure weights are valid.
ipums_ready <- ipums_data_adjusted %>%
  mutate(
    HHWT = as.numeric(HHWT),
    REAL_INCOME = as.numeric(REAL_INCOME)
  ) %>%
  filter(!is.na(HHWT), HHWT > 0, !is.na(REAL_INCOME))

# SAFETY CHECK: Stop if data is empty to prevent interpolation error
if (nrow(ipums_ready) == 0) {
  stop("FATAL ERROR: ipums_ready has 0 rows. Check PERNUM filter and REAL_INCOME calculation.")
}

message(paste("Processing Design with", nrow(ipums_ready), "Households..."))

# 4.2. Simplified Survey Design
# ids = ~1 treats the sample as a single cluster if PUMA-level variance is failing.
survey_design <- survey::svydesign(
  ids = ~1, 
  weights = ~HHWT, 
  data = ipums_ready
)

# 4.3. Calculate Weighted Household Tercile Cutoffs
# Use a basic weighted quantile to avoid the 't' method evaluation error entirely.
# This calculates the 33rd and 66th percentiles for the household population.
message("Calculating terciles using weighted quantile extraction...")

# We use the Hmisc or simple weighted.quantile logic to be robust
main_cutoffs_raw <- survey::svyquantile(
  ~REAL_INCOME, 
  design = survey_design, 
  quantiles = c(1/3, 2/3), 
  na.rm = TRUE,
  ci = FALSE
)

# Extracting values based on modern survey package structure
main_cutoffs <- list(
  main_cutoff1 = as.numeric(main_cutoffs_raw$REAL_INCOME[1]),
  main_cutoff2 = as.numeric(main_cutoffs_raw$REAL_INCOME[2])
)

# Save the final boundaries
saveRDS(main_cutoffs, here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds"))

message("SUCCESS: Household Borders Calculated.")
message(paste("T1 Cutoff:", scales::dollar(main_cutoffs$main_cutoff1)))

# ==== 5. CALCULATE WITHIN-TERCILE QUANTILE BORDERS (Groups_20) ====

message("Calculating 20-group quantiles within each household tercile...")

all_borders_df <- ipums_ready %>%
  mutate(income_tercile_group = case_when(
    REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Tercile 1 (Bottom)",
    REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Tercile 2 (Middle)",
    TRUE ~ "Tercile 3 (Top)"
  )) %>%
  group_by(income_tercile_group) %>%
  group_modify(~ {
    # Re-build design for the specific tercile subset
    tdesign <- survey::svydesign(ids = ~1, weights = ~HHWT, data = .x)
    
    # Calculate quantiles at 5% intervals
    q_probs <- seq(0.05, 0.95, by = 0.05)
    q_vals  <- survey::svyquantile(~REAL_INCOME, tdesign, quantiles = q_probs, na.rm = TRUE, ci = FALSE)
    
    # Robust extraction of numeric values from matrix or list
    numeric_cutoffs <- if(is.list(q_vals)) as.numeric(q_vals$REAL_INCOME[1,]) else as.numeric(q_vals[1,])
    
    tibble(
      QuantileGroup = "Groups_20", 
      QuantileProbability = q_probs, 
      CutoffValue = numeric_cutoffs
    )
  }) %>%
  rename(MainTercile = income_tercile_group) %>%
  ungroup()

# Save final CSV for Viz scripts
write_csv(all_borders_df, income_borders_file)

message("SUCCESS: within_tercile_quantile_borders_2023.csv has been created.")