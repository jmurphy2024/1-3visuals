# ==== 0. ABOUT ====
## Script: II-C_Border_Setup_Adjusted.R
## Purpose: Definitive Household income borders for the 1/3 Country Project.
## Logic: Household Universe (PERNUM 1) | 5-Year Normalization (ADJUST) | RPP Adjusted
## Updated: 2026-02-09 | Added survey-based weighted diagnostics and demo extraction.

# Clear environment
rm(list = ls()); gc() 

# ==== 1. PROJECT SETUP ====
library(ipumsr); library(dplyr); library(readr); library(srvyr)
library(survey); library(rlang); library(tibble); library(here); library(scales) 

# ====== 1.2. Parameters & File Paths ======
ACS_SAMPLE_ID  <- "us2023c" # 2019-2023 5-Year Sample
RAW_INCOME_VAR <- "HHINCOME"
ADJUSTED_VAR   <- "REAL_INCOME" 
HH_WEIGHT_VAR  <- "HHWT"

processed_data_dir  <- here::here("01_data", "processed")
income_borders_file <- file.path(processed_data_dir, "within_tercile_quantile_borders_2023.csv")
main_cutoffs_file   <- file.path(processed_data_dir, "main_tercile_cutoffs_2023.rds")

# ==== 2. DATA ACQUISITION ====
# Ensure we get demographics and inflation factors
border_vars <- c(RAW_INCOME_VAR, HH_WEIGHT_VAR, "STATEFIP", "PUMA", "ADJUST", "PERNUM", "AGE", "RACE")

minimal_extract_def <- define_extract_micro(
  collection = "usa", 
  description = "2023 5-Year Household Borders with Diagnostics", 
  samples = ACS_SAMPLE_ID, 
  variables = border_vars
)

message("Submitting 5-year extract...")
submitted_extract <- submit_extract(minimal_extract_def)
ready_extract     <- wait_for_extract(submitted_extract)
minimal_files     <- download_extract(ready_extract, download_dir = here::here("01_data", "raw"), overwrite = TRUE)

# ==== 2.3. Load Data ====
ddi_file   <- minimal_files[grep("\\.xml$", minimal_files)]
ipums_data <- read_ipums_micro(ddi_file, verbose = FALSE) %>% lbl_clean()

# ==== 3. NORMALIZATION & GEOGRAPHIC CLEANING ====

# 3.1. Reference Tables
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

# ==== 3.2. Apply Logic & Household Filtering ====

ipums_data_adjusted <- ipums_data %>%
  # CRITICAL: PERNUM 1 ensures we analyze households, not individuals
  filter(PERNUM == 1) %>% 
  mutate(
    across(c(STATEFIP, PUMA), as.numeric),
    
    # 1. Clean HHINCOME
    income_raw = as.numeric(HHINCOME),
    income_cleaned = if_else(income_raw >= 9999999 | income_raw < 0, NA_real_, income_raw),
    
    # 2. ADJUST Normalization (Handles 5-year inflation)
    adj_factor = if_else(as.numeric(ADJUST) > 100, as.numeric(ADJUST) / 1000000, as.numeric(ADJUST)),
    
    # 3. Demographics for Head of Household
    AGE = as.numeric(AGE),
    RACE_label = as_factor(RACE)
  ) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>% 
  mutate(
    # 4. Final Real Income Calculation (Price-Adjusted)
    REAL_INCOME = (income_cleaned * adj_factor) * (100 / coalesce(STATE_RPP, 100))
  ) %>%
  filter(!is.na(REAL_INCOME), REAL_INCOME > 0, !is.na(HHWT), HHWT > 0)

# ==== 4. SURVEY DESIGN & DIAGNOSTICS ====

# Create weighted survey object
survey_design <- survey::svydesign(ids = ~1, weights = ~HHWT, data = ipums_data_adjusted)

# Calculate weighted median for validation
median_check <- survey::svyquantile(~REAL_INCOME, design = survey_design, quantiles = 0.5, na.rm = TRUE, ci = FALSE)
weighted_median_val <- as.numeric(median_check$REAL_INCOME)

message("\n--- CRITICAL UNIT VALIDATION ---")
message(paste("Total Households in Sample: ", scales::comma(nrow(ipums_data_adjusted))))
message(paste("Weighted Median Real Income:", scales::dollar(weighted_median_val)))
message("--------------------------------\n")

# ALERT: National 5-year sample should be ~6M households
if(nrow(ipums_data_adjusted) < 1000000) {
  message("!!! WARNING: Sample size is extremely low. Check your IPUMS extract geographic selection.")
}

# ==== 5. CALCULATE HOUSEHOLD BORDERS (1/3 SPLITS) ====

# Calculate weighted 33.3% and 66.6% quantiles
main_cutoffs_raw <- survey::svyquantile(
  ~REAL_INCOME, design = survey_design, quantiles = c(1/3, 2/3), na.rm = TRUE, ci = FALSE
)

main_cutoffs <- list(
  main_cutoff1 = as.numeric(main_cutoffs_raw$REAL_INCOME[1]),
  main_cutoff2 = as.numeric(main_cutoffs_raw$REAL_INCOME[2])
)

saveRDS(main_cutoffs, main_cutoffs_file)

message("---------------------------------------------------------")
message("SUCCESS: 2023 5-Year Main Household Borders Calculated")
message(paste(">>> T1/T2 Cutoff (Bottom to Middle):", scales::dollar(main_cutoffs$main_cutoff1)))
message(paste(">>> T2/T3 Cutoff (Middle to Top):   ", scales::dollar(main_cutoffs$main_cutoff2)))
message("---------------------------------------------------------")

# ==== 6. CALCULATE WITHIN-TERCILE QUANTILE BORDERS ====

all_borders_df <- ipums_data_adjusted %>%
  mutate(income_tercile_group = case_when(
    REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Tercile 1 (Bottom)",
    REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Tercile 2 (Middle)",
    TRUE ~ "Tercile 3 (Top)"
  )) %>%
  group_by(income_tercile_group) %>%
  group_modify(~ {
    tdesign <- survey::svydesign(ids = ~1, weights = ~HHWT, data = .x)
    q_probs <- seq(0.05, 0.95, by = 0.05)
    q_vals  <- survey::svyquantile(~REAL_INCOME, tdesign, quantiles = q_probs, na.rm = TRUE, ci = FALSE)
    numeric_cutoffs <- if(is.list(q_vals)) as.numeric(q_vals$REAL_INCOME[1,]) else as.numeric(q_vals[1,])
    
    tibble(QuantileGroup = "Groups_20", QuantileProbability = q_probs, CutoffValue = numeric_cutoffs)
  }) %>%
  ungroup()

write_csv(all_borders_df, income_borders_file)
message("SUCCESS: within_tercile_quantile_borders_2023.csv created.")