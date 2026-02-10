# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_Age_Prepare.R
# Purpose: Calculates Real Income and Assigns Terciles to ALL Persons.
# Output:  01_data/processed/IPUMS_Microdata/prepared_ACS_Population_Age_us2023c.rds
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(tibble); library(ipumsr)

# --- 1. SETUP ---
SAMPLE_ID <- "us2023c"
INDICATOR <- "Population_Age"
# Note: Points to the specific _age folder created in Acquire
RAW_FILE  <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", SAMPLE_ID, "_age"), "raw_data.rds")

if(!file.exists(RAW_FILE)) stop("Raw Age file not found. Run Acquire script first.")
raw_data <- readRDS(RAW_FILE)

# --- 2. RPP LOOKUP (Same as II-C) ---
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

# --- 3. CLEANING & CALCULATION ---
message("Processing Population Data...")

prepared_data <- raw_data %>%
  # Remove Puerto Rico
  filter(STATEFIP != 72) %>%
  mutate(STATEFIP = as.numeric(STATEFIP)) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  
  mutate(
    # 1. Clean Income (Household level)
    income_raw = as.numeric(HHINCOME),
    income_cleaned = if_else(income_raw >= 9999999 | income_raw < 0, NA_real_, income_raw),
    
    # 2. Adjust Factors
    adj_factor = if_else(as.numeric(ADJUST) > 100, as.numeric(ADJUST) / 1000000, as.numeric(ADJUST)),
    
    # 3. Calculate REAL INCOME (Assigned to everyone in the HH)
    REAL_INCOME = (income_cleaned * adj_factor) * (100 / coalesce(STATE_RPP, 100)),
    
    # 4. Clean Age
    age_clean = as.numeric(AGE)
  ) %>%
  # Filter out missing income/age
  filter(!is.na(REAL_INCOME), REAL_INCOME > 0, !is.na(age_clean))

# --- 4. SAVE ---
out_dir <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(prepared_data, file.path(out_dir, paste0("prepared_ACS_", INDICATOR, "_", SAMPLE_ID, ".rds")))

message(paste("Success. Saved", nrow(prepared_data), "person records."))