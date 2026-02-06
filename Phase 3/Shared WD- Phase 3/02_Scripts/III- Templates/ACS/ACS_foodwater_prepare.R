# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## Script: ACS_provisioning_prepare.R
## Purpose: Clean IPUMS ACS 5-Year data with an analytical focus on 
##          Water Security (Access vs. Burden) and SNAP/PAP.
## Author: Janica Murphy / Gemini
## Last Modified: 2026-01-26

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "Food_Water_Security"

# Mapping per Data Dictionary [cite: 3, 5, 9]
USER_PERSON_WEIGHT_VAR <- "PERWT"    # Population Denominator
USER_INCOME_VAR        <- "HHINCOME"
USER_SNAP_VAR          <- "FOODSTMP" 
USER_PAP_VAR           <- "INCWELFR" 
USER_WATER_VAR         <- "COSTWATR" # WATP in dictionary
USER_WATER_FLAG        <- "WATFP"    # Water cost flag variable

# ================================================================= #
# ==== 2. DATA PREPARATION (Analytical Burden Focus) ====
# ================================================================= #
RAW_DATA_PATH <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID), "raw_data.rds")
raw_data <- readRDS(RAW_DATA_PATH)

message("Refining Tier 1: Calculating Water-to-Income Burden Ratios...")

prepared_data <- raw_data %>%
  # NEW UNIVERSE: Individuals aged 16 and older with valid weights
  filter(PERWT > 0 & AGE >= 16) %>% 
  mutate(
    # Clean Income (9999999 = N/A)
    HHINCOME_CLEAN = if_else(HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME)),
    
    # NUMERATOR 1: SNAP Recipiency (2 = Yes)
    ind_snap = if_else(FOODSTMP == 2, 1, 0),
    
    # NUMERATOR 2: Public Assistance Recipiency
    ind_pap  = if_else(INCWELFR > 0 & INCWELFR < 999999, 1, 0),
    
    # Calculate Water-to-Income Ratio (COSTWATR is annual)
    water_ratio = if_else(HHINCOME_CLEAN > 0, (as.numeric(COSTWATR) / HHINCOME_CLEAN), 0),
    
    # High Water Cost Burden (>=1% of household income)
    ind_water_high_burden = if_else(water_ratio >= 0.01, 1, 0),
    
    # Standard Water Expense (<1% of income)
    ind_water_standard_pay = if_else(COSTWATR > 0 & water_ratio < 0.01, 1, 0),
    
    # No Direct Expense ($0 reported)
    ind_no_direct_cost = if_else(COSTWATR == 0, 1, 0)
  )

# --- 3. SAVE PREPPED OBJECT ---
essential_cols <- c(
  "SERIAL", "PERWT", "HHINCOME_CLEAN", "ind_snap", "ind_pap", 
  "ind_water_high_burden", "ind_water_standard_pay", "ind_no_direct_cost"
)

PROCESSED_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_ACS_", USER_INDICATOR_NAME, ".rds"))
saveRDS(prepared_data %>% select(any_of(essential_cols)), file = PROCESSED_FILE)