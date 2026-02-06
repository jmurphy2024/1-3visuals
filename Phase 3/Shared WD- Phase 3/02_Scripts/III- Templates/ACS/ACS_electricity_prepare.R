# ==== 0. ABOUT ====
## Script: ACS_electricity_prepare.R
## Purpose: Clean raw IPUMS ACS data and create binary indicators for electricity insecurity.
## Author: Janica Murphy, Gemini / User
## Last Modified: 2026-01-27
## Dependencies: dplyr, readr, here, rlang, stringr

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA PREPARATION ====
# ================================================================= #

USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "Electricity_Metrics"

USER_HHID_VAR          <- "SERIAL"
USER_PERSON_WEIGHT_VAR <- "PERWT"
USER_HH_WEIGHT_VAR     <- "HHWT"
USER_INCOME_VAR        <- "HHINCOME"

# =============================================================================== #
# ==== 2. GENERIC LOGIC ====
# =============================================================================== #

# --- 2.1. Define File Paths ---
RAW_DATA_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID))
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

RAW_DATA_FILE <- file.path(RAW_DATA_DIR, "raw_data.rds")
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

# --- 2.2. Load Raw Data ---
if (!file.exists(RAW_DATA_FILE)) { stop(paste("FATAL ERROR: Raw data file not found at:", RAW_DATA_FILE)) }
raw_data <- readRDS(RAW_DATA_FILE)

# --- 2.3. Data Cleaning & Normalization ---
message("Cleaning data and handling missing values...")
cleaned_data <- raw_data %>%
  mutate(
    # Clean Household Income (9999999 is N/A)
    HHINCOME = if_else(HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME)),
    # Clean Electricity Cost (9999 is N/A; 0 is No Cost)
    COSTELEC = if_else(COSTELEC >= 9992, NA_real_, as.numeric(COSTELEC))
  )

# --- 2.4. Indicator Logic (The Metrics) ---
message("Creating electricity metric indicators...")
prepared_data <- cleaned_data %>%
  mutate(
    # 1. NO ELECTRICITY: Households reporting < $100 in annual electricity costs.
    # This captures households that are effectively unpowered or off-grid.
    ind_no_electricity = if_else(COSTELEC > 0 & COSTELEC < 100, 1, 0),
    
    # 2. AT RISK: Energy Burdened (Electricity costs > 10% of total household income)
    energy_burden = if_else(HHINCOME > 0, COSTELEC / HHINCOME, 0),
    ind_at_risk = if_else(energy_burden > 0.10, 1, 0)
  )

# --- 2.5. Finalize and Save Processed Data ---
essential_cols <- c(
  USER_HHID_VAR, USER_HH_WEIGHT_VAR, USER_PERSON_WEIGHT_VAR, USER_INCOME_VAR,
  "ind_no_electricity", "ind_at_risk", 
  "SAMPLE", "MULTYEAR"
)
final_prepared_data <- prepared_data %>% select(any_of(essential_cols))

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)

# --- 2.6. Clean Up Memory ---
rm(list=setdiff(ls(), lsf.str())); gc()
message("\n--- Electricity preparation script complete (Steal Proxy omitted). ---")