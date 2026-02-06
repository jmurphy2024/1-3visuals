# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: TEST_ACS_data_template_prepare.R
## Purpose: A standardized template for cleaning raw TEST IPUMS microdata.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Last Modified: 2025-12-17
## Dependencies: dplyr, here, rlang
## Input: A raw RDS data file from the corresponding 'acquire' script.
## Output: A processed RDS file in `01_data/processed/IPUMS_Microdata/`.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)


# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA PREPARATION ====
# ================================================================= #

# --- 1.1. Define IPUMS Sample and Indicator Name ---
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "TEST_Employment_Rate" # Added "TEST_" prefix

# --- 1.2. Define Core Variable Names ---
USER_HHID_VAR          <- "SERIAL"
USER_PERSON_WEIGHT_VAR <- "PERWT"
USER_HH_WEIGHT_VAR     <- "HHWT"
USER_INCOME_VAR        <- "HHINCOME"
USER_AGE_VAR           <- "AGE"


# =============================================================================== #
# ==== 2. GENERIC LOGIC ====
# =============================================================================== #

# --- 2.1. Define File Paths ---
RAW_DATA_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID))
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

# UPDATED: Points to the TEST raw file
RAW_DATA_FILE <- file.path(RAW_DATA_DIR, "TEST_raw_data.rds")
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

# --- 2.2. Load Raw Data ---
if (!file.exists(RAW_DATA_FILE)) { stop(paste("FATAL ERROR: TEST raw data file not found at:", RAW_DATA_FILE)) }
# Loads full dataset
raw_data <- readRDS(RAW_DATA_FILE) 
message("Full TEST IPUMS data loaded successfully.")

# --- 2.3. USER ACTION: Handle Missing/Special Values ---
message("Cleaning data...")
cleaned_data <- raw_data %>%
  mutate(
    HHINCOME = if_else(HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME)),
    EMPSTAT = if_else(EMPSTAT %in% c(0, 9), NA_integer_, as.integer(EMPSTAT))
  )

# --- 2.4. USER ACTION: Create Derived Indicator Variable ---
message("Creating derived variables...")
prepared_data <- cleaned_data %>%
  mutate(
    indicator_to_plot = if_else(AGE >= 25 & AGE <= 65 & EMPSTAT == 1, 1, 0, missing = NA_real_)
  )

# --- 2.5. Finalize and Save Processed Data ---
essential_cols <- c(
  USER_HHID_VAR, USER_HH_WEIGHT_VAR, USER_PERSON_WEIGHT_VAR, USER_INCOME_VAR, USER_AGE_VAR,
  "indicator_to_plot", "SAMPLE", "MULTYEAR"
)
final_prepared_data <- prepared_data %>% select(any_of(essential_cols))

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)
message(paste("Full Data preparation complete. TEST file saved to:", PROCESSED_DATA_FILE))

# --- 2.6. Clean Up Memory ---
rm(list=setdiff(ls(), lsf.str())); gc()