# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: ACS_transportation_prepare.R
## Purpose: Clean raw IPUMS ACS data and create a binary indicator for private transport.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-23
## Dependencies: dplyr, readr, here, rlang, stringr

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA PREPARATION ====
# ================================================================= #

USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "Transportation_Mode"

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

RAW_DATA_FILE <- file.path(RAW_DATA_DIR, "raw_data.rds")
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

# --- 2.2. Load Raw Data ---
if (!file.exists(RAW_DATA_FILE)) { stop(paste("FATAL ERROR: Raw data file not found at:", RAW_DATA_FILE)) }
raw_data <- readRDS(RAW_DATA_FILE)

# --- 2.3. LOGIC VALIDATION: Universe & Cleaning ---
message("Cleaning data and defining Universe (Workers 16+)...")
cleaned_data <- raw_data %>%
  # UNIVERSE: Only include those 16+ who have a transportation mode (excludes N/A)
  filter(AGE >= 16, TRANWORK != 0) %>% 
  mutate(
    HHINCOME = if_else(HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME))
  )

# --- 2.4. LOGIC VALIDATION: Numerator vs Denominator ---
message("Creating binary prevalence indicator...")
prepared_data <- cleaned_data %>%
  mutate(
    # NUMERATOR: Private (10-Auto/Truck/Van, 20-Motorcycle)
    # DENOMINATOR: All workers remaining in the filtered dataset (includes public/active transit)
    ind_private = if_else(TRANWORK %in% c(10, 20), 1, 0)
  )

# --- 2.5. Finalize and Save Processed Data ---
essential_cols <- c(
  USER_HHID_VAR, USER_HH_WEIGHT_VAR, USER_PERSON_WEIGHT_VAR, USER_INCOME_VAR, USER_AGE_VAR,
  "ind_private", "SAMPLE", "MULTYEAR"
)
final_prepared_data <- prepared_data %>% select(any_of(essential_cols))

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)

# --- 2.6. Clean Up Memory ---
rm(list=setdiff(ls(), lsf.str())); gc()
message("\n--- Data preparation script complete. ---")