# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: CPS_ASEC_employmentrate_prepare.R
## Purpose: Clean raw IPUMS CPS ASEC data and create a binary employment indicator.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-23

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyselect)

USER_IPUMS_SAMPLE_ID  <- "cps2023_03s"
USER_INDICATOR_NAME   <- "Employment_Rate"

# --- 2.1. Define File Paths ---
RAW_DATA_FILE <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_IPUMS_SAMPLE_ID), "raw_data.rds")
PROCESSED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_CPS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

if (!file.exists(RAW_DATA_FILE)) stop("Raw data file not found!")
raw_data <- readRDS(RAW_DATA_FILE)

# --- 2.2. LOGIC VALIDATION: Universe & Numerator ---
prepared_data <- raw_data %>%
  # Robust renaming: Handle both upper and lower case names from IPUMS
  rename_with(toupper, everything()) %>% 
  rename(
    PERWT = ASECWT # Ensure we use the Supplement Weight for ASEC
  ) %>%
  # UNIVERSE: Civilian Non-Institutionalized Adults 16+
  filter(AGE >= 16) %>% 
  mutate(
    # Handle CPS missing income code 99999999
    HHINCOME = if_else(HHINCOME == 99999999, NA_real_, as.numeric(HHINCOME)),
    # NUMERATOR: Employed (10) or Has Job but not at work (12)
    # DENOMINATOR: Everyone in the universe (including unemployed/NILF)
    ind_employed = if_else(EMPSTAT %in% c(10, 12), 1, 0)
  )

# --- 2.3. Finalize and Save ---
saveRDS(prepared_data, file = PROCESSED_DATA_FILE)
message("Success: Prepared data saved for visualization.")