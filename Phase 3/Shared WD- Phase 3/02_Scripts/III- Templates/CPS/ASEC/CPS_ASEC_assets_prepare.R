# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: CPS_ASEC_assets_prepare.R
## Purpose: Clean raw IPUMS CPS ASEC data and create binary asset indicators.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)

USER_INDICATOR_NAME <- "Net Assets by Income"
USER_ASEC_SAMPLE_ID <- "cps2023_03s"

# --- 1. LOAD RAW DATA ---
RAW_DATA_PATH <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_INDICATOR_NAME), paste0("raw_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))
raw_data <- readRDS(RAW_DATA_PATH)

# --- 2. PREVALENCE & SECURITY LOGIC ---
message("Defining Universe: All individuals with valid Weight + Income data...")

prepared_data <- raw_data %>%
  # UNIVERSE: Everyone with a valid supplement weight
  filter(ASECWT > 0) %>% 
  mutate(
    # Clean Income: Handle CPS N/A code 99999998/9
    HHINCOME = if_else(as.numeric(HHINCOME) >= 99999998, NA_real_, as.numeric(HHINCOME)),
    
    # NUMERATOR/DENOMINATOR LOGIC: 
    # Binary Indicators (0/1). 0 represents everyone else in the universe.
    ind_int  = if_else(as.numeric(INCINT)   > 0 & as.numeric(INCINT)   < 99999999, 1, 0, missing = 0),
    ind_rent = if_else(as.numeric(INCRENT)  > 0 & as.numeric(INCRENT)  < 99999999, 1, 0, missing = 0),
    ind_div  = if_else(as.numeric(INCDIVID) > 0 & as.numeric(INCDIVID) < 99999999, 1, 0, missing = 0)
  ) %>%
  # Ensure income is known for visualization grouping
  filter(!is.na(HHINCOME)) %>%
  select(SERIAL, PERNUM, ASECWT, HHINCOME, ind_int, ind_rent, ind_div)

# --- 3. SAVE ---
PROCESSED_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))
saveRDS(prepared_data, PROCESSED_FILE)
message("Success: Prepared asset data saved.")