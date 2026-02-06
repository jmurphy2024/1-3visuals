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

# --- 2. INCOME SHARE LOGIC (The Fix for 'share_int' not found) ---
prepared_data <- raw_data %>%
  filter(ASECWT > 0) %>% 
  mutate(
    # Clean Household Income
    HHINCOME = if_else(as.numeric(HHINCOME) >= 99999998, NA_real_, as.numeric(HHINCOME)),
    
    # Clean asset components (Convert N/A 99999999 to 0)
    clean_int  = if_else(as.numeric(INCINT)   >= 99999999, 0, as.numeric(INCINT)),
    clean_rent = if_else(as.numeric(INCRENT)  >= 99999999, 0, as.numeric(INCRENT)),
    clean_div  = if_else(as.numeric(INCDIVID) >= 99999999, 0, as.numeric(INCDIVID)),
    
    # MATHEMATICAL SHARE: Component / Total Income
    share_int  = if_else(HHINCOME > 0, clean_int / HHINCOME, 0),
    share_rent = if_else(HHINCOME > 0, clean_rent / HHINCOME, 0),
    share_div  = if_else(HHINCOME > 0, clean_div / HHINCOME, 0)
  ) %>%
  filter(!is.na(HHINCOME) & HHINCOME > 0) %>% 
  select(SERIAL, PERNUM, ASECWT, HHINCOME, share_int, share_rent, share_div)

# --- 3. SAVE ---
PROCESSED_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))
saveRDS(prepared_data, PROCESSED_FILE)
message("Success: Prepared income share data saved.")