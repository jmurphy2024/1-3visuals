## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: ASEC_Stability_prepare.R
## Purpose: Recodes ASEC records for 4-Anchor faceted analysis by Education.
##          Applies AGE >= 25 filter for prime-age stability analysis.
## Author: Max Goshert, EPAG / Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)

# --- 1. LOAD RAW DATA ---
RAW_DATA_PATH <- here::here("01_data", "raw", "IPUMS_Microdata", "cps_Economic Stability Portfolios by Education", "raw_stability_data.rds")
raw_data <- readRDS(RAW_DATA_PATH)

# --- 2. PREVALENCE & SECURITY LOGIC ---
message("Processing observations into 4 Anchor Pillars (Age 25+)...")

prepared_data <- raw_data %>%
  filter(ASECWT > 0) %>% 
  # --- ADDED AGE FILTER ---
  filter(as.numeric(AGE) >= 25) %>% 
  mutate(
    # Clean Household Income
    HHINCOME_clean = if_else(as.numeric(HHINCOME) >= 99999998, NA_real_, as.numeric(HHINCOME)),
    
    # Pillar 1: Full-Year Work (50-52 weeks)
    ind_full_year = if_else(as.numeric(WKSWORK1) >= 50, 1, 0),
    
    # Pillar 2: Public Retirement/SS Income
    ind_public_retirement = if_else(
      (as.numeric(INCRETIR) > 0 & as.numeric(INCRETIR) < 999999) | 
        (as.numeric(INCSS) > 0 & as.numeric(INCSS) < 99999), 1, 0, missing = 0
    ),
    
    # Pillar 3: Private Asset Income
    ind_private_assets = if_else(
      (as.numeric(INCDIVID) > 0 & as.numeric(INCDIVID) < 999999) | 
        (as.numeric(INCINT) > 0 & as.numeric(INCINT) < 999999) | 
        (as.numeric(INCRENT) > 0 & as.numeric(INCRENT) < 999999), 1, 0, missing = 0
    ),
    
    # Pillar 4: Homeownership
    ind_homeowner = if_else(as.numeric(OWNERSHP) == 10, 1, 0),
    
    # EDUCATION DIVISIONS
    edu_group = case_when(
      as.numeric(EDUC) <= 073 ~ "1. HS or Lower",
      as.numeric(EDUC) == 111 ~ "2. Bachelors",
      as.numeric(EDUC) >= 123 ~ "3. Masters & Up",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(edu_group)) %>%
  select(SERIAL, PERNUM, AGE, ASECWT, HHINCOME = HHINCOME_clean, edu_group, starts_with("ind_"))

# --- 3. SAVE ---
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
saveRDS(prepared_data, file.path(PROCESSED_DIR, "prepared_stability_mosaic_2023.rds"))