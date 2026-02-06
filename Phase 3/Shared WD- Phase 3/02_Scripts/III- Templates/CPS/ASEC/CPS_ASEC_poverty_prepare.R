# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: CPS_ASEC_poverty_prepare.R
## Purpose: Recodes ASEC records for population-based poverty prevalence.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)

USER_INDICATOR_NAME <- "poverty_rate"
USER_ASEC_SAMPLE_ID <- "cps2023_03s"

# --- 1. LOAD RAW DATA ---
RAW_DATA_PATH <- here::here("01_data", "raw", "IPUMS_Microdata", 
                            paste0("cps_", USER_INDICATOR_NAME), 
                            paste0("raw_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))

if (!file.exists(RAW_DATA_PATH)) stop("File not found! Check the path: ", RAW_DATA_PATH)
raw_data <- readRDS(RAW_DATA_PATH)

# --- 2. PREVALENCE LOGIC ---
message("Defining Universe: All individuals with valid Weight + Income data...")

prepared_data <- raw_data %>%
  # UNIVERSE: Include everyone with a valid supplement weight
  filter(ASECWT > 0) %>% 
  mutate(
    # Clean Household Income: Handle ASEC N/A code 99999998/9
    HHINCOME_clean = if_else(as.numeric(HHINCOME) >= 99999998, NA_real_, as.numeric(HHINCOME)),
    
    # NUMERATOR: Below Poverty (Code 10)
    # DENOMINATOR: Everyone in the universe (including those codes 20-23)
    ind_poverty = if_else(as.numeric(POVERTY) == 10, 1, 0, missing = 0)
  ) %>%
  # Ensure income is known for visualization grouping
  filter(!is.na(HHINCOME_clean)) %>%
  select(SERIAL, PERNUM, ASECWT, HHINCOME = HHINCOME_clean, ind_poverty)

# --- 3. SAVE ---
PROCESSED_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))
saveRDS(prepared_data, PROCESSED_FILE)
message("Success: Prepared poverty data saved.")