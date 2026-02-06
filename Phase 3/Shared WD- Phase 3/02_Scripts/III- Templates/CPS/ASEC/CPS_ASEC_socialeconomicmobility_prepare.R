# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)

USER_INDICATOR_NAME <- "social_mobility"
USER_ASEC_SAMPLE_ID <- "cps2023_03s"

# --- 1. LOAD RAW DATA ---
RAW_DATA_PATH <- here::here("01_data", "raw", "IPUMS_Microdata", 
                            paste0("cps_", USER_INDICATOR_NAME), 
                            paste0("raw_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))

if (!file.exists(RAW_DATA_PATH)) stop("File not found! Check path: ", RAW_DATA_PATH)
raw_data <- readRDS(RAW_DATA_PATH)

# --- 2. PREVALENCE & MOBILITY LOGIC ---
message("Processing 2023 ASEC into Mobility Pillars (Ages 25-64)...")

prepared_data <- raw_data %>%
  # UNIVERSE: Filter for prime working-age adults for a stable prevalence baseline
  filter(AGE >= 25 & AGE <= 64, ASECWT > 0) %>% 
  mutate(
    # Project Standard Income Cleaning
    HHINCOME_clean = if_else(as.numeric(HHINCOME) >= 99999998, NA_real_, as.numeric(HHINCOME)),
    
    # PILLAR 1: ECONOMIC (Earnings > Median)
    ind_high_earnings = if_else(as.numeric(INCWAGE) > median(as.numeric(INCWAGE), na.rm = TRUE), 1, 0, missing = 0),
    
    # PILLAR 2: SOCIAL (College Grad and Professional Class)
    ind_college_grad = if_else(as.numeric(EDUC) >= 111, 1, 0, missing = 0),
    ind_prof_class = if_else(as.numeric(CLASSWKR) %in% c(21:28), 1, 0, missing = 0)
  ) %>%
  filter(!is.na(HHINCOME_clean)) %>%
  select(SERIAL, PERNUM, ASECWT, HHINCOME = HHINCOME_clean, starts_with("ind_"))

# --- 3. SAVE ---
PROCESSED_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))
saveRDS(prepared_data, PROCESSED_FILE)
message("SUCCESS: Mobility processed for ", nrow(prepared_data), " individuals.")