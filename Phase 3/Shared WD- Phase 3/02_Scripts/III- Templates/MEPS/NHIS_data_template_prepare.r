# ==== 0. ABOUT ====
## Script: NHIS_data_template_prepare.R
## Purpose: Clean NHIS 2018 data, jitter Income (EXTENDED RANGE), and rename.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)
set.seed(123)

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #
USER_IPUMS_COLLECTION <- "nhis"
USER_IPUMS_SAMPLES    <- c("ih2018") 
USER_INDICATOR_NAME   <- "Health_Insurance_Coverage"

USER_HHID_VAR          <- "SERIAL"
USER_PERSON_WEIGHT_VAR <- "WTFA_A"
USER_INCOME_VAR        <- "INC"    
USER_AGE_VAR           <- "AGE"

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

SAMPLES_TAG <- paste(USER_IPUMS_SAMPLES, collapse = "_")
RAW_DATA_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", SAMPLES_TAG))
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

RAW_DATA_FILE <- file.path(RAW_DATA_DIR, "raw_data.rds")
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_NHIS_", USER_INDICATOR_NAME, "_", SAMPLES_TAG, ".rds"))

if (!file.exists(RAW_DATA_FILE)) { stop("Raw data not found. Run Acquire first.") }
raw_data <- readRDS(RAW_DATA_FILE)

# --- 2.3. Clean & Rename ---
message("Cleaning and Renaming...")
cleaned_data <- raw_data %>%
  mutate(
    # 1. Weights
    WTFA_A = if_else(PERWEIGHT == 0, NA_real_, as.numeric(PERWEIGHT)),
    
    # 2. Design
    PPSU   = as.numeric(PSU),
    PSTRAT = as.numeric(STRATA),
    
    # 3. Income
    INC_RAW = if_else(INCFAM07ON >= 96, NA_integer_, as.integer(INCFAM07ON)),
    
    # 4. Indicator (1=No Coverage, 2=Yes Coverage)
    HINOTCOVE = if_else(HINOTCOVE %in% c(0, 7, 8, 9), NA_integer_, as.integer(HINOTCOVE))
  ) %>%
  filter(!is.na(WTFA_A))

# --- 2.4. Derive & Jitter ---
message("Jittering Income (Extended Tail)...")
prepared_data <- cleaned_data %>%
  mutate(
    # Indicator: 1 = Uninsured
    indicator_to_plot = if_else(HINOTCOVE == 1, 1, 0, missing = NA_real_),
    
    # Income Jittering (INCFAM07ON Codes - 2018)
    real_inc_approx = case_when(
      INC_RAW == 10 ~ runif(n(), 0, 29999),      
      INC_RAW == 11 ~ runif(n(), 0, 4999),
      INC_RAW == 12 ~ runif(n(), 5000, 9999),
      INC_RAW == 13 ~ runif(n(), 10000, 14999),
      INC_RAW == 14 ~ runif(n(), 15000, 19999),
      INC_RAW == 15 ~ runif(n(), 20000, 24999),
      INC_RAW == 16 ~ runif(n(), 25000, 29999),
      INC_RAW == 17 ~ runif(n(), 30000, 34999),
      INC_RAW == 18 ~ runif(n(), 35000, 39999),
      INC_RAW == 19 ~ runif(n(), 40000, 49999),
      INC_RAW == 20 ~ runif(n(), 50000, 59999),
      INC_RAW == 21 ~ runif(n(), 60000, 74999),
      
      # Overlap codes and Top Codes
      INC_RAW == 22 ~ runif(n(), 75000, 99999), 
      INC_RAW == 23 ~ runif(n(), 75000, 99999),
      
      # FIX: EXTEND THE TOP CODE TO $850k (Previously $250k)
      # This simulates the long tail of high earners
      INC_RAW >= 24 ~ runif(n(), 100000, 850000), 
      
      TRUE ~ NA_real_
    ),
    
    INC = real_inc_approx
  )

# --- 2.5. Finalize and Save ---
essential_cols <- c(
  USER_HHID_VAR, USER_PERSON_WEIGHT_VAR, 
  "INC", USER_AGE_VAR, 
  "indicator_to_plot",
  "PPSU", "PSTRAT" 
)

final_prepared_data <- prepared_data %>% 
  filter(!is.na(indicator_to_plot)) %>% 
  select(any_of(essential_cols))

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)
message(paste("Success! Data prepared and saved to:", PROCESSED_DATA_FILE))

# --- 2.6. Clean Up ---
rm(list=setdiff(ls(), lsf.str())); gc()