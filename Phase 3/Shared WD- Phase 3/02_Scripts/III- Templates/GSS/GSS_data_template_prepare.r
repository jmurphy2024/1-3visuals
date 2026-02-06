# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/GSS
## Script: GSS_data_template_prepare.R
## Purpose: A standardized template for cleaning raw GSS microdata.
##          UPDATED: Includes imputation for top-coded incomes to fix visualization.
## Author: Max Goshert, Janica Murphy, EPAG / Gemini 
## Date Created: 2025-10-28
## Last Modified: 2025-12-15 ## Added top-code jittering
## Dependencies: dplyr, here, readr, stringr, haven
## Input: A raw RDS file from 01_data/raw/GSS_Data/
## Output: A processed RDS file in `01_data/processed/GSS_Data/`

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(stringr); library(haven)

# Set seed for reproducibility of the random imputation
set.seed(123) 

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR VISUALIZATION ====
# ================================================================= #

USER_GSS_YEAR_ID     <- "GSS2024"  
USER_INDICATOR_NAME  <- "Happiness" 

USER_ID_VAR            <- "id"       
USER_PERSON_WEIGHT_VAR <- "wtssnrps" 
USER_INCOME_VAR        <- "real_inc_approx" 
USER_AGE_VAR           <- "age"      
USER_VSTRAT_VAR        <- "vstrat"   
USER_VPSU_VAR          <- "vpsu"

USER_GSS_INCOME_CAT_VAR <- "income16"


# =============================================================================== #
# ==== 2. GENERIC LOGIC ====
# =============================================================================== #

# --- 2.1. Define File Paths ---
RAW_DATA_DIR <- here::here("01_data", "raw", "GSS_Data", USER_GSS_YEAR_ID)
PROCESSED_DIR <- here::here("01_data", "processed", "GSS_Data")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

RAW_DATA_FILE <- file.path(RAW_DATA_DIR, paste0("raw_data_", USER_INDICATOR_NAME, ".rds"))
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_", USER_GSS_YEAR_ID, "_", USER_INDICATOR_NAME, ".rds"))

# --- 2.2. Load Raw Data (RDS) ---
if (!file.exists(RAW_DATA_FILE)) { 
  stop(paste("FATAL ERROR: Raw RDS file not found at:", RAW_DATA_FILE, 
             "\nDid you run the 'Acquire' script first?")) 
}

message("Loading raw GSS data from RDS file...")
raw_data <- readRDS(RAW_DATA_FILE)
names(raw_data) <- tolower(names(raw_data))
message("Raw GSS data loaded successfully.")

# --- 2.3. USER ACTION: Handle Missing/Special Values ---
gss_missing_codes <- c(-100, -99, -98, -97, -96, -95, -94, -93, -90, -80, -70, -60, -40)

message("Cleaning raw data by recoding special GSS values to NA...")
cleaned_data <- raw_data %>%
  mutate(
    across(where(is.numeric), ~ if_else(. %in% gss_missing_codes, NA_real_, .)),
    happy = as.numeric(happy)
  )
message("...Data cleaning complete.")

# --- 2.4. USER ACTION: Create Derived Indicator Variable ---
message("Creating derived variables for analysis...")
prepared_data <- cleaned_data %>%
  mutate(
    # 1. Convert GSS Income Codes (1-26) to Approximate Dollar Amounts
    # INCLUDES IMPUTATION FOR TOP CODE (26)
    real_inc_approx = case_when(
      income16 == 1 ~ 500,       # Under $1,000
      income16 == 2 ~ 2000,      # $1,000 to 2,999
      income16 == 3 ~ 3500,      # $3,000 to 3,999
      income16 == 4 ~ 4500,      # $4,000 to 4,999
      income16 == 5 ~ 5500,      # $5,000 to 5,999
      income16 == 6 ~ 6500,      # $6,000 to 6,999
      income16 == 7 ~ 7500,      # $7,000 to 7,999
      income16 == 8 ~ 9000,      # $8,000 to 9,999
      income16 == 9 ~ 11250,     # $10,000 to 12,499
      income16 == 10 ~ 13750,    # $12,500 to 14,999
      income16 == 11 ~ 16250,    # $15,000 to 17,499
      income16 == 12 ~ 18750,    # $17,500 to 19,999
      income16 == 13 ~ 21250,    # $20,000 to 22,499
      income16 == 14 ~ 23750,    # $22,500 to 24,999
      income16 == 15 ~ 27500,    # $25,000 to 29,999
      income16 == 16 ~ 32500,    # $30,000 to 34,999
      income16 == 17 ~ 37500,    # $35,000 to 39,999
      income16 == 18 ~ 45000,    # $40,000 to 49,999
      income16 == 19 ~ 55000,    # $50,000 to 59,999
      income16 == 20 ~ 67500,    # $60,000 to 74,999
      income16 == 21 ~ 82500,    # $75,000 to 89,999
      income16 == 22 ~ 100000,   # $90,000 to 109,999
      income16 == 23 ~ 120000,   # $110,000 to 129,999
      income16 == 24 ~ 140000,   # $130,000 to 149,999
      income16 == 25 ~ 160000,   # $150,000 to 169,999
      
      # --- IMPUTATION FIX: Distribute top earners between $170k and $650k ---
      income16 == 26 ~ runif(n(), min = 170000, max = 650000), 
      
      TRUE ~ NA_real_
    ),
    
    # 2. Create your Indicator (Happiness)
    is_very_happy = if_else(happy == 1, 1, 0, missing = NA_real_),
    indicator_to_plot = is_very_happy
  )

# --- 2.5. Finalize and Save Processed Data ---
essential_cols <- c(
  USER_ID_VAR, USER_PERSON_WEIGHT_VAR, 
  USER_INCOME_VAR, USER_AGE_VAR,
  USER_VSTRAT_VAR, USER_VPSU_VAR,
  "indicator_to_plot",
  "sex", "race", "hispanic", "educ", "degree",
  USER_GSS_INCOME_CAT_VAR
)

available_prepared_vars <- tolower(names(prepared_data))
essential_cols_lower <- tolower(essential_cols)
cols_to_select <- essential_cols_lower[essential_cols_lower %in% available_prepared_vars]

final_prepared_data <- prepared_data %>% 
  filter(!is.na(indicator_to_plot)) %>% 
  select(all_of(cols_to_select))

# Safety Check
if (!"indicator_to_plot" %in% names(final_prepared_data)) {
  stop("FATAL ERROR: 'indicator_to_plot' is missing from final data.")
}

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)

message(paste("SUCCESS: Data preparation complete."))
message(paste("Saved to:", PROCESSED_DATA_FILE))

rm(list=setdiff(ls(), lsf.str())); gc()