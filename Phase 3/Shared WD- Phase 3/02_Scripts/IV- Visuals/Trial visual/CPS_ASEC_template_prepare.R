# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: CPS_ASEC_template_prepare.R
## Purpose: Cleans and merges raw CPS ASEC and basic monthly data, then creates
##          derived variables for a final indicator (e.g., unemployment rates).
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Dependencies: dplyr, here, rlang
## Input: Raw RDS files for ASEC and basic monthly data from the 'acquire' script.
## Output: A processed RDS file in `01_data/processed/IPUMS_Microdata/` containing
##         the merged and prepared data.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)


# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA PREPARATION ====
# ================================================================= #

# --- 1.1. Define Sample IDs and Indicator Name (must match acquire script) ---
USER_INDICATOR_NAME          <- "Unemployment_Rates"
USER_ASEC_SAMPLE_ID          <- "cps2023_03s"
USER_BASIC_MONTHLY_SAMPLE_ID <- "cps2023_03b"

# --- 1.2. Define Core Variable Names ---
USER_HHID_VAR          <- "SERIAL"
USER_PERSON_WEIGHT_VAR <- "ASECWT"
USER_HH_WEIGHT_VAR     <- "ASECWTH"
USER_INCOME_VAR        <- "HHINCOME"
USER_AGE_VAR           <- "AGE"
USER_LINKING_KEY       <- "MARBASECIDP"


# =================================================================================== #
# ==== 2. GENERIC LOGIC (Modify cleaning & derivation in Sections 2.3 & 2.4) ====
# =================================================================================== #

# --- 2.1. Define File Paths ---
RAW_DATA_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID))
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_CPS_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))

# --- 2.2. Load Raw Data ---
raw_asec_data <- readRDS(file.path(RAW_DATA_DIR, "raw_asec_data.rds"))
raw_basic_data <- readRDS(file.path(RAW_DATA_DIR, "raw_basic_monthly_data.rds"))

# --- 2.3. USER ACTION: Clean Raw ASEC and Basic Monthly Data ---
message("Cleaning raw data...")
cleaned_asec_data <- raw_asec_data %>%
  mutate(
    HHINCOME = if_else(HHINCOME == 99999999, NA_real_, as.numeric(HHINCOME)),
    EMPSTAT = if_else(EMPSTAT == 0, NA_integer_, as.integer(EMPSTAT)),
    WHYPTLWK = if_else(WHYPTLWK == 0, NA_integer_, as.integer(WHYPTLWK)),
    WKSTAT = if_else(WKSTAT == 0, NA_integer_, as.integer(WKSTAT))
  )

cleaned_basic_data <- raw_basic_data %>%
  select(all_of(c(USER_LINKING_KEY, "UH_DSCWK_B2"))) %>%
  mutate(
    UH_DSCWK_B2 = if_else(UH_DSCWK_B2 == 0, NA_integer_, as.integer(UH_DSCWK_B2))
  )

# --- 2.4. Merge Data ---
message("Merging ASEC and Basic Monthly data...")
if(class(cleaned_asec_data[[USER_LINKING_KEY]]) != class(cleaned_basic_data[[USER_LINKING_KEY]])) {
  warning("Linking key types differ. Attempting conversion to character.")
  cleaned_asec_data[[USER_LINKING_KEY]] <- as.character(cleaned_asec_data[[USER_LINKING_KEY]])
  cleaned_basic_data[[USER_LINKING_KEY]] <- as.character(cleaned_basic_data[[USER_LINKING_KEY]])
}
merged_data <- left_join(cleaned_asec_data, cleaned_basic_data, by = USER_LINKING_KEY)

# --- 2.5. USER ACTION: Create Derived Indicator Variables ---
message("Creating derived variables for analysis...")
prepared_data <- merged_data %>%
  mutate(
    # Create the final variables you intend to plot.
    is_employed = if_else(EMPSTAT %in% c(1, 10, 12), 1, 0, missing = NA_real_),
    is_unemployed = if_else(EMPSTAT %in% c(20, 21, 22), 1, 0, missing = NA_real_),
    is_in_labor_force = if_else(EMPSTAT %in% c(1, 10, 12, 20, 21, 22), 1, 0, missing = NA_real_),
    is_part_time_economic = if_else(WKSTAT %in% c(11:15) & WHYPTLWK %in% c(1:4), 1, 0, missing = 0),
    is_marginally_attached = if_else(UH_DSCWK_B2 %in% 1:2, 1, 0, missing = 0)
  ) %>%
  # This part calculates two potential indicators. The user selects one in the `visualize` script.
  mutate(
    unemployment_rate_u3 = if_else(AGE >= 25, is_unemployed / is_in_labor_force, NA_real_),
    unemployment_rate_u6 = if_else(AGE >= 25,
                                   (is_unemployed + is_part_time_economic + is_marginally_attached) / (is_in_labor_force + is_marginally_attached),
                                   NA_real_)
  )

# --- 2.6. Finalize and Save Processed Data ---
essential_cols <- c(
  USER_HHID_VAR, USER_HH_WEIGHT_VAR, USER_PERSON_WEIGHT_VAR, USER_INCOME_VAR, USER_AGE_VAR,
  "RACE", "HISPAN", # Keep demographics for potential disaggregation
  "unemployment_rate_u3", "unemployment_rate_u6" # Keep the final derived variables
)
final_prepared_data <- prepared_data %>% select(any_of(essential_cols))

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)
message(paste("Data preparation complete. Final file saved to:", PROCESSED_DATA_FILE))
rm(list=setdiff(ls(), lsf.str())); gc()