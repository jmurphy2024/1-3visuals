# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: ACS_nonstandard_job_prepare.R
## Purpose: Clean raw IPUMS ACS data and create a binary indicator for non-standard jobs.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-27

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #

USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "NonStandard_Job"

USER_HHID_VAR          <- "SERIAL"
USER_PERSON_WEIGHT_VAR <- "PERWT"
USER_HH_WEIGHT_VAR     <- "HHWT"
USER_INCOME_VAR        <- "HHINCOME"
USER_AGE_VAR           <- "AGE"

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

RAW_DATA_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID))
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

RAW_DATA_FILE <- file.path(RAW_DATA_DIR, "raw_data.rds")
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

if (!file.exists(RAW_DATA_FILE)) { stop("FATAL ERROR: Raw data file not found.") }
raw_data <- readRDS(RAW_DATA_FILE)

# --- 2.3. LOGIC VALIDATION: Universe & Cleaning ---
# We use as.character() then as.numeric() to strip haven_labelled metadata safely.
cleaned_data <- raw_data %>%
  mutate(
    AGE      = as.numeric(as.character(AGE)),
    UHRSWORK = as.numeric(as.character(UHRSWORK)),
    HHINCOME = if_else(as.numeric(as.character(HHINCOME)) == 9999999, NA_real_, as.numeric(as.character(HHINCOME)))
  ) %>%
  # UNIVERSE: 16+ who are currently working
  filter(AGE >= 16, !is.na(UHRSWORK), UHRSWORK > 0)

# --- 2.4. LOGIC VALIDATION: Numerator vs Denominator ---
prepared_data <- cleaned_data %>%
  mutate(
    # NUMERATOR: Non-standard/Part-time (< 35 hours per week)
    # DENOMINATOR: All workers (16+) in the workforce
    ind_nonstandard = if_else(UHRSWORK < 35, 1, 0)
  )

# --- 2.5. Save Processed Data ---
essential_cols <- c(
  USER_HHID_VAR, USER_HH_WEIGHT_VAR, USER_PERSON_WEIGHT_VAR, USER_INCOME_VAR, USER_AGE_VAR,
  "ind_nonstandard", "SAMPLE", "MULTYEAR"
)
final_prepared_data <- prepared_data %>% select(any_of(essential_cols))

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)
message("\n--- Data preparation complete. ---")