# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: ACS_hhincome_prepare.R
## Purpose: Clean raw IPUMS ACS data for household-level income analysis.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here)

USER_IPUMS_SAMPLE_ID  <- "us2023b"
USER_INDICATOR_NAME   <- "avg_hh_income"

# --- 2.1. Define File Paths ---
RAW_DATA_FILE <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID), "raw_data.rds")
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

raw_data <- readRDS(RAW_DATA_FILE)

# --- 2.2. LOGIC VALIDATION: Universe & Cleaning ---
message("Defining Universe: Households with valid income data...")
prepared_data <- raw_data %>%
  # UNIVERSE: Householder (PERNUM 1) to ensure the Household is the unit of analysis
  filter(PERNUM == 1) %>% 
  mutate(
    # Clean HHINCOME: 9999999 is N/A
    HHINCOME_clean = if_else(HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME))
  ) %>%
  filter(!is.na(HHINCOME_clean)) %>%
  select(SERIAL, HHWT, HHINCOME = HHINCOME_clean)

saveRDS(prepared_data, file.path(PROCESSED_DIR, paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds")))
message("\n--- Data preparation complete. ---")