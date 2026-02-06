# ==== 0. ABOUT ====
## Script: ACS_hhincome_prepare.R
## Purpose: Prepare 2023 5-Year ACS data for HH Income analysis.
## Logic: Household Universe (PERNUM 1) | Normalization: ADJUST Factor

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(readr); library(here); library(tidyr)

# Load shared functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# --- 1.1. Parameters ---
USER_IPUMS_SAMPLE_ID <- "us2023b"
USER_INDICATOR_NAME  <- "avg_hh_income"

# --- 1.2. Paths ---
processed_dir <- here("01_data", "processed")
raw_dir       <- here("01_data", "raw")
# Load the borders derived from the $15,904.48 logic
borders_path  <- file.path(processed_dir, "main_tercile_cutoffs_2023.rds")
main_cutoffs  <- readRDS(borders_path)

# ==== 2. LOAD DATA ====
ddi_file <- list.files(raw_dir, pattern = "\\.xml$", full.names = TRUE) %>% sort() %>% last()
data_raw <- read_ipums_micro(ddi_file, verbose = FALSE) %>% lbl_clean()

# ==== 3. HARMONIZATION & NORMALIZATION ====
data_prepared <- data_raw %>%
  # UNIVERSE: Householders only to represent unique households
  filter(PERNUM == 1) %>%
  mutate(
    across(c(STATEFIP, PUMA), as.numeric),
    
    # 1. Clean nominal income
    income_raw = as.numeric(HHINCOME),
    income_cleaned = if_else(income_raw >= 9999998 | income_raw < 0, NA_real_, income_raw),
    
    # 2. Apply ADJUST factor for 5-year calendar normalization
    adj_multiplier = if_else(as.numeric(ADJUST) > 100, as.numeric(ADJUST) / 1000000, as.numeric(ADJUST)),
    
    # 3. Calculate REAL_INCOME (Normalized Household Income)
    REAL_INCOME = income_cleaned * adj_multiplier,
    
    # Variable name required by the Viz template logic
    indicator_to_plot = REAL_INCOME 
  ) %>%
  filter(!is.na(REAL_INCOME)) %>%
  # 4. Assign Terciles
  mutate(
    income_tercile_label = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "T1: Bottom 1/3",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "T2: Middle 1/3",
      TRUE ~ "T3: Top 1/3"
    )
  )

# ==== 4. SAVE ====
OUTPUT_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                          paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))
saveRDS(data_prepared, OUTPUT_FILE)
message("Preparation complete. Household data saved to processed folder.")