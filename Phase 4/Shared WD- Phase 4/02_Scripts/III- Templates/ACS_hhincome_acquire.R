# ==== 0. ABOUT ====
# Script: ACS_hhincome_acquire.R
## Purpose: Acquires 2019-2023 5-year IPUMS ACS microdata.
## Author: Janica, Murphy, Max Goshert, EPAG / Gemini
## Date Created: 2025-09-24
## Last Modified: 2026-02-06 (Updated for 5-Year Sample and ADJUST variable)

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# --- 1.1. USER INPUTS ---
USER_IPUMS_COLLECTION <- "usa"
# Updated to 2019-2023 5-year sample ID
USER_IPUMS_SAMPLE_ID  <- "us2023b" 
USER_INDICATOR_NAME  <- "avg_hh_income"

# UNIVERSE/DENOMINATOR CHECK: 
# Added 'ADJUST' for 5-year inflation normalization.
# Added 'PERNUM' to ensure we can filter for Householders (Denominator logic).
USER_VARIABLES_NEEDED <- c("SERIAL", "HHWT", "HHINCOME", "PERNUM", 
                           "STATEFIP", "PUMA", "AGE", "RACE", "ADJUST")

# --- 2.2. Acquire Data ---
extract_def <- define_extract_micro(
  collection = USER_IPUMS_COLLECTION,
  samples = USER_IPUMS_SAMPLE_ID,
  variables = unique(USER_VARIABLES_NEEDED),
  description = paste("ACS 5-Year Real Income Analysis -", Sys.Date())
)

submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, 
                                     download_dir = here::here("01_data", "raw"), 
                                     overwrite = TRUE)