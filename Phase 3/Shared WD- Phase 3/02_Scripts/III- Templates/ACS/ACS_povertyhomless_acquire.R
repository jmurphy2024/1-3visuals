# ==== 0. ABOUT ====
## Script: ACS_poverty_acquire.R
## Purpose: Pulling IPUMS ACS microdata for poverty status and living arrangements.
## Author: Gemini / User
## Date Created: 2026-01-27

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# Source shared utilities (assumes path from your template)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# --- 1.1. Define IPUMS Extract Details ---
USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023a" 
USER_INDICATOR_NAME   <- "Poverty_Living_Arrangements"

# --- 1.2. Define Variables ---
# GQ: Group Quarters (701 = Emergency/transitional shelters)
# RELATE: Relationship to head (Identifying non-relatives/doubled-up)
# POVERTY: Percent of poverty threshold (below 100 = in poverty)
USER_VARIABLES_NEEDED <- c(
  "SERIAL", "HHWT", "PERWT", 
  "GQ", "RELATE", "POVERTY",
  "HHINCOME", "AGE", "RACE", "HISPAN"
)

# --- 2. ACQUISITION LOGIC ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

extract_def <- define_extract_micro(
  collection = USER_IPUMS_COLLECTION,
  samples = USER_IPUMS_SAMPLE_ID,
  variables = unique(USER_VARIABLES_NEEDED),
  description = paste("Poverty and Living Arrangements -", Sys.Date())
)

submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)

# Process and Save
ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
saveRDS(raw_data, file = file.path(OUTPUT_RAW_DIR, "raw_data.rds"))

message("\n--- Poverty acquisition script complete. ---")