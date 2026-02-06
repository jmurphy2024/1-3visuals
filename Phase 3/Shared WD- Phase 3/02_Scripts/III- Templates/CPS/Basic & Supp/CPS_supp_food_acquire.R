# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: ACS_foodwater_acquire.R
## Purpose: Acquires ACS 5-year data for Food (FS), Public Assistance (PAP), and Water (WATP).
## Aligned with 2019-2023 ACS PUMS Data Dictionary.
## Author: Janica Murphy / Gemini
## Date Created: 2026-01-21


# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA DOWNLOAD ====
# ================================================================= #

USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023b" # 5-Year Sample
USER_INDICATOR_NAME   <- "Food_Water_Security"

# Aligned with PUMS Data Dictionary variables
USER_VARIABLES_NEEDED <- c(
  "SERIAL",   # Household Serial Number
  "PERNUM",   # Person number within household
  "HHWT",     # Household weight
  "PERWT",    # Person weight
  "HHINCOME", # Total household income
  "FOODSTMP", # Harmonized 'FS' (SNAP recipiency)
  "INCWELFR",    # Harmonized Public assistance income (replaces 'PAP'/'PUBASST')
  "COSTWATR" # Harmonized Annual water cost (replaces 'WATP'/'WATER')
)

# ================================================================= #
# ==== 2. GENERIC LOGIC (No changes needed) ====
# ================================================================= #
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

extract_def <- define_extract_micro(
  collection = USER_IPUMS_COLLECTION,
  samples = USER_IPUMS_SAMPLE_ID,
  variables = unique(USER_VARIABLES_NEEDED),
  description = paste("Population-Based Provisioning -", USER_INDICATOR_NAME)
)

submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)

ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
saveRDS(raw_data, file = file.path(OUTPUT_RAW_DIR, "raw_data.rds"))

message("\n--- ACS 5-Year Data Acquisition Complete ---")