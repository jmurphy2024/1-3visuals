# ==== 0. ABOUT ====
# Script: ACS_hhincome_acquire.R
## Purpose: Acquires 2019-2023 5-year IPUMS ACS microdata.
## Author: Janica, Murphy, Max Goshert, EPAG / Gemini
## Date Created: 2025-09-24
## Last Modified: 2026-02-12 (Update to person-level by assigning household income to all in household)

# ==============================================================================
# SCRIPT: ACS_hhincome_acquire.R
# Purpose: Acquires inclusive person-level ACS data (All members, shared HH income).
# Logic: Includes PERWT for person-level weighting and uses the 5-year sample.
# ==============================================================================

rm(list = ls()); gc()
library(ipumsr); library(here)

# --- 1. CONFIG ---
# Use 'us2023c' for the 5-year inclusive sample to ensure statistical stability
ACS_SAMPLE_ID <- "us2023c" 

# Include PERWT (Person Weight) and PERNUM (to identify individuals)
vars <- c("HHINCOME", "HHWT", "PERWT", "STATEFIP", "PUMA", "ADJUST", "PERNUM")

# --- 2. DEFINE EXTRACT ---
message(paste("Defining extract for:", ACS_SAMPLE_ID))
extract_def <- define_extract_micro(
  collection = "usa",
  description = "Inclusive Person-Level Income Analysis (1/3 Country Project)",
  samples = ACS_SAMPLE_ID,
  variables = vars
)

# --- 3. SUBMIT & DOWNLOAD ---
message("Submitting extract request to IPUMS...")
submitted <- submit_extract(extract_def)
ready     <- wait_for_extract(submitted)

# Define download directory
download_dir <- here::here("01_data", "raw")
if(!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)

message("Downloading extract...")
files <- download_extract(ready, download_dir = download_dir, overwrite = TRUE)

# --- 4. LOAD AND SAVE RAW RDS ---
ddi   <- read_ipums_ddi(files[grep("\\.xml$", files)])
data  <- read_ipums_micro(ddi, verbose = FALSE)

# Ensure output directory exists
processed_dir <- here::here("01_data", "processed")
if(!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

saveRDS(data, file.path(processed_dir, "ipums_data_raw.rds"))

message("Acquisition Complete: Raw data saved to 01_data/processed/ipums_data_raw.rds")