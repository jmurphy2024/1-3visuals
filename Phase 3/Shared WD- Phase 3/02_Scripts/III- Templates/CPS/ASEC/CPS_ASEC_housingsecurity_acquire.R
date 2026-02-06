## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: ASEC_Housing_acquire.R
## Purpose: Final validated acquisition for Housing Security (Tenure & Assistance).
## Author: Janica Murphy, Max Goshert, EPAG / Gemini
## Created: January 21, 2026

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# ==== 1. USER INPUTS ====
USER_INDICATOR_NAME <- "Housing Security"
USER_ASEC_SAMPLE_ID <- "cps2023_03s" 

# Core validated variables for 2023 ASEC Housing Security
USER_ASEC_VARIABLES <- c(
  "SERIAL", "PERNUM", "ASECWT", "HHINCOME", "AGE",
  "OWNERSHP", # Core Tenure (Own/Rent)
  "RENTSUB",  # Gov't Rent Subsidy
  "PUBHOUS",  # Public Housing
  "MIGRATE1", # Residential Stability
  "ASECWTH"
)

# ==== 2. API DOWNLOAD ====
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", "cps_housing_2023")
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

extract_def <- define_extract_micro(
  collection = "cps", samples = USER_ASEC_SAMPLE_ID, variables = USER_ASEC_VARIABLES,
  description = "Housing Security 2023 - Validated Variable Set"
)

submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR)

ddi_file <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
data <- read_ipums_micro(ddi = ddi_file, verbose = FALSE)
saveRDS(data, file.path(OUTPUT_RAW_DIR, "raw_housing_data.rds"))

message("SUCCESS: Validated Housing data acquired.")