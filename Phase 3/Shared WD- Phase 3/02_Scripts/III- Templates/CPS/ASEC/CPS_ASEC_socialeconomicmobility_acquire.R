# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: CPS_ASEC_socialeconomicmobility_acquire.R
## Purpose: Acquires 2023 ASEC data for Mobility Analysis.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# Standardized for folder and file consistency
USER_INDICATOR_NAME    <- "social_mobility"
USER_ASEC_SAMPLE_ID    <- "cps2023_03s" 

# Core variables for Economic, Social, and Geographic Pillars
USER_ASEC_VARIABLES <- c(
  "SERIAL", "PERNUM", "ASECWT", "HHINCOME", "AGE",
  "INCTOT", "INCWAGE", "EDUC", "OCC", "CLASSWKR", 
  "MIGRATE1", "METRO", "COUNTY", "ASECWTH"
)

# --- 2. API DOWNLOAD ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_INDICATOR_NAME))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

extract_def <- define_extract_micro(
  collection = "cps", samples = USER_ASEC_SAMPLE_ID, variables = USER_ASEC_VARIABLES,
  description = "Economic and Social Mobility 2023"
)

submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR)

# Save as RDS with standardized naming
ddi_file <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
data <- read_ipums_micro(ddi = ddi_file, verbose = FALSE)
saveRDS(data, file.path(OUTPUT_RAW_DIR, paste0("raw_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds")))

message("SUCCESS: Mobility raw data acquired.")