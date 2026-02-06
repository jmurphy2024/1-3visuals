# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: ACS_hhincome_acquire.R
## Purpose: Acquires 2019-2023 5-year IPUMS ACS microdata for Income analysis.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# --- 1.1. USER INPUTS ---
USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023b" # 5-Year Sample (2019-2023)
USER_INDICATOR_NAME   <- "avg_hh_income"

USER_VARIABLES_NEEDED <- c("SERIAL", "HHWT", "PERWT", "HHINCOME", "PERNUM")

# --- 2.1. Define Output Directory ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 2.2. Acquire Data ---
extract_def <- define_extract_micro(
  collection = USER_IPUMS_COLLECTION,
  samples = USER_IPUMS_SAMPLE_ID,
  variables = unique(USER_VARIABLES_NEEDED),
  description = paste("ACS Household Income Analysis -", Sys.Date())
)

submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)

# --- 2.3. Save Raw Data as RDS ---
ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
saveRDS(raw_data, file = file.path(OUTPUT_RAW_DIR, "raw_data.rds"))

message("\n--- Data acquisition complete. ---")