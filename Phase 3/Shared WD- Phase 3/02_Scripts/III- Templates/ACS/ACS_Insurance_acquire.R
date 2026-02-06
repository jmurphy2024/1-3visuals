# ==== 0. ABOUT ====
## Script: ACS_health_acquire.R
## Purpose: Download 2024 ACS data for Health Coverage analysis.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-29

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA DOWNLOAD ====
# ================================================================= #

USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2024a" # 2024 1-Year Sample
USER_INDICATOR_NAME   <- "Health_Coverage"

# --- 1.2. Variables from your Table ---
USER_VARIABLES_NEEDED <- c(
  "SERIAL", "PERNUM", 
  "HHWT", "PERWT", 
  "FTOTINC",  # Total Family Income (Requested)
  "AGE",      # For context/filtering if needed
  "HCOVANY",  # Any Health Coverage
  "HCOVPRIV",  # Private Health Coverage
  "HCOVPUB"   # Public Health Coverage
)

USER_DDI_FILE_PATH <- NULL

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

if (is.null(USER_DDI_FILE_PATH) || !file.exists(USER_DDI_FILE_PATH)) {
  message("--- Requesting 2024 ACS Data from IPUMS ---")
  
  extract_def <- define_extract_usa(
    description = paste("Health Coverage 2024 -", USER_INDICATOR_NAME),
    samples = USER_IPUMS_SAMPLE_ID,
    variables = unique(USER_VARIABLES_NEEDED)
  )
  
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)
  ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
  
} else {
  ddi_file_path <- USER_DDI_FILE_PATH
}

if (length(ddi_file_path) > 0 && file.exists(ddi_file_path[1])) {
  # Save raw data as RDS
  raw_data <- read_ipums_micro(ddi = ddi_file_path[1], verbose = FALSE)
  saveRDS(raw_data, file = file.path(OUTPUT_RAW_DIR, "raw_data.rds"))
  
  # Generate Codebook
  generate_codebook_from_ddi(ddi_file_path[1], USER_VARIABLES_NEEDED, file.path(OUTPUT_RAW_DIR, "codebook_health.txt"))
  
  message(paste("\nRaw 2024 data saved successfully to:", OUTPUT_RAW_DIR))
} else {
  stop("FATAL ERROR: DDI file not found.")
}