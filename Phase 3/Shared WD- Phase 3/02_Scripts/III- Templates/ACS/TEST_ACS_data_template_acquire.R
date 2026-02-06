# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: TEST_ACS_data_template_acquire.R
## Purpose: A standardized template for downloading IPUMS ACS microdata for a new indicator (TEST MODE).
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Last Modified: 2025-12-17
## Dependencies: ipumsr, dplyr, here, purrr
## Input: User-defined IPUMS parameters.
## Output: A raw RDS data file and a text codebook in `01_data/raw/IPUMS_Microdata/`.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)

# Source the shared utility functions to get the codebook generator.
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))


# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA DOWNLOAD ====
# ================================================================= #

# --- 1.1. Define IPUMS Extract Details ---
USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "TEST_Employment_Rate" # Added "TEST_" prefix

# --- 1.2. Define All Variables Needed for Your Analysis ---
USER_VARIABLES_NEEDED <- c(
  "SERIAL", "HHWT", "PERWT", "HHINCOME", "AGE", "RACE", "HISPAN",
  "EMPSTAT" 
)

# --- 1.3. (Optional) Skip Download If Data Already Exists ---
USER_DDI_FILE_PATH <- NULL


# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

# --- 2.1. Define Output Directory ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 2.2. Acquire Data ---
if (is.null(USER_DDI_FILE_PATH) || !file.exists(USER_DDI_FILE_PATH)) {
  message("--- DDI file path not provided or invalid, proceeding with IPUMS download ---")
  
  extract_def <- define_extract_micro(
    collection = USER_IPUMS_COLLECTION,
    samples = USER_IPUMS_SAMPLE_ID,
    variables = unique(USER_VARIABLES_NEEDED),
    description = paste("TEST RUN -", USER_INDICATOR_NAME, "-", Sys.Date())
  )
  
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  
  downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)
  ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
  
} else {
  message(paste("--- Skipping IPUMS download. Using provided DDI file:", USER_DDI_FILE_PATH, "---"))
  ddi_file_path <- USER_DDI_FILE_PATH
}

# --- 2.3. Generate Codebook and Save Raw Data as RDS ---
if (length(ddi_file_path) > 0 && file.exists(ddi_file_path[1])) {
  ddi_file_path <- ddi_file_path[1]
  
  codebook_path <- file.path(OUTPUT_RAW_DIR, paste0("codebook_", USER_INDICATOR_NAME, ".txt"))
  generate_codebook_from_ddi(ddi_file_path, USER_VARIABLES_NEEDED, codebook_path)
  
  raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
  
  # UPDATED: Renamed to TEST_raw_data.rds to separate from production data
  rds_output_path <- file.path(OUTPUT_RAW_DIR, "TEST_raw_data.rds")
  saveRDS(raw_data, file = rds_output_path)
  message(paste("\nTEST raw data and codebook saved successfully to:", OUTPUT_RAW_DIR))
  
} else {
  stop("FATAL ERROR: DDI file (.xml) not found after download/check.")
}

message("\n--- Data acquisition script complete. ---")