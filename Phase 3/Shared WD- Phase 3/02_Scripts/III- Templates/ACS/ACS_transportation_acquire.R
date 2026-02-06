# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: ACS_transportation_acquire.R
## Purpose: A standardized template for downloading IPUMS ACS microdata for a new indicator.
##          This script downloads the specified data and saves it as a raw RDS file.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Last Modified: 2025-10-02
## Dependencies: ipumsr, dplyr, here, purrr
## Input: User-defined IPUMS parameters.
## Output: A raw RDS data file and a text codebook in `01_data/raw/IPUMS_Microdata/`.

# ==== 0. SETUP ====
# This section loads necessary libraries. It should not need to be modified.
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
USER_INDICATOR_NAME   <- "Transportation_Mode"

# --- 1.2. Define All Variables Needed for Your Analysis ---
USER_VARIABLES_NEEDED <- c(
  "SERIAL", "HHWT", "PERWT", "HHINCOME", "AGE", "RACE", "HISPAN","TRANWORK"
)
# --- 1.3. (Optional) Skip Download If Data Already Exists ---
# If you have already downloaded the data, provide the full path to the .xml DDI file.
# Otherwise, leave it as NULL to perform the download.
USER_DDI_FILE_PATH <- NULL
# Example: USER_DDI_FILE_PATH <- here::here("01_data", "raw", "IPUMS_Microdata", "usa_us2023a", "usa_00001.xml")


# ================================================================= #
# ==== 2. GENERIC LOGIC (No changes needed below this line) ====
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
    description = paste("1/3 Country -", USER_INDICATOR_NAME, "-", Sys.Date())
  )
  
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  
  # IPUMS API now downloads into the specified folder directly.
  downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)
  ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
  
} else {
  message(paste("--- Skipping IPUMS download. Using provided DDI file:", USER_DDI_FILE_PATH, "---"))
  ddi_file_path <- USER_DDI_FILE_PATH
}

# --- 2.3. Generate Codebook and Save Raw Data as RDS ---
if (length(ddi_file_path) > 0 && file.exists(ddi_file_path[1])) {
  ddi_file_path <- ddi_file_path[1]
  
  # Generate and save a human-readable codebook in the raw data directory
  codebook_path <- file.path(OUTPUT_RAW_DIR, paste0("codebook_", USER_INDICATOR_NAME, ".txt"))
  generate_codebook_from_ddi(ddi_file_path, USER_VARIABLES_NEEDED, codebook_path)
  
  # Load the data using the DDI
  raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
  
  # Save the raw data as an RDS file for much faster loading in the next script
  rds_output_path <- file.path(OUTPUT_RAW_DIR, "raw_data.rds")
  saveRDS(raw_data, file = rds_output_path)
  message(paste("\nRaw data and codebook saved successfully to:", OUTPUT_RAW_DIR))
  
} else {
  stop("FATAL ERROR: DDI file (.xml) not found after download/check. Cannot proceed.")
}

message("\n--- Data acquisition script complete. ---")