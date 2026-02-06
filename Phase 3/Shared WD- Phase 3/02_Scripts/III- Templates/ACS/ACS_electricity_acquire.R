# ==== 0. ABOUT ====
## Script: ACS_electricity_acquire.R
## Purpose: Pulling IPUMS ACS microdata for electricity access and utility metrics.
## Author: Janica Murphy, Gemini / User
## Date Created: 2026-01-27
## Dependencies: ipumsr, dplyr, here, purrr

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)

# Source the shared utility functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA DOWNLOAD ====
# ================================================================= #

# --- 1.1. Define IPUMS Extract Details ---
USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023a" 
USER_INDICATOR_NAME   <- "Electricity_Metrics"

# --- 1.2. Define Variables ---
# COSTELEC: Annual electricity cost (0000 = No cost/no charge)
# KITCHEN: Used to identify households lacking basic powered appliances
# FUELHEAT: Primary heating fuel (used to identify households with no heat/power)
USER_VARIABLES_NEEDED <- c(
  "SERIAL", "HHWT", "PERWT", 
  "COSTELEC",   
  "KITCHEN",    
  "FUELHEAT",   
  "HHINCOME", "AGE", "RACE", "HISPAN"
)

# --- 1.3. (Optional) Skip Download If Data Already Exists ---
USER_DDI_FILE_PATH <- NULL 

# ================================================================= #
# ==== 2. GENERIC LOGIC (No changes needed) ====
# ================================================================= #

OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

if (is.null(USER_DDI_FILE_PATH) || !file.exists(USER_DDI_FILE_PATH)) {
  message("--- DDI file path not provided or invalid, proceeding with IPUMS download ---")
  
  extract_def <- define_extract_micro(
    collection = USER_IPUMS_COLLECTION,
    samples = USER_IPUMS_SAMPLE_ID,
    variables = unique(USER_VARIABLES_NEEDED),
    description = paste("Electricity Metrics Download -", Sys.Date())
  )
  
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)
  ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
  
} else {
  message(paste("--- Skipping IPUMS download. Using provided DDI file:", USER_DDI_FILE_PATH, "---"))
  ddi_file_path <- USER_DDI_FILE_PATH
}

if (length(ddi_file_path) > 0 && file.exists(ddi_file_path[1])) {
  ddi_file_path <- ddi_file_path[1]
  codebook_path <- file.path(OUTPUT_RAW_DIR, paste0("codebook_", USER_INDICATOR_NAME, ".txt"))
  generate_codebook_from_ddi(ddi_file_path, USER_VARIABLES_NEEDED, codebook_path)
  
  raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
  
  rds_output_path <- file.path(OUTPUT_RAW_DIR, "raw_data.rds")
  saveRDS(raw_data, file = rds_output_path)
  message(paste("\nRaw data and codebook saved successfully to:", OUTPUT_RAW_DIR))
  
} else {
  stop("FATAL ERROR: DDI file (.xml) not found.")
}

message("\n--- Data acquisition script complete. ---")