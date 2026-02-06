# ==== 0. ABOUT ====
## Script: ACS_bottomelectricity_acquire.R
## Purpose: Pull IPUMS ACS microdata for electricity metrics.
## Author: Gemini / User
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

USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023a" 
USER_INDICATOR_NAME   <- "Electricity_Insecurity_Bottom"

# Define All Variables Needed
USER_VARIABLES_NEEDED <- c(
  "SERIAL", "HHWT", "PERWT", 
  "COSTELEC",   # Annual electricity cost
  "HHINCOME", "AGE", "RACE", "HISPAN"
)

USER_DDI_FILE_PATH <- NULL 

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

if (is.null(USER_DDI_FILE_PATH) || !file.exists(USER_DDI_FILE_PATH)) {
  message("--- Proceeding with IPUMS download ---")
  extract_def <- define_extract_micro(
    collection = USER_IPUMS_COLLECTION,
    samples = USER_IPUMS_SAMPLE_ID,
    variables = unique(USER_VARIABLES_NEEDED),
    description = paste("Electricity Insecurity -", Sys.Date())
  )
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)
  ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
} else {
  ddi_file_path <- USER_DDI_FILE_PATH
}

if (length(ddi_file_path) > 0 && file.exists(ddi_file_path[1])) {
  ddi_file_path <- ddi_file_path[1]
  codebook_path <- file.path(OUTPUT_RAW_DIR, paste0("codebook_", USER_INDICATOR_NAME, ".txt"))
  generate_codebook_from_ddi(ddi_file_path, USER_VARIABLES_NEEDED, codebook_path)
  raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
  saveRDS(raw_data, file = file.path(OUTPUT_RAW_DIR, "raw_data.rds"))
} else {
  stop("FATAL ERROR: DDI file not found.")
}

message("\n--- Data acquisition script complete. ---")