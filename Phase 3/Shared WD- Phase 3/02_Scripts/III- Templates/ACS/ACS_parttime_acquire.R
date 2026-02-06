# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: ACS_nonstandard_job_acquire.R
## Purpose: A standardized template for downloading IPUMS ACS microdata for Non-Standard Jobs.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-27

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA DOWNLOAD ====
# ================================================================= #

USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "NonStandard_Job"

# --- 1.2. Variables ---
USER_VARIABLES_NEEDED <- c(
  "SERIAL", "HHWT", "PERWT", "HHINCOME", "AGE", "RACE", "HISPAN", 
  "UHRSWORK"  # Required for part-time logic
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
    description = paste("1/3 Country -", USER_INDICATOR_NAME, "-", Sys.Date())
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

message("\n--- Data acquisition complete. ---")