# ==== 0. ABOUT ====
## Script: NHIS_data_template_acquire.R
## Purpose: Download NHIS 2018 data (Last year with public Income data).

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #

USER_IPUMS_COLLECTION <- "nhis"
# USING 2018: The last year with public Family Income data
USER_IPUMS_SAMPLES    <- c("ih2018") 
USER_INDICATOR_NAME   <- "Health_Insurance_Coverage"

# --- 1.2. Variables (Harmonized IPUMS Names) ---
USER_VARIABLES_NEEDED <- c(
  "YEAR", "SERIAL", "AGE", "SEX", "RACENEW", "HISPETH",
  
  # Weights & Design (2018 Standards)
  "PERWEIGHT",   # <--- 2018 uses PERWEIGHT, not SAMPWEIGHT
  "PSU", "STRATA",
  
  # Indicator & Income
  "HINOTCOVE",   # Harmonized Insurance Variable
  "INCFAM07ON"   # <--- The VALID Income Variable (Available in 2018)
)

USER_DDI_FILE_PATH <- NULL 

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

SAMPLES_TAG <- paste(USER_IPUMS_SAMPLES, collapse = "_")
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", SAMPLES_TAG))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

if (is.null(USER_DDI_FILE_PATH) || !file.exists(USER_DDI_FILE_PATH)) {
  message("--- Downloading NHIS 2018 Data ---")
  extract_def <- define_extract_micro(
    collection = USER_IPUMS_COLLECTION,
    samples = USER_IPUMS_SAMPLES,
    variables = unique(USER_VARIABLES_NEEDED),
    description = paste("NHIS 2018 -", USER_INDICATOR_NAME)
  )
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)
  ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
} else {
  ddi_file_path <- USER_DDI_FILE_PATH
}

if (length(ddi_file_path) > 0) {
  codebook_path <- file.path(OUTPUT_RAW_DIR, paste0("codebook_", USER_INDICATOR_NAME, ".txt"))
  if(exists("generate_codebook_from_ddi")) generate_codebook_from_ddi(ddi_file_path[1], USER_VARIABLES_NEEDED, codebook_path)
  
  raw_data <- read_ipums_micro(ddi = ddi_file_path[1], verbose = FALSE)
  saveRDS(raw_data, file = file.path(OUTPUT_RAW_DIR, "raw_data.rds"))
  message("Success: 2018 Raw data saved.")
} else {
  stop("FATAL ERROR: Download failed.")
}