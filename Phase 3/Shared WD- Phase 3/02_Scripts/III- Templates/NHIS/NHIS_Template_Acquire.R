# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA DOWNLOAD ====
# ================================================================= #

USER_IPUMS_COLLECTION <- "nhis"
# CHANGE: Use 2014. This is the latest sample with public mortality variables.
USER_IPUMS_SAMPLE_ID  <- "ih2014"  
USER_INDICATOR_NAME   <- "Life_Expectancy"

USER_VARIABLES_NEEDED <- c(
  "SERIAL", "PERNUM", "NHISPID",
  "HHWEIGHT", "PERWEIGHT",
  "INCFAM07ON", 
  "AGE",        
  "YEAR",       
  "MORTELIG",   
  "MORTSTAT",   
  "MORTDODY",   
  "MORTWT"      
)

USER_DDI_FILE_PATH <- NULL

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

if (is.null(USER_DDI_FILE_PATH) || !file.exists(USER_DDI_FILE_PATH)) {
  message("--- DDI file path not provided or invalid, proceeding with IPUMS download ---")
  
  extract_def <- define_extract_micro(
    collection = USER_IPUMS_COLLECTION,
    samples = USER_IPUMS_SAMPLE_ID,
    variables = unique(USER_VARIABLES_NEEDED),
    description = paste("Life Expectancy Analysis -", USER_IPUMS_SAMPLE_ID)
  )
  
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR, overwrite = TRUE)
  ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
  
} else {
  ddi_file_path <- USER_DDI_FILE_PATH
}

if (length(ddi_file_path) > 0 && file.exists(ddi_file_path[1])) {
  raw_data <- read_ipums_micro(ddi = ddi_file_path[1], verbose = FALSE)
  saveRDS(raw_data, file = file.path(OUTPUT_RAW_DIR, "raw_data.rds"))
  message(paste("\nRaw data saved successfully to:", OUTPUT_RAW_DIR))
} else {
  stop("FATAL ERROR: DDI file not found.")
}