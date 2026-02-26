# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_PUMAheatmap_acquire.R
# Purpose: Re-downloads ACS data including PUMA variable.
# Output:  01_data/raw/IPUMS_Microdata/usa_us2023b_puma/raw_data.rds
# ==============================================================================

rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# --- 1. CONFIGURATION ---
USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023c" 

# --- 2. DEFINE EXTRACT ---
extract_def <- define_extract_micro(
  description = paste("PUMA Level Race Data -", Sys.Date()),
  collection = USER_IPUMS_COLLECTION,
  samples = USER_IPUMS_SAMPLE_ID,
  variables = list(
    "STATEFIP", "PUMA", "COUNTYFIP", # <--- ADDED PUMA
    "YEAR", "SAMPLE", "SERIAL", "HHWT", "PERWT",
    "HHINCOME", "ADJUST", "PERNUM", "AGE", 
    "RACE", "HISPAN", "POVERTY"
  )
)

# --- 3. DOWNLOAD ---
raw_dir <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID, "_puma"))
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

if(Sys.getenv("IPUMS_API_KEY") != "") {
  message("Submitting extract...")
  submitted <- submit_extract(extract_def)
  ready <- wait_for_extract(submitted)
  downloaded <- download_extract(ready, download_dir = raw_dir, overwrite = TRUE)
  
  ddi_file <- downloaded[grep("\\.xml$", downloaded)]
  raw_data <- read_ipums_micro(ddi_file, verbose = FALSE)
  saveRDS(raw_data, file.path(raw_dir, "raw_data.rds"))
  message("Acquisition Complete.")
} else {
  stop("IPUMS_API_KEY not found.")
}