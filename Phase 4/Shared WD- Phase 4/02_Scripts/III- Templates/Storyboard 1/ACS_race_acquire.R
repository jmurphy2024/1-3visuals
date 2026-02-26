# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_race_acquire.R
## Purpose: Acquires inclusive person-level data for Race analysis.
# Logic: Includes RACE, HISPAN, and PERWT for full population representation.
# ==============================================================================


rm(list = ls()); gc()
library(ipumsr); library(here)

# --- 1. CONFIG ---
ACS_SAMPLE_ID <- "us2023c" # 5-Year Sample

# Essential variables for Race and Income distribution
vars <- c("RACE", "HISPAN", "HHINCOME", "HHWT", "PERWT", "STATEFIP", "PUMA", "ADJUST", "PERNUM")

# --- 2. DEFINE EXTRACT ---
message(paste("Defining race-inclusive extract for:", ACS_SAMPLE_ID))
extract_def <- define_extract_micro(
  collection = "usa",
  description = "Inclusive Race and Income Analysis (1/3 Country Project)",
  samples = ACS_SAMPLE_ID,
  variables = vars
)

# --- 3. SUBMIT & DOWNLOAD ---
message("Submitting extract request...")
submitted <- submit_extract(extract_def)
ready     <- wait_for_extract(submitted)

download_dir <- here::here("01_data", "raw")
if(!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)

message("Downloading extract...")
files <- download_extract(ready, download_dir = download_dir, overwrite = TRUE)

# --- 4. LOAD AND SAVE RAW RDS ---
ddi   <- read_ipums_ddi(files[grep("\\.xml$", files)])
data  <- read_ipums_micro(ddi, verbose = FALSE)

processed_dir <- here::here("01_data", "processed")
if(!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

saveRDS(data, file.path(processed_dir, "ipums_data_raw.rds"))

message("Acquisition Complete: Raw data saved for Race analysis.")