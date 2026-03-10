# ==============================================================================
## Script: ACS_poverty_acquire.R
# Purpose: Acquires person-level data including Poverty Status for "Three Countries" analysis.
# Logic: Includes POVERTY, HHINCOME, and PERWT for weighted representation.
# ==============================================================================

rm(list = ls()); gc()
library(ipumsr); library(here)

# --- 1. CONFIG ---
ACS_SAMPLE_ID <- "us2023c" # 5-Year Sample

# Core variables + POVERTY
vars <- c("AGE", "HHINCOME", "POVERTY", "HHWT", "PERWT", "STATEFIP", "PUMA", "ADJUST", "PERNUM")

# --- 2. DEFINE EXTRACT ---
message(paste("Defining poverty-inclusive extract for:", ACS_SAMPLE_ID))
extract_def <- define_extract_micro(
  collection = "usa",
  description = "Three Countries: Poverty Rate Analysis",
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

# --- 4. LOAD AND SAVE ---
ddi   <- read_ipums_ddi(files[grep("\\.xml$", files)])
data  <- read_ipums_micro(ddi, verbose = FALSE)

processed_dir <- here::here("01_data", "processed")
if(!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

saveRDS(data, file.path(processed_dir, "ipums_poverty_raw.rds"))
message("Acquisition Complete.")