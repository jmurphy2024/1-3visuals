## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: ASEC_assets_acquire.R
## Purpose: Acquires 2025 ASEC data for Net Asset components.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #

USER_INDICATOR_NAME    <- "Net Assets by Income"
USER_ASEC_SAMPLE_ID    <- "cps2023_03s" 

# Final Validated IPUMS API Variables
USER_ASEC_VARIABLES <- c(
  "SERIAL",    # Household Identifier
  "PERNUM",    # Person Number 
  "ASECWT",    # Annual Supplement Weight
  "HHINCOME",  # Total Household Income (for grouping)
  "INCINT",    # Interest Income
  "INCRENT",   # Rent Income
  "INCDIVID",  # Dividend Income
  "ASECWTH"    # Household Weight
)

# --- 2. API DOWNLOAD ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_INDICATOR_NAME))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

extract_def <- define_extract_micro(
  collection = "cps", samples = USER_ASEC_SAMPLE_ID, variables = USER_ASEC_VARIABLES,
  description = "Net Assets by Income components"
)

submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR)

ddi_file <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
data <- read_ipums_micro(ddi = ddi_file, verbose = FALSE)

# Standardized Naming Convention
saveRDS(data, file.path(OUTPUT_RAW_DIR, paste0("raw_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds")))
message("SUCCESS: Net Assets raw data acquired.")