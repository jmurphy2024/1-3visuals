# ==== 0. ABOUT ====
## WD location: 02_Scripts/IV-Visuals/ACS & CPS
## Script: acquire_ACS-ChildPop_for_dual_axis.R
## Purpose: Acquires IPUMS ACS microdata for calculating the child population count
##          for the dual-axis child enrollment visualization.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Dependencies: ipumsr, dplyr, here, stringr
## Output: A raw RDS data file and codebook in `01_data/raw/IPUMS_Microdata/`.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(stringr); library(purrr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ==== 1. PARAMETERS ====
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "Child_Population"
USER_VARIABLES_NEEDED <- c("SERIAL", "HHWT", "PERWT", "HHINCOME", "AGE", "RACE", "HISPAN")

# ==== 2. ACQUISITION LOGIC ====
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

message(paste("\n--- Acquiring data for:", USER_INDICATOR_NAME, "---"))
extract_def <- define_extract_micro("usa", samples = USER_IPUMS_SAMPLE_ID, variables = unique(USER_VARIABLES_NEEDED), description = paste("1/3 Country -", USER_INDICATOR_NAME))
submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR)

ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
codebook_path <- file.path(OUTPUT_RAW_DIR, paste0("codebook_", USER_INDICATOR_NAME, ".txt"))
generate_codebook_from_ddi(ddi_file_path, USER_VARIABLES_NEEDED, codebook_path)

raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
saveRDS(raw_data, file = file.path(OUTPUT_RAW_DIR, "raw_data.rds"))

message(paste("\nRaw ACS data and codebook saved successfully to:", OUTPUT_RAW_DIR))
message("\n--- ACS data acquisition script complete. ---")