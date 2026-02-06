# ===================================================================
# TEMPLATE 1a: DATA ACQUISITION (IPUMS ACS)
# ===================================================================
# Project: 1/3 Country Project Visualizations
# Author: Janica Murphy/ Gemini
# Date: 2025-10-01
#
# Purpose:
# This script template pulls data from the IPUMS API for an ACS sample.
# Its only job is to download the data and save it as an .rds file
# to prevent needing to re-download. This is the first step in the
# modular workflow.
# ===================================================================

# ==== 0. SETUP & PARAMETERS ====

# ---- 0.1 Load Core Packages ----
library(ipumsr)
library(dplyr)
library(here)
library(readr)

# ---- 0.2 Source Shared Functions ----
shared_functions_path <- here::here("II_Shared_Functions.R")
if (file.exists(shared_functions_path)) {
  source(shared_functions_path)
}

# ---- 0.3 USER-DEFINED PARAMETERS (ACS) ----
IPUMS_COLLECTION <- "usa"
IPUMS_SAMPLE_ID <- "us2023a"
VARIABLES_TO_EXTRACT <- c(
  "YEAR", "SAMPLE", "SERIAL", "HHWT", "PERWT", "CLUSTER", "STRATA", "HHINCOME",
  "OWNCOST", "RENTGRS", "AGE", "RACE", "HISPAN", "CITIZEN", "SEX", "EDUCD",
  "EMPSTAT", "POVERTY", "HWSEI", "OCC", "OWNERSHP", "BEDROOMS", "NUMPREC",
  "KITCHEN", "HOTWATER", "VEHICLES", "UNITSSTR", "MOVEDIN", "METRO",
  "TRANTIME", "TRANWORK", "CIHISPEED", "CINETHH", "CILAPTOP", "CISMRTPHN",
  "CITABLET", "HCOVANY"
)
VARIABLES_TO_EXTRACT <- unique(VARIABLES_TO_EXTRACT)

# ---- 0.4 Define File Paths ----
extract_download_parent_dir <- here::here("data", "ipums_extracts")
dir.create(extract_download_parent_dir, showWarnings = FALSE, recursive = TRUE)


# ==== 1. DEFINE & SUBMIT IPUMS EXTRACT ====
extract_definition <- define_extract_micro(
  collection = IPUMS_COLLECTION,
  samples    = IPUMS_SAMPLE_ID,
  variables  = VARIABLES_TO_EXTRACT,
  description = paste("ACS Template extract for", IPUMS_SAMPLE_ID, Sys.Date())
)

# ---- 1.2 SET UP YOUR API KEY (ONE-TIME ACTION) ----
# To use the IPUMS API, you need to set your API key.
# DO NOT PASTE YOUR KEY DIRECTLY INTO THIS SCRIPT.
# Instead, run the following command ONCE in your R Console.
# This will save your key securely for all future R sessions.

# >>> Run this in your Console:
# ipumsr::set_ipums_api_key("59cba10d8a5da536fc06b59d54447f708bfd48398e401fd99a06ed35", save = TRUE)

# ---- 1.3 Submit the Extract to IPUMS ----
if (Sys.getenv("IPUMS_API_KEY") == "") {
  stop("IPUMS API key not found. Please run the command in section 1.2 in your console.")
}
submitted_extract <- submit_extract(extract_definition)


# ==== 2. DOWNLOAD & LOAD DATA ====
downloadable_extract <- wait_for_extract(submitted_extract)
download_subdir_name <- paste0("ACS_", IPUMS_SAMPLE_ID, "_Extract_", submitted_extract$number, "_", Sys.Date())
download_dir_specific <- file.path(extract_download_parent_dir, download_subdir_name)
dir.create(download_dir_specific, recursive = TRUE)
downloaded_files <- download_extract(downloadable_extract, download_dir = download_dir_specific, overwrite = TRUE)

ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
ipums_data <- read_ipums_micro(ddi = ddi_file_path[1], verbose = FALSE)


# ==== 3. SAVE RAW DATA AS .RDS CHECKPOINT ====
# This is the key output of this script.
raw_data_output_path <- file.path(download_dir_specific, paste0(IPUMS_SAMPLE_ID, "_raw_data.rds"))
print(paste("Saving raw ACS data checkpoint to:", raw_data_output_path))
saveRDS(ipums_data, file = raw_data_output_path)

print("--- ACS Data Acquisition Template Finished ---")

