# ===================================================================
# TEMPLATE 1b: DATA ACQUISITION (IPUMS CPS)
# ===================================================================
# Project: 1/3 Country Project Visualizations
# Author: Gemini
# Date: 2025-10-01
#
# Purpose:
# This script template pulls data from IPUMS CPS. It downloads TWO
# datasets required for imputation workflows: the target CPS supplement
# and a corresponding ASEC "donor" file. It saves both as separate
# .rds checkpoints.
# ===================================================================

# ==== 0. SETUP & PARAMETERS ====

# ---- 0.1 Load Core Packages ----
library(ipumsr); library(dplyr); library(here); library(readr)

# ---- 0.2 Source Shared Functions ----
source(here::here("II_Shared_Functions.R"))

# ---- 0.3 USER-DEFINED PARAMETERS (CPS) ----
IPUMS_COLLECTION <- "cps"
# --- Target CPS Supplement ---
CPS_SUPPLEMENT_SAMPLE_ID <- "cps2023_10s" # Oct 2023 Education Supplement
CPS_SUPPLEMENT_VARIABLES <- c(
  "YEAR", "SERIAL", "PERNUM", "CPSID", "CPSIDP", "EDSUPPWT", "FAMINC",
  "AGE", "SEX", "RACE", "HISPAN", "EDATT", "EDVOCA"
)
# --- Donor ASEC file ---
CPS_ASEC_SAMPLE_ID <- "cps2023_03s" # March 2023 ASEC
CPS_ASEC_VARIABLES <- c("SERIAL", "PERNUM", "ASECWT", "HHINCOME")

# ---- 0.4 Define File Paths ----
extract_download_parent_dir <- here::here("data", "ipums_extracts")
dir.create(extract_download_parent_dir, showWarnings = FALSE, recursive = TRUE)


# ==== 1. DOWNLOAD CPS SUPPLEMENT & ASEC DATA ====
download_ipums_data <- function(extract_def, sample_id, parent_dir) {
  print(paste("--- Acquiring Sample:", sample_id, "---"))
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  subdir <- file.path(parent_dir, paste0("CPS_", sample_id, "_Extract_", submitted$number, "_", Sys.Date()))
  dir.create(subdir, showWarnings=FALSE, recursive=TRUE)
  files <- download_extract(downloadable, download_dir = subdir, overwrite=TRUE)
  ddi_path <- files[grep("\\.xml$", files)][1]
  data <- read_ipums_micro(ddi_path, verbose=FALSE)
  # Save the checkpoint .rds file
  rds_path <- file.path(subdir, paste0(sample_id, "_raw_data.rds"))
  saveRDS(data, rds_path)
  print(paste(sample_id, "data downloaded and saved to:", rds_path))
  return(data)
}

# --- Download Target Supplement ---
supp_extract_def <- define_extract_micro(collection = IPUMS_COLLECTION, samples = CPS_SUPPLEMENT_SAMPLE_ID, variables = CPS_SUPPLEMENT_VARIABLES)
cps_supplement_data <- download_ipums_data(supp_extract_def, CPS_SUPPLEMENT_SAMPLE_ID, extract_download_parent_dir)

# --- Download ASEC Donor File ---
asec_extract_def <- define_extract_micro(collection = IPUMS_COLLECTION, samples = CPS_ASEC_SAMPLE_ID, variables = CPS_ASEC_VARIABLES)
cps_asec_data <- download_ipums_data(asec_extract_def, CPS_ASEC_SAMPLE_ID, extract_download_parent_dir)

print("--- CPS Data Acquisition Template Finished ---")
