# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## Script: UIED_absence_suspensions_acquire.R
## Purpose: Ingests PRE-DOWNLOADED UIED data files.
## Author: Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(purrr)

# ================================================================= #
# ==== 1. USER CONFIGURATION ====
# ================================================================= #
USER_YEAR <- 2017

# --- 1.1 Source Directory (FIXED) ---
# Since your R Project root is likely "Shared WD", we start looking from "01_Data".
SOURCE_DIR <- here::here("01_Data", "Raw", "Urban Institute Education Data", "2017")

# --- 1.2 Output Directory (Pipeline Standard) ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "Urban Institute Education Data", USER_YEAR)
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

message(paste("--- Ingesting UIED Data for Year:", USER_YEAR, "---"))
message(paste("Source:", SOURCE_DIR))

# ================================================================= #
# ==== 2. DATA INGESTION ====
# ================================================================= #

files_to_ingest <- list(
  enrollment  = paste0("raw_uied_enrollment_", USER_YEAR, ".rds"),
  suspensions = paste0("raw_uied_suspensions_", USER_YEAR, ".rds"),
  absence     = paste0("raw_uied_absence_", USER_YEAR, ".rds"),
  directory   = paste0("raw_uied_directory_", USER_YEAR, ".rds"),
  nhgis       = paste0("raw_uied_nhgis_geo_", USER_YEAR, ".rds")
)

# Loop through and save to pipeline
purrr::iwalk(files_to_ingest, ~{
  source_path <- file.path(SOURCE_DIR, .x)
  
  if (file.exists(source_path)) {
    data <- readRDS(source_path)
    dest_path <- file.path(OUTPUT_RAW_DIR, .x)
    saveRDS(data, file = dest_path)
    
    message(paste("✅ Found & Saved:", .x))
    
  } else {
    warning(paste("❌ MISSING FILE:", .x, "\n   Checked at:", source_path))
  }
})

message("\n--- Data ingestion complete. Proceed to Prepare script. ---")