## WD location: 02_Scripts/III-Data Prep Templates
## Script: NCVS_data_template_acquire.R
## Purpose: Ingests raw NCVS .rda files (DS2, DS3, DS5) and converts them to standardized .rds objects for 3-way join analysis.
## Author: Janica Murphy, Maxwell Goshert EPAG/ Gemini
## Date Created: 2026-01-08
## Last Modified: 2026-01-09 1:47

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here)

# ==== 1. PARAMETERS ====
# Filenames exactly as they appear in your screenshot
DS2_FILE <- "ncvs_household_2023.rda"
DS3_FILE <- "ncvs_person_2023.rda"
DS5_FILE <- "ncvs_extract_2023.rda"

# Destination for standardized RDS files
OUTPUT_DIR <- here::here("01_Data", "Raw", "NCVS_Microdata", "ncvs_2023_annual")
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

# ==== 2. SAFE INGESTION LOGIC ====

# This function opens the "box" and grabs the data inside, whatever it's named
safe_ingest <- function(filename) {
  path <- here::here("01_Data", "Raw", filename)
  if (!file.exists(path)) stop(paste("Cannot find file:", path))
  
  temp_env <- new.env()
  load(path, envir = temp_env)
  # Grab the first object found in the loaded file
  return(temp_env[[ls(temp_env)[1]]])
}

# Process all three files
ds2_raw <- safe_ingest(DS2_FILE)
saveRDS(ds2_raw, file.path(OUTPUT_DIR, "ds2_raw.rds"))
message("SUCCESS: DS2 (Household) converted.")

ds3_raw <- safe_ingest(DS3_FILE)
saveRDS(ds3_raw, file.path(OUTPUT_DIR, "ds3_raw.rds"))
message("SUCCESS: DS3 (Person) converted.")

ds5_raw <- safe_ingest(DS5_FILE)
saveRDS(ds5_raw, file.path(OUTPUT_DIR, "ds5_raw.rds"))
message("SUCCESS: DS5 (Incident) converted.")

message("\n--- ALL DATA STANDARDIZED TO RDS ---")