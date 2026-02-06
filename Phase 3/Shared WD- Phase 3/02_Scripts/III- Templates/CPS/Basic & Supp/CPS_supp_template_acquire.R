# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: 01_acquire_income_wealth.R
## Purpose: Acquires data for Quality of Life Narrative.
##          (Fixed: Uses AHRSWORKT. Drops unavailable VLTRUST).
## Author: 1/3 Country Project Assistant
## Date Created: 2026-01-08

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)

# Check for shared functions
shared_utils_path <- here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r")
if(file.exists(shared_utils_path)) source(shared_utils_path)

# ================================================================= #
# ==== 1. USER INPUTS: DEFINE THREE SEPARATE EXTRACTS ====
# ================================================================= #

# --- EXTRACT 1: ASEC (Balance & Security) ---
# Narrative: "Thriving Worker" (HEALTH + AHRSWORKT)
EXTRACT_1_NAME <- "ASEC_Income_Base"
SAMPLE_ID_1    <- "cps2023_03s"
# CORRECTED: AHRSWORKT is the IPUMS var for Actual Hours Worked
VARS_1         <- c("SERIAL", "PERNUM", "ASECWT", "HHINCOME", 
                    "INCTOT", "HEALTH", "AHRSWORKT") 

# --- EXTRACT 2: FOOD SECURITY (Basic Needs) ---
# Narrative: "Secure Foundation" (Food Security)
EXTRACT_2_NAME <- "Food_Security_Supp"
SAMPLE_ID_2    <- "cps2023_12s" 
VARS_2         <- c("SERIAL", "PERNUM", "FSSUPPWTH", "FSSTATUS", "FAMINC")

# --- EXTRACT 3: CIVIC ENGAGEMENT (Community Bonds) ---
# Narrative: "Reciprocal Neighbor"
# Supplement: September 2023 (Volunteering and Civic Life)
EXTRACT_3_NAME <- "Civic_Engagement_Supp"
SAMPLE_ID_3    <- "cps2023_09s" 
# CORRECTED VARIABLES: 
# Talk -> VLNEIGH (matches PES4)
# Favors -> VLHELPN (matches PES6)
# Note: VLTRUST is currently unavailable in IPUMS 2023, so it is removed.
VARS_3         <- c("SERIAL", "PERNUM", "VLSUPPWT", 
                    "VLNEIGH", "VLHELPN", "FAMINC")


# ================================================================= #
# ==== 2. GENERIC LOGIC (Download Loop) ====
# ================================================================= #

OUTPUT_ROOT <- here::here("01_data", "raw", "IPUMS_Microdata")

process_extract <- function(sample_id, variables, extract_name) {
  
  # Create specific folder for this extract
  output_dir <- file.path(OUTPUT_ROOT, paste0("cps_", extract_name, "_", sample_id))
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  message(paste("\n--- Processing:", extract_name, "---"))
  
  extract_def <- define_extract_micro(
    collection = "cps",
    samples = sample_id,
    variables = unique(variables),
    description = paste("1/3 Project -", extract_name, "-", Sys.Date())
  )
  
  # Submit and Wait
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  files <- download_extract(downloadable, download_dir = output_dir)
  
  # Load and Save RDS
  ddi_file <- files[grep("\\.xml$", files)][1]
  data <- read_ipums_micro(ddi = ddi_file, verbose = FALSE)
  
  save_path <- file.path(output_dir, paste0("raw_", extract_name, ".rds"))
  saveRDS(data, file = save_path)
  message(paste("Saved RDS to:", save_path))
}

# --- EXECUTE THE THREE DOWNLOADS ---

# 1. Get ASEC (Balance & Security)
process_extract(SAMPLE_ID_1, VARS_1, EXTRACT_1_NAME)

# 2. Get Food Security (Basic Needs)
process_extract(SAMPLE_ID_2, VARS_2, EXTRACT_2_NAME)

# 3. Get Civic Engagement (Community Bonds)
process_extract(SAMPLE_ID_3, VARS_3, EXTRACT_3_NAME)

message("\n--- All acquisitions complete. ---")