## WD location: 02_Scripts/III-Data Prep Templates/MEPS
## Script: MEPS_Infrastructure_acquire.R
## Purpose: Acquires 2023 MEPS data using a Hierarchical structure.
##          Ensures linking identifiers exist for both Person and Event records.
## Author: Gemini Thought Partner (1/3 Country Project Standards)

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA DOWNLOAD ====
# ================================================================= #

USER_INDICATOR_NAME    <- "Medical Infrastructure and Life Outcomes"
USER_MEPS_SAMPLE_ID    <- "mp2023" # 2023 Full Year Consolidated

# --- DEFINE VARIABLES ---
# These must be defined as objects before they are used in the extract definition
PERSON_VARS <- c(
  "RECTYPE",     # Vital for table identification
  "MEPSID",      # Primary linking ID
  "PID",         # Secondary linking ID
  "DUID",        # Household/Dwelling Unit ID
  "PERWEIGHT",   # Population Shift multiplier
  "INCWAGE",      # Income Anchor (Three Countries logic)
  "AGE",         # Lifecycle Anchor
  "HINOTCOV",    # Reliability Anchor
  "COVERTYPE",   # type of insurance
  "USUALPL",     # Access Anchor
  "STRATANN",    # Variance Estimation
  "PSUANN"       # Variance Estimation
)

EVENT_VARS <- c(
  "VISITCTGRY"   # Medical Infrastructure Catalyst
)

# Combine for the API request
ALL_USER_VARS <- c(PERSON_VARS, EVENT_VARS)

# --- 2. API DOWNLOAD ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_MEPS", paste0("meps_", gsub(" ", "_", USER_INDICATOR_NAME)))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

# Define the extract with 'hierarchical' data structure to allow VISITCTGRY
extract_def <- define_extract_micro(
  collection = "meps", 
  samples = USER_MEPS_SAMPLE_ID, 
  variables = ALL_USER_VARS,
  data_structure = "hierarchical", # Required for event-level variables
  description = "MEPS 2023: Infrastructure Catalyst for Life Expectancy"
)

message("Submitting Hierarchical extract request for: ", USER_MEPS_SAMPLE_ID)
submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)

# Download files to local directory
message("Downloading files...")
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR)

# --- 3. DATA PERSISTENCE ---
ddi_file <- downloaded_files[grep("\\.xml$", downloaded_files)][1]

# We use read_ipums_micro_list to keep PERSON and EVENT records separate
data_list <- read_ipums_micro_list(ddi = ddi_file, verbose = FALSE)

# Save the list object for the Preparation script
saveRDS(data_list, file.path(OUTPUT_RAW_DIR, "raw_meps_hierarchical_2023.rds"))

message("SUCCESS: Hierarchical MEPS data acquired with validated identifiers.")