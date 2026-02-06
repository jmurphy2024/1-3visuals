# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: CPS_ASEC_template_acquire.R
## Purpose: Acquires a CPS ASEC supplement and its corresponding basic monthly
##          CPS data from IPUMS. Saves both as raw RDS files.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Dependencies: ipumsr, dplyr, here, purrr, stringr
## Input: User-defined IPUMS parameters for the ASEC and basic monthly files.
## Output: Raw RDS files for ASEC and basic monthly data in `01_data/raw/IPUMS_Microdata/`.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr); library(stringr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))


# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA DOWNLOAD ====
# ================================================================= #

# --- 1.1. Define Sample IDs and Indicator Name ---
USER_INDICATOR_NAME            <- "Unemployment_Rates"
USER_ASEC_SAMPLE_ID            <- "cps2023_03s" # The ASEC supplement
USER_BASIC_MONTHLY_SAMPLE_ID   <- "cps2023_03b"   # The corresponding basic monthly file

# --- 1.2. Define Variables for the ASEC Supplement ---
# Includes variables for U-3/U-6 unemployment, demographics, and income. MARBASECIDP is the required linking key.
USER_ASEC_VARIABLES <- c(
  "SERIAL", "PERNUM", "ASECWTH", "ASECWT", "HHINCOME", "AGE", "RACE", "HISPAN",
  "EMPSTAT", "WHYPTLWK", "WKSTAT", "MARBASECIDP"
)

# --- 1.3. Define Variables for the Basic Monthly File ---
# Requires only the linking key and the variable for identifying marginally attached workers.
USER_BASIC_MONTHLY_VARIABLES <- c("MARBASECIDP", "UH_DSCWK_B2")


# ================================================================= #
# ==== 2. GENERIC LOGIC (No changes needed below this line) ====
# ================================================================= #

# --- 2.1. Define Output Directory ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 2.2. Helper Function for Downloading ---
download_and_save <- function(sample_id, variables, collection, description_suffix, output_path) {
  message(paste("\n--- Acquiring data for:", description_suffix, "---"))
  extract_def <- define_extract_micro(
    collection = collection, samples = sample_id, variables = unique(variables),
    description = paste("1/3 Country -", description_suffix, "-", Sys.Date())
  )
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR)
  
  ddi_file <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
  
  codebook_path <- file.path(dirname(ddi_file), paste0("codebook_", str_replace_all(tolower(description_suffix), " ", "_"), ".txt"))
  generate_codebook_from_ddi(ddi_file, variables, codebook_path)
  
  data <- read_ipums_micro(ddi = ddi_file, verbose = FALSE)
  saveRDS(data, file = output_path)
  message(paste("Data and codebook for", description_suffix, "saved to:", OUTPUT_RAW_DIR))
}

# --- 2.3. Execute Downloads ---
# Download ASEC Supplement Data
download_and_save(
  sample_id = USER_ASEC_SAMPLE_ID, variables = USER_ASEC_VARIABLES,
  collection = "cps", description_suffix = "ASEC Data",
  output_path = file.path(OUTPUT_RAW_DIR, "raw_asec_data.rds")
)

# Download Basic Monthly Data
download_and_save(
  sample_id = USER_BASIC_MONTHLY_SAMPLE_ID, variables = USER_BASIC_MONTHLY_VARIABLES,
  collection = "cps", description_suffix = "Basic Monthly Data",
  output_path = file.path(OUTPUT_RAW_DIR, "raw_basic_monthly_data.rds")
)

message("\n--- Data acquisition script complete. ---")