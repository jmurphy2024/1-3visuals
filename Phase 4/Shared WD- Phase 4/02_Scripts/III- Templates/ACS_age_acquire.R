# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
## Script: ACS_age_viz
# Purpose: Downloads 5-Year ACS data for Population Age Analysis.
#          Includes ALL PERSONS (not just Householders).
# Output:  01_data/raw/IPUMS_Microdata/usa_us2023b_age/raw_data.rds
# ==============================================================================

rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(purrr)

# --- 1. CONFIGURATION ---
USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023c" 
USER_INDICATOR_NAME   <- "Population_Age"

# --- 2. DEFINE VARIABLES ---
# We need Person Weight (PERWT) and Age, plus Income vars for context
vars_needed <- c(
  "YEAR", "SAMPLE", "SERIAL", "CBSERIAL", 
  "HHWT", "PERWT",             # Weights (Household & Person)
  "STATEFIP", "MET2013",       # Geography
  "HHINCOME", "ADJUST",        # Income Factors
  "PERNUM",                    # Person Number (to track hierarchy)
  "AGE", "SEX"                 # Demographics
)

# --- 3. DEFINE EXTRACT ---
# Explicitly request US States (01-56)
state_fips_spec <- var_spec(
  "STATEFIP",
  case_selections = c(
    "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16", "17", "18",
    "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33",
    "34", "35", "36", "37", "38", "39", "40", "41", "42", "44", "45", "46", "47", "48", "49",
    "50", "51", "53", "54", "55", "56"
  )
)

extract_def <- define_extract_micro(
  description = paste("1/3 Country -", USER_INDICATOR_NAME, "-", Sys.Date()),
  collection = USER_IPUMS_COLLECTION,
  samples = USER_IPUMS_SAMPLE_ID,
  variables = list(
    state_fips_spec,
    "YEAR", "SAMPLE", "SERIAL", "HHWT", "PERWT",
    "HHINCOME", "ADJUST", "PERNUM", "AGE"
  )
)

# --- 4. SUBMIT & DOWNLOAD ---
raw_dir <- here::here("01_data", "raw", "IPUMS_Microdata", paste0(USER_IPUMS_COLLECTION, "_", USER_IPUMS_SAMPLE_ID, "_age"))
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

if(Sys.getenv("IPUMS_API_KEY") != "") {
  message("Submitting extract to IPUMS...")
  submitted <- submit_extract(extract_def)
  ready <- wait_for_extract(submitted)
  downloaded <- download_extract(ready, download_dir = raw_dir, overwrite = TRUE)
  
  # Load and Save as RDS
  ddi_file <- downloaded[grep("\\.xml$", downloaded)]
  raw_data <- read_ipums_micro(ddi_file, verbose = FALSE)
  saveRDS(raw_data, file.path(raw_dir, "raw_data.rds"))
  
  message("Acquisition Complete. Raw population data saved.")
} else {
  stop("IPUMS_API_KEY not found.")
}