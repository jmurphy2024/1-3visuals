# ==============================================================================
# SCRIPT: ACS_master_acquire.R (Master Variable List)
# ==============================================================================

rm(list = ls()); gc()
library(ipumsr); library(here)

# --- 1. CONFIG ---
ACS_SAMPLE_ID <- "us2023c" 

# FULL PROJECT VARIABLE LIST
vars <- c(
  "HHINCOME", "ADJUST",                  # Income & Inflation
  "PERWT", "HHWT", "PERNUM",             # Weights & Person ID
  "STATEFIP", "PUMA", "METRO",           # Geography
  "AGE", "SEX",                          # Age & Gender
  "RACE", "HISPAN",                      # Race & Ethnicity
  "EDUC", "EDUCD",                       # Education (New)
  "POVERTY"                              
)

# --- 2. DEFINE EXTRACT ---
message(paste("Defining Master Extract for:", ACS_SAMPLE_ID))
extract_def <- define_extract_micro(
  collection = "usa",
  description = "Master 1/3 Country Project: All Demographic + Education Vars",
  samples = ACS_SAMPLE_ID,
  variables = vars
)

# --- 3. SUBMIT & DOWNLOAD ---
message("Submitting extract to IPUMS...")
submitted <- submit_extract(extract_def)
ready     <- wait_for_extract(submitted)

raw_dir <- here::here("01_data", "raw")
if(!dir.exists(raw_dir)) dir.create(raw_dir, recursive = TRUE)

message("Downloading data...")
files <- download_extract(ready, download_dir = raw_dir, overwrite = TRUE)

# --- 4. SAVE RAW RDS ---
ddi   <- read_ipums_ddi(files[grep("\\.xml$", files)])
data  <- read_ipums_micro(ddi, verbose = FALSE)

processed_dir <- here::here("01_data", "processed")
if(!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)

saveRDS(data, file.path(processed_dir, "ipums_data_raw.rds"))
message("Success! Raw Master Data (with Education) saved.")