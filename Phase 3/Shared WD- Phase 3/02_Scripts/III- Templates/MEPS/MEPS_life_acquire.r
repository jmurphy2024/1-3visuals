## WD location: 02_Scripts/III-Data Prep Templates/MEPS
## Script: MEPS_life_acquire.r
## Purpose: Acquires 2022 MEPS data including ADHD and Chronic markers.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here)

# ==== 1. USER INPUTS ====
USER_MEPS_SAMPLE_ID <- "mp2022" 

# Corrected Person-level variables
PERSON_VARS <- c(
  "MEPSID", "PERWEIGHT", "INCWAGE", "DUID", "PID",
  "HEALTH", "HYPERTENEV", "ASTHMAEV", "STROKEV", 
  "CHEARTDIEV", "ANGIPECEV", "CHOLHIGHEV", "ARTHGLUPEV", 
  "CANCEREV", "DIABETICEV", "ADDEV", "ANYLMT"
)

# Event-level variables
EVENT_VARS <- c("MHLTHRD") 

# ==== 2. API DOWNLOAD ====
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_MEPS", "meps_life_expectancy")
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

extract_def <- define_extract_micro(
  collection     = "meps", 
  samples        = USER_MEPS_SAMPLE_ID, 
  variables      = unique(c(PERSON_VARS, EVENT_VARS)),
  data_structure = "hierarchical",
  description    = "MEPS 2022: Final Synchronized Life Expectancy Extract"
)

message("Submitting corrected extract request...")
submitted_extract <- submit_extract(extract_def)
downloadable_extract <- wait_for_extract(submitted_extract)
downloaded_files <- download_extract(downloadable_extract, download_dir = OUTPUT_RAW_DIR)

# ==== 3. DATA PERSISTENCE ====
ddi_file <- downloaded_files[grep("\\.xml$", downloaded_files)][1]
data_list <- read_ipums_micro(ddi = ddi_file, verbose = FALSE)
saveRDS(data_list, file.path(OUTPUT_RAW_DIR, "raw_meps_life_expectancy_2022.rds"))
message("SUCCESS: Hierarchical data with ADDEV acquired.")