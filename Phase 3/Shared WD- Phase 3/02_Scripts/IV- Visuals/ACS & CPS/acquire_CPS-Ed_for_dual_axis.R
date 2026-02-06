# ==== 0. ABOUT ====
## WD location: 02_Scripts/IV-Visuals/ACS & CPS
## Script: acquire_CPS-Ed_for_dual_axis.R
## Purpose: Acquires the CPS Education Supplement and the ASEC donor data from IPUMS
##          for the dual-axis child enrollment visualization.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Dependencies: ipumsr, dplyr, here, stringr
## Output: Raw RDS files for the supplement and ASEC donor in `01_data/raw/IPUMS_Microdata/`.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(stringr); library(purrr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ==== 1. PARAMETERS ====
USER_INDICATOR_NAME       <- "Child_Enrollment"
USER_SUPPLEMENT_SAMPLE_ID <- "cps2023_10s"
USER_ASEC_DONOR_SAMPLE_ID <- "cps2023_03s"

USER_SUPPLEMENT_VARIABLES <- c(
  "SERIAL", "CPSIDP", "EDSUPPWT", "FAMINC", "AGE", "RACE", "HISPAN", "EDATT"
)
USER_ASEC_DONOR_VARIABLES <- c("SERIAL", "PERNUM", "ASECWT", "HHINCOME")

# ==== 2. ACQUISITION LOGIC ====
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_INDICATOR_NAME, "_", USER_SUPPLEMENT_SAMPLE_ID))
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

download_and_save <- function(sample_id, variables, collection, desc, path) {
  message(paste("\n--- Acquiring data for:", desc, "---"))
  extract <- define_extract_micro(collection, samples = sample_id, variables = unique(variables), description = paste("1/3 Country -", desc))
  submitted <- submit_extract(extract)
  downloadable <- wait_for_extract(submitted)
  files <- download_extract(downloadable, download_dir = OUTPUT_RAW_DIR)
  ddi <- files[grep("\\.xml$", files)][1]
  codebook_path <- file.path(dirname(ddi), paste0("codebook_", str_replace_all(tolower(desc), " ", "_"), ".txt"))
  generate_codebook_from_ddi(ddi, variables, codebook_path)
  data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(data, file = path)
  message(paste("Data and codebook for", desc, "saved."))
}

download_and_save(USER_SUPPLEMENT_SAMPLE_ID, USER_SUPPLEMENT_VARIABLES, "cps", "Child Enrollment Supplement", file.path(OUTPUT_RAW_DIR, "raw_supplement_data.rds"))
download_and_save(USER_ASEC_DONOR_SAMPLE_ID, USER_ASEC_DONOR_VARIABLES, "cps", "ASEC Donor", file.path(OUTPUT_RAW_DIR, "raw_asec_donor_data.rds"))

message("\n--- CPS data acquisition script complete. ---")