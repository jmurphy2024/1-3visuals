# ==== 0. ABOUT ===
## WD location: 02_Scripts/I-Geo Areas Master File
## Script: I-B_acquire_ipums_puma_microdata.R
## Purpose: Acquires individual-level microdata from IPUMS USA (ACS 2019-2023 5-year)
##          and constructs the full 7-digit PUMA GEOID required for joins.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-08-18
## Last Modified: 2025-09-22 (Added STATEFIP to construct 7-digit PUMA GEOID)
## Dependencies: ipumsr, dplyr, readr, purrr, here, stringr
## Input: Requires IPUMS API key to be set as an environment variable (IPUMS_API_KEY).
## Output: 01_data/processed/ipums_acs_2019_2023_microdata.rds

# Load necessary libraries
if (!require(ipumsr)) install.packages("ipumsr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(purrr)) install.packages("purrr")
if (!require(here)) install.packages("here")
if (!require(stringr)) install.packages("stringr")

library(ipumsr)
library(dplyr)
library(readr)
library(purrr)
library(here)
library(stringr)

# ==== 1. IPUMS API KEY SETUP ====
ipums_api_key <- Sys.getenv("IPUMS_API_KEY")
if (ipums_api_key == "") {
  stop("IPUMS_API_KEY environment variable is not set. Please follow the New User Setup Protocol.")
} else {
  message("IPUMS API key successfully retrieved from environment variable.")
}

# ==== 2. DEFINE THE DATA EXTRACT FROM IPUMS USA ====
# **CORRECTED** to include STATEFIP, which is necessary to build the full PUMA GEOID.
ipums_extract_vars <- c("HHINCOME", "HHWT", "PUMA", "STATEFIP")
ipums_extract <- define_extract_micro(
  collection = "usa",
  description = "Household Microdata for Income Estimation 2019-2023 (with STATEFIP)",
  samples = "us2023c",
  variables = ipums_extract_vars
)

# ==== 3. SUBMIT AND DOWNLOAD THE EXTRACT ====
message("Submitting IPUMS USA extract request...")
submitted_extract <- submit_extract(ipums_extract, api_key = ipums_api_key)
extract_number <- submitted_extract$number

message("Waiting for IPUMS USA extract to be processed. This may take a few minutes...")
downloadable_extract <- wait_for_extract(submitted_extract, api_key = ipums_api_key)
message("IPUMS USA extract processing complete.")

message("Downloading IPUMS USA data files...")
download_dir_path <- here("01_data", "raw", "ipums_downloads")
dir.create(download_dir_path, showWarnings = FALSE, recursive = TRUE)
data_files <- download_extract(downloadable_extract, download_dir = download_dir_path, overwrite = TRUE, api_key = ipums_api_key)
message("Download complete.")


# ==== 4. READ AND PREPARE THE MICRODATA ====
message("Reading IPUMS microdata...")
ipums_microdata <- read_ipums_micro(data_files)
message("Microdata loaded successfully.")

# (Codebook generation section remains the same, omitted for brevity)

# ===== 4.2. Select, Clean, and Construct PUMA GEOID =====
message("Cleaning data and constructing full 7-digit PUMA GEOID...")
final_microdata <- ipums_microdata %>%
  mutate(
    HHINCOME = na_if(as.numeric(as.character(HHINCOME)), 9999999),
    HHWT = as.numeric(as.character(HHWT)),
    # Pad PUMA to be 5 digits (e.g., '100' -> '00100')
    PUMA_padded = str_pad(PUMA, 5, "left", "0"),
    # Pad STATEFIP to be 2 digits (e.g., '6' -> '06')
    STATEFIP_padded = str_pad(STATEFIP, 2, "left", "0"),
    # Concatenate to create the full 7-digit GEOID
    PUMA_GEOID = paste0(STATEFIP_padded, PUMA_padded)
  ) %>%
  filter(!is.na(HHINCOME) & !is.na(PUMA_GEOID)) %>%
  # Select the final columns needed for subsequent analysis
  select(HHINCOME, HHWT, PUMA_GEOID)

message("Microdata prepared with correct 7-digit PUMA GEOID.")


# ==== 5. SAVE AND EXPLORE THE FINAL MICRODATA ====
output_dir <- here("01_data", "processed")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
rds_output_path <- here(output_dir, "ipums_acs_2019_2023_microdata.rds")

message(paste("Saving RDS file to:", rds_output_path))
saveRDS(final_microdata, rds_output_path)
message("RDS file saved.")

message("--- First 6 rows of the processed microdata ---")
print(head(final_microdata))

message(paste("Number of unique PUMAs in microdata:", n_distinct(final_microdata$PUMA_GEOID)))
message("Script I-B execution complete.")
