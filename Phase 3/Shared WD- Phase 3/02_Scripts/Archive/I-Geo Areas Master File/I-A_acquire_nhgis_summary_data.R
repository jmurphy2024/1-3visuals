# ==== 0. ABOUT ====
## WD location: 02_Scripts/I-Geo Areas Master File
## Script: I-A_acquire_nhgis_summary_data.R
## Purpose: Acquires geographic summary data by making a single, combined request
##          to IPUMS NHGIS for both median income (B19013) and income
##          distribution (B19001), using the correct column name conventions.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-08-18
## Last Modified: 2025-09-21 (Corrected geography_level assignment to use filenames)
## Dependencies: ipumsr, dplyr, readr, purrr, here, stringr
## Input: Requires IPUMS API key set as an environment variable (IPUMS_API_KEY).
## Output: 01_data/processed/geographic_income_database_nhgis_summary.rds


# ==== 0. SETUP ====
# ===== 0.1. Load Libraries =====
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


# ==== 1. IPUMS API KEY & PARAMETERS SETUP ====
# ===== 1.1. Verification of API Key =====
ipums_api_key <- Sys.getenv("IPUMS_API_KEY")
if (ipums_api_key == "") {
  stop("IPUMS_API_KEY environment variable is not set. Please follow the New User Setup Protocol.")
} else {
  message("IPUMS API key successfully retrieved.")
}

# ===== 1.2. Define Geographies & Directories =====
nhgis_dataset <- "2019_2023_ACS5a"
nhgis_geog_levels <- c("tract", "cty_sub", "sd_elm", "sd_sec", "sd_uni", "puma", "urb_area", "zcta", "county", "state")

# Define project directory structure
raw_zip_dir <- here("01_data", "raw", "nhgis_downloads")
raw_unzip_dir <- here("01_data", "raw", "nhgis_unzipped")
codebook_dir <- here("04_documentation", "Codebooks")
processed_dir <- here("01_data", "processed")

# Create directories if they don't exist
dir.create(raw_zip_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_unzip_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(codebook_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(processed_dir, showWarnings = FALSE, recursive = TRUE)


# ==== 2. DEFINE THE COMBINED DATA EXTRACT ====
message("Defining a single extract for tables B19013 and B19001...")
nhgis_extract <- define_extract_nhgis(
  description = "Combined Median Income (B19013) & Distribution (B19001) Extract",
  datasets = ds_spec(
    nhgis_dataset,
    data_tables = c("B19013", "B19001"),
    geog_levels = nhgis_geog_levels
  )
)


# ==== 3. SUBMIT AND DOWNLOAD THE EXTRACT ====
message("Submitting combined extract request to NHGIS...")
submitted_extract <- submit_extract(nhgis_extract, api_key = ipums_api_key)
extract_number <- submitted_extract$number
message(paste("Waiting for extract #", extract_number, "to be processed..."))
downloadable_extract <- wait_for_extract(submitted_extract, api_key = ipums_api_key)
message("Extract processing complete.")

message("Downloading data files...")
zip_file_path <- download_extract(downloadable_extract, download_dir = raw_zip_dir, overwrite = TRUE, api_key = ipums_api_key)
message(paste("... Raw zip file saved to:", raw_zip_dir))


# ==== 4. READ, COMBINE, AND VALIDATE DATA ====
# ===== 4.1. Unzip, Read, and Combine (Corrected Logic) =====
unzip_specific_dir <- file.path(raw_unzip_dir, paste0("extract_", extract_number))
dir.create(unzip_specific_dir, showWarnings = FALSE, recursive = TRUE)
unzip(zip_file_path, exdir = unzip_specific_dir)
message(paste("... Unzipped files to:", unzip_specific_dir))

codebook_file <- list.files(unzip_specific_dir, pattern = "codebook\\.txt", full.names = TRUE, recursive = TRUE)
if (length(codebook_file) == 1) {
  new_codebook_name <- paste0("nhgis_extract_", extract_number, "_combined_codebook.txt")
  file.copy(from = codebook_file, to = file.path(codebook_dir, new_codebook_name), overwrite = TRUE)
  message(paste("... Codebook copied to:", codebook_dir))
}

csv_files <- list.files(unzip_specific_dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
if (length(csv_files) == 0) {
  stop("No CSV files found in the unzipped extract.")
}

# Use map2 to read each CSV and add the geography level from its filename
all_data <- map2(csv_files, basename(csv_files), ~{
  # Extract the geography level (e.g., "tract", "county") from the filename
  geo_level_raw <- str_extract(.y, "(?<=_)[a-z_]+(?=\\.csv)")
  
  # A simple mapping to clean up the names for clarity
  geo_level_clean <- case_when(
    geo_level_raw == "tract" ~ "Census Tract",
    geo_level_raw == "cty_sub" ~ "County Subdivision",
    geo_level_raw == "sd_elm" ~ "Elementary School District",
    geo_level_raw == "sd_sec" ~ "Secondary School District",
    geo_level_raw == "sd_uni" ~ "Unified School District",
    geo_level_raw == "puma" ~ "PUMA",
    geo_level_raw == "urb_area" ~ "Urban Area",
    geo_level_raw == "zcta" ~ "ZCTA",
    geo_level_raw == "county" ~ "County",
    geo_level_raw == "state" ~ "State",
    TRUE ~ str_to_title(str_replace_all(geo_level_raw, "_", " "))
  )
  
  read_csv(.x, col_types = cols(.default = "c")) %>%
    mutate(geography_level = geo_level_clean)
})

combined_data <- bind_rows(all_data)
message("All CSV files have been read, tagged with geography_level, and combined.")


# ===== 4.2. **CRITICAL VALIDATION STEP** =====
b19013_cols_to_check <- c("ASQPE001", "ASQPM001")
b19001_cols_to_check <- paste0("ASQOE", sprintf("%03d", 1:17)) # Corrected to B19001 (was ASQOE before)
all_required_cols <- c(b19013_cols_to_check, b19001_cols_to_check)

missing_cols <- setdiff(all_required_cols, names(combined_data))

if (length(missing_cols) > 0) {
  stop(paste("FATAL ERROR: The combined NHGIS data is missing required columns.\nMissing columns:", paste(missing_cols, collapse = ", ")))
} else {
  message("[PASS] Validation successful: All required columns from B19013 and B19001 are present.")
}


# ==== 5. CLEAN AND PREPARE THE FINAL DATASET ====
message("--- Finalizing the combined dataset ---")

# **CORRECTION**: Define the column vectors *before* the final select statement
b19001_estimate_cols <- paste0("ASQOE", sprintf("%03d", 1:17))
b19001_moe_cols <- paste0("ASQOM", sprintf("%03d", 1:17))

processed_data <- combined_data %>%
  mutate(across(where(is.character) & (starts_with("ASQO") | starts_with("ASQP")), as.numeric)) %>%
  mutate(
    NAME = coalesce(NAME_E, NAME_M),
    PUMA = PUMAA,
    STATE = STATEA,
    COUNTY = COUNTYA,
    median_household_income = ASQPE001, # Correct column for B19013
    median_household_income_moe = ASQPM001, # Correct column for B19013
    median_household_income = ifelse(median_household_income < 0, NA, median_household_income)
    # The 'geography_level' column is now added during the reading step,
    # so the incorrect case_when() statement is removed from here.
  )

final_database <- processed_data %>%
  select(
    GISJOIN, TL_GEO_ID, YEAR, STATE, COUNTY, PUMA, geography_level, NAME,
    median_household_income, median_household_income_moe,
    all_of(b19001_estimate_cols),
    all_of(b19001_moe_cols)
  )

message("Final database prepared.")


# ==== 6. SAVE AND EXPLORE ====
rds_output_path <- file.path(processed_dir, "geographic_income_database_nhgis_summary.rds")
csv_output_path <- file.path(processed_dir, "geographic_income_database_nhgis_summary.csv")

saveRDS(final_database, rds_output_path)
write.csv(final_database, csv_output_path, row.names = FALSE)
message(paste("Final NHGIS summary database saved to:", processed_dir))

message("\n--- First 6 rows of the final database ---")
print(head(final_database))

message("\nScript execution complete.")
