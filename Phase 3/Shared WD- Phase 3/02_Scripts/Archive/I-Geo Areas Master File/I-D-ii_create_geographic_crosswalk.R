# ==== 0. ABOUT ====
## WD location: 02_Scripts/I-Geo Areas Master File
## Script: I-D-ii_create_geographic_crosswalks.R
## Purpose: Processes the official U.S. Census Bureau 2010-to-2020 geographic
##          relationship files (crosswalks). This script now explicitly handles
##          the unique column names and file paths in each subdirectory.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-09-21
## Last Modified: 2025-09-21 (Updated file paths to reflect new subdirectory structure)
## Dependencies: dplyr, readr, here, purrr, stringr
## Input: Crosswalk .txt files located in subdirectories within '01_Data/Crosswalks/2010 to 2020 Crosswalks/'.
## Output: 01_data/processed/geographic_crosswalk_2010_to_2020.rds (Master one-to-many)
##         01_data/processed/definitive_crosswalk_2010_to_2020.rds (Definitive one-to-one)


# Load necessary libraries
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(here)) install.packages("here")
if (!require(purrr)) install.packages("purrr")
if (!require(stringr)) install.packages("stringr")

library(dplyr)
library(readr)
library(here)
library(purrr)
library(stringr)


# ==== 1. DEFINE PARAMETERS AND HELPER FUNCTION ====

# ===== 1.1. Define File and Column Mappings =====
crosswalk_base_dir <- here("01_data", "Crosswalks", "2010 to 2020 Crosswalks")

# This list now defines the subdirectory and file for each geography, along with specific GEOID columns.
crosswalk_files_info <- list(
  TRACT = list(
    path = file.path("Census Tract", "2010-2020 Census Tract Crosswalk.txt"),
    geoid_10_col = "GEOID_TRACT_10",
    geoid_20_col = "GEOID_TRACT_20"
  ),
  COUSUB = list(
    path = file.path("County Subdivision", "2010-2020 County Subdivision Crosswalk.txt"),
    geoid_10_col = "GEOID_COUSUB_10",
    geoid_20_col = "GEOID_COUSUB_20"
  ),
  PUMA = list(
    path = file.path("PUMA", "2010-2020 PUMA Crosswalk.txt"),
    geoid_10_col = "GEOID_PUMA5_10",
    geoid_20_col = "GEOID_PUMA5_20"
  ),
  UA = list(
    path = file.path("Urban Area", "2010-2020 Urban Area Crosswalk.txt"),
    geoid_10_col = "GEOID_UA_10",
    geoid_20_col = "GEOID_UA_20"
  ),
  ZCTA = list(
    path = file.path("ZCTA", "2010-2020 ZCTA Crosswalk.txt"),
    geoid_10_col = "GEOID_ZCTA5_10",
    geoid_20_col = "GEOID_ZCTA5_20"
  )
)

# ===== 1.2. Updated Helper Function =====
process_crosswalk_file <- function(file_info, geography_name) {
  message(paste("--- Processing crosswalk for:", geography_name, "---"))
  
  file_path <- file.path(crosswalk_base_dir, file_info$path)
  
  if (!file.exists(file_path)) {
    warning(paste("File not found for", geography_name, "at path:", file_path, ". Skipping."))
    return(NULL)
  }
  
  # Read all columns as character to preserve leading zeros in GEOIDs
  crosswalk_data <- read_delim(file_path, delim = "|", col_types = cols(.default = "c"))
  
  # Ensure the specific columns exist before proceeding
  required_cols <- c(file_info$geoid_10_col, file_info$geoid_20_col, "AREALAND_PART")
  if (!all(required_cols %in% names(crosswalk_data))) {
    warning(paste("Could not find required columns for", geography_name, ". Expected:", paste(required_cols, collapse=", "), ". Skipping this file."))
    return(NULL)
  }
  
  processed_df <- crosswalk_data %>%
    select(
      GEOID10 = all_of(file_info$geoid_10_col),
      GEOID20 = all_of(file_info$geoid_20_col),
      AREALAND_PART
    ) %>%
    mutate(
      GEOGRAPHY_TYPE = geography_name,
      AREALAND_PART = as.numeric(AREALAND_PART)
    ) %>%
    filter(!is.na(GEOID10) & !is.na(GEOID20))
  
  message(paste("... Successfully processed", nrow(processed_df), "relationships for", geography_name, "."))
  return(processed_df)
}


# ==== 2. PROCESS EACH GEOGRAPHIC CROSSWALK FILE ====
message("Starting to process all provided geographic crosswalk files.")

# Use `imap` to iterate through the list, passing both the info and the name
all_crosswalks_list <- imap(crosswalk_files_info, ~process_crosswalk_file(.x, .y))


# ==== 3. COMBINE AND SAVE THE MASTER RELATIONSHIP FILE ====
message("Combining all processed crosswalks into a single master relationship table.")

master_crosswalk <- bind_rows(compact(all_crosswalks_list))

if (nrow(master_crosswalk) == 0) {
  stop("Master crosswalk table is empty after processing all files. Please check input files and column names.")
}

message("Master crosswalk table (containing one-to-many relationships) created successfully.")

output_dir <- here("01_data", "processed")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

rds_output_path <- here(output_dir, "geographic_crosswalk_2010_to_2020.rds")
csv_output_path <- here(output_dir, "geographic_crosswalk_2010_to_2020.csv")

saveRDS(master_crosswalk, rds_output_path)
write.csv(master_crosswalk, csv_output_path, row.names = FALSE)
message("...Master relationship files saved.")


# ==== 4. CREATE DEFINITIVE ONE-TO-ONE LOOKUP TABLE ====
message("Creating a definitive one-to-one 2010-to-2020 lookup table using areal weighting...")

if (exists("master_crosswalk") && nrow(master_crosswalk) > 0) {
  definitive_crosswalk_10_to_20 <- master_crosswalk %>%
    group_by(GEOID10, GEOGRAPHY_TYPE) %>%
    slice_max(order_by = AREALAND_PART, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(GEOID10, GEOID20, GEOGRAPHY_TYPE)
  
  message("...Definitive lookup table created successfully.")
  
} else {
  stop("Master crosswalk table is empty or does not exist. Cannot create definitive lookup.")
}


# ==== 5. SAVE AND VALIDATE THE DEFINITIVE LOOKUP TABLE ====
rds_definitive_path <- here(output_dir, "definitive_crosswalk_2010_to_2020.rds")
csv_definitive_path <- here(output_dir, "definitive_crosswalk_2010_to_2020.csv")

saveRDS(definitive_crosswalk_10_to_20, rds_definitive_path)
write.csv(definitive_crosswalk_10_to_20, csv_definitive_path, row.names = FALSE)
message("...Definitive lookup files saved.")

# --- Validation ---
message("--- Summary of the definitive lookup table ---")
total_mappings <- nrow(definitive_crosswalk_10_to_20)
geography_counts <- definitive_crosswalk_10_to_20 %>%
  count(GEOGRAPHY_TYPE, name = "number_of_mappings")

message(paste("Total definitive 2010-to-2020 mappings created:", format(total_mappings, big.mark = ",")))
message("Mapping counts by geography type:")
print(geography_counts)

message("Script I-D-ii execution complete.")

