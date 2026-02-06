## WD location: 02_Scripts/III-Data Prep Templates/Urban Institute Education Data
## Script: UEID_absence_suspensions_prepare_data.R
## Purpose: Loads and merges raw UIED data, correctly maps schools to geographic
##          districts, aggregates metrics to the administrative district level (leaid),
##          and joins with the harmonized income database.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-01
## Last Modified: 2025-10-01 (Added cleaning for negative/implausible values and a final validation section)
## Dependencies: dplyr, readr, here, stringr, purrr, rlang
## Input: Five raw RDS files from `01_data/raw/Urban Institute Education Data/[YEAR]/`
##        `01_data/processed/geographic_income_database_harmonized.rds`
## Output: `01_data/processed/Finalized RDS outputs/Urban Institute Education Data/uied_district_data_with_income_[YEAR].rds`

# ==== 0. SETUP ====
# ===== 0.1. Clear Environment =====
rm(list = ls())
gc()

# ===== 0.2. Load Libraries =====
library(dplyr)
library(readr)
library(here)
library(stringr)
library(purrr)
library(rlang)

message("Setup complete. Environment cleared and libraries loaded.")


# ==== 1. USER CONFIGURATION ====
USER_YEAR <- 2017
RAW_DATA_DIR <- here::here("01_data", "raw", "Urban Institute Education Data", USER_YEAR)
OUTPUT_DIR <- here::here("01_data", "processed", "Finalized RDS outputs", "Urban Institute Education Data")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
PROCESSED_DATA_FILE <- file.path(OUTPUT_DIR, paste0("uied_district_data_with_income_", USER_YEAR, ".rds"))


# ==== 2. LOAD & MERGE RAW DATA ====
# ===== 2.1. Load Individual Raw Files =====
message(paste("Loading raw data files from:", RAW_DATA_DIR))
raw_file_names <- c("enrollment", "suspensions", "absence", "directory", "nhgis_geo")
raw_datasets <- purrr::map(raw_file_names, ~{
  file_path <- file.path(RAW_DATA_DIR, paste0("raw_uied_", .x, "_", USER_YEAR, ".rds"))
  if (!file.exists(file_path)) stop(paste("Raw data file not found:", file_path))
  readRDS(file_path)
}) %>% setNames(raw_file_names)
message("...All raw datasets loaded successfully.")

# ===== 2.2. Filter Subtopics and Merge Datasets =====
message("Filtering subtopics and merging raw datasets...")

# Filter for a single subtopic dimension to prevent data duplication
raw_datasets$suspensions <- raw_datasets$suspensions %>% filter(!is.na(sex))
raw_datasets$absence <- raw_datasets$absence %>% filter(!is.na(sex))

# Helper to correctly aggregate disaggregated data to the school level
collapse_school_totals <- function(df) {
  if (!"ncessch" %in% names(df)) return(df)
  group_keys <- intersect(c("ncessch", "year"), names(df))
  analysis_cols <- c("enrollment", "days_suspended", "students_chronically_absent")
  
  cols_to_sum <- intersect(analysis_cols, names(df))
  # All other columns, including identifiers, will be carried forward
  cols_to_first <- setdiff(names(df), c(group_keys, cols_to_sum))
  
  df %>%
    group_by(across(all_of(group_keys))) %>%
    summarise(
      across(all_of(cols_to_sum), ~ sum(as.numeric(.x), na.rm = TRUE)),
      across(all_of(cols_to_first), ~ first(na.omit(.x))), 
      .groups = "drop"
    )
}

# Collapse datasets that are disaggregated by subtopics
school_data_list <- list(
  enrollment = collapse_school_totals(raw_datasets$enrollment),
  suspensions = collapse_school_totals(raw_datasets$suspensions),
  absence = collapse_school_totals(raw_datasets$absence),
  nhgis_geo = raw_datasets$nhgis_geo # NHGIS is already at the school level
)

# Merge all school-level data sources
merged_school_data <- school_data_list %>%
  reduce(full_join, by = c("ncessch", "year")) %>%
  # Coalesce key identifiers that appear in multiple files
  mutate(
    leaid = coalesce(!!!syms(grep("^leaid", names(.), value = TRUE))),
    gleaid = coalesce(!!!syms(grep("^gleaid", names(.), value = TRUE))),
    fips = coalesce(!!!syms(grep("^fips", names(.), value = TRUE)))
  ) %>%
  select(ncessch, year, leaid, gleaid, fips, enrollment, days_suspended, students_chronically_absent, everything())

message("...Raw data merged and identifiers coalesced.")


# ==== 3. MAP SCHOOLS TO DISTRICTS & FILTER FOR GEOGRAPHIC AGENCIES ====
message("Mapping schools to districts and filtering for geographic agency types...")

# Join with district directory data to get district-level attributes like `agency_type`
school_with_district_info <- merged_school_data %>%
  left_join(raw_datasets$directory, by = c("leaid", "year"), suffix = c("_school", ""))

# Filter for schools that belong to regular, geographic districts
schools_in_geo_districts <- school_with_district_info %>%
  filter(agency_type %in% c(1, 2))

message(paste("Filtered to", nrow(schools_in_geo_districts), "school records within geographic districts."))


# ==== 4. AGGREGATE TO ADMINISTRATIVE DISTRICT (`leaid`) LEVEL ====
message("Aggregating school data to the administrative district (`leaid`) level...")

special_values_to_exclude <- c(-1, -2, -3, -5, -6, -9)

# Now, when we group by `leaid`, the `fips` column already exists from the school-level data.
district_level_data <- schools_in_geo_districts %>%
  mutate(across(c(enrollment, days_suspended, students_chronically_absent), 
                ~ if_else(. %in% special_values_to_exclude, NA_real_, as.numeric(.))),
         # **MODIFICATION 1**: Set negative suspension days to NA
         days_suspended = if_else(days_suspended < 0, NA_real_, days_suspended)
  ) %>%
  filter(!is.na(enrollment) & enrollment > 0) %>%
  group_by(leaid) %>%
  summarise(
    total_enrollment = sum(enrollment, na.rm = TRUE),
    total_days_suspended = sum(days_suspended, na.rm = TRUE),
    total_students_chronically_absent = sum(students_chronically_absent, na.rm = TRUE),
    # Carry forward the geographic identifiers, which are consistent for each `leaid`
    gleaid = first(na.omit(gleaid)),
    fips = first(na.omit(fips)),
    agency_level = first(na.omit(agency_level)),
    .groups = "drop"
  )
message(paste("Aggregated into", nrow(district_level_data), "unique administrative districts."))


# ==== 5. CREATE FINAL GEOGRAPHIC ID & JOIN WITH INCOME DATA ====
message("Preparing final geographic ID and joining with income database...")
districts_prepped_for_join <- district_level_data %>%
  filter(!is.na(gleaid) & !is.na(fips)) %>%
  mutate(
    full_gleaid = paste0(str_pad(fips, 2, "left", "0"), gleaid),
    GEOGRAPHY_TYPE = case_when(
      agency_level == 1 ~ "Elementary School District",
      agency_level %in% c(2, 3, 7) ~ "Secondary School District",
      agency_level == 4 ~ "Unified School District",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(GEOGRAPHY_TYPE))

GEO_INCOME_DB_FILE <- here::here("01_data", "processed", "geographic_income_database_harmonized.rds")
geo_income_db <- readRDS(GEO_INCOME_DB_FILE)

geo_income_db_prepped <- geo_income_db %>%
  filter(geography_level %in% c("Elementary School District", "Secondary School District", "Unified School District")) %>%
  select(TL_GEO_ID, GEOGRAPHY_TYPE = geography_level, median_household_income_final)

data_with_income <- districts_prepped_for_join %>%
  left_join(geo_income_db_prepped, by = c("full_gleaid" = "TL_GEO_ID", "GEOGRAPHY_TYPE")) %>%
  filter(!is.na(median_household_income_final))

if(nrow(data_with_income) == 0) {
  stop("Join with income data resulted in zero matches. Check `full_gleaid` and `GEOGRAPHY_TYPE` alignment.")
}
message(paste("...Join successful.", nrow(data_with_income), "districts were matched with income data."))


# ==== 6. CALCULATE FINAL RATES & SAVE PREPARED DATA ====
final_analysis_data <- data_with_income %>%
  mutate(
    chronic_absent_rate = total_students_chronically_absent / total_enrollment,
    avg_suspension_days_per_student = total_days_suspended / total_enrollment,
    # **MODIFICATION 2**: Set any chronic absenteeism rates > 1 to NA
    chronic_absent_rate = if_else(chronic_absent_rate > 1, NA_real_, chronic_absent_rate)
  )

message(paste("Saving analysis-ready data to:", PROCESSED_DATA_FILE))
saveRDS(final_analysis_data, file = PROCESSED_DATA_FILE)
message("...Data preparation complete.")


# ==== 7. VALIDATION REPORT ====
# **MODIFICATION 3**: Added validation section
message("\n\n=======================================================")
message("====      FINAL DATA VALIDATION REPORT           ====")
message("=======================================================")

if (file.exists(PROCESSED_DATA_FILE)) {
  final_data_check <- readRDS(PROCESSED_DATA_FILE)
  
  message("--- First 6 rows of the final processed data ---")
  print(head(final_data_check))
  
  message("\n--- Structure of the final data ---")
  str(final_data_check)
  
  message("\n--- Summary of key analysis variables ---")
  summary_report <- final_data_check %>%
    summarise(
      n_districts = n(),
      n_na_income = sum(is.na(median_household_income_final)),
      min_chronic_rate = min(chronic_absent_rate, na.rm = TRUE),
      max_chronic_rate = max(chronic_absent_rate, na.rm = TRUE),
      n_na_chronic_rate = sum(is.na(chronic_absent_rate)),
      min_suspension_days = min(avg_suspension_days_per_student, na.rm = TRUE),
      max_suspension_days = max(avg_suspension_days_per_student, na.rm = TRUE),
      n_na_suspension_days = sum(is.na(avg_suspension_days_per_student))
    )
  print(as.data.frame(summary_report))
  
  if (summary_report$max_chronic_rate > 1) {
    message("  - ❌ FAIL: `chronic_absent_rate` contains values greater than 1.")
  } else {
    message("  - ✅ PASS: `chronic_absent_rate` values are all 1 or less.")
  }
  
  if (summary_report$min_suspension_days < 0) {
    message("  - ❌ FAIL: `avg_suspension_days_per_student` contains negative values.")
  } else {
    message("  - ✅ PASS: `avg_suspension_days_per_student` values are all non-negative.")
  }
  
} else {
  message("❌ VALIDATION FAILED: Final output file was not found.")
}

message("=======================================================\n")
message("Script complete.")