# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## Script: UIED_absence_suspensions_prepare.R
## Purpose: Aggregates school data, bridges IDs using Directory, 
##          and joins Income data via LEAID.
##          (FIXED: Uses 'leaid' directly since 'gleaid' is missing)
## Author: Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(stringr); library(purrr)

# ================================================================= #
# ==== 1. USER CONFIGURATION ====
# ================================================================= #
USER_YEAR <- 2017

# --- SMART PATH DETECTION ---
base_data_path <- here::here("01_data")
if (!dir.exists(base_data_path)) base_data_path <- here::here("01_Data")

RAW_DATA_DIR <- file.path(base_data_path, "raw", "Urban Institute Education Data", USER_YEAR)
OUTPUT_DIR   <- file.path(base_data_path, "processed", "Urban Institute Education Data")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

INCOME_DB_FILE <- file.path(base_data_path, "processed", "geographic_income_database_harmonized.rds")
PROCESSED_DATA_FILE <- file.path(OUTPUT_DIR, paste0("prepared_UIED_Absence_", USER_YEAR, ".rds"))

# Verify Files Exist
if (!file.exists(INCOME_DB_FILE)) stop(paste("Fatal Error: Income DB not found at", INCOME_DB_FILE))

# ================================================================= #
# ==== 2. LOAD DATA ====
# ================================================================= #
message("Loading raw datasets...")

safe_read <- function(filename) {
  f <- file.path(RAW_DATA_DIR, filename)
  if(!file.exists(f)) stop(paste("Missing file:", f))
  readRDS(f)
}

raw_enroll <- safe_read(paste0("raw_uied_enrollment_", USER_YEAR, ".rds"))
raw_susp   <- safe_read(paste0("raw_uied_suspensions_", USER_YEAR, ".rds"))
raw_abs    <- safe_read(paste0("raw_uied_absence_", USER_YEAR, ".rds"))
raw_dir    <- safe_read(paste0("raw_uied_directory_", USER_YEAR, ".rds"))

# ================================================================= #
# ==== 3. AGGREGATE TO DISTRICT LEVEL ====
# ================================================================= #
message("Aggregating School Data...")

# 1. Collapse School Subgroups
collapse_school <- function(df, target_col) {
  if (is.null(df)) return(NULL)
  df %>%
    group_by(ncessch) %>%
    summarise(value = sum(as.numeric(get(target_col)), na.rm = TRUE), .groups = "drop") %>%
    rename(!!target_col := value)
}

schools_enroll <- collapse_school(raw_enroll, "enrollment")
schools_susp   <- collapse_school(raw_susp, "days_suspended")
schools_abs    <- collapse_school(raw_abs, "students_chronically_absent")

# 2. Merge School Data
schools_merged <- schools_enroll %>%
  left_join(schools_susp, by = "ncessch") %>%
  left_join(schools_abs, by = "ncessch")

# 3. Attach District ID (leaid) from Enrollment file
school_district_map <- raw_enroll %>% select(ncessch, leaid) %>% distinct()
schools_final <- schools_merged %>% inner_join(school_district_map, by = "ncessch")

# 4. Aggregate to District
districts_agg <- schools_final %>%
  group_by(leaid) %>%
  summarise(
    total_enrollment = sum(enrollment, na.rm = TRUE),
    total_suspensions = sum(days_suspended, na.rm = TRUE),
    total_chronically_absent = sum(students_chronically_absent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(total_enrollment > 0)

# ================================================================= #
# ==== 4. BRIDGE IDS & JOIN INCOME ====
# ================================================================= #
message("Bridging IDs and Joining Income...")

# 1. Prepare Directory (The "Bridge" File)
# UPDATE: 'leaid' is the 7-digit ID (FIPS+Agency). 'gleaid' is not needed.
directory_bridge <- raw_dir %>%
  select(leaid, fips, agency_type) %>%
  distinct() %>%
  filter(agency_type %in% c(1, 2)) # Keep only regular/local school districts

# 2. Join Aggregated Data with Directory Info
districts_with_geo <- districts_agg %>%
  inner_join(directory_bridge, by = "leaid") %>%
  mutate(leaid = as.character(leaid)) # Ensure character for join

# 3. Load Income Database
income_db <- readRDS(INCOME_DB_FILE) %>%
  mutate(TL_GEO_ID = as.character(TL_GEO_ID))

# 4. Perform the Join
# NOTE: 'leaid' (NCES ID) matches 'TL_GEO_ID' (Census ID) for school districts
data_with_income <- districts_with_geo %>%
  inner_join(income_db, by = c("leaid" = "TL_GEO_ID")) %>%
  mutate(
    median_income = if("median_household_income" %in% names(.)) median_household_income else median_household_income_final
  ) %>%
  filter(!is.na(median_income))

# ================================================================= #
# ==== 5. FINALIZE & SAVE ====
# ================================================================= #
message("Finalizing...")

final_prepared_data <- data_with_income %>%
  mutate(
    # Indicators
    raw_rate = total_chronically_absent / total_enrollment,
    indicator_to_plot = if_else(raw_rate > 1, 1, raw_rate), # Cap at 100%
    suspension_rate = total_suspensions / total_enrollment
  ) %>%
  rename(
    PERWT = total_enrollment, 
    HHINCOME = median_income
  ) %>%
  select(leaid, PERWT, HHINCOME, indicator_to_plot, suspension_rate)

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)
message(paste("Success! Prepared data saved to:", PROCESSED_DATA_FILE))