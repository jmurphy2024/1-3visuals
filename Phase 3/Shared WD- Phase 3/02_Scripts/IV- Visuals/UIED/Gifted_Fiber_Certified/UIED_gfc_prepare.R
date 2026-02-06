## WD location: 02_Scripts/IV- Variable Visual Scripts/Urban Institute Education Data/Gifted_Fiber_Certified
## Script: UIED_gfc_prepare.R
## Purpose: Prepares raw UIED data by merging, aggregating to the district level,
##          calculating specified rates, and joining with the income database.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Last Modified: 2025-10-02 (v4 - Final logic for weighted fiber rate)

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(stringr); library(purrr); library(rlang)

# ================================================================= #
# ==== 1. SCRIPT CONFIGURATION ====
# ================================================================= #

USER_YEAR <- 2017
USER_INDICATOR_NAME <- "gfc"

DISAGGREGATED_DATASETS <- c("advanced_coursework", "enrollment_crdc")
VARIABLES_TO_SUM <- c(
  "enrl_IB", "enrl_gifted_talented", "enrl_AP", 
  "teachers_certified_fte", "teachers_fte_crdc", "enrollment_crdc"
)
SPECIAL_VALUES_TO_NA <- c(-1, -2, -3, -5, -6, -9, -99)

# ================================================================= #
# ==== 2. GENERIC LOGIC (with custom calculations) ====
# ================================================================= #

RAW_DATA_DIR <- here::here("01_data", "raw", "Urban Institute Education Data", USER_INDICATOR_NAME, USER_YEAR)
OUTPUT_DIR <- here::here("01_data", "processed", "Finalized RDS outputs", "Urban Institute Education Data")
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
PROCESSED_DATA_FILE <- file.path(OUTPUT_DIR, paste0("uied_district_data_with_income_", USER_INDICATOR_NAME, "_", USER_YEAR, ".rds"))

raw_file_names <- c("directory", "nhgis_geo", "certified_teachers", "internet_access", "advanced_coursework", "enrollment_crdc")

raw_datasets <- purrr::map(raw_file_names, ~{
  file_year <- if (.x == "internet_access") 2020 else USER_YEAR
  readRDS(file.path(RAW_DATA_DIR, paste0("raw_uied_", .x, "_", file_year, ".rds")))
}) %>% setNames(raw_file_names)

collapse_school_totals <- function(df, vars_to_sum) {
  if (!"ncessch" %in% names(df)) return(df)
  group_keys <- intersect(c("ncessch", "year"), names(df))
  cols_to_sum <- intersect(names(df), vars_to_sum)
  cols_to_first <- setdiff(names(df), c(group_keys, cols_to_sum))
  df %>%
    group_by(across(all_of(group_keys))) %>%
    summarise(across(all_of(cols_to_sum), ~ sum(as.numeric(.x), na.rm = TRUE)),
              across(all_of(cols_to_first), ~ first(na.omit(.x))), .groups = "drop")
}

for (dataset_name in DISAGGREGATED_DATASETS) {
  raw_datasets[[dataset_name]] <- collapse_school_totals(raw_datasets[[dataset_name]], VARIABLES_TO_SUM)
}

# Isolate the 2020 internet data, keeping only school ID and the fiber variable
internet_data_2020 <- raw_datasets$internet_access %>%
  select(ncessch, sch_internet_fiber)

# Get the list of all other datasets (which are from 2017)
school_data_list_2017 <- raw_datasets[c("certified_teachers", "advanced_coursework", "enrollment_crdc", "nhgis_geo")]

# Merge all the 2017 datasets together first by school ID and year
merged_school_data_2017 <- school_data_list_2017 %>%
  reduce(full_join, by = c("ncessch", "year"))

# Now, join the 2020 internet data onto the main 2017 data using ONLY the school ID
merged_school_data <- left_join(merged_school_data_2017, internet_data_2020, by = "ncessch") %>%
  mutate(leaid = coalesce(!!!syms(grep("^leaid", names(.), value = TRUE))),
         gleaid = coalesce(!!!syms(grep("^gleaid", names(.), value = TRUE))),
         fips = coalesce(!!!syms(grep("^fips", names(.), value = TRUE))))

school_with_district_info <- merged_school_data %>% left_join(raw_datasets$directory, by = c("leaid", "year"), suffix = c("_school", ""))
schools_in_geo_districts <- school_with_district_info %>% filter(agency_type %in% c(1, 2))

district_level_data <- schools_in_geo_districts %>%
  mutate(
    across(all_of(VARIABLES_TO_SUM), ~ if_else(. %in% SPECIAL_VALUES_TO_NA, NA_real_, as.numeric(.))),
    across(c(enrl_IB, enrl_gifted_talented, enrl_AP), ~ if_else(. < 0, NA_real_, .)),
    teachers_certified_fte = if_else(!is.na(teachers_certified_fte) & !is.na(teachers_fte_crdc) & teachers_certified_fte > teachers_fte_crdc, teachers_fte_crdc, teachers_certified_fte),
    has_fiber = if_else(as.numeric(sch_internet_fiber) == 1, 1, 0, missing = 0)
  ) %>%
  filter(!is.na(enrollment_crdc) & enrollment_crdc > 0) %>%
  group_by(leaid) %>%
  summarise(
    across(all_of(VARIABLES_TO_SUM), ~ sum(.x, na.rm = TRUE), .names = "total_{.col}"),
    total_schools_with_fiber = sum(has_fiber, na.rm = TRUE),
    total_schools = n(),
    gleaid = first(na.omit(gleaid)), fips = first(na.omit(fips)), agency_level = first(na.omit(agency_level)), .groups = "drop"
  )

districts_prepped_for_join <- district_level_data %>%
  filter(!is.na(gleaid) & !is.na(fips)) %>%
  mutate(full_gleaid = paste0(str_pad(fips, 2, "left", "0"), gleaid),
         GEOGRAPHY_TYPE = case_when(agency_level == 1 ~ "Elementary School District", agency_level %in% c(2, 3, 7) ~ "Secondary School District",
                                    agency_level == 4 ~ "Unified School District", TRUE ~ NA_character_)) %>%
  filter(!is.na(GEOGRAPHY_TYPE))

geo_income_db <- readRDS(here::here("01_data", "processed", "geographic_income_database_harmonized.rds"))
geo_income_db_prepped <- geo_income_db %>%
  filter(geography_level %in% c("Elementary School District", "Secondary School District", "Unified School District")) %>%
  select(TL_GEO_ID, GEOGRAPHY_TYPE = geography_level, median_household_income_final)

data_with_income <- districts_prepped_for_join %>% left_join(geo_income_db_prepped, by = c("full_gleaid" = "TL_GEO_ID", "GEOGRAPHY_TYPE")) %>%
  filter(!is.na(median_household_income_final))

final_analysis_data <- data_with_income %>%
  mutate(
    ## MODIFICATION: Calculate fiber_internet_rate here at the district level.
    certified_teacher_rate = total_teachers_certified_fte / total_teachers_fte_crdc,
    fiber_internet_rate = total_schools_with_fiber / total_schools,
    rate_ib = if_else(total_enrollment_crdc > 0, total_enrl_IB / total_enrollment_crdc, NA_real_),
    rate_gt = if_else(total_enrollment_crdc > 0, total_enrl_gifted_talented / total_enrollment_crdc, NA_real_),
    rate_ap = if_else(total_enrollment_crdc > 0, total_enrl_AP / total_enrollment_crdc, NA_real_)
  ) %>%
  rowwise() %>%
  mutate(
    rates_vector = list(c(rate_ib, rate_gt, rate_ap)),
    positive_rates = list(rates_vector[rates_vector > 0 & !is.na(rates_vector)]),
    advanced_coursework_rate = if(length(positive_rates) > 0) mean(positive_rates) else 0
  ) %>%
  ungroup() %>%
  select(-rates_vector, -positive_rates) %>%
  mutate(across(c(certified_teacher_rate, advanced_coursework_rate, fiber_internet_rate), ~if_else(is.nan(.) | is.infinite(.), NA_real_, .)))

saveRDS(final_analysis_data, file = PROCESSED_DATA_FILE)
message(paste("Data preparation complete. Final file saved to:", PROCESSED_DATA_FILE))