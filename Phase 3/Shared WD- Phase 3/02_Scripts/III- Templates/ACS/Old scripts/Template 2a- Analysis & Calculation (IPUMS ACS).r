# ===================================================================
# TEMPLATE 2a: ANALYSIS & CALCULATION (IPUMS ACS)
# ===================================================================
# Project: 1/3 Country Project Visualizations
# Author: Janica Murphy/ Gemini
# Date: 2025-10-01
#
# Purpose:
# This script loads the raw ACS data checkpoint (.rds file) created by
# Template 1a. It then performs all cleaning, derived variable
# creation, and summary statistic calculations before saving the final,
# aggregated data as a new .rds checkpoint.
# ===================================================================

# ==== 0. SETUP & PARAMETERS ====

# ---- 0.1 Load Core Packages ----
library(dplyr); library(readr); library(here); library(survey); library(rlang); library(tidyr)

# ---- 0.2 Source Shared Functions ----
source(here::here("II_Shared_Functions.R"))

# ---- 0.3 USER-DEFINED PARAMETERS (ACS) ----
# >>> Point this to the .rds file created by script 1a <<<
RAW_DATA_RDS_PATH <- here::here("data", "ipums_extracts", "ACS_us2023a_Extract_1_2025-10-01", "us2023a_raw_data.rds")
INCOME_VAR <- "HHINCOME"
HH_WEIGHT_VAR <- "HHWT"
PERSON_WEIGHT_VAR <- "PERWT"
SUMMARY_OUTPUT_FILENAME <- "acs_us2023a_summary_stats_G_groups.rds"

# ---- 0.4 Define Input/Output Paths ----
summary_output_dir <- here::here("data", "summary_outputs")
dir.create(summary_output_dir, showWarnings = FALSE, recursive = TRUE)
borders_csv_file <- file.path(summary_output_dir, "within_tercile_quantile_borders.csv")
main_cutoffs_rds_file <- file.path(summary_output_dir, "main_tercile_cutoffs.rds")
summary_output_path <- file.path(summary_output_dir, SUMMARY_OUTPUT_FILENAME)


# ==== 1. LOAD & CLEAN ACS DATA ====
if (!file.exists(RAW_DATA_RDS_PATH)) stop(paste("FATAL ERROR: Raw data file not found:", RAW_DATA_RDS_PATH))
data_to_process <- readRDS(RAW_DATA_RDS_PATH)

# ---- 1.2 Comprehensive Data Cleaning ----
data_cleaned <- data_to_process %>%
  mutate(
    HHINCOME = if_else(HHINCOME %in% c(9999999), NA_real_, as.numeric(HHINCOME)),
    POVERTY = if_else(POVERTY %in% c(0), NA_integer_, as.integer(POVERTY)),
    EMPSTAT = if_else(EMPSTAT %in% c(0, 9), NA_integer_, as.integer(EMPSTAT)),
    HISPAN = if_else(HISPAN %in% c(9), NA_integer_, as.integer(HISPAN)),
    CITIZEN = if_else(CITIZEN %in% c(0, 8, 9), NA_integer_, as.integer(CITIZEN)),
    OWNCOST = if_else(OWNCOST %in% c(99999), NA_real_, as.numeric(OWNCOST)),
    RENTGRS = if_else(RENTGRS %in% c(0, 1), NA_real_, as.numeric(RENTGRS))
  ) %>%
  filter(HHWT > 0 & PERWT > 0)
data_cleaned_unfiltered_age <- data_cleaned
data_cleaned <- data_cleaned %>% filter(AGE >= 18)


# ==== 2. ASSIGN INCOME GROUPS ====
borders_df <- read_csv(borders_csv_file, show_col_types = FALSE)
main_cutoffs <- readRDS(main_cutoffs_rds_file)
temp_data_for_grouping <- data_cleaned_unfiltered_age %>% filter(!is.na(.data[[INCOME_VAR]]))
temp_survey_obj <- svydesign(ids = ~1, data = temp_data_for_grouping, weights = as.formula(paste0("~", HH_WEIGHT_VAR)))
temp_survey_obj_with_groups <- assign_income_groups(
  survey_obj = temp_survey_obj, borders_df = borders_df, income_var_name = INCOME_VAR,
  detail_level = "Groups_4", main_cutoff1 = main_cutoffs$main_cutoff1, main_cutoff2 = main_cutoffs$main_cutoff2
)
group_assignments <- temp_survey_obj_with_groups$variables %>% distinct(SERIAL, income_tercile, fine_income_group)
data_final <- data_cleaned %>% left_join(group_assignments, by = "SERIAL")
data_final_unfiltered_age <- data_cleaned_unfiltered_age %>% left_join(group_assignments, by = "SERIAL")


# ==== 3. CALCULATE DERIVED & SUMMARY VARIABLES ====
calculate_derived_vars_acs <- function(df) {
  df %>% mutate(
    is_employed = if_else(EMPSTAT == 1, 1, 0, missing = NA_real_),
    is_below_poverty = if_else(!is.na(POVERTY) & POVERTY <= 100, 1, 0, missing = 0),
    is_owner = if_else(OWNERSHP == 1, 1, 0, missing = 0), is_renter = if_else(OWNERSHP == 2, 1, 0, missing = 0),
    cost_burden = if_else(is_owner == 1 & !is.na(HHINCOME) & HHINCOME > 0, (OWNCOST * 12) / HHINCOME, NA_real_),
    is_burdened_owner = if_else(is_owner == 1 & !is.na(cost_burden) & cost_burden > 0.30, 1, 0, missing = 0),
    rent_burden = if_else(is_renter == 1 & !is.na(HHINCOME) & HHINCOME > 0, (RENTGRS * 12) / HHINCOME, NA_real_),
    is_burdened_renter = if_else(is_renter == 1 & !is.na(rent_burden) & rent_burden > 0.30, 1, 0, missing = 0),
    has_insurance = if_else(HCOVANY == 2, 1, 0, missing = 0), is_non_citizen = if_else(CITIZEN == 5, 1, 0, missing = NA_real_),
    race_category = case_when(
      !is.na(HISPAN) & HISPAN != 0 ~ "Hispanic", !is.na(HISPAN) & HISPAN == 0 & RACE == 1 ~ "White",
      !is.na(HISPAN) & HISPAN == 0 & RACE == 2 ~ "Black", !is.na(HISPAN) & HISPAN == 0 & RACE == 3 ~ "Indigenous",
      !is.na(HISPAN) & HISPAN == 0 & (RACE == 4 | RACE == 5) ~ "Asian", !is.na(HISPAN) & HISPAN == 0 & RACE == 6 ~ "Other",
      !is.na(HISPAN) & HISPAN == 0 & RACE %in% c(7,8,9) ~ "Two_Plus", TRUE ~ NA_character_
    ),
    is_employed_25_65 = if_else(AGE >= 25 & AGE <= 65, is_employed, NA_integer_),
    is_below_poverty_child = if_else(AGE < 18, is_below_poverty, NA_integer_)
  )
}
data_final <- calculate_derived_vars_acs(data_final)
data_final_unfiltered_age <- calculate_derived_vars_acs(data_final_unfiltered_age)

summary_stats_main <- data_final %>%
  filter(!is.na(fine_income_group)) %>% group_by(income_tercile, fine_income_group) %>%
  summarise(
    avg_age = weighted.mean(AGE, w = .data[[PERSON_WEIGHT_VAR]], na.rm = TRUE),
    prop_non_citizen = weighted.mean(is_non_citizen, w = .data[[PERSON_WEIGHT_VAR]], na.rm = TRUE),
    employment_rate_25_65 = weighted.mean(is_employed_25_65, w = .data[[PERSON_WEIGHT_VAR]], na.rm = TRUE),
    .groups = "drop"
  )
summary_child_poverty <- data_final_unfiltered_age %>%
  filter(!is.na(fine_income_group), AGE < 18) %>% group_by(income_tercile, fine_income_group) %>%
  summarise(prop_child_below_poverty = weighted.mean(is_below_poverty_child, w = .data[[PERSON_WEIGHT_VAR]], na.rm = TRUE), .groups = "drop")
total_pop <- data_final_unfiltered_age %>% filter(!is.na(fine_income_group)) %>% group_by(income_tercile, fine_income_group) %>% summarise(total_w_pop = sum(.data[[PERSON_WEIGHT_VAR]]), .groups="drop")
race_pop <- data_final_unfiltered_age %>% filter(!is.na(fine_income_group), !is.na(race_category)) %>% group_by(income_tercile, fine_income_group, race_category) %>% summarise(race_w_pop = sum(.data[[PERSON_WEIGHT_VAR]]), .groups="drop")
race_summary <- race_pop %>% left_join(total_pop, by=c("income_tercile", "fine_income_group")) %>% mutate(prop = race_w_pop / total_w_pop) %>% select(-race_w_pop, -total_w_pop) %>% pivot_wider(names_from = race_category, values_from = prop, names_prefix = "prop_race_", values_fill = 0)
summary_statistics_acs <- summary_stats_main %>%
  left_join(summary_child_poverty, by = c("income_tercile", "fine_income_group")) %>%
  left_join(race_summary, by = c("income_tercile", "fine_income_group"))

# ==== 4. SAVE RESULTS AS .RDS CHECKPOINT ====
print(paste("Saving ACS summary statistics checkpoint to:", summary_output_path))
saveRDS(summary_statistics_acs, file = summary_output_path)
print("--- ACS Analysis & Calculation Template Finished ---")
