# ===================================================================
# TEMPLATE 2b: ANALYSIS & CALCULATION (IPUMS CPS)
# ===================================================================
# Project: 1/3 Country Project Visualizations
# Author: Gemini
# Date: 2025-10-01
#
# Purpose:
# This script loads the raw CPS supplement and ASEC donor .rds files
# created by Template 1b. It performs income imputation, assigns
# income groups, calculates derived variables, and saves the final
# aggregated data as a new .rds checkpoint.
# ===================================================================

# ==== 0. SETUP & PARAMETERS ====
library(dplyr); library(readr); library(here); library(survey); library(rlang)
source(here::here("II_Shared_Functions.R"))

# ---- 0.3 USER-DEFINED PARAMETERS (CPS) ----
SUPPLEMENT_DATA_PATH <- here::here("data", "ipums_extracts", "CPS_cps2023_10s_Extract_3_2025-10-01", "cps2023_10s_raw_data.rds")
ASEC_DONOR_DATA_PATH <- here::here("data", "ipums_extracts", "CPS_cps2023_03s_Extract_4_2025-10-01", "cps2023_03s_raw_data.rds")
SUPPLEMENT_WEIGHT_VAR <- "EDSUPPWT"; CATEGORICAL_INCOME_VAR <- "FAMINC"; IMPUTED_INCOME_VAR <- "HHINCOME_IMP_2023"
CPI_2022 <- 292.655; CPI_2023 <- 304.702; INFLATION_FACTOR <- CPI_2023 / CPI_2022
SUMMARY_OUTPUT_FILENAME <- "cps_edsupp_cps2023_10s_summary_stats_G_groups.rds"

# ---- 0.4 Define Input/Output Paths ----
summary_output_dir <- here::here("data", "summary_outputs")
dir.create(summary_output_dir, showWarnings=FALSE, recursive=TRUE)
borders_csv_file <- file.path(summary_output_dir, "within_tercile_quantile_borders.csv")
main_cutoffs_rds_file <- file.path(summary_output_dir, "main_tercile_cutoffs.rds")
summary_output_path <- file.path(summary_output_dir, SUMMARY_OUTPUT_FILENAME)


# ==== 1. LOAD & CLEAN DATA ====
cps_supplement <- readRDS(SUPPLEMENT_DATA_PATH)
cps_asec <- readRDS(ASEC_DONOR_DATA_PATH)
asec_cleaned <- cps_asec %>%
  filter(!is.na(ASECWT) & ASECWT > 0, !is.na(HHINCOME) & HHINCOME < 99999999) %>%
  mutate(HHINCOME_ADJUSTED = HHINCOME * INFLATION_FACTOR)
supplement_cleaned <- cps_supplement %>%
  mutate(EDATT = if_else(EDATT == 99, NA_integer_, as.integer(EDATT)), FAMINC = if_else(FAMINC %in% c(995:997, 999), NA_integer_, as.integer(FAMINC))) %>%
  filter(!is.na(.data[[SUPPLEMENT_WEIGHT_VAR]]) & .data[[SUPPLEMENT_WEIGHT_VAR]] > 0)


# ==== 2. PERFORM INCOME IMPUTATION ====
faminc_ranges <- list(`100`=c(0,4999), `210`=c(5000,7499), `300`=c(7500,9999), `430`=c(10000,12499), `470`=c(12500,14999), `500`=c(15000,19999), `600`=c(20000,24999), `710`=c(25000,29999), `720`=c(30000,34999), `730`=c(35000,39999), `740`=c(40000,49999), `810`=c(50000,74999), `841`=c(75000,99999), `842`=c(100000,149999), `843`=c(150000, Inf))
faminc_ranges_adj <- lapply(faminc_ranges, function(r) r * INFLATION_FACTOR)
imputation_pools <- lapply(faminc_ranges_adj, function(range) { asec_cleaned %>% filter(HHINCOME_ADJUSTED >= range[1] & HHINCOME_ADJUSTED < range[2]) })
impute_income <- function(faminc_code) {
  pool <- imputation_pools[[as.character(faminc_code)]]
  if (is.null(pool) || nrow(pool) == 0) return(NA_real_)
  sample(pool$HHINCOME_ADJUSTED, size = 1, prob = pool$ASECWT, replace = TRUE)
}
set.seed(2025042101)
supplement_cleaned[[IMPUTED_INCOME_VAR]] <- sapply(supplement_cleaned[[CATEGORICAL_INCOME_VAR]], impute_income)
data_imputed <- supplement_cleaned %>% filter(!is.na(.data[[IMPUTED_INCOME_VAR]]))


# ==== 3. ASSIGN INCOME GROUPS ====
borders_df <- read_csv(borders_csv_file, show_col_types = FALSE)
main_cutoffs <- readRDS(main_cutoffs_rds_file)
temp_survey_obj <- svydesign(ids=~1, data=data_imputed, weights=as.formula(paste0("~", SUPPLEMENT_WEIGHT_VAR)))
temp_survey_obj_with_groups <- assign_income_groups(
  survey_obj = temp_survey_obj, borders_df = borders_df, income_var_name = IMPUTED_INCOME_VAR,
  detail_level = "Groups_4", main_cutoff1 = main_cutoffs$main_cutoff1, main_cutoff2 = main_cutoffs$main_cutoff2
)
data_with_groups <- temp_survey_obj_with_groups$variables
data_final <- data_imputed %>%
  left_join(select(data_with_groups, SERIAL, PERNUM, income_tercile, fine_income_group), by = c("SERIAL", "PERNUM"))


# ==== 4. CALCULATE DERIVED & SUMMARY VARIABLES ====
data_final <- data_final %>%
  mutate(
    is_child_enrolled_5_17 = case_when(AGE >= 5 & AGE <= 17 & EDATT == 1 ~ 1, AGE >= 5 & AGE <= 17 & EDATT == 2 ~ 0, TRUE ~ NA_real_),
    is_adult_formal_enrolled = if_else(AGE >= 25 & (EDATT == 1 | EDVOCA == 2), 1, 0, missing = NA_real_)
  )
summary_statistics_cps <- data_final %>%
  filter(!is.na(fine_income_group)) %>% group_by(income_tercile, fine_income_group) %>%
  summarise(
    prop_child_enrolled_5_17 = weighted.mean(is_child_enrolled_5_17, w = .data[[SUPPLEMENT_WEIGHT_VAR]], na.rm = TRUE),
    n_weighted_children_age_5_to_17 = sum(if_else(AGE >= 5 & AGE <= 17, .data[[SUPPLEMENT_WEIGHT_VAR]], 0), na.rm = TRUE),
    .groups = "drop"
  )


# ==== 5. SAVE RESULTS AS .RDS CHECKPOINT ====
print(paste("Saving CPS summary statistics checkpoint to:", summary_output_path))
saveRDS(summary_statistics_cps, file = summary_output_path)
print("--- CPS Analysis & Calculation Template Finished ---")
