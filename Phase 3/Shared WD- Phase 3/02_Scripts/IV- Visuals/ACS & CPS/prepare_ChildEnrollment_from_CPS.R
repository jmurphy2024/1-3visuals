# ==== 0. ABOUT ====
## WD location: 02_Scripts/IV-Visuals/ACS & CPS
## Script: prepare_ChildEnrollment_from_CPS.R
## Purpose: Prepares CPS Education Supplement data to calculate the child enrollment rate
##          by fine-grained income group for the dual-axis visualization.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Dependencies: dplyr, here, rlang, readr, purrr, stringr
## Input: Raw RDS files for CPS Ed Supplement (cps2023_10s) and ASEC donor (cps2023_03s).
## Output: 01_data/processed/summary_cps_child_enrollment.rds

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr); library(purrr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ==== 1. PARAMETERS ====
USER_SUPPLEMENT_SAMPLE_ID <- "cps2023_10s"
USER_INDICATOR_NAME       <- "Child_Enrollment"
USER_WEIGHT_VARIABLE      <- "EDSUPPWT"
USER_FINE_GROUP_LEVEL     <- "Groups_20"
USER_RANDOM_SEED          <- 20251002
INFLATION_FACTOR          <- 304.702 / 292.655 # CPI 2023 / 2022
FAMINC_RANGES             <- list(`100`=c(0,4999),`210`=c(5000,7499),`300`=c(7500,9999),`430`=c(10000,12499),`470`=c(12500,14999),`500`=c(15000,19999),`600`=c(20000,24999),`710`=c(25000,29999),`720`=c(30000,34999),`730`=c(35000,39999),`740`=c(40000,49999),`810`=c(50000,74999),`841`=c(75000,99999),`842`=c(100000,149999),`843`=c(150000,Inf))

# ==== 2. DATA PREPARATION ====
set.seed(USER_RANDOM_SEED)

RAW_DATA_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_INDICATOR_NAME, "_", USER_SUPPLEMENT_SAMPLE_ID))
raw_supp_data <- readRDS(file.path(RAW_DATA_DIR, "raw_supplement_data.rds"))
raw_asec_data <- readRDS(file.path(RAW_DATA_DIR, "raw_asec_donor_data.rds"))

cleaned_supp_data <- raw_supp_data %>% mutate(EDATT = if_else(EDATT == 99, NA_integer_, as.integer(EDATT)), FAMINC = if_else(FAMINC %in% c(995, 996, 997, 999), NA_integer_, as.integer(FAMINC)))
cleaned_asec_data <- raw_asec_data %>% mutate(HHINCOME = if_else(HHINCOME == 99999999, NA_real_, as.numeric(HHINCOME))) %>% filter(!is.na(HHINCOME) & ASECWT > 0)

message("Imputing income for CPS Ed Supplement...")
cleaned_asec_data <- cleaned_asec_data %>% mutate(HHINCOME_ADJ = HHINCOME * INFLATION_FACTOR)
imputation_pools <- map(FAMINC_RANGES, ~cleaned_asec_data %>% filter(HHINCOME >= .x[1] & HHINCOME < .x[2]))
imputed_incomes <- map_dbl(cleaned_supp_data$FAMINC, function(code) {
  if (is.na(code)) return(NA_real_)
  pool <- imputation_pools[[as.character(code)]]; if (is.null(pool) || nrow(pool) == 0) return(NA_real_)
  sample(pool$HHINCOME_ADJ, size = 1, prob = pool$ASECWT)
})
data_with_income <- cleaned_supp_data %>% mutate(HHINCOME_IMP = imputed_incomes) %>% filter(!is.na(HHINCOME_IMP))

main_cutoffs <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
within_tercile_borders <- read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)
data_with_groups <- assign_income_groups(data_with_income, within_tercile_borders, "HHINCOME_IMP", USER_FINE_GROUP_LEVEL, main_cutoffs$main_cutoff1, main_cutoffs$main_cutoff2)

summary_child_enrollment <- data_with_groups %>%
  mutate(is_child_enrolled = if_else(AGE < 18, if_else(EDATT == 1, 1, 0, missing = NA), NA_real_)) %>%
  filter(!is.na(fine_income_group)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(prop_child_enrolled = weighted.mean(is_child_enrolled, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE), .groups = "drop")

saveRDS(summary_child_enrollment, here::here("01_data", "processed", "summary_cps_child_enrollment.rds"))
message("CPS child enrollment summary saved.")
