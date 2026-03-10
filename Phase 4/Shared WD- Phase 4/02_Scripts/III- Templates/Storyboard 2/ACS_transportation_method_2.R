# ==============================================================================
# SCRIPT: ACS_transportation_private_wfh_2.R
# Purpose: Comparison of Private Vehicle vs. Work From Home
# Logic:   V2 Aggregated Income, RPP Adjusted, Definitive Skyline Standard
# Engine:  data.table for high-speed 15M+ row aggregation
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2); library(data.table); library(scales); library(tidyr)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
# NOTE: Ensure your II-D Income Normalization2.R has the plot_economic_skyline_2 function!
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & DATA LOADING
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 
VARS_NEEDED <- c("SERIAL", "PERWT", "INCTOT", "STATEFIP", "AGE", "TRANWORK")
TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_transportation")
TARGET_FILE <- file.path(TARGET_DIR, "acs_transportation_aggregated.rds")

# Load existing data from your earlier transportation runs
if (!file.exists(TARGET_FILE)) stop("Data file not found. Run the extraction script first.")
raw_data <- readRDS(TARGET_FILE)

# 3. CONSTRUCT AGGREGATED INCOME (Standardized Name)
# ------------------------------------------------------------------------------
message("Aggregating household income...")
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

# We name this column HH_INCOME_V2 to be safe and distinct
hh_aggregated <- dt_raw[
  INCTOT != 9999999, 
  .(HH_INCOME_V2 = sum(INCTOT, na.rm = TRUE)), 
  by = SERIAL
]
message("Aggregation complete.")

# 4. NORMALIZATION & FOCUSED RECODING
# ------------------------------------------------------------------------------
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

prepared_data <- as_tibble(dt_raw) %>%
  left_join(hh_aggregated, by = "SERIAL") %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    PERWT = as.numeric(PERWT),
    tran_num = as.numeric(TRANWORK),
    
    # --------------------------------------------------------------------------
    # INDICATOR LOGIC: Focusing on Private vs. WFH
    # --------------------------------------------------------------------------
    Private_Vehicle = if_else(tran_num >= 10 & tran_num <= 29, 1, 0),
    Work_From_Home  = if_else(tran_num == 40, 1, 0), # Correct Code for WFH
    
    # Using the synchronized HH_INCOME_V2 name
    REAL_INCOME = (HH_INCOME_V2 * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # Filter for working population
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), tran_num > 0)

# 5. VISUALIZATION (MULTI-LINE)
# ------------------------------------------------------------------------------
p <- plot_economic_skyline_2(
  data           = prepared_data, 
  indicator_vars = c("Private_Vehicle", "Work_From_Home"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Commute Mode Share (%)",
  plot_title     = "ACS_transportation_private_wfh_2"
)

print(p)