# ==============================================================================
# SCRIPT: CPS_Basic_Voting_2.R
# Purpose: Generate 3-Country Skyline for Nov 2024 Voting Supplement
# Logic:   FAMINC Midpoints + Spatial RPP + Temporal Inflation + Dynamic V2 Borders
# Note:    INCTOT is not collected in CPS Basic months; FAMINC midpoints are required.
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales); library(tidyr)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION 
# ------------------------------------------------------------------------------
USER_SAMPLE      <- "cps2024_11s" 
SUPP_WEIGHT_VAR  <- "WTFINL"      
VARS_NEEDED      <- c(SUPP_WEIGHT_VAR, "FAMINC", "STATEFIP", "AGE", "VOTED", "VOREG")

TARGET_DIR       <- here::here("01_data", "raw", "IPUMS_Microdata", "cps_voting")
TARGET_FILE      <- file.path(TARGET_DIR, "cps_voting_raw_2.rds")

# 3. ACQUISITION (API Recovery)
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Fetching Nov 2024 Voting Data via IPUMS API ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "cps", 
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED,
    description = "Three Countries Nov 2024 Voting Extract (V2)"
  )
  
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  raw_data <- readRDS(TARGET_FILE)
}

# 4. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

# Deflate 2024 dollars back to the 2023 Master Base Year
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2024, base_year = 2023)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    PERWT = as.numeric(get(SUPP_WEIGHT_VAR)),
    
    # FAMINC Midpoint Logic
    raw_dollars = case_when(
      as.numeric(FAMINC) == 100 ~ 2500,   as.numeric(FAMINC) == 210 ~ 7500,   as.numeric(FAMINC) == 300 ~ 11250,
      as.numeric(FAMINC) == 430 ~ 13750,  as.numeric(FAMINC) == 470 ~ 17500,  as.numeric(FAMINC) == 500 ~ 22500,
      as.numeric(FAMINC) == 600 ~ 27500,  as.numeric(FAMINC) == 710 ~ 32500,  as.numeric(FAMINC) == 720 ~ 37500,
      as.numeric(FAMINC) == 730 ~ 45000,  as.numeric(FAMINC) == 740 ~ 55000,  as.numeric(FAMINC) == 820 ~ 67500,
      as.numeric(FAMINC) == 830 ~ 87500,  as.numeric(FAMINC) == 841 ~ 125000, as.numeric(FAMINC) == 842 ~ 200000, 
      TRUE ~ NA_real_
    ),
    
    # Apply Inflation and RPP
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    
    # Dynamic V2 Boundaries
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    ),
    
    # INDICATOR: Voted (Code 2)
    target_indicator = if_else(as.numeric(VOTED) == 2, 1, 0)
  ) %>%
  filter(AGE >= 18, !is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), !is.na(target_indicator))

# 5. VISUALIZATION
# ------------------------------------------------------------------------------
p <- plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "2024 Voter Turnout Rate (%)",
  plot_title = "CPS_voting_turnout_2"
) +
  labs(
    caption = stringr::str_wrap("Note: Universe restricted to adults 18+. Income derived from CPS categorical midpoints (FAMINC), adjusted for inflation (2024 to 2023) and spatial price parity. Boundaries align with Master ACS Baseline V2.", width = 110)
  )

print(p)