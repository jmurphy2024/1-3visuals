# ==============================================================================
# SCRIPT: CPS_ASEC_template_2.R
# Purpose: Generate 3-Country Skyline for [Insert Variable] using CPS-ASEC
# Logic:   V2 Aggregated Income (SERIAL + INCTOT), RPP Adjusted, Dynamic Borders
# Engine:  data.table for high-speed household aggregation
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales); library(data.table)

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
USER_SAMPLE   <- "cps2023_03s" 

# CHANGED: Replaced HHINCOME with SERIAL and INCTOT
VARS_NEEDED   <- c("SERIAL", "ASECWT", "INCTOT", "STATEFIP", "AGE", "EMPSTAT")

TARGET_DIR    <- here::here("01_data", "raw", "IPUMS_Microdata", "cps_asec_v2")
TARGET_FILE   <- file.path(TARGET_DIR, "cps_asec_raw_2.rds")

# 3. ACQUISITION
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "cps", 
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED,
    description = "Three Countries CPS-ASEC Extract (V2 INCTOT Aggregation)"
  )
  
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  message("--- Loading existing CPS-ASEC data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 4. CONSTRUCT TRUE HOUSEHOLD INCOME (data.table Engine)
# ------------------------------------------------------------------------------
message("Aggregating individual incomes by household...")
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

# Native aggregation of INCTOT (Filters out CPS missing/NIU codes like 99999999)
hh_aggregated <- dt_raw[
  INCTOT < 99999999, 
  .(AGGREGATED_HHINCOME = sum(INCTOT, na.rm = TRUE)), 
  by = SERIAL
]
hh_aggregated <- as_tibble(hh_aggregated)
message("Aggregation complete.")

# 5. NORMALIZATION & RECODING
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
    PERWT = ASECWT,
    
    # --------------------------------------------------------------------------
    # INDICATOR LOGIC: Employment Rate
    # --------------------------------------------------------------------------
    target_indicator = if_else(EMPSTAT %in% c(10, 12), 1, 0),
    
    # CHANGED: Using aggregated income rather than pre-packaged HHINCOME
    REAL_INCOME = (AGGREGATED_HHINCOME * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), !is.na(target_indicator), AGE >= 16)

# 6. VISUALIZATION
# ------------------------------------------------------------------------------
p <- plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Employment Rate (%)",
  plot_title = "CPS_ASEC_employment_rate_2" 
) +
  labs(
    caption = stringr::str_wrap("Note: Universe restricted to civilian adults 16+. Income is generated by natively aggregating personal income (INCTOT) within households. Data is adjusted for inflation and spatial price parity.", width = 110)
  )

print(p)