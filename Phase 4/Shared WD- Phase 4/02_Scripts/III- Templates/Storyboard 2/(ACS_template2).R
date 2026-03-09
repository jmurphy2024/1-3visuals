# ==============================================================================
# SCRIPT: ACS_[variable]_2.R
# Purpose: Generate 3-Country Skyline for [Insert Topic]
# Logic:   V2 Aggregated Income (SERIAL + INCTOT), RPP Adjusted, Dynamic Borders
# Engine:  data.table for high-speed 15M+ row aggregation
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tibble)
library(data.table); library(scales)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
# Source the V2 Normalization script which contains plot_economic_skyline()
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization 2.R"))

# Load the mathematically pure V2 cutoffs
cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT
# ------------------------------------------------------------------------------
# [TODO 1]: Set your ACS Sample (e.g., "us2023c" for 5-Year, "us2022a" for 1-Year)
USER_SAMPLE <- "us2023c" 

# [TODO 2]: Add your new variable(s) to the end of this list. 
VARS_NEEDED <- c("SERIAL", "PERWT", "INCTOT", "STATEFIP", "AGE", "YOUR_VARIABLE_HERE")

# [TODO 3]: Create a unique folder and filename for this specific extract
TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_YOUR_VARIABLE")
TARGET_FILE <- file.path(TARGET_DIR, "acs_YOUR_VARIABLE_aggregated.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "usa", 
    samples = USER_SAMPLE,
    variables = VARS_NEEDED,
    description = "Three Countries ACS Skyline - [INSERT VARIABLE] (V2)"
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

# 3. CONSTRUCT TRUE HOUSEHOLD INCOME (data.table Engine)
# ------------------------------------------------------------------------------
message("Aggregating individual incomes by household...")
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

hh_aggregated <- dt_raw[
  INCTOT != 9999999, 
  .(AGGREGATED_HHINCOME = sum(INCTOT, na.rm = TRUE)), 
  by = SERIAL
]
hh_aggregated <- as_tibble(hh_aggregated)
message("Aggregation complete.")

# 4. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

INFLATION_ADJ <- get_inflation_multiplier(data_year = as.numeric(substring(USER_SAMPLE, 3, 6)), base_year = 2023)

prepared_data <- as_tibble(dt_raw) %>%
  left_join(hh_aggregated, by = "SERIAL") %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    PERWT = as.numeric(PERWT),
    
    # --------------------------------------------------------------------------
    # [TODO 4]: DEFINE YOUR TARGET INDICATOR HERE
    # --------------------------------------------------------------------------
    target_indicator = NA_real_, 
    
    REAL_INCOME = (AGGREGATED_HHINCOME * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # ----------------------------------------------------------------------------
# [TODO 5]: SET YOUR UNIVERSE FILTERS
# ----------------------------------------------------------------------------
filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), !is.na(target_indicator))

# 5. VISUALIZATION
# ------------------------------------------------------------------------------
p <- plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Your Y-Axis Label (%)",
  
  # --------------------------------------------------------------------------
  # [TODO 6]: PLOT TITLE RULES (DICTATES SAVED FILENAME)
  # Format: Database_Variable_2 
  # Example: "ACS_educational_attainment_2" or "ACS_health_insurance_2"
  # --------------------------------------------------------------------------
  plot_title = "ACS_your_variable_2"
) +
  labs(
    # [TODO 7]: Write your explanatory caption
    caption = stringr::str_wrap("Note: Universe restricted to [X]. Income is generated by natively aggregating personal income (INCTOT) within households. Data is adjusted for inflation and spatial price parity.", width = 110)
  ) +
  theme(
    plot.margin = margin(t = 20, r = 10, b = 50, l = 10),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey30", margin = margin(t = 20))
  )

print(p)