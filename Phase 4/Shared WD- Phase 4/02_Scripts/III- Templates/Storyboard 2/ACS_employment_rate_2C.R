# ==============================================================================
# SCRIPT: ACS_employment_rate_2.R
# Purpose: Generate 3-Country Skyline for Employment-to-Population Ratio
# Logic:   Native HHINCOME, Negative Households Dropped, Person-Weighted
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tibble)
library(data.table); library(scales)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2B.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("Cutoffs not found.")
cutoffs <- readRDS(cutoffs_path)

USER_SAMPLE <- "us2023c" 
VARS_NEEDED <- c("SERIAL", "PERWT", "HHINCOME", "STATEFIP", "AGE", "EMPSTAT")
TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_employment_rate_native_hh")
TARGET_FILE <- file.path(TARGET_DIR, "acs_employment_rate_native_hh.rds")

if (!file.exists(TARGET_FILE)) {
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", 
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED,
    description = "Three Countries ACS Skyline - Employment Rate (Native HHINCOME)"
  )
  submitted <- submit_extract(extract_def)
  path <- download_extract(wait_for_extract(submitted), download_dir = TARGET_DIR)
  raw_data <- read_ipums_micro(read_ipums_ddi(path[grep("\\.xml$", path)]), verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  raw_data <- readRDS(TARGET_FILE)
}

dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

dt_raw[, HHINCOME_clean := as.numeric(HHINCOME)]
dt_raw[HHINCOME_clean == 9999999, HHINCOME_clean := NA_real_]
dt_filtered <- dt_raw[HHINCOME_clean >= 0]

INFLATION_ADJ <- get_inflation_multiplier(data_year = as.numeric(substring(USER_SAMPLE, 3, 6)), base_year = 2023)

prepared_data <- as_tibble(dt_filtered) %>%
  mutate(
    PERWT = as.numeric(PERWT),
    emp_num = as.numeric(EMPSTAT),
    target_indicator = if_else(emp_num == 1, 1, 0),
    REAL_INCOME = HHINCOME_clean * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), 
         emp_num > 0, as.numeric(AGE) >= 25 & as.numeric(AGE) <= 64)

message("\n=== EMPLOYMENT RATE SUMMARY ===")
print(as.data.frame(get_country_summary(prepared_data, "target_indicator", "PERWT")))

plot_caption <- paste0(
  "Note: Universe restricted to prime working-age adults (25-64) to calculate a structural employment-to-population ratio.\n",
  "Income uses the native Census household income variable (HHINCOME) with negative households excluded.\n",
  "Data is adjusted for inflation and spatial price parity."
)

p <- plot_economic_skyline(
  data          = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var    = "PERWT", 
  y_axis_label  = "Employed (Ages 25-64) (%)",
  plot_title    = "ACS_employment_rate_2",
  caption_text  = plot_caption
)
print(p)