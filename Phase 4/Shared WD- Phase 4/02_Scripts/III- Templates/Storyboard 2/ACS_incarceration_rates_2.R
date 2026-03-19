# ==============================================================================
# SCRIPT: ACS_incarceration_rate_2.R
# Purpose: Generate 3-Country Skyline for the Adult Incarceration Rate
# Logic:   Isolates Census Group Quarters (GQ) codes for Correctional Facilities
# Engine:  data.table for high-speed 15M+ row aggregation
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tibble)
library(data.table); library(scales); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 
# We pull GQ (Group Quarters) and GQTYPE to isolate correctional populations
VARS_NEEDED <- c("SERIAL", "PERWT", "INCTOT", "STATEFIP", "AGE", "GQ", "GQTYPE")
TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_incarceration_rate")
TARGET_FILE <- file.path(TARGET_DIR, "acs_incarceration_rate_aggregated.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", samples = USER_SAMPLE, variables = VARS_NEEDED,
    description = "Three Countries ACS Skyline - Incarceration Rate (V2)"
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

# 3. CONSTRUCT TRUE HOUSEHOLD INCOME
# ------------------------------------------------------------------------------
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

# Aggregate household income (Note: Incarcerated individuals are treated as 1-person households by the Census)
hh_aggregated <- as_tibble(dt_raw[INCTOT != 9999999, .(AGGREGATED_HHINCOME = sum(INCTOT, na.rm = TRUE)), by = SERIAL])
INFLATION_ADJ <- get_inflation_multiplier(data_year = as.numeric(substring(USER_SAMPLE, 3, 6)), base_year = 2023)

# 4. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
prepared_data <- as_tibble(dt_raw) %>%
  left_join(hh_aggregated, by = "SERIAL") %>%
  mutate(
    PERWT      = as.numeric(PERWT),
    gq_num     = as.numeric(GQ),
    gqtype_num = as.numeric(GQTYPE),
    
    # INDICATOR LOGIC: 
    # GQ == 3 (Institutional Group Quarters)
    # GQTYPE == 1 or 10-19 (Correctional Facilities: Federal, State, and Local Jails)
    target_indicator = if_else(gq_num == 3 & (gqtype_num == 1 | (gqtype_num >= 10 & gqtype_num < 20)), 1, 0),
    
    # --- ONE-STEP REAL INCOME NORMALIZATION ---
    REAL_INCOME = AGGREGATED_HHINCOME * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # UNIVERSE FILTER: All adults ages 18+
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), as.numeric(AGE) >= 18)

# 5. SUMMARY STATISTICS
# ------------------------------------------------------------------------------
message("\n=== INCARCERATION RATE SUMMARY ===")
print(as.data.frame(get_country_summary(prepared_data, "target_indicator", "PERWT")))

# 6. VISUALIZATION
# ------------------------------------------------------------------------------
plot_caption <- paste0(
  "Note: Universe includes all adults ages 18+. Incarceration is structurally identified via the Census Group Quarters (GQ) classification for Federal Prisons, State Prisons, and Local Jails."
)

p <- plot_economic_skyline(
  data          = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var    = "PERWT", 
  y_axis_label  = "Incarceration Rate (%)",
  plot_title    = "ACS_incarceration_rate_2",
  caption_text  = stringr::str_wrap(plot_caption, width = 130)
)
print(p)