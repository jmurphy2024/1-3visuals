# ==============================================================================
# SCRIPT: NHIS_chronic_disease_2.R
# Purpose: Full 3-Country Chronic Disease Prevalence using NHIS
# Logic:   INCFAM07ON Midpoints + Spatial RPP + Temporal Inflation + Dynamic V2 Borders
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tidyr)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

# Load dynamic V2 cutoffs 
cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT
# ------------------------------------------------------------------------------
USER_SAMPLE <- "ih2020"

VARS_NEEDED <- c("SAMPWEIGHT", "INCFAM07ON", "REGION", "AGE",
                 "DIABETICEV", "CHEARTDIEV", "CANCEREV", "HYPERTENEV")

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "nhis_chronic_disease")
TARGET_FILE <- file.path(TARGET_DIR, "nhis_chronic_disease_raw_2.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS NHIS API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "nhis",
    samples = USER_SAMPLE,
    variables = VARS_NEEDED,
    description = "Three Countries NHIS Skyline - Chronic Disease (V2)"
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

# 3. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
region_rpp_lookup <- tibble(
  REGION_ID = c(1, 2, 3, 4),
  REG_RPP   = c(105.2, 92.8, 95.4, 104.1) # NE, MW, S, W
)

# NEW V2 STEP: Adjust 2020 dollars to 2023 Master Base Year dollars
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2020, base_year = 2023)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  left_join(region_rpp_lookup, by = c("REGION" = "REGION_ID")) %>%
  mutate(
    PERWT = as.numeric(SAMPWEIGHT),
    
    # Map the exact INCFAM07ON codes to continuous dollar midpoints
    raw_dollars = case_when(
      as.numeric(INCFAM07ON) == 11 ~ 17500,  # $0 - $34,999
      as.numeric(INCFAM07ON) == 12 ~ 42500,  # $35,000 - $49,999
      as.numeric(INCFAM07ON) == 22 ~ 62500,  # $50,000 - $74,999
      as.numeric(INCFAM07ON) == 23 ~ 87500,  # $75,000 - $99,999
      as.numeric(INCFAM07ON) == 24 ~ 150000, # $100,000+
      TRUE ~ NA_real_            
    ),
    
    # REAL INCOME: Adjusted for Inflation AND Purchasing Power (RPP)
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)), 
    
    # Dynamic V2 Country Assignment (Replaces hardcoded 45000/115000)
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    ),
    
    # INDICATOR: Combined Chronic Burden (1=No, 2=Yes)
    target_indicator = if_else(
      as.numeric(DIABETICEV) == 2 | as.numeric(CHEARTDIEV) == 2 | 
        as.numeric(CANCEREV) == 2 | as.numeric(HYPERTENEV) == 2, 1, 0
    )
  ) %>%
  # Filter universe: adults 18+, non-missing weights and income
  filter(!is.na(PERWT), PERWT > 0, AGE >= 18, !is.na(REAL_INCOME), !is.na(Country), !is.na(target_indicator))

# 4. VISUALIZATION
# ------------------------------------------------------------------------------
p <- plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Chronic Disease Prevalence (%)",
  plot_title = "NHIS_chronic_disease_2" # Updated V2 Title Convention
) +
  labs(
    caption = stringr::str_wrap("Note: Universe restricted to adults 18+. Income derived from NHIS categorical midpoints (INCFAM07ON), adjusted for inflation (2020 to 2023) and regional price parity (Census 4-Region). Boundaries align with Master ACS Baseline V2.", width = 110)
  ) +
  theme(
    plot.margin = margin(t = 20, r = 10, b = 50, l = 10),
    plot.caption = element_text(hjust = 0, size = 9, color = "grey30", margin = margin(t = 20))
  )

print(p)