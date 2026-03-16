# ==============================================================================
# SCRIPT: IPUMS_NHIS_Full_Skyline_2020.R
# Purpose: Full 3-Country Chronic Disease Prevalence using NHIS 2020
# Logic: INCFAM07ON Midpoints + Spatial RPP + Country Assignment
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2)

# 1. SOURCE MASTER LOGIC
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

# 2. CONFIGURATION
USER_SAMPLE <- "ih2020" 

VARS_NEEDED <- c("SAMPWEIGHT", "INCFAM07ON", "REGION", "AGE",
                 "DIABETICEV", "CHEARTDIEV", "CANCEREV", "HYPERTENEV")

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", USER_SAMPLE)
TARGET_FILE <- file.path(TARGET_DIR, "raw_data.rds")

# 3. ACQUISITION
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS NHIS 2020 API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "nhis", 
    samples = USER_SAMPLE,
    variables = VARS_NEEDED,
    description = "Three Countries NHIS 2020 Full Spectrum"
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

# 4. COMPLEX RECODING & SPATIAL NORMALIZATION
region_rpp_lookup <- tibble(
  REGION_ID = c(1, 2, 3, 4),
  REG_RPP   = c(105.2, 92.8, 95.4, 104.1) # NE, MW, S, W
)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  left_join(region_rpp_lookup, by = c("REGION" = "REGION_ID")) %>%
  mutate(
    PERWT = as.numeric(SAMPWEIGHT),
    
    # Map the exact INCFAM07ON codes to continuous dollar midpoints
    raw_dollars = case_when(
      INCFAM07ON == 11 ~ 17500,  # $0 - $34,999
      INCFAM07ON == 12 ~ 42500,  # $35,000 - $49,999
      INCFAM07ON == 22 ~ 62500,  # $50,000 - $74,999
      INCFAM07ON == 23 ~ 87500,  # $75,000 - $99,999
      INCFAM07ON == 24 ~ 150000, # $100,000+
      TRUE ~ NA_real_            
    ),
    
    # REAL INCOME: Adjusted for Purchasing Power
    REAL_INCOME = raw_dollars * (100 / coalesce(REG_RPP, 100)),
    
    # MISSING LOGIC ADDED HERE: Assign to the Three Countries
    Country = case_when(
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    ),
    
    # INDICATOR: Combined Chronic Burden (1=No, 2=Yes)
    target_indicator = if_else(
      DIABETICEV == 2 | CHEARTDIEV == 2 | CANCEREV == 2 | HYPERTENEV == 2, 1, 0
    )
  ) %>%
  # Filter out missing values, including unmapped Country codes
  filter(!is.na(PERWT), PERWT > 0, AGE >= 18, !is.na(REAL_INCOME), !is.na(Country))

# 5. VISUALIZATION
plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Chronic Disease Prevalence (%)",
  plot_title = "NHIS_2020_Chronic_Disease"
)