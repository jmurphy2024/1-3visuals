# ==============================================================================
# SCRIPT: IPUMS_MEPS_Chronic_Disease.R
# Purpose: Analyzing Chronic Condition Prevalence across the Three Countries
# Logic: II-D Normalization with Lowercase API Mnemonics
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales)

# 1. SOURCE MASTER LOGIC
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

# 2. CONFIGURATION 
USER_SAMPLE   <- "mp2023" 

# STRICT LOWERCASE API MNEMONICS:
# - diabdx:  Diabetes diagnosis
# - asthdx:  Asthma diagnosis
# - hibpdx:  High blood pressure diagnosis
# - choldx:  High cholesterol diagnosis
# - perweight, ftotval, region: Required for normalization
VARS_NEEDED   <- c("perweight", "ftotval", "region", "age", 
                   "diabdx", "asthdx", "hibpdx", "choldx")

TARGET_DIR    <- here::here("01_data", "raw", "IPUMS_Microdata", USER_SAMPLE)
TARGET_FILE   <- file.path(TARGET_DIR, "raw_data.rds")

# 3. ACQUISITION
if (!file.exists(TARGET_FILE)) {
  message("--- Fetching MEPS Chronic Disease Data ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "meps", 
    samples = USER_SAMPLE,
    variables = VARS_NEEDED,
    description = "Three Countries Chronic Disease Extract"
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

# 4. COMPLEX RECODING & REGIONAL RPP MAPPING
region_rpp_lookup <- tibble(
  REGION_CODE = c(1, 2, 3, 4),
  REG_RPP     = c(107.5, 93.1, 95.8, 103.8) # NE, MW, S, W
)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  left_join(region_rpp_lookup, by = c("REGION" = "REGION_CODE")) %>%
  mutate(
    PERWT      = as.numeric(PERWEIGHT),
    income_raw = as.numeric(FTOTVAL),
    
    # REAL INCOME: Adjusted for inflation (1.18) and Regional RPP
    REAL_INCOME = (income_raw * 1.18) * (100 / coalesce(REG_RPP, 100)),
    
    # INDICATOR: Multi-Morbidity / Any Chronic Condition
    # MEPS Codes: 1 = Yes, 2 = No
    # Recode so 'Yes' = 1 for the Skyline calculation
    target_indicator = if_else(
      DIABDX == 1 | ASTHDX == 1 | HIBPDX == 1 | CHOLDX == 1, 1, 0
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, AGE >= 18)

# 5. EXECUTION
final_data <- prepared_data %>%
  apply_three_countries_logic("REAL_INCOME", "REGION", adj_val = 1.0) # Logic already applied in mutate

# 6. VISUALIZATION
plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_format = "percent",
  y_axis_label = "Chronic Disease Prevalence (%)",
  plot_title = "MEPS_2018_Chronic_Condition_Skyline"
)