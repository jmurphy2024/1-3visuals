# ==============================================================================
# SCRIPT: ACS_Educational_Attainment
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2)

# 1. SOURCE MASTER LOGIC
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

# 2. CONFIGURATION
USER_SAMPLE <- "us2023c" 

# IPUMS USA (ACS) Variables:
# - FTOTINC: Total family income (Continuous dollars to define your Countries)
# - EDUC:    Educational attainment category
# - AGE:     Needed to restrict the universe to 25+
VARS_NEEDED <- c("PERWT", "FTOTINC", "EDUC", "STATEFIP", "AGE")

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", USER_SAMPLE)
TARGET_FILE <- file.path(TARGET_DIR, "raw_data.rds")

# 3. ACQUISITION
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "usa", 
    samples = USER_SAMPLE,
    variables = VARS_NEEDED,
    description = "Three Countries ACS Educational Attainment (BA+)"
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

# 4. COMPLEX RECODING
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  mutate(
    PERWT = as.numeric(PERWT),
    
    # Filter missing/top-coded FTOTINC (9999999 is N/A in ACS)
    REAL_INCOME = if_else(FTOTINC < 9999999, as.numeric(FTOTINC), NA_real_),
    
    # INDICATOR: Bachelor's Degree or Higher
    # EDUC codes: 10 = 4 years of college (BA), 11 = 5+ years (Grad/Prof)
    target_indicator = if_else(as.numeric(EDUC) >= 10, 1, 0),
    
    # Assign to the Three Countries strictly based on Nominal Income
    Country = case_when(
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # UNIVERSE: Must restrict to AGE >= 25 for accurate education statistics
  filter(!is.na(PERWT), PERWT > 0, AGE >= 25, !is.na(REAL_INCOME), !is.na(Country))

# 5. VISUALIZATION
plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Bachelor's Degree or Higher (%)",
  plot_title = "ACS_2022_Educational_Attainment_Skyline"
)