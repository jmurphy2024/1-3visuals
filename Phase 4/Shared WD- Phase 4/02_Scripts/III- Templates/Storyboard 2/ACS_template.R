# ==============================================================================
# SCRIPT: ACS_template.R
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales)

# 1. SOURCE MASTER LOGIC
# Points to your renamed II-D Income Normalization and Design script
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

# 2. CONFIGURATION 
USER_SAMPLE   <- "acs_2023_5yr" 
VARS_NEEDED   <- c("PERWT", "HHINCOME", "STATEFIP", "ADJUST", "AGE", "EDUC")
TARGET_DIR    <- here::here("01_data", "raw", "IPUMS_Microdata", USER_SAMPLE)
TARGET_FILE   <- file.path(TARGET_DIR, "raw_data.rds")

# 3. ACQUISITION (API Recovery)
# Automatically handles the download if the file is missing from your ASU Dropbox
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS API for 2023 5-Year Sample ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "usa", 
    samples = "us2023c", # Official IPUMS code for 2023 5-year
    variables = VARS_NEEDED,
    description = "Three Countries ACS 2023 5-Year Master"
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
  # UNIVERSE: Adults 25+ for longitudinal education analysis
  filter(AGE >= 25) %>% 
  mutate(
    # Clean top-coded household income
    HHINCOME = if_else(HHINCOME >= 9999999, NA_real_, as.numeric(HHINCOME)),
    # INDICATOR: Bachelor's Degree or Higher (EDUC >= 10)
    target_indicator = if_else(EDUC >= 10, 1, 0),
    # 5-Year Inflation Adjustment factor
    ADJUST_FACTOR = as.numeric(ADJUST)
  )

# 5. EXECUTION & VISUALIZATION
# Normalizes dollars using RPP and the multi-year ADJUST factor
final_data <- prepared_data %>%
  apply_three_countries_logic("HHINCOME", "STATEFIP", adj_val = prepared_data$ADJUST_FACTOR)

# Generate the Connected Economic Skyline with transition-colored labels
plot_economic_skyline(
  data = final_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_format = "percent",
  y_axis_label = "College Graduation Rate (%)",
  plot_title = "ACS_College_Graduation_Rate"
)