# ==============================================================================
# SCRIPT: CPS_Voting_Supplement_Master.R
# Purpose: Processing the November 2024 Voting & Registration Supplement
# Logic: II-D Normalization with API-Corrected Variable Names
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales)

# 1. SOURCE MASTER LOGIC
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

# 2. CONFIGURATION 
USER_SAMPLE      <- "cps2024_11s" 

# Correct Variable Names for 2024 IPUMS API:
# WTFINL (Final Weight), VOTED (Did you vote?), VOREG (Were you registered?)
SUPP_WEIGHT_VAR  <- "WTFINL"      
# New: FAMINC (Available in Nov)
VARS_NEEDED <- c(SUPP_WEIGHT_VAR, "FAMINC", "STATEFIP", "AGE", "VOTED", "VOREG")

TARGET_DIR       <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_SAMPLE))
TARGET_FILE      <- file.path(TARGET_DIR, "raw_data.rds")

# 3. ACQUISITION (API Recovery)
if (!file.exists(TARGET_FILE)) {
  message("--- Fetching Nov 2024 Voting Data via IPUMS API ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "cps", 
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED,
    description = "Three Countries Nov 2024 Voting Extract (API V3)"
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

# ==============================================================================
# SCRIPT: CPS_Voting_Supplement_Master.R (FAMINC Fix)
# ==============================================================================
# ... (Source and Config sections remain same)

# 4. COMPLEX RECODING
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  rename(PERWT = !!sym(SUPP_WEIGHT_VAR)) %>% 
  filter(AGE >= 18) %>% 
  mutate(
    # FAMINC Midpoint Logic: Converts categorical IPUMS codes to numeric dollars
    # IPUMS code 841 ($75k-100k) becomes 87500, etc.
    income_numeric = case_when(
      FAMINC == 100 ~ 2500,   FAMINC == 210 ~ 7500,   FAMINC == 300 ~ 11250,
      FAMINC == 430 ~ 13750,  FAMINC == 470 ~ 17500,  FAMINC == 500 ~ 22500,
      FAMINC == 600 ~ 27500,  FAMINC == 710 ~ 32500,  FAMINC == 720 ~ 37500,
      FAMINC == 730 ~ 45000,  FAMINC == 740 ~ 55000,  FAMINC == 820 ~ 67500,
      FAMINC == 830 ~ 87500,  FAMINC == 841 ~ 125000, FAMINC == 842 ~ 200000, 
      TRUE ~ NA_real_
    ),
    
    target_indicator = if_else(VOTED == 2, 1, 0)
  )

# 5. EXECUTION
final_data <- prepared_data %>%
  apply_three_countries_logic("income_numeric", "STATEFIP", adj_val = 1.0)

# Generate Skyline
plot_economic_skyline(
  data = final_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "2024 Voter Turnout Rate (%)",
  plot_title = "CPS_2024_Nov_Voting_Turnout"
)