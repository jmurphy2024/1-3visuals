# ==============================================================================
# SCRIPT: CPS_ASEC_template.R
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales)

# 1. SOURCE MASTER LOGIC
# This handles directory setup, RPP lookup, and shared functions
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

# 2. CONFIGURATION 
# Define the specific IPUMS sample and variables needed
USER_SAMPLE   <- "cps2023_03s"
VARS_NEEDED   <- c("ASECWT", "HHINCOME", "STATEFIP", "EMPSTAT", "AGE")
TARGET_DIR    <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_SAMPLE))
TARGET_FILE   <- file.path(TARGET_DIR, "raw_data.rds")

# 3. ACQUISITION (API Recovery)
# Automatically triggers IPUMS API if local data is missing
if (!file.exists(TARGET_FILE)) {
  message("--- File not found locally. Triggering IPUMS API Recovery ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "cps", 
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED,
    description = "Three Countries CPS-ASEC Extract"
  )
  
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  
  # Process DDI and save as RDS for future speed
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  message("--- Loading existing CPS-ASEC data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 4. COMPLEX RECODING
# Recode logic for Employment Rate
prepared_data <- raw_data %>%
  # Robust renaming: Handle both upper and lower case names from IPUMS
  rename_with(toupper, everything()) %>% 
  rename(PERWT = ASECWT) %>% # Ensure we use the Supplement Weight for ASEC
  # UNIVERSE: Civilian Non-Institutionalized Adults 16+
  filter(AGE >= 16) %>% 
  mutate(
    # Handle CPS missing income code 99999999
    HHINCOME = if_else(HHINCOME == 99999999, NA_real_, as.numeric(HHINCOME)),
    # NUMERATOR: Employed (10) or Has Job but not at work (12)
    target_indicator = if_else(EMPSTAT %in% c(10, 12), 1, 0)
  )

# 5. EXECUTION & VISUALIZATION
# Normalizes income using RPP and generates the clinical skyline visual
final_data <- prepared_data %>%
  # For 2023 data, adj_val is anchored at 1.0 to meet the Master Base Year
  apply_three_countries_logic("HHINCOME", "STATEFIP", adj_val = 1.0)

# Generate the 60-Ventile Economic Skyline with the #9B2226, #E9C46A, #386641 palette
# Example for Employment Rate
plot_economic_skyline(
  data = final_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_format = "percent",
  y_axis_label = "Employment Rate (%)", # This reflects what's being visualized
  plot_title = "CPS_ASEC_Employment_Rate"
)