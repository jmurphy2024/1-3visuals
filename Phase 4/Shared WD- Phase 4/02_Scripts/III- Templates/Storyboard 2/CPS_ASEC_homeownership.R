# ==============================================================================
# SCRIPT: CPS_ASEC_Housing_Security.R
# PURPOSE: Analyzes Housing Security (Ownership & Assistance) by Income
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales)

# 1. SOURCE MASTER LOGIC
# This handles directory setup, RPP lookup, and shared functions
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

# 2. CONFIGURATION 
# Define the specific IPUMS sample and housing variables needed
USER_SAMPLE   <- "cps2023_03s"
VARS_NEEDED   <- c("ASECWT", "HHINCOME", "STATEFIP", "AGE", "OWNERSHP", "RENTSUB", "PUBHOUS")
TARGET_DIR    <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("cps_", USER_SAMPLE))
TARGET_FILE   <- file.path(TARGET_DIR, "raw_data_housing.rds")

# 3. ACQUISITION (API Recovery)
# Automatically triggers IPUMS API if local data is missing
if (!file.exists(TARGET_FILE)) {
  message("--- File not found locally. Triggering IPUMS API Recovery ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "cps", 
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED,
    description = "Three Countries CPS-ASEC Extract - Housing Security"
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
# Recode logic for Housing Variables
prepared_data <- raw_data %>%
  # Robust renaming: Handle both upper and lower case names from IPUMS
  rename_with(toupper, everything()) %>% 
  rename(PERWT = ASECWT) %>% # Ensure we use the Supplement Weight for ASEC
  
  # UNIVERSE: Civilian Non-Institutionalized Adults 16+ 
  # (Note: Use PERNUM == 1 if you want Household-level rather than Adult-level rates)
  filter(AGE >= 16) %>% 
  mutate(
    # Handle CPS missing income code 99999999
    HHINCOME = if_else(HHINCOME == 99999999, NA_real_, as.numeric(HHINCOME)),
    
    # DUMMY 1: Homeownership (ind_homeowner)
    # IPUMS Code 10 = Owned or being bought (20 = Rented, 00 = NIU)
    ind_homeowner = if_else(OWNERSHP == 10, 1, 0),
    
    # DUMMY 2: Housing Assistance (ind_housing_assist)
    # Combines Public Housing (PUBHOUS) and Rent Subsidy (RENTSUB)
    # IPUMS Code 1 = Yes (2 = No, 0 = NIU)
    ind_housing_assist = if_else(PUBHOUS == 1 | RENTSUB == 1, 1, 0)
  ) %>%
  # Filter out rows where housing data might be Not In Universe (NIU = 0)
  filter(OWNERSHP != 0)

# 5. EXECUTION & VISUALIZATION
# Normalizes income using RPP and generates the clinical skyline visuals
final_data <- prepared_data %>%
  # For 2023 data, adj_val is anchored at 1.0 to meet the Master Base Year
  apply_three_countries_logic("HHINCOME", "STATEFIP", adj_val = 1.0)


# Generate Skyline Plot 1: Homeownership
plot_economic_skyline(
  data = final_data, 
  indicator_var = "ind_homeowner", 
  weight_var = "PERWT", 
  y_format = "percent",
  y_axis_label = "Homeownership Rate (%)",
  plot_title = "CPS_ASEC_Homeownership_Rate"
)

# Generate Skyline Plot 2: Housing Assistance
plot_economic_skyline(
  data = final_data, 
  indicator_var = "ind_housing_assist", 
  weight_var = "PERWT", 
  y_format = "percent",
  y_axis_label = "Housing Assistance Rate (%)", 
  plot_title = "CPS_ASEC_Housing_Assistance_Rate"
)