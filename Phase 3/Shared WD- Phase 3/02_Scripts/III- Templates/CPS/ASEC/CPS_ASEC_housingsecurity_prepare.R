## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: ASEC_housing_prepare.R
## Purpose: Housing pillars for ALL individuals with valid housing/income data.
## Author: Janica Murphy, Max Goshert, EPAG / Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here)

# ==== 1. LOAD DATA ====
raw_data <- readRDS(here::here("01_data", "raw", "IPUMS_Microdata", "cps_housing_2023", "raw_housing_data.rds"))

# ==== 2. HIERARCHICAL HOUSING LOGIC ====
message("Defining Universe: All individuals with valid Housing + Income data...")

prepared_data <- raw_data %>%
  # UNIVERSE: Include everyone with a weight and valid tenure.
  # OWNERSHP != 0 ensures we only look at those with recorded housing status.
  filter(ASECWT > 0, OWNERSHP != 0) %>% 
  mutate(
    # Clean Income: 9999999 is the N/A code in ASEC.
    HHINCOME = if_else(HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME)),
    
    # Pillar 1: Homeowners (Code 10)
    ind_homeowner = if_else(as.numeric(OWNERSHP) == 10, 1, 0, missing = 0),
    
    # Pillar 2: Assisted Renters (Subsidy or Public Housing)
    ind_housing_assist = if_else(ind_homeowner == 0 & 
                                   (as.numeric(RENTSUB) == 2 | as.numeric(PUBHOUS) == 2), 
                                 1, 0, missing = 0),
    
    # Pillar 3: Private Renters (Residual: Rented 21/22 and not assisted)
    ind_private_renter = if_else(as.numeric(OWNERSHP) %in% c(21, 22) & ind_housing_assist == 0, 
                                 1, 0, missing = 0)
  ) %>%
  # Filter out rows that failed income cleaning to ensure the plot has no gaps.
  filter(!is.na(HHINCOME)) %>%
  select(SERIAL, PERNUM, ASECWT, HHINCOME, AGE, starts_with("ind_"))

# ==== 3. VALIDATION ====
message("Logic Check: Pillars sum for the full population universe.")
print(colMeans(prepared_data %>% select(starts_with("ind_"))))

saveRDS(prepared_data, here::here("01_data", "processed", "IPUMS_Microdata", "prepared_housing_2023.rds"))