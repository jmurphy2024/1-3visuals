# ==============================================================================
# SCRIPT: GSS_life_satisfaction_2.R
# Purpose: Generate 3-Country Skyline for Life Satisfaction from SPSS Data
# Logic:   GSS INCOME16 (Jittered) + Spatial RPP + Dynamic V2 Borders
# ==============================================================================
rm(list = ls()); gc()
library(haven); library(dplyr); library(here); library(tidyr)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

# Load dynamic V2 cutoffs
cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

set.seed(123) 

# 2. DATA ACQUISITION
# ------------------------------------------------------------------------------
TARGET_FILE <- here::here("01_data", "raw", "GSS2024.sav")
if(!file.exists(TARGET_FILE)) stop("GSS .sav file not found. Please verify the path.")
raw_data <- read_sav(TARGET_FILE)

# 3. SPATIAL & TEMPORAL CONFIGURATION
# ------------------------------------------------------------------------------
# 2023 Regional Price Parities (RPP)
region_rpp_lookup <- tibble(
  REGION_ID = c(1, 2, 3, 4),
  REG_RPP   = c(105.2, 92.8, 95.4, 104.1) # NE, MW, S, W
)

# Inflation Multiplier (2024 to 2023 dollars)
INFLATION_ADJ <- 0.97 

# 4. COMPLEX RECODING & JITTERING
# ------------------------------------------------------------------------------
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  mutate(
    # Use specific GSS weight for this metric
    PERWT = as.numeric(WTSSNRPS),
    
    # --- RPP CROSSWALK: Collapse GSS 9 Regions to 4 Regions ---
    # 1-2 = Northeast, 3-4 = Midwest, 5-7 = South, 8-9 = West
    MAPPED_REGION = case_when(
      as.numeric(REGION) %in% c(1, 2) ~ 1,
      as.numeric(REGION) %in% c(3, 4) ~ 2,
      as.numeric(REGION) %in% c(5, 6, 7) ~ 3,
      as.numeric(REGION) %in% c(8, 9) ~ 4,
      TRUE ~ NA_real_
    ),
    
    # --- SUB-INDICATORS (Life Satisfaction Logic) ---
    ind_econ_infra = if_else(as.numeric(SATJOB) <= 2 | as.numeric(FINRELA) >= 4, 1, 0, missing = 0),
    ind_soc_infra  = if_else(as.numeric(SOCFREND) <= 3 | as.numeric(HAPPY) <= 1, 1, 0, missing = 0),
    ind_phys_infra = if_else(as.numeric(HEALTH) <= 2, 1, 0, missing = 0),
    target_indicator = if_else(ind_econ_infra == 1 & ind_soc_infra == 1 & ind_phys_infra == 1, 1, 0),
    
    # --- INCOME JITTERING ---
    income_num = as.numeric(INCOME16)
  ) %>%
  filter(!is.na(income_num) & income_num > 0) %>%
  mutate(
    # Convert categorical brackets into continuous numeric dollars
    raw_dollars = case_when(
      income_num <= 10 ~ runif(n(), 0, 19999),      
      income_num <= 17 ~ runif(n(), 20000, 49999),   
      income_num <= 21 ~ runif(n(), 50000, 89999),   
      income_num <= 25 ~ runif(n(), 90000, 169999),  
      income_num == 26 ~ runif(n(), 170000, 500000), 
      TRUE             ~ runif(n(), 50000, 74999) 
    )
  ) %>%
  # --- SPATIAL & TEMPORAL ADJUSTMENT ---
  left_join(region_rpp_lookup, by = c("MAPPED_REGION" = "REGION_ID")) %>%
  mutate(
    # Apply Inflation adjustment FIRST, then Spatial RPP adjustment
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)),
    
    # Assign to the Three Countries using Dynamic V2 Borders
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country))

# 5. VISUALIZATION
# ------------------------------------------------------------------------------
p <- plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Overall Life Satisfaction Index (%)",
  plot_title = "GSS_life_satisfaction_2" # Updated to V2 naming standard
)

print(p)