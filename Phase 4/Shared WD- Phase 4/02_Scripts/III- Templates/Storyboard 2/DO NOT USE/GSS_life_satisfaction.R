# ==============================================================================
# SCRIPT: GSS_template.R
# Purpose: Generate 3-Country Skyline for Life Satisfaction from SPSS Data
# Logic: GSS INCOME16 (Jittered) + Spatial RPP + Temporal Inflation
# ==============================================================================
rm(list = ls()); gc()
library(haven); library(dplyr); library(here); library(tidyr)

# 1. SOURCE MASTER LOGIC
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))
set.seed(123) 

# 2. DATA ACQUISITION
TARGET_FILE <- here::here("01_data", "raw", "GSS2024.sav")
raw_data <- read_sav(TARGET_FILE)

# 3. SPATIAL & TEMPORAL CONFIGURATION
# 2023 Regional Price Parities (RPP)
region_rpp_lookup <- tibble(
  REGION_ID = c(1, 2, 3, 4),
  REG_RPP   = c(105.2, 92.8, 95.4, 104.1) # NE, MW, S, W
)

# Inflation Multiplier (2024 to 2023 dollars)
# If thresholds ($45k/$115k) are 2023 dollars, 2024 income must be deflated slightly.
INFLATION_ADJ <- 0.97  

# 4. COMPLEX RECODING
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  mutate(
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
    
    # --- SUB-INDICATORS ---
    ind_econ_infra = if_else(as.numeric(SATJOB) <= 2 | as.numeric(FINRELA) >= 4, 1, 0, missing = 0),
    ind_soc_infra  = if_else(as.numeric(SOCFREND) <= 3 | as.numeric(HAPPY) <= 1, 1, 0, missing = 0),
    ind_phys_infra = if_else(as.numeric(HEALTH) <= 2, 1, 0, missing = 0),
    target_indicator = if_else(ind_econ_infra == 1 & ind_soc_infra == 1 & ind_phys_infra == 1, 1, 0),
    
    # --- INCOME JITTERING ---
    income_num = as.numeric(INCOME16)
  ) %>%
  filter(!is.na(income_num) & income_num > 0) %>%
  mutate(
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
    
    # Assign to the Three Countries
    Country = case_when(
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country))

# 5. VISUALIZATION
plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Overall Life Satisfaction Index (%)",
  plot_title = "GSS_2024_Life_Satisfaction_Adjusted"
)