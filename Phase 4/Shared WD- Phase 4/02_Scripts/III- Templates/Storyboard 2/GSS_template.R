# ==============================================================================
# SCRIPT: NORC_GSS_LifeSatisfaction_SAV.R
# Purpose: Generate 3-Country Skyline for Life Satisfaction from SPSS Data
# Logic: GSS INCOME16 (Jittered) + Composite Satisfaction Indicators
# ==============================================================================
rm(list = ls()); gc()
library(haven); library(dplyr); library(here); library(tidyr)

# 1. SOURCE MASTER LOGIC
# Ensure this points to your shared normalization and plotting functions
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

set.seed(123) # Critical for reproducible jittering

# 2. DATA ACQUISITION
# Point this to wherever you saved the downloaded SPSS file
TARGET_FILE <- here::here("01_data", "raw", "GSS2024.sav")

if (!file.exists(TARGET_FILE)) {
  stop("GSS2024.sav not found. Please place the downloaded SPSS file in the specified directory.")
}

message("--- Loading GSS 2024 SPSS Data ---")
raw_data <- read_sav(TARGET_FILE)

# 3. COMPLEX RECODING & JITTERING
prepared_data <- raw_data %>%
  # Convert all column names to uppercase to match your logic perfectly
  rename_with(toupper, everything()) %>% 
  mutate(
    # Standardize Weight to your 2024 GSS variable
    PERWT = as.numeric(WTSSNRPS),
    
    # ---------------------------------------------------------
    # SUB-INDICATORS (From your provided logic)
    # ---------------------------------------------------------
    # Economic Infrastructure: Satisfaction with job and financial standing
    ind_econ_infra = if_else(as.numeric(SATJOB) <= 2 | as.numeric(FINRELA) >= 4, 1, 0, missing = 0),
    
    # Social Infrastructure: Regular social interaction and general happiness
    ind_soc_infra  = if_else(as.numeric(SOCFREND) <= 3 | as.numeric(HAPPY) <= 1, 1, 0, missing = 0),
    
    # Physical/Env Infrastructure: Self-reported health assessment
    ind_phys_infra = if_else(as.numeric(HEALTH) <= 2, 1, 0, missing = 0),
    
    # OVERALL INDICATOR: Cognitive assessment across all infrastructures
    target_indicator = if_else(ind_econ_infra == 1 & ind_soc_infra == 1 & ind_phys_infra == 1, 1, 0),
    
    # ---------------------------------------------------------
    # INCOME NORMALIZATION (Jittering INCOME16)
    # ---------------------------------------------------------
    income_num = as.numeric(INCOME16)
  ) %>%
  filter(!is.na(income_num) & income_num > 0) %>%
  mutate(
    # Jittering logic to convert categorical brackets to continuous dollars
    REAL_INCOME = case_when(
      income_num <= 10 ~ runif(n(), 0, 19999),      
      income_num <= 17 ~ runif(n(), 20000, 49999),   
      income_num <= 21 ~ runif(n(), 50000, 89999),   
      income_num <= 25 ~ runif(n(), 90000, 169999),  
      income_num == 26 ~ runif(n(), 170000, 500000), 
      TRUE             ~ runif(n(), 50000, 74999) 
    ),
    
    # Assign to the Three Countries using the jittered continuous income
    Country = case_when(
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # Final cleanup for the visualization function
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country))

# 4. VISUALIZATION
plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Overall Life Satisfaction Index (%)",
  plot_title = "GSS_2024_Life_Satisfaction"
)