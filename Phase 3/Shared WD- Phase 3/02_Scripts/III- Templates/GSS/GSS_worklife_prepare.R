## WD location: 1-3 Visualization Analysis/Phase 3/Shared WD
## Script: GSS_worklife_prepare.R
## Purpose: Execute pillar logic and income jittering for Work-Life Balance prevalence.
## Author: Janica Murphy, Gemini / User
## Date Created: 2026-01-27
## Dependencies: dplyr, here, tidyr

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)

# Load custom functions for income group assignment
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# CRITICAL: Ensures HHINCOME jittering is reproducible
set.seed(123) 

# ==== 1. LOAD DATA & BORDERS ====
# Loads the raw subset created by the Acquire script
prepared_data <- readRDS(here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_wlb_raw.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

# ==== 2. PILLAR & INDEX CREATION ====
# Logic follows the 'at least one' threshold for pillars and 'all three' for the Index.
prepared_data <- prepared_data %>%
  mutate(
    # Time Sovereignty (Opportunity Pillar): High if work hours <= 40 OR regular daytime schedule.
    # Note: Handles missing WRKSCHED by checking !is.na to protect denominator.
    ind_sovereignty = if_else(HRS1 <= 40 | (!is.na(WRKSCHED) & WRKSCHED == 1), 1, 0, missing = 0),
    
    # Support Resources (Resources Pillar): High if family income >= $75k OR married.
    ind_support     = if_else(REALINC >= 75000 | MARITAL == 1, 1, 0, missing = 0),
    
    # Life Fulfillment (Outcomes Pillar): High if satisfied with city OR 'Very Happy'.
    ind_fulfillment = if_else(SATCITY <= 2 | HAPPY == 1, 1, 0, missing = 0),
    
    # Composite WLB Index: High Capital in all three pillars (restrictive 'AND' logic).
    ind_wlb_index = if_else(ind_sovereignty == 1 & ind_support == 1 & ind_fulfillment == 1, 1, 0)
  )

# ==== 3. INCOME JITTERING & GROUPING ====
# Standardized jittering logic to transform categorical GSS income into continuous terciles.
prepared_data <- prepared_data %>%
  mutate(income_num = as.numeric(INCOME16)) %>%
  filter(!is.na(income_num) & income_num > 0) %>%
  mutate(HHINCOME = case_when(
    income_num <= 10 ~ runif(n(), 0, 19999),      
    income_num <= 17 ~ runif(n(), 20000, 49999),   
    income_num <= 21 ~ runif(n(), 50000, 89999),   
    income_num <= 25 ~ runif(n(), 90000, 169999),  
    income_num == 26 ~ runif(n(), 170000, 500000), 
    TRUE             ~ runif(n(), 50000, 74999) 
  )) %>%
  assign_income_groups(
    borders_df = borders_df, 
    income_var_name = "HHINCOME", 
    detail_level = "Groups_20",
    main_cutoff1 = main_cutoffs$main_cutoff1, 
    main_cutoff2 = main_cutoffs$main_cutoff2
  ) %>%
  # Select only standardized indicators and weights for the Viz script
  select(ID, YEAR, WTSSNRPS, HHINCOME, income_tercile, fine_income_group, 
         ind_wlb_index, ind_sovereignty, ind_support, ind_fulfillment)

# ==== 4. SAVE ====
saveRDS(prepared_data, here::here("01_Data", "processed", "GSS_Microdata", "prepared_gss_wlb_2024.rds"))
message("SUCCESS: WLB Prepare script complete with n=", nrow(prepared_data), " observations.")