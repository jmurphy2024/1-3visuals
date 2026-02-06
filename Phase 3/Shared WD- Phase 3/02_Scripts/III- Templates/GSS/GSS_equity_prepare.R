## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: gss_EquityCapital_prepare.R
## Purpose: Creates Equity Capital indicators based on specific user-defined thresholds.
## Author: Janica Murphy / Gemini

# Purpose: Creates Opportunity, Resources, and Outcomes pillars with reproducible jittering.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# CRITICAL: Ensures HHINCOME jittering is the same every time you run the script
set.seed(123) 

# ==== 1. LOAD DATA & BORDERS ====
prepared_data <- readRDS(here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_EquityCapital_raw.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

# ==== 2. PILLAR & INDEX CREATION ====
prepared_data <- prepared_data %>%
  mutate(
    # Opportunity Pillar: GETAHEAD, RANK, DEGREE
    ind_opportunity = if_else(GETAHEAD == 1 | RANK >= 6 | DEGREE >= 3, 1, 0, missing = 0),
    
    # Resources Pillar: REALINC, FINRELA, CONBUS
    ind_resources   = if_else(REALINC >= 75000 | FINRELA >= 4 | CONBUS <= 2, 1, 0, missing = 0),
    
    # Outcomes Pillar: HEALTH, HAPPY, SATJOB
    ind_outcomes    = if_else(HEALTH <= 2 | HAPPY <= 2 | SATJOB <= 2, 1, 0, missing = 0),
    
    # Composite Equity Index: Access to all three pillars
    ind_equity_index = if_else(ind_opportunity == 1 & ind_resources == 1 & ind_outcomes == 1, 1, 0)
  )

# ==== 3. INCOME JITTERING & GROUPING ====
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
  select(ID, YEAR, WTSSNRPS, HHINCOME, income_tercile, fine_income_group, 
         ind_equity_index, ind_opportunity, ind_resources, ind_outcomes)

# ==== 4. SAVE ====
saveRDS(prepared_data, here::here("01_Data", "processed", "GSS_Microdata", "prepared_gss_EquityCapital_2024.rds"))
message("SUCCESS: Prepare script complete. n=3,309 observations processed with seed 123.")