## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: gss_LifeSatisfaction_prepare.R
## Purpose: Creates Life Satisfaction indicators (Economic, Social, Physical Infrastructure).
## Author: Janica Murphy, Max Goshert, EPAG / Gemini
## Created: January 21, 2026

rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

set.seed(123) 

prepared_data <- readRDS(here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_LifeSatisfaction_raw.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

prepared_data <- prepared_data %>%
  mutate(
    # Economic Infrastructure: Satisfaction with job and financial standing
    ind_econ_infra = if_else(SATJOB <= 2 | FINRELA >= 4, 1, 0, missing = 0),
    
    # Social Infrastructure: Regular social interaction and general happiness
    ind_soc_infra  = if_else(SOCFREND <= 3 | HAPPY <= 1, 1, 0, missing = 0),
    
    # Physical/Env Infrastructure: Self-reported health assessment
    ind_phys_infra = if_else(HEALTH <= 2, 1, 0, missing = 0),
    
    # Overall Life Satisfaction: Cognitive assessment across all infrastructures
    ind_life_sat_index = if_else(ind_econ_infra == 1 & ind_soc_infra == 1 & ind_phys_infra == 1, 1, 0)
  )

# Income Jittering (Maintained from original logic)
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
         ind_life_sat_index, ind_econ_infra, ind_soc_infra, ind_phys_infra)

saveRDS(prepared_data, here::here("01_Data", "processed", "GSS_Microdata", "prepared_gss_LifeSatisfaction_2024.rds"))