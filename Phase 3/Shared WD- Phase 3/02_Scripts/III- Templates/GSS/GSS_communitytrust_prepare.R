## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: GSS_communitytrust_prepare.R
## Purpose: Creates Pillars for Social Integration (Third Places) and Social Cohesion (Trust).
## Author: Janica Murphy / Gemini
## Created: January 21, 2026

## Purpose: Creates individual pillars for Trust, Fair, and Helpful to normalize gaps.

rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

set.seed(123) 

# LOAD DATA
prepared_data <- readRDS(here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_ThirdPlaces_raw.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

# PILLAR CREATION (DECONSTRUCTED COHESION)
prepared_data <- prepared_data %>%
  mutate(
    # Pillar 1: High-Intensity Social Integration (Weekly Threshold)
    ind_social_integration = if_else(
      SOCBAR <= 3 | ATTEND >= 6 | SOCFREND <= 3 | SOCREL <= 3 | SOCOMMUN <= 3, 1, 0, missing = 0
    ),
    
    # Pillar 2: Deconstructed Cohesion Metrics (Individual Numerators)
    ind_trust    = if_else(TRUST == 1, 1, 0, missing = 0),
    ind_fair     = if_else(FAIR == 1, 1, 0, missing = 0),
    ind_helpful  = if_else(HELPFUL == 1, 1, 0, missing = 0),
    
    # Composite: Community Trust Level 
    # (Requires weekly integration AND positive views on all three cohesion markers)
    ind_comm_trust_index = if_else(ind_social_integration == 1 & ind_trust == 1 & ind_fair == 1 & ind_helpful == 1, 1, 0)
  )

# INCOME JITTERING & GROUPING
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
  select(ID, WTSSNRPS, HHINCOME, income_tercile, fine_income_group, 
         ind_comm_trust_index, ind_social_integration, ind_trust, ind_fair, ind_helpful)

saveRDS(prepared_data, here::here("01_Data", "processed", "GSS_Microdata", "prepared_gss_CommunityTrust_2024.rds"))