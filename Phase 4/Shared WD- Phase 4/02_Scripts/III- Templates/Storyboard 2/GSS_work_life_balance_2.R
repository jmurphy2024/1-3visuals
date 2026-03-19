# ==============================================================================
# SCRIPT: GSS_Work_Life_Balance_2.R
# Purpose: Generate 3-Country Skyline for Work-Life Balance (WLB)
# ==============================================================================
rm(list = ls()); gc()
library(haven); library(dplyr); library(here); library(tidyr); library(ggplot2); library(stringr)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found.")
cutoffs <- readRDS(cutoffs_path)
set.seed(123)

raw_data <- read_sav(here::here("01_data", "raw", "GSS2024.sav"))
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2024, base_year = 2023)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  mutate(
    PERWT = as.numeric(WTSSNRPS),
    MAPPED_REGION = case_when(
      as.numeric(REGION) %in% c(1, 2) ~ 1,
      as.numeric(REGION) %in% c(3, 4) ~ 2,
      as.numeric(REGION) %in% c(5, 6, 7) ~ 3,
      as.numeric(REGION) %in% c(8, 9) ~ 4,
      TRUE ~ NA_real_
    ),
    ind_sovereignty = if_else(HRS1 <= 40 | (!is.na(WRKSTAT) & WRKSTAT == 2), 1, 0, missing = 0),
    ind_support     = if_else(REALINC >= 75000 | MARITAL == 1, 1, 0, missing = 0),
    ind_fulfillment = if_else(HAPPY == 1 | (!is.na(SATJOB) & SATJOB == 1), 1, 0, missing = 0),
    ind_wlb_index   = if_else(ind_sovereignty == 1 & ind_support == 1 & ind_fulfillment == 1, 1, 0),
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
    ),
    REAL_INCOME = raw_dollars * INFLATION_ADJ * get_regional_rpp_multiplier(MAPPED_REGION),
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country))

message("\n=== WLB INDEX SUMMARY ===")
print(as.data.frame(get_country_summary(prepared_data, "ind_wlb_index", "PERWT")))

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "ind_wlb_index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Work-Life Balance Index",
  plot_title     = "GSS_WLB_index_composite_2",
  caption_text   = "How to read the index: Full Work-Life Balance is a metric requiring capital in all three pillars." 
)
print(p_index)