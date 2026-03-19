# ==============================================================================
# SCRIPT: GSS_life_satisfaction_2.R
# Purpose: Generate 3-Country Multi-Variable Skyline for Overall Life Satisfaction
# Logic:   Personal Happiness, Financial Satisfaction, & Life Engagement Pillars
# ==============================================================================
rm(list = ls()); gc()
library(haven); library(dplyr); library(here); library(tidyr); library(ggplot2); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)
set.seed(123)

# 2. DATA ACQUISITION
# ------------------------------------------------------------------------------
TARGET_FILE <- here::here("01_data", "raw", "GSS2024.sav")
if(!file.exists(TARGET_FILE)) stop("GSS .sav file not found.")
raw_data <- read_sav(TARGET_FILE)

# 3. SPATIAL & TEMPORAL CONFIGURATION
# ------------------------------------------------------------------------------
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2024, base_year = 2023)

# 4. THE UNIFIED PIPELINE: LIFE SATISFACTION LOGIC
# ------------------------------------------------------------------------------
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  mutate(
    PERWT = as.numeric(WTSSNRPS),
    
    # --- RPP CROSSWALK ---
    MAPPED_REGION = case_when(
      as.numeric(REGION) %in% c(1, 2) ~ 1,
      as.numeric(REGION) %in% c(3, 4) ~ 2,
      as.numeric(REGION) %in% c(5, 6, 7) ~ 3,
      as.numeric(REGION) %in% c(8, 9) ~ 4,
      TRUE ~ NA_real_
    ),
    
    # --- PILLAR & INDEX LOGIC (OVERALL LIFE SATISFACTION) ---
    # 1. Personal Happiness: Very happy (1) or Pretty happy (2)
    `Personal Happiness`     = if_else(!is.na(HAPPY) & HAPPY <= 2, 1, 0, missing = 0), 
    
    # 2. Financial Satisfaction: Satisfied (1) or More/less satisfied (2)
    `Financial Satisfaction` = if_else(!is.na(SATFIN) & SATFIN <= 2, 1, 0, missing = 0),
    
    # 3. Life Engagement: Life is Exciting (1) or Routine (2) [Excludes Dull (3)]
    `Life Engagement`        = if_else(!is.na(LIFE) & LIFE <= 2, 1, 0, missing = 0),
    
    # Composite Satisfaction Index
    Life_Satisfaction_Index  = if_else(`Personal Happiness` == 1 & 
                                         `Financial Satisfaction` == 1 & 
                                         `Life Engagement` == 1, 1, 0),
    
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
    ),
    
    # --- ONE-STEP REAL INCOME NORMALIZATION ---
    REAL_INCOME = raw_dollars * INFLATION_ADJ * get_regional_rpp_multiplier(MAPPED_REGION),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country))

# 5. SUMMARY STATISTICS: LIFE SATISFACTION INDEX
# ------------------------------------------------------------------------------
summary_stats <- get_country_summary(prepared_data, "Life_Satisfaction_Index", "PERWT")
message("\n=== OVERALL LIFE SATISFACTION INDEX SUMMARY ===")
print(as.data.frame(summary_stats))

# 6. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data 

pillar_note <- paste0(
  "The Dimensions of Life Satisfaction: This graph tracks the three pillars of overall well-being.\n",
  "1. Personal Happiness: Individuals reporting they are 'very' or 'pretty' happy overall (HAPPY).\n",
  "2. Financial Satisfaction: Individuals who are at least 'more or less' satisfied with their finances (SATFIN).\n",
  "3. Life Engagement: Individuals who find their daily life 'exciting' or 'routine' rather than 'dull' (LIFE)."
)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Personal Happiness", "Financial Satisfaction", "Life Engagement"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Exhibiting Satisfaction (%)",
  plot_title     = "GSS_life_satisfaction_pillars_2",
  caption_text   = stringr::str_wrap(pillar_note, width = 130)
)
print(p_pillars)

# 7. VISUALIZATION: THE COMPOSITE SATISFACTION INDEX
# ------------------------------------------------------------------------------
satisfaction_explanation <- paste0(
  "Note: The Overall Life Satisfaction Index measures the percentage of individuals who are simultaneously fulfilled across all three pillars.\n",
  "It tracks the intersection of emotional well-being, financial contentment, and engagement with daily life."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "Life_Satisfaction_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Life Satisfaction Index (%)",
  plot_title     = "GSS_life_satisfaction_composite_2",
  caption_text   = stringr::str_wrap(satisfaction_explanation, width = 130) 
)
print(p_index)