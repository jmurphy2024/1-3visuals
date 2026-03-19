# ==============================================================================
# SCRIPT: GSS_emergency_readiness_2.R
# Purpose: Generate 3-Country Skyline for Emergency Readiness & Response Ability
# Definition: The structural ability of individuals and communities to physically 
#             endure, socially cooperate, and rely on institutions during a crisis.
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

# 4. THE UNIFIED PIPELINE: EMERGENCY READINESS LOGIC
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
    
    # --- PILLAR & INDEX LOGIC (READINESS & RESILIENCE) ---
    # 1. Physical Readiness: Excellent (1) or Good (2) health to endure/evacuate
    `Physical Readiness`      = if_else(!is.na(HEALTH) & HEALTH <= 2, 1, 0, missing = 0), 
    
    # 2. Social Readiness / Mutual Aid: Believes people try to be helpful (1)
    `Social Readiness`        = if_else(!is.na(HELPFUL) & HELPFUL == 1, 1, 0, missing = 0),
    
    # 3. Institutional Readiness: A great deal (1) or only some (2) confidence in medicine/public health
    `Institutional Readiness` = if_else(!is.na(CONMEDIC) & CONMEDIC <= 2, 1, 0, missing = 0),
    
    # Composite Readiness Index: High capacity across all three dimensions
    Emergency_Readiness_Index = if_else(`Physical Readiness` == 1 & 
                                          `Social Readiness` == 1 & 
                                          `Institutional Readiness` == 1, 1, 0),
    
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

# 5. SUMMARY STATISTICS: READINESS INDEX & PILLARS
# ------------------------------------------------------------------------------
message("\n=== PILLAR 1: PHYSICAL READINESS SUMMARY ===")
summary_physical <- get_country_summary(prepared_data, "Physical Readiness", "PERWT")
print(as.data.frame(summary_physical))

message("\n=== PILLAR 2: SOCIAL READINESS SUMMARY ===")
summary_social <- get_country_summary(prepared_data, "Social Readiness", "PERWT")
print(as.data.frame(summary_social))

message("\n=== PILLAR 3: INSTITUTIONAL READINESS SUMMARY ===")
summary_institutional <- get_country_summary(prepared_data, "Institutional Readiness", "PERWT")
print(as.data.frame(summary_institutional))

message("\n=== OVERALL COMPOSITE: EMERGENCY READINESS INDEX SUMMARY ===")
summary_stats <- get_country_summary(prepared_data, "Emergency_Readiness_Index", "PERWT")
print(as.data.frame(summary_stats))

# 6. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data 

pillar_note <- paste0(
  "Note: This graph tracks three core dimensions of emergency readiness.\n",
  "1. Physical Readiness: Populations with the baseline bodily health required to endure or evacuate.\n",
  "2. Social Readiness: Populations embedded in communities where neighbors practice mutual aid.\n",
  "3. Institutional Readiness: Populations maintaining confidence in the medical/public health response systems."
)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Physical Readiness", "Social Readiness", "Institutional Readiness"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Exhibiting Readiness (%)",
  plot_title     = "GSS_emergency_readiness_pillars_2",
  caption_text   = stringr::str_wrap(pillar_note, width = 130)
)
print(p_pillars)

# 7. VISUALIZATION: THE COMPOSITE READINESS INDEX
# ------------------------------------------------------------------------------
readiness_explanation <- paste0(
  "Note: The Emergency Readiness Index represents the percentage of individuals who are structurally prepared for a crisis.\n",
  "It isolates those who simultaneously possess physical endurance, a reliable community support network, and faith in public health institutions."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "Emergency_Readiness_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Emergency Readiness Index (%)",
  plot_title     = "GSS_emergency_readiness_composite_2",
  caption_text   = stringr::str_wrap(readiness_explanation, width = 130) 
)
print(p_index)