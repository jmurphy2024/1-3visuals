# ==============================================================================
# SCRIPT: GSS_LEO_Attitudes_2.R
# Purpose: Generate 3-Country Skyline for Law Enforcement Beliefs & Attitudes
# Definition: Community beliefs on how LEOs should be used, their desired 
#             prevalence, and overall attitude toward the justice system.
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

# 4. THE UNIFIED PIPELINE: LEO ATTITUDE LOGIC
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
    
    # --- PILLAR & INDEX LOGIC (LEO BELIEFS & ATTITUDES) ---
    # 1. Prevalence Beliefs: Believes the nation spends "Too little" (1) on halting crime
    `Prevalence Beliefs` = if_else(!is.na(NATCRIME) & NATCRIME == 1, 1, 0, missing = 0), 
    
    # 2. Utilization Beliefs: Approves of police using physical force in certain situations (1 = Yes)
    `Utilization Beliefs`= if_else(!is.na(POLHITOK) & POLHITOK == 1, 1, 0, missing = 0),
    
    # 3. Systemic Attitude: Believes local courts do "Not harshly enough" (2) with criminals
    `Systemic Attitude`  = if_else(!is.na(COURTS) & COURTS == 2, 1, 0, missing = 0),
    
    # Composite Alignment Index: High prevalence demand, force authorization, and strict justice attitude
    LEO_Attitude_Index   = if_else(`Prevalence Beliefs` == 1 & 
                                     `Utilization Beliefs` == 1 & 
                                     `Systemic Attitude` == 1, 1, 0),
    
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

# 5. SUMMARY STATISTICS: LEO ATTITUDE INDEX
# ------------------------------------------------------------------------------
summary_stats <- get_country_summary(prepared_data, "LEO_Attitude_Index", "PERWT")
message("\n=== LEO ATTITUDES & BELIEFS INDEX SUMMARY ===")
print(as.data.frame(summary_stats))

# 6. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data 

pillar_note <- paste0(
  "Community Beliefs on Law Enforcement: This graph tracks three core dimensions of LEO attitudes.\n",
  "1. Prevalence: Populations demanding higher resource allocation to halt crime (NATCRIME).\n",
  "2. Utilization: Populations authorizing the use of physical police force (POLHITOK).\n",
  "3. Systemic Attitude: Populations demanding harsher sentences from local courts (COURTS)."
)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Prevalence Beliefs", "Utilization Beliefs", "Systemic Attitude"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Exhibiting Pro-LEO Beliefs (%)",
  plot_title     = "GSS_LEO_attitudes_pillars_2",
  caption_text   = stringr::str_wrap(pillar_note, width = 130)
)
print(p_pillars)

# 7. VISUALIZATION: THE COMPOSITE ATTITUDE INDEX
# ------------------------------------------------------------------------------
attitude_explanation <- paste0(
  "Note: The LEO Attitude Index represents the percentage of individuals who are fully aligned with a 'tough on crime' law enforcement posture.\n",
  "It isolates those who simultaneously demand a higher police prevalence, authorize police utilization of force, and want stricter local courts."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "LEO_Attitude_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Pro-LEO Alignment Index (%)",
  plot_title     = "GSS_LEO_attitudes_composite_2",
  caption_text   = stringr::str_wrap(attitude_explanation, width = 130) 
)
print(p_index)