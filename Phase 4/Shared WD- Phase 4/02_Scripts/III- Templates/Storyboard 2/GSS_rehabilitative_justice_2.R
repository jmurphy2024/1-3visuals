# ==============================================================================
# SCRIPT: GSS_rehabilitative_justice_2.R
# Purpose: Generate 3-Country Skyline for Rehabilitative & Restorative Justice
# Definition: Maps the structural belief in criminal rehabilitation, treatment, 
#             and systemic proportionality over punitive/lethal measures.
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

# 4. THE UNIFIED PIPELINE: REHABILITATIVE JUSTICE LOGIC
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
    
    # --- PILLAR LOGIC ---
    # 1. Systemic Proportionality: Rejects that courts are "not harsh enough" (1 = Too harshly, 3 = About right)
    `Systemic Proportionality` = if_else(!is.na(COURTS) & COURTS != 2, 1, 0, missing = 0), 
    
    # 2. Restorative Sentencing: Opposes the death penalty for convicted murderers (2 = Oppose)
    `Restorative Sentencing`  = if_else(!is.na(CAPPUN) & CAPPUN == 2, 1, 0, missing = 0),
    
    # 3. Treatment Funding Support: Believes we spend too little (1) or the right amount (2) on drug addiction
    `Treatment Support`  = if_else(!is.na(NATDRUG) & NATDRUG <= 2, 1, 0, missing = 0),
    
    # Composite Rehabilitative Index: Possesses pro-rehabilitation stance across all three dimensions
    Rehabilitative_Justice_Index = if_else(`Systemic Proportionality` == 1 & 
                                             `Restorative Sentencing` == 1 & 
                                             `Treatment Support` == 1, 1, 0),
    
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

# 5. SUMMARY STATISTICS: REHABILITATION INDEX & PILLARS
# ------------------------------------------------------------------------------
message("\n=== PILLAR 1: SYSTEMIC PROPORTIONALITY SUMMARY ===")
summary_proportionality <- get_country_summary(prepared_data, "Systemic Proportionality", "PERWT")
print(as.data.frame(summary_proportionality))

message("\n=== PILLAR 2: RESTORATIVE SENTENCING SUMMARY ===")
summary_sentencing <- get_country_summary(prepared_data, "Restorative Sentencing", "PERWT")
print(as.data.frame(summary_sentencing))

message("\n=== PILLAR 3: TREATMENT FUNDING SUPPORT SUMMARY ===")
summary_treatment <- get_country_summary(prepared_data, "Treatment Support", "PERWT")
print(as.data.frame(summary_treatment))

message("\n=== OVERALL COMPOSITE: REHABILITATIVE JUSTICE INDEX SUMMARY ===")
summary_stats <- get_country_summary(prepared_data, "Rehabilitative_Justice_Index", "PERWT")
print(as.data.frame(summary_stats))

# 6. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data 

pillar_note <- paste0(
  "The Architecture of Justice: This graph maps public support for restorative and rehabilitative policies.\n",
  "1. Systemic Proportionality: Populations actively rejecting the notion that local courts should be harsher on criminals (COURTS).\n",
  "2. Restorative Sentencing: Populations who oppose the death penalty, leaving systemic room for rehabilitation (CAPPUN).\n",
  "3. Treatment Support: Populations favoring funding for the medical treatment of drug addiction (NATDRUG)."
)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Systemic Proportionality", "Restorative Sentencing", "Treatment Support"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Exhibiting Support (%)",
  plot_title     = "GSS_rehabilitative_justice_pillars_2",
  caption_text   = stringr::str_wrap(pillar_note, width = 130)
)
print(p_pillars)

# 7. VISUALIZATION: THE COMPOSITE REHABILITATIVE INDEX
# ------------------------------------------------------------------------------
rehab_explanation <- paste0(
  "Note: The Rehabilitative Justice Index isolates populations whose criminal justice philosophy prioritizes reform over retribution.\n",
  "It maps the percentage of the population that simultaneously rejects extreme judicial harshness, opposes lethal sentencing, and supports funding for addiction treatment."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "Rehabilitative_Justice_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Rehabilitative Justice Index (%)",
  plot_title     = "GSS_rehabilitative_justice_composite_2",
  caption_text   = stringr::str_wrap(rehab_explanation, width = 130) 
)
print(p_index)