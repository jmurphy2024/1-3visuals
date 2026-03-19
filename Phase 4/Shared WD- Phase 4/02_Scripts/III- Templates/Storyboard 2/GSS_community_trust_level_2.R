# ==============================================================================
# SCRIPT: GSS_community_trust_2.R
# Purpose: Generate 3-Country Skyline for Overall Community Trust Level
# Definition: The extent to which individuals view their surrounding community 
#             and peers as generally trustworthy, fair, and helpful.
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

# 4. THE UNIFIED PIPELINE: COMMUNITY TRUST LOGIC
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
    
    # --- PILLAR & INDEX LOGIC (COMMUNITY TRUST) ---
    # 1. Generalized Trust: Believes most people can be trusted (1)
    `Generalized Trust`     = if_else(!is.na(TRUST) & TRUST == 1, 1, 0, missing = 0), 
    
    # 2. Perceived Fairness: Believes most people would try to be fair (2)
    `Perceived Fairness`    = if_else(!is.na(FAIR) & FAIR == 2, 1, 0, missing = 0),
    
    # 3. Perceived Helpfulness: Believes most people try to be helpful (1)
    `Perceived Helpfulness` = if_else(!is.na(HELPFUL) & HELPFUL == 1, 1, 0, missing = 0),
    
    # Composite Trust Index: High social trust across all three dimensions
    Community_Trust_Index   = if_else(`Generalized Trust` == 1 & 
                                        `Perceived Fairness` == 1 & 
                                        `Perceived Helpfulness` == 1, 1, 0),
    
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

# 5. SUMMARY STATISTICS: COMMUNITY TRUST INDEX
# ------------------------------------------------------------------------------
summary_stats <- get_country_summary(prepared_data, "Community_Trust_Index", "PERWT")
message("\n=== COMMUNITY TRUST INDEX SUMMARY ===")
print(as.data.frame(summary_stats))

# 6. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data 

pillar_note <- paste0(
  "The Architecture of Social Capital: This graph tracks three core dimensions of community trust.\n",
  "1. Generalized Trust: Populations believing that 'most people can be trusted' (TRUST).\n",
  "2. Perceived Fairness: Populations believing that others 'would try to be fair' rather than take advantage (FAIR).\n",
  "3. Perceived Helpfulness: Populations believing that others 'try to be helpful' rather than look out for themselves (HELPFUL)."
)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Generalized Trust", "Perceived Fairness", "Perceived Helpfulness"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Exhibiting Trust (%)",
  plot_title     = "GSS_community_trust_pillars_2",
  caption_text   = stringr::str_wrap(pillar_note, width = 130)
)
print(p_pillars)

# 7. VISUALIZATION: THE COMPOSITE TRUST INDEX
# ------------------------------------------------------------------------------
trust_explanation <- paste0(
  "Note: The Community Trust Index represents the percentage of individuals who exhibit high social capital and trust across all three dimensions.\n",
  "It isolates those who view their surrounding community as fundamentally cooperative, fair, and reliable, revealing how economic standing impacts social cohesion."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "Community_Trust_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Community Trust Index (%)",
  plot_title     = "GSS_community_trust_composite_2",
  caption_text   = stringr::str_wrap(trust_explanation, width = 130) 
)
print(p_index)