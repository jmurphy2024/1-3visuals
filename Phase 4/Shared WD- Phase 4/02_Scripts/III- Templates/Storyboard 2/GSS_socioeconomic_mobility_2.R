# ==============================================================================
# SCRIPT: GSS_socioeconomic_mobility_2.R
# Purpose: Generate 3-Country Multi-Variable Skyline for Socioeconomic Mobility
# Definition: The extent to which individuals/families can improve their 
#             economic and social position over time or across generations.
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

# 4. THE UNIFIED PIPELINE: TRUE MOBILITY LOGIC
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
    
    # --- PILLAR & INDEX LOGIC (TRAJECTORY AND CHANGE) ---
    # 1. Across Generations: Standard of living is better than parents (1=Much better, 2=Somewhat better)
    `Intergenerational Mobility` = if_else(!is.na(PARSOL) & PARSOL <= 2, 1, 0, missing = 0), 
    
    # 2. Over Time: Personal financial situation is actively getting better (1=Getting better)
    `Intragenerational Mobility` = if_else(!is.na(FINALTER) & FINALTER == 1, 1, 0, missing = 0),
    
    # 3. Capacity to Improve: Belief that they/their family have a good chance to improve (1=Strongly Agree, 2=Agree)
    `Systemic Mobility`          = if_else(!is.na(GOODLIFE) & GOODLIFE <= 2, 1, 0, missing = 0),
    
    # Composite Mobility Index
    Mobility_Index               = if_else(`Intergenerational Mobility` == 1 & 
                                             `Intragenerational Mobility` == 1 & 
                                             `Systemic Mobility` == 1, 1, 0),
    
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

# 5. SUMMARY STATISTICS: MOBILITY INDEX
# ------------------------------------------------------------------------------
summary_stats <- get_country_summary(prepared_data, "Mobility_Index", "PERWT")
message("\n=== SOCIOECONOMIC MOBILITY INDEX SUMMARY ===")
print(as.data.frame(summary_stats))

# 6. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data 

pillar_note <- paste0(
  "The Architecture of Mobility: This graph tracks the three pillars of Socioeconomic Mobility.\n",
  "1. Intergenerational: Individuals whose standard of living exceeds their parents' (PARSOL).\n",
  "2. Intragenerational: Individuals whose financial situation has recently improved (FINALTER).\n",
  "3. Systemic: Individuals who believe their family has a good chance of improving their standing (GOODLIFE)."
)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Intergenerational Mobility", "Intragenerational Mobility", "Systemic Mobility"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Experiencing Upward Mobility (%)",
  plot_title     = "GSS_socioeconomic_mobility_pillars_2",
  caption_text   = stringr::str_wrap(pillar_note, width = 130) # Safeguard wrapper
)
print(p_pillars)

# 7. VISUALIZATION: THE COMPOSITE MOBILITY INDEX
# ------------------------------------------------------------------------------
mobility_explanation <- paste0(
  "Note: The Socioeconomic Mobility Index represents the percentage of individuals who are simultaneously experiencing upward trajectory across all three pillars.\n",
  "This metric moves beyond current static wealth to measure the actual velocity of economic and social improvement over time and across generations."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "Mobility_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Socioeconomic Mobility Index (%)",
  plot_title     = "GSS_socioeconomic_mobility_composite_2",
  caption_text   = stringr::str_wrap(mobility_explanation, width = 130) # Safeguard wrapper 
)
print(p_index)