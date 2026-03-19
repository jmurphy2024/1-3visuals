# ==============================================================================
# SCRIPT: GSS_Equity_2.R
# Purpose: Generate 3-Country Multi-Variable Skyline for the Equity Index
# Definition: The degree to which opportunities, resources, and outcomes are 
#             distributed fairly across populations.
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

# 4. THE UNIFIED PIPELINE: EQUITY LOGIC & INCOME JITTERING
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
    
    # --- EQUITY PILLAR LOGIC ---
    # 1. Opportunities: Access to upward mobility (Education, Meritocracy, Social Rank)
    Opportunities = if_else(GETAHEAD == 1 | RANK >= 6 | DEGREE >= 3, 1, 0, missing = 0), 
    
    # 2. Resources: Distribution of financial and institutional capital
    Resources     = if_else(REALINC >= 75000 | FINRELA >= 4 | CONBUS <= 2, 1, 0, missing = 0),
    
    # 3. Outcomes: The final distribution of human flourishing and wellbeing
    Outcomes      = if_else(HEALTH <= 2 | HAPPY <= 2 | SATJOB <= 2, 1, 0, missing = 0),
    
    # Composite Equity Index
    Equity_Index  = if_else(Opportunities == 1 & Resources == 1 & Outcomes == 1, 1, 0),
    
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

# 5. SUMMARY STATISTICS: EQUITY INDEX
# ------------------------------------------------------------------------------
summary_stats <- get_country_summary(prepared_data, "Equity_Index", "PERWT")
message("\n=== EQUITY INDEX SUMMARY STATISTICS ===")
print(as.data.frame(summary_stats))

# 6. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data 

pillar_note <- paste0(
  "The Distribution of Equity: This graph displays the three pillars of the Equity Index.\n",
  "1. Opportunities: The degree to which populations have access to systemic advancement.\n",
  "2. Resources: The degree to which financial and material capital are fairly distributed.\n",
  "3. Outcomes: The degree to which final human flourishing (health, happiness) is realized."
)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Opportunities", "Resources", "Outcomes"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population with Pillar Access (%)",
  plot_title     = "GSS_equity_pillars_2",
  caption_text   = stringr::str_wrap(pillar_note, width = 130) # Safeguard wrapper
)
print(p_pillars)

# 7. VISUALIZATION: THE COMPOSITE EQUITY INDEX
# ------------------------------------------------------------------------------
equity_explanation <- paste0(
  "Note: The Equity Index measures the percentage of individuals who simultaneously possess access to all three pillars.\n",
  "It reveals the stark differences in how fairly opportunities, resources, and outcomes are distributed across the Three Countries.\n",
  "A 'Hollow' result occurs when a person has wealth (Resources) but lacks health (Outcomes), or has potential (Opportunities) but lacks means."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "Equity_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Equity Index (%)",
  plot_title     = "GSS_equity_index_composite_2",
  caption_text   = stringr::str_wrap(equity_explanation, width = 130) # Safeguard wrapper 
)
print(p_index)