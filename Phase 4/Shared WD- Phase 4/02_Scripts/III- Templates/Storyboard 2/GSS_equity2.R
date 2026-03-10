# ==============================================================================
# SCRIPT: GSS_Equity_Pillars_2.R
# Purpose: Generate 3-Country Multi-Variable Skyline for Equity Pillars
# Logic:   Opportunity, Resources, & Outcomes Pillars + Dynamic V2 Borders
# ==============================================================================
rm(list = ls()); gc()
library(haven); library(dplyr); library(here); library(tidyr); library(ggplot2); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

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
region_rpp_lookup <- tibble(
  REGION_ID = c(1, 2, 3, 4),
  REG_RPP   = c(105.2, 92.8, 95.4, 104.1) # NE, MW, S, W
)

# Adjustment for 2024 to 2023 dollars
INFLATION_ADJ <- 0.97

# 4. THE UNIFIED PIPELINE: PILLAR LOGIC & INCOME JITTERING
# ------------------------------------------------------------------------------
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  mutate(
    PERWT = as.numeric(WTSSNRPS),
    
    # --- RPP CROSSWALK: Collapse GSS 9 Regions to 4 Regions ---
    MAPPED_REGION = case_when(
      as.numeric(REGION) %in% c(1, 2) ~ 1,
      as.numeric(REGION) %in% c(3, 4) ~ 2,
      as.numeric(REGION) %in% c(5, 6, 7) ~ 3,
      as.numeric(REGION) %in% c(8, 9) ~ 4,
      TRUE ~ NA_real_
    ),
    
    # --- [NEW] PILLAR & INDEX LOGIC ---
    # Opportunity: Get Ahead, Social Rank, or Higher Degree
    Opportunity = if_else(GETAHEAD == 1 | RANK >= 6 | DEGREE >= 3, 1, 0, missing = 0), 
    
    # Resources: High Income, Financial Relative Status, or Confidence in Biz
    Resources   = if_else(REALINC >= 75000 | FINRELA >= 4 | CONBUS <= 2, 1, 0, missing = 0),
    
    # Outcomes: Health, General Happiness, or Job Satisfaction
    Outcomes    = if_else(HEALTH <= 2 | HAPPY <= 2 | SATJOB <= 2, 1, 0, missing = 0),
    
    # Composite Index: Full Equity Access (Intersection of all three)
    Equity_Index = if_else(Opportunity == 1 & Resources == 1 & Outcomes == 1, 1, 0),
    
    # --- INCOME JITTERING (INCOME16) ---
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
    )
  ) %>%
  left_join(region_rpp_lookup, by = c("MAPPED_REGION" = "REGION_ID")) %>%
  mutate(
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country))

# 5. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
# We plot the three pillars together to show the "Infrastructure of Equity"
p <- plot_economic_skyline_2(
  data           = prepared_data, 
  indicator_vars = c("Opportunity", "Resources", "Outcomes"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population with Pillar Access (%)",
  plot_title     = "GSS_equity_pillars_2",
  caption_text   = stringr::str_wrap("How to read this chart: This graph displays the three pillars of the Equity Index. The lines show the percentage of the population in each income decile who meet the criteria for Opportunity, Resources, and Outcomes. The Three Countries are divided at the Master V2 boundaries ($45k/$115k nominal equivalents), adjusted for inflation and regional costs of living.", width = 115)
)

print(p)
# 6. VISUALIZATION: THE COMPOSITE EQUITY INDEX
# ------------------------------------------------------------------------------
equity_explanation <- paste0(
  "Note: The Equity Index represents the percentage of individuals who simultaneously possess access to all three pillars:\n",
  "1. Opportunity (Agency): Belief in meritocracy, social rank, and formal education.\n",
  "2. Resources (Stability): Financial security provided by high income or institutional trustt.\n",
  "3. Outcomes (Well-being): physical health, general happiness, and job satisfaction. \n\n."
  # "This represents holistic flourishing. A 'Hollow' result occurs when a person has wealth (Resources) but lacks health (Outcomes),\n",
  # "or has potential (Opportunity) but lacks the means (Resources) to realize it."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "Equity_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Equity Access Index (%)",
  plot_title     = "GSS_equity_index_composite_2",
  caption_text   = equity_explanation 
)

print(p_index)