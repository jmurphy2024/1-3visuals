# ==============================================================================
# SCRIPT: GSS_Work_Life_Balance_2.R
# Purpose: Generate 3-Country Skyline for Work-Life Balance (WLB)
# Logic:   Sovereignty, Support, & Fulfillment Pillars + Dynamic V2 Borders
# ==============================================================================
rm(list = ls()); gc()
library(haven); library(dplyr); library(here); library(tidyr); library(ggplot2); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

# Load dynamic V2 cutoffs
cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

set.seed(123)

# 2. DATA ACQUISITION
# ------------------------------------------------------------------------------
TARGET_FILE <- here::here("01_data", "raw", "GSS2024.sav")
if(!file.exists(TARGET_FILE)) stop("GSS .sav file not found.")
raw_data <- read_sav(TARGET_FILE)

# 3. THE UNIFIED PIPELINE: UPDATED PILLAR & INDEX LOGIC
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
    
    # --- [REVISED] WLB PILLAR & INDEX LOGIC ---
    # Time Sovereignty: High if hours <= 40 OR working part-time (WRKSTAT == 2)
    ind_sovereignty = if_else(HRS1 <= 40 | (!is.na(WRKSTAT) & WRKSTAT == 2), 1, 0, missing = 0),
    
    # Support Resources: High if family income >= $75k OR married (MARITAL == 1)
    ind_support     = if_else(REALINC >= 75000 | MARITAL == 1, 1, 0, missing = 0),
    
    # Life Fulfillment: High if 'Very Happy' (HAPPY == 1) OR 'Very Satisfied' with job (SATJOB == 1)
    ind_fulfillment = if_else(HAPPY == 1 | (!is.na(SATJOB) & SATJOB == 1), 1, 0, missing = 0),
    
    # Composite WLB Index: High Capital in all three pillars (restrictive 'AND' logic)
    ind_wlb_index = if_else(ind_sovereignty == 1 & ind_support == 1 & ind_fulfillment == 1, 1, 0),
    
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
    )
  ) %>%
  mutate(
    # Adjustment for 2024 to 2023 dollars (Inflation)
    REAL_INCOME = raw_dollars * 0.97, 
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country))

# 4. VISUALIZATION: THE WLB PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data %>%
  rename(
    `Time Sovereignty` = ind_sovereignty,
    `Support Resources` = ind_support,
    `Life Fulfillment` = ind_fulfillment
  )

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Time Sovereignty", "Support Resources", "Life Fulfillment"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population with Pillar Access (%)",
  plot_title     = "GSS_WLB_pillars_2",
  caption_text   = stringr::str_wrap("The Infrastructure of Balance: This chart tracks access to the three pillars of Work-Life Balance. Sovereignty measures control over time (HRS1/WRKSTAT); Support measures financial and partnership stability (REALINC/MARITAL); Fulfillment measures emotional and professional satisfaction (HAPPY/SATJOB).", width = 115)
)

# 5. VISUALIZATION: THE COMPOSITE WLB INDEX
# ------------------------------------------------------------------------------
# We use \n to force each pillar onto its own line for better scannability
wlb_explanation <- paste0(
  "How to read the index: Full Work-Life Balance is a metric requiring capital in all three pillars.\n",
  "1. Time Sovereignty (Agency): Measures control over one's schedule, identifying those not burdened by excessive hours. \n",
  "2. Support Resources (Stability): Measures the floor of security provided by high income or partnership. \n",
  "3. Life Fulfillment (Well-being): Measures the end result—subjective happiness and professional satisfaction.\n\n."
  #"The Index reveals the 'Time-Wealth Gap.' Balance is only achieved where these three dimensions finally converge."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "ind_wlb_index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Work-Life Balance Index",
  plot_title     = "GSS_WLB_index_composite_2",
  # IMPORTANT: We remove stringr::str_wrap here so it respects our manual \n breaks
  caption_text   = wlb_explanation 
)

print(p_index)