# ==============================================================================
# SCRIPT: GSS_lifelong_learning_2.R
# Purpose: Generate 3-Country Skyline for Lifelong Learning & Competency
# Definition: The ongoing process through which individuals acquire and apply 
#             new knowledge, skills, and cognitive competencies across life.
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

# 4. THE UNIFIED PIPELINE: LIFELONG LEARNING LOGIC
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
    
    # --- PILLAR & INDEX LOGIC (LIFELONG LEARNING) ---
    # 1. Daily Knowledge Acquisition: Reads news everyday (1) or a few times a week (2)
    `Knowledge Acquisition` = if_else(!is.na(NEWS) & NEWS <= 2, 1, 0, missing = 0), 
    
    # 2. Cognitive Maintenance: Scores >= 6 on the WORDSUM vocabulary competency test
    `Cognitive Maintenance` = if_else(!is.na(WORDSUM) & WORDSUM >= 6, 1, 0, missing = 0),
    
    # 3. Foundational Competency: Junior College (2), Bachelor (3), or Graduate (4) degree
    `Foundational Competency` = if_else(!is.na(DEGREE) & DEGREE >= 2, 1, 0, missing = 0),
    
    # Composite Lifelong Learning Index: High capacity across all three dimensions
    Lifelong_Learning_Index = if_else(`Knowledge Acquisition` == 1 & 
                                        `Cognitive Maintenance` == 1 & 
                                        `Foundational Competency` == 1, 1, 0),
    
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

# 5. SUMMARY STATISTICS: LEARNING INDEX & PILLARS
# ------------------------------------------------------------------------------
message("\n=== PILLAR 1: KNOWLEDGE ACQUISITION SUMMARY ===")
summary_knowledge <- get_country_summary(prepared_data, "Knowledge Acquisition", "PERWT")
print(as.data.frame(summary_knowledge))

message("\n=== PILLAR 2: COGNITIVE MAINTENANCE SUMMARY ===")
summary_cognitive <- get_country_summary(prepared_data, "Cognitive Maintenance", "PERWT")
print(as.data.frame(summary_cognitive))

message("\n=== PILLAR 3: FOUNDATIONAL COMPETENCY SUMMARY ===")
summary_competency <- get_country_summary(prepared_data, "Foundational Competency", "PERWT")
print(as.data.frame(summary_competency))

message("\n=== OVERALL COMPOSITE: LIFELONG LEARNING INDEX SUMMARY ===")
summary_stats <- get_country_summary(prepared_data, "Lifelong_Learning_Index", "PERWT")
print(as.data.frame(summary_stats))

# 6. VISUALIZATION: THE PILLAR SKYLINE
# ------------------------------------------------------------------------------
plot_data_pillars <- prepared_data 

pillar_note <- paste0(
  "The Architecture of Lifelong Learning: This graph tracks three core dimensions of ongoing knowledge and skill acquisition.\n",
  "1. Knowledge Acquisition: Populations actively seeking and consuming information via regular news readership (NEWS).\n",
  "2. Cognitive Maintenance: Populations exhibiting high sustained reading and verbal comprehension (WORDSUM).\n",
  "3. Foundational Competency: Populations who pursued non-mandatory higher education, establishing structural learning skills (DEGREE)."
)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data_pillars, 
  indicator_vars = c("Knowledge Acquisition", "Cognitive Maintenance", "Foundational Competency"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Exhibiting Competency (%)",
  plot_title     = "GSS_lifelong_learning_pillars_2",
  caption_text   = stringr::str_wrap(pillar_note, width = 130)
)
print(p_pillars)

# 7. VISUALIZATION: THE COMPOSITE LEARNING INDEX
# ------------------------------------------------------------------------------
learning_explanation <- paste0(
  "Note: The Lifelong Learning Index represents the percentage of individuals who are actively maintaining their cognitive and informational competencies.\n",
  "It isolates those who simultaneously possess a structural foundation in higher education, sustained vocabulary retention, and active daily information consumption habits."
)

p_index <- plot_economic_skyline(
  data           = prepared_data, 
  indicator_var  = "Lifelong_Learning_Index", 
  weight_var     = "PERWT", 
  y_axis_label   = "Full Lifelong Learning Index (%)",
  plot_title     = "GSS_lifelong_learning_composite_2",
  caption_text   = stringr::str_wrap(learning_explanation, width = 130) 
)
print(p_index)