# ==============================================================================
# SCRIPT: NCVS_Prevalence_Skyline_2.R
# Purpose: Generate 3-Country Skyline for Crime PREVALENCE (% of people victimized)
# ==============================================================================
rm(list = ls()); gc()
library(dplyr); library(here); library(stringr); library(tidyr); library(ggplot2)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found.")
cutoffs <- readRDS(cutoffs_path)
set.seed(123) 

extract_code <- function(x) { as.numeric(str_extract(as.character(x), "\\d+")) }

hh_obj_name  <- load(here::here("01_data", "raw", "NCVS", "ncvs_household_2023.rda"))
per_obj_name <- load(here::here("01_data", "raw", "NCVS", "ncvs_person_2023.rda"))
inc_obj_name <- load(here::here("01_data", "raw", "NCVS", "ncvs_extract_2023.rda"))
ds2_raw <- get(hh_obj_name[1]); ds3_raw <- get(per_obj_name[1]); ds5_raw <- get(inc_obj_name[1])
rm(list = c(hh_obj_name, per_obj_name, inc_obj_name)); gc()

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023) 

ds2_unique <- ds2_raw %>% group_by(IDHH) %>% slice(1) %>% ungroup() 
ds3_unique <- ds3_raw %>% group_by(IDHH, IDPER) %>% slice(1) %>% ungroup() 

prepared_data <- ds3_unique %>%
  left_join(ds2_unique %>% select(IDHH, V2026, V2127B), by = "IDHH") %>% 
  left_join(ds5_raw %>% select(IDHH, IDPER, V4529), by = c("IDHH", "IDPER"), relationship = "one-to-many") %>% 
  mutate(
    income_code   = extract_code(V2026), 
    PERWT         = as.numeric(as.character(WGTPERCY)), 
    crime_code    = extract_code(V4529), 
    MAPPED_REGION = as.numeric(V2127B), 
    Violent_Crime = if_else(!is.na(crime_code) & crime_code >= 1 & crime_code <= 20, 1, 0), 
    Property_Crime= if_else(!is.na(crime_code) & crime_code >= 31 & crime_code <= 59, 1, 0) 
  ) %>%
  group_by(IDHH, IDPER) %>%
  summarise(
    PERWT         = first(PERWT), 
    income_code   = first(income_code), 
    MAPPED_REGION = first(MAPPED_REGION), 
    Violent_Crime = max(Violent_Crime, na.rm = TRUE), # MAX for Prevalence
    Property_Crime= max(Property_Crime, na.rm = TRUE), 
    .groups       = "drop" 
  ) %>%
  filter(!is.na(income_code) & income_code < 99 & PERWT > 0) %>% 
  mutate(
    raw_dollars = case_when(
      income_code == 1 ~ runif(n(),0,4999),       income_code == 2 ~ runif(n(),5000,9999),
      income_code == 3 ~ runif(n(),10000,14999),  income_code == 4 ~ runif(n(),15000,24999),
      income_code == 5 ~ runif(n(),25000,34999),  income_code == 6 ~ runif(n(),35000,49999),
      income_code == 7 ~ runif(n(),50000,74999),  income_code == 17 ~ runif(n(),75000,250000), 
      TRUE             ~ runif(n(),35000,49999) 
    ),
    REAL_INCOME = raw_dollars * INFLATION_ADJ * get_regional_rpp_multiplier(MAPPED_REGION), 
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third", 
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third", 
      TRUE ~ "Top Third" 
    )
  ) %>%
  filter(!is.na(REAL_INCOME), !is.na(Country)) 

plot_data <- prepared_data %>% rename(`Property Crime` = Property_Crime, `Violent Crime` = Violent_Crime)

message("\n=== PROPERTY CRIME PREVALENCE ===")
print(as.data.frame(get_country_summary(plot_data, "Property Crime", "PERWT")))
message("\n=== VIOLENT CRIME PREVALENCE ===")
print(as.data.frame(get_country_summary(plot_data, "Violent Crime", "PERWT")))

p <- plot_economic_skyline_2(
  data           = plot_data, 
  indicator_vars = c("Property Crime", "Violent Crime"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Prevalence Rate (%)", 
  plot_title     = "NCVS_Prevalence_Skyline_2",
  caption_text   = "Prevalence Rate: Measures the percentage of the population victimized at least once this year."
)
print(p)