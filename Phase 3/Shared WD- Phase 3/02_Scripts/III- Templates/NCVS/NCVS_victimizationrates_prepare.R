## WD location: 02_Scripts/III- Templates
## Script: NCVS_victimizationrates_prepare.R
## Purpose: 3-Way Join with explicit deduplication to fix many-to-many errors.
## Author: Janica Murphy, Maxwell Goshert EPAG/ Gemini
## Date Created: 2026-01-08
## Last Modified: 2026-01-14

# ==== 0. ABOUT ====
rm(list = ls()); gc()
library(dplyr); library(here); library(stringr); library(tidyr)
set.seed(123)

# ==== 1. LOAD DATA ====
ds2_raw <- readRDS(here::here("01_Data", "Raw", "NCVS_Microdata", "ncvs_2023_annual", "ds2_raw.rds"))
ds3_raw <- readRDS(here::here("01_Data", "Raw", "NCVS_Microdata", "ncvs_2023_annual", "ds3_raw.rds"))
ds5_raw <- readRDS(here::here("01_Data", "Raw", "NCVS_Microdata", "ncvs_2023_annual", "ds5_raw.rds"))

extract_code <- function(x) { as.numeric(str_extract(as.character(x), "\\d+")) }

# ==== 2. DEDUPLICATION (CRITICAL FIX) ====

# A. Deduplicate Household file: One income per IDHH
ds2_unique <- ds2_raw %>%
  group_by(IDHH) %>%
  slice(1) %>% 
  ungroup()

# B. Deduplicate Person file: One anchor per person
ds3_unique <- ds3_raw %>%
  group_by(IDHH, IDPER) %>%
  slice(1) %>% 
  ungroup()

# ==== 3. RELATIONAL JOIN ====

prepared_data <- ds3_unique %>%
  # Join A: Attach Household Income to Persons (Many-to-One)
  left_join(ds2_unique %>% select(IDHH, V2026), by = "IDHH") %>%
  
  # Join B: Attach Incidents to Persons (One-to-Many)
  # We use relationship = "one-to-many" because one person can have multiple crimes
  left_join(ds5_raw %>% select(IDHH, IDPER, V4529), 
            by = c("IDHH", "IDPER"),
            relationship = "one-to-many") %>%
  
  mutate(
    income_code = extract_code(V2026),
    PERWT       = as.numeric(as.character(WGTPERCY)),
    crime_code  = extract_code(V4529),
    
    # Prevalence logic: 1 if match found in DS5, else 0
    ind_violent  = if_else(!is.na(crime_code) & crime_code >= 1 & crime_code <= 20, 1, 0),
    ind_property = if_else(!is.na(crime_code) & crime_code >= 31 & crime_code <= 59, 1, 0),
 
  ) %>%
  
  # ==== 4. COLLAPSE TO PREVALENCE ====
# Ensure we only have one row per person for the final rate
group_by(IDHH, IDPER) %>%
  summarise(
    PERWT        = first(PERWT),
    income_code  = first(income_code),
    ind_violent  = max(ind_violent, na.rm = TRUE),
    ind_property = max(ind_property, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # ==== 5. FILTER & JITTER ====
# Income from DS2 allows non-victims to be included
filter(!is.na(income_code) & income_code < 99 & PERWT > 0) %>%
  mutate(
    HHINCOME = case_when(
      income_code == 1  ~ runif(n(), 0, 4999), 
      income_code == 17 ~ runif(n(), 200000, 500000), 
      TRUE ~ runif(n(), 50000, 74999) 
    )
  ) %>%
  select(PERWT, HHINCOME, ind_violent, ind_property)

# ==== 6. SAVE ====
saveRDS(prepared_data, here::here("01_data", "processed", "NCVS_Microdata", "prepared_NCVS_Prevalence_2023.rds"))
message("SUCCESS: Many-to-many fixed. Final count: ", nrow(prepared_data))