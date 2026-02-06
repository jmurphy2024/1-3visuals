## WD location: 02_Scripts/III- Templates
## Script: NCVS_criminal_prepare.R
## Purpose: Unified 3-Way Join for Overall Incidents and Reporting behavior.
## Author: Janica Murphy, Maxwell Goshert EPAG/ Gemini
## Date Created: 2026-01-08
## Last Modified: 2026-01-26

# ==== 0. ABOUT ====
rm(list = ls()); gc()
library(dplyr); library(here); library(stringr); library(tidyr)
set.seed(123)
message("--- Starting Unified Data Preparation Pipeline ---")

# ==== 1. LOAD DATA ====
message("Step 1: Loading standardized RDS files...")
ds2_raw <- readRDS(here::here("01_Data", "Raw", "NCVS_Microdata", "ncvs_2023_annual", "ds2_raw.rds"))
ds3_raw <- readRDS(here::here("01_Data", "Raw", "NCVS_Microdata", "ncvs_2023_annual", "ds3_raw.rds"))
ds5_raw <- readRDS(here::here("01_Data", "Raw", "NCVS_Microdata", "ncvs_2023_annual", "ds5_raw.rds"))

extract_code <- function(x) { as.numeric(str_extract(as.character(x), "\\d+")) }

# ==== 2. DEDUPLICATION ====
message("Step 2: Performing deduplication...")
ds2_unique <- ds2_raw %>% group_by(IDHH) %>% slice(1) %>% ungroup()
ds3_unique <- ds3_raw %>% group_by(IDHH, IDPER) %>% slice(1) %>% ungroup()

# ==== 3. UNIFIED JOIN & INDICATOR CREATION ====
message("Step 3: Beginning Unified Relational Join...")

prepared_data <- ds3_unique %>%
  # Join A: Household Income (DS2)
  left_join(ds2_unique %>% select(IDHH, V2026), by = "IDHH") %>%
  
  # Join B: Incidents, Reporting, and Presence (DS5)
  left_join(ds5_raw %>% select(IDHH, IDPER, V4529, V4022, V4407), 
            by = c("IDHH", "IDPER"),
            relationship = "one-to-many") %>%
  
  # Step 3.1: Define indicators
  mutate(
    income_code = extract_code(V2026),
    PERWT       = as.numeric(as.character(WGTPERCY)),
    crime_code  = extract_code(V4529),
    
    # Victimization Indicators
    ind_violent  = if_else(!is.na(crime_code) & crime_code >= 1 & crime_code <= 20, 1, 0, missing = 0),
    ind_property = if_else(!is.na(crime_code) & crime_code >= 31 & crime_code <= 59, 1, 0, missing = 0),
    
    # Reporting Indicators
    ind_reported       = if_else(extract_code(V4022) == 1, 1, 0, missing = 0),
    ind_police_present = if_else(extract_code(V4407) %in% c(1, 2), 1, 0, missing = 0)
  ) %>%
  
  # ==== 4. COLLAPSE TO PERSON-LEVEL PREVALENCE ====
group_by(IDHH, IDPER) %>%
  summarise(
    PERWT              = first(PERWT),
    income_code        = first(income_code),
    ind_violent        = max(ind_violent, na.rm = TRUE),
    ind_property       = max(ind_property, na.rm = TRUE),
    ind_reported       = max(ind_reported, na.rm = TRUE),
    ind_police_present = max(ind_police_present, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # ==== 5. FILTER & JITTER ==== 
# THE MESSAGE MUST NOT INTERRUPT THE %>% CHAIN
filter(!is.na(income_code) & income_code < 99 & PERWT > 0) %>%
  mutate(
    HHINCOME = case_when(
      income_code == 1  ~ runif(n(), 0, 4999), 
      income_code == 17 ~ runif(n(), 200000, 500000), 
      TRUE ~ runif(n(), 50000, 74999) 
    )
  ) %>%
  # Ensure these are ALL selected before saving
  select(
    PERWT, HHINCOME, income_code, 
    ind_violent, ind_property,    # <--- THESE MUST BE HERE
    ind_reported, ind_police_present
  )

# Place your status message HERE, after the 'prepared_data' object is fully created
message("Step 5: Filtering and applying income jitter complete.")

# ==== 6. SAVE ====
saveRDS(prepared_data, here::here("01_data", "processed", "NCVS_Microdata", "prepared_NCVS_Prevalence_2023.rds"))
message("SUCCESS: Unified dataset saved. Final count: ", scales::comma(nrow(prepared_data)))