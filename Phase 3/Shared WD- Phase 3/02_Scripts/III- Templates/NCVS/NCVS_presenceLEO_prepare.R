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

# ==== 3. RELATIONAL JOIN & 4. COLLAPSE TO PREVALENCE ====

message("  > Starting join and transformation...")

prepared_data <- ds3_unique %>%
  # Join Household Income
  left_join(ds2_unique %>% select(IDHH, V2026), by = "IDHH") %>%
  
  # Join Incidents (V4022 for Reporting, V4407 for Presence)
  left_join(ds5_raw %>% select(IDHH, IDPER, V4022, V4407), 
            by = c("IDHH", "IDPER"),
            relationship = "one-to-many") %>%
  
  # Create the indicators
  mutate(
    income_code = extract_code(V2026),
    PERWT       = as.numeric(as.character(WGTPERCY)),
    
    # Standard Reporting: 1 = Yes, 2 = No. Treat NAs (non-victims) as 0.
    ind_reported = if_else(extract_code(V4022) == 1, 1, 0, missing = 0),
    
    # Physical Presence: 1 = Scene, 2 = Elsewhere. Treat others/NAs as 0.
    ind_police_present = if_else(extract_code(V4407) %in% c(1, 2), 1, 0, missing = 0)
  ) %>%
  
  # Summarize to the person level
  group_by(IDHH, IDPER) %>%
  summarise(
    PERWT              = first(PERWT),
    income_code        = first(income_code),
    # Now these objects exist and can be summarized
    ind_reported       = max(ind_reported, na.rm = TRUE),
    ind_police_present = max(ind_police_present, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  
  # Filter for valid income and positive weights
  filter(!is.na(income_code), income_code < 99, PERWT > 0) %>%
  
  # Generate HHINCOME for jittering/grouping
  mutate(
    HHINCOME = case_when(
      income_code == 1  ~ runif(n(), 0, 4999), 
      income_code == 17 ~ runif(n(), 200000, 500000), 
      TRUE              ~ runif(n(), 50000, 74999) 
    )
  ) %>%
  select(PERWT, HHINCOME, income_code, ind_reported, ind_police_present)

message("  > Relational join and filtering successful.")
# ==== 6. SAVE ====
saveRDS(prepared_data, here::here("01_data", "processed", "NCVS_Microdata", "prepared_NCVS_Prevalence_2023.rds"))
message("SUCCESS: Many-to-many fixed. Final count: ", nrow(prepared_data))