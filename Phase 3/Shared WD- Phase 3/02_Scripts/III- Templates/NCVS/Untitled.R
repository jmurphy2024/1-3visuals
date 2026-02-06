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

message("  > Starting join and transformation...")

prepared_data <- ds3_unique %>%
  # Join Household Income
  left_join(ds2_unique %>% select(IDHH, V2026), by = "IDHH") %>%
  
  # Join Incidents
  left_join(ds5_raw %>% select(IDHH, IDPER, V4399, V4412, V4413, V4414), 
            by = c("IDHH", "IDPER"),
            relationship = "one-to-many") %>%
  
  # Fix NAs immediately to prevent the -Inf warning loop
  mutate(
    income_code = extract_code(V2026),
    PERWT       = as.numeric(as.character(WGTPERCY)),
    # If the join found no incident, these columns are NA; we convert them to 0
    ind_not_reported = if_else(extract_code(V4399) == 0, 1, 0, missing = 0),
    ind_inefficient  = if_else(extract_code(V4412) == 1, 1, 0, missing = 0),
    ind_biased       = if_else(extract_code(V4413) == 1, 1, 0, missing = 0),
    ind_leo_offender = if_else(extract_code(V4414) == 1, 1, 0, missing = 0)
  ) %>%
  # ==== 4. COLLAPSE TO PREVALENCE ====
group_by(IDHH, IDPER) %>%
  summarise(
    PERWT            = first(PERWT),
    income_code      = first(income_code),
    # Use 'any' or 'max' but ensure the NA handling is safe
    ind_not_reported = max(ind_not_reported, na.rm = TRUE),
    ind_inefficient  = max(ind_inefficient, na.rm = TRUE),
    ind_biased       = max(ind_biased, na.rm = TRUE),
    ind_leo_offender = max(ind_leo_offender, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Filter and Jitter
  filter(!is.na(income_code), income_code < 99, PERWT > 0) %>%
  mutate(
    HHINCOME = case_when(
      income_code == 1  ~ runif(n(), 0, 4999), 
      income_code == 17 ~ runif(n(), 200000, 500000), 
      TRUE              ~ runif(n(), 50000, 74999) 
    )
  ) %>%
  select(PERWT, HHINCOME, starts_with("ind_"))

message("  > Relational join and filtering successful.")
# ==== 6. SAVE ====
saveRDS(prepared_data, here::here("01_data", "processed", "NCVS_Microdata", "prepared_NCVS_Prevalence_2023.rds"))
message("SUCCESS: Many-to-many fixed. Final count: ", nrow(prepared_data))