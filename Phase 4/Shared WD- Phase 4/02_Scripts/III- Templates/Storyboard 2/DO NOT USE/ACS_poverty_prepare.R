# ==============================================================================
# SCRIPT: ACS_poverty_prepare.R
# Purpose: Prepares Master Data with Poverty Flags and Real Purchasing Power.
# Logic:   RPP adjustment, $0 Floor Income, 342M Pop Scale.
# Source:  Uses output from ACS_main_acquire.R (ipums_data_raw.rds)
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(here); library(readr); library(tidyr)

# --- 1. CONFIG ---
TARGET_US_POPULATION <- 342000000 

# --- 2. LOAD RAW DATA ---
raw_data <- readRDS(here::here("01_data", "processed", "ipums_data_raw.rds"))

# --- 3. STATE RPP LOOKUP (MASTER FRAMEWORK) ---
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

# --- 4. MASTER POVERTY TRANSFORMATION ---
message("Processing Master Poverty Dataset...")

prepared_data <- raw_data %>%
  filter(STATEFIP <= 56) %>%
  mutate(
    # A. Income & RPP Adjustments
    income_raw = as.numeric(HHINCOME),
    income_clamped = case_when(
      income_raw == 9999999 ~ NA_real_,
      income_raw < 0 ~ 0,
      TRUE ~ income_raw
    ),
    adj_factor = if_else(as.numeric(ADJUST) > 100, as.numeric(ADJUST) / 1000000, as.numeric(ADJUST)),
    STATEFIP = as.numeric(STATEFIP)
  ) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    # Formula: HHINCOME * Adjustment / (RPP / 100)
    REAL_INCOME = (income_clamped * adj_factor) * (100 / coalesce(STATE_RPP, 100)),
    
    # B. Poverty Specific Flag
    # Official Poverty: POVERTY < 100 (excluding 0, which often means N/A)
    is_poverty = if_else(as.numeric(POVERTY) > 0 & as.numeric(POVERTY) < 100, 1, 0),
    
    # C. Demographic Categories (From Master Logic)
    Race_Ethnicity = case_when(
      HISPAN > 0           ~ "Hispanic",
      RACE == 1            ~ "White (NH)",
      RACE == 2            ~ "Black (NH)",
      RACE %in% c(4, 5, 6) ~ "Asian & PI (NH)",
      RACE == 3            ~ "Native American (NH)",
      RACE %in% c(8, 9)    ~ "Multiracial (NH)",
      TRUE                 ~ "Other (NH)"
    )
  ) %>%
  filter(!is.na(REAL_INCOME), !is.na(PERWT))

# --- 5. POPULATION WEIGHT SCALING (342M TARGET) ---
pop_scalar    <- TARGET_US_POPULATION / sum(prepared_data$PERWT)
prepared_data <- prepared_data %>% mutate(PERWT = PERWT * pop_scalar)

# --- 6. SAVE PREPARED DATA ---
saveRDS(prepared_data, here::here("01_data", "processed", "prepared_ACS_poverty.rds"))
message("Poverty Preparation Complete: 342M weights and RPP applied.")