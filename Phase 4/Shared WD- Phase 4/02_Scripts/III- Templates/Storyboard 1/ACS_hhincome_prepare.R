# ==============================================================================
# SCRIPT: ACS_hhincome_prepare.R
# Purpose: Prepares inclusive person-level data (Synced with Border Logic).
# Logic:   1. Negatives -> 0 (Zero Floor).
#          2. Weights -> Scaled to 342M Target.
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(here); library(readr); library(tidyr)

# --- 1. CONFIG ---
TARGET_US_POPULATION <- 342000000 

# --- 2. LOAD RAW DATA ---
raw_file <- here::here("01_data", "processed", "ipums_data_raw.rds")
if(!file.exists(raw_file)) stop("Raw data not found. Run the Acquire script first.")
raw_data <- readRDS(raw_file)

# --- 3. RPP LOOKUP ---
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

# --- 4. TRANSFORM & SCALE ---
message("Applying Zero-Floor Income & Population Scaling...")

data_intermediate <- raw_data %>%
  mutate(
    STATEFIP = as.numeric(STATEFIP),
    income_raw = as.numeric(HHINCOME),
    
    # 1. Zero-Floor Logic (Matches Border Script)
    income_clamped = case_when(
      income_raw == 9999999 ~ NA_real_,
      income_raw < 0 ~ 0,
      TRUE ~ income_raw
    ),
    
    adj_factor = if_else(as.numeric(ADJUST) > 100, as.numeric(ADJUST) / 1000000, as.numeric(ADJUST))
  ) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    REAL_INCOME = (income_clamped * adj_factor) * (100 / coalesce(STATE_RPP, 100))
  ) %>%
  filter(!is.na(REAL_INCOME), !is.na(PERWT), PERWT > 0)

# 2. Apply Weight Scaling
current_total <- sum(data_intermediate$PERWT)
pop_scalar    <- TARGET_US_POPULATION / current_total

prepared_data <- data_intermediate %>%
  mutate(
    PERWT_RAW = PERWT,            # Keep original for reference
    PERWT = PERWT * pop_scalar    # Update PERWT to scaled value
  )

# --- 5. SAVE ---
output_file <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
saveRDS(prepared_data, output_file)

message(paste("Preparation Complete."))
message(paste("Scalar Applied:", round(pop_scalar, 4)))
message(paste("Final Pop Count:", format(sum(prepared_data$PERWT), big.mark=",")))