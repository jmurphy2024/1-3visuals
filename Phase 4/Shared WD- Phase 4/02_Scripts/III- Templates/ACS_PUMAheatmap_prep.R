# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_PUMAheatmap_prepare.R
# Purpose: Cleans data and creates a unique 'puma_geoid' for mapping.
# Output:  01_data/processed/IPUMS_Microdata/prepared_ACS_PUMA_us2023c.rds
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(tibble); library(stringr)

# --- 1. SETUP ---
SAMPLE_ID <- "us2023c"
RAW_FILE  <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", SAMPLE_ID, "_puma"), "raw_data.rds")

if(!file.exists(RAW_FILE)) stop("Raw PUMA file not found. Run Script 1.")
raw_data <- readRDS(RAW_FILE)

# RPP Lookup
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

message("Processing PUMA Data...")

prepared_data <- raw_data %>%
  filter(STATEFIP != 72) %>% 
  mutate(STATEFIP = as.numeric(STATEFIP)) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    # Income Calc
    income_raw = as.numeric(HHINCOME),
    income_cleaned = if_else(income_raw >= 9999999 | income_raw < 0, NA_real_, income_raw),
    adj_factor = if_else(as.numeric(ADJUST) > 100, as.numeric(ADJUST) / 1000000, as.numeric(ADJUST)),
    REAL_INCOME = (income_cleaned * adj_factor) * (100 / coalesce(STATE_RPP, 100)),
    
    # Race/Ethnicity Recoding
    race_ethnicity = case_when(
      as.numeric(HISPAN) > 0 & as.numeric(HISPAN) < 9 ~ "Hispanic",
      as.numeric(RACE) == 1 ~ "White",
      as.numeric(RACE) == 2 ~ "Black",
      as.numeric(RACE) %in% c(4, 5, 6) ~ "Asian",
      as.numeric(RACE) == 3 ~ "Native American",
      TRUE ~ "Other/Multiracial"
    ),
    
    # --- PUMA GEOID CREATION ---
    # IPUMS PUMA is often numeric. We need 5 digits padded with zeros.
    # GEOID = 2-digit State FIPS + 5-digit PUMA
    puma_clean = sprintf("%05d", as.numeric(PUMA)),
    state_clean = sprintf("%02d", STATEFIP),
    puma_geoid = paste0(state_clean, puma_clean)
  ) %>%
  filter(!is.na(REAL_INCOME), REAL_INCOME > 0, as.numeric(PUMA) > 0) # Remove non-identified areas

# Save
out_dir <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
saveRDS(prepared_data, file.path(out_dir, "prepared_ACS_PUMA_us2023c.rds"))
message("Success.")