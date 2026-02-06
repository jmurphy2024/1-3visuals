# ==== 0. ABOUT ====
## Script: ACS_poverty_prepare.R
## Purpose: Identify precarious housing indicators among the impoverished.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here)

# --- 1. CONFIGURE ---
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "Poverty_Living_Arrangements"

# --- 2. LOGIC ---
RAW_DATA_FILE <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID), "raw_data.rds")
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

raw_data <- readRDS(RAW_DATA_FILE)

prepared_data <- raw_data %>%
  mutate(
    # Clean Poverty (000 is N/A)
    poverty_pct = if_else(POVERTY == 0, NA_real_, as.numeric(POVERTY)),
    is_impoverished = if_else(poverty_pct < 100, 1, 0),
    
    # 1. Indicator: Sheltered Precarious (GQ 3 or 4: Other non-institutional / Shelters)
    # Note: 701 is the specific code for shelters in the detailed GQ variable.
    ind_sheltered_precarious = if_else(is_impoverished == 1 & GQ %in% c(3, 4), 1, 0),
    
    # 2. Indicator: Doubled-Up (In Poverty + Not a relative of the household head)
    # RELATE codes for non-relatives are typically 1100-1200+
    ind_doubled_up = if_else(is_impoverished == 1 & RELATE >= 1100, 1, 0)
  )

# Save processed data
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))
saveRDS(prepared_data, file = PROCESSED_DATA_FILE)

message("\n--- Poverty preparation script complete. ---")