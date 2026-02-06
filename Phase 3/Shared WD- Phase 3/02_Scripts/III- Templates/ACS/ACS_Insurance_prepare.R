# ==== 0. ABOUT ====
## Script: ACS_health_prepare.R
## Purpose: Clean 2024 ACS data and create binary coverage indicators.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-29

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)

USER_IPUMS_SAMPLE_ID   <- "us2024a"
USER_INDICATOR_NAME    <- "Health_Coverage_Dual"

RAW_DATA_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID))
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

raw_data <- readRDS(file.path(RAW_DATA_DIR, "raw_data.rds"))

# --- CLEANING ---
cleaned_data <- raw_data %>%
  filter(FTOTINC != 9999999) %>%
  mutate(
    HHINCOME = as.numeric(FTOTINC),
    PERWT = as.numeric(PERWT),
    
    # 1. Private Coverage (1=No, 2=Yes -> 0/1)
    ind_private_coverage = if_else(HCOVPRIV == 2, 1, 0),
    
    # 2. Public Coverage (1=No, 2=Yes -> 0/1)
    ind_public_coverage = if_else(HCOVPUB == 2, 1, 0)
  )

# --- SAVE ---
essential_cols <- c(
  "SERIAL", "HHWT", "PERWT", "HHINCOME", "AGE",
  "ind_private_coverage", "ind_public_coverage", 
  "SAMPLE"
)
saveRDS(cleaned_data %>% select(any_of(essential_cols)), 
        file = file.path(PROCESSED_DIR, paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds")))
message("Data prepared with both Private and Public indicators.")