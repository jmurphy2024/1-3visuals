## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: GSS_foodwater_acquire.R
## Purpose: Ingests raw GSS .sav for Food and Clean Water Security.## Author: Janica Murphy / Gemini
## Author: Janica Murphy / Gemini
## Created: January 21, 2026

rm(list = ls()); gc()
library(dplyr); library(here); library(haven) 

USER_GSS_SAV_FILE_PATH <- here::here("02_Scripts", "III- Templates", "GSS", "GSS2024.sav")
OUTPUT_PATH <- here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_FoodWater_raw.rds")

gss_full <- haven::read_sav(USER_GSS_SAV_FILE_PATH)

gss_subset <- gss_full %>%
  rename(
    ID       = any_of(c("id", "ID", "CASEID")),
    # Provisioning & Infrastructure Indicators
    FOOD     = any_of(c("food", "FOOD")),         # Access to adequate food
    WATER    = any_of(c("water", "WATER")),       # Access to safe drinking water
    SATFIN   = any_of(c("satfin", "SATFIN")),     # Satisfaction with financial state
    FINRELA  = any_of(c("finrela", "FINRELA")),   # Relative financial standing
    # Anchors & Weights
    INCOME16 = any_of(c("income16", "INCOME16")),
    WTSSNRPS = any_of(c("wtssnrps", "WTSSNRPS"))
  ) %>%
  select(ID, WTSSNRPS, INCOME16, FOOD, WATER, SATFIN, FINRELA) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(WTSSNRPS) & WTSSNRPS > 0)

saveRDS(gss_subset, OUTPUT_PATH)
message("SUCCESS: Food and Water raw subset saved.")