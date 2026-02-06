## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: gss_EquityCapital_acquire.R
## Purpose: Ingests raw GSS .sav and keeps only Equity Capital indicators and anchors.

rm(list = ls()); gc()
library(dplyr); library(here); library(haven) 

USER_GSS_SAV_FILE_PATH <- here::here("02_Scripts", "III- Templates", "GSS", "GSS2024.sav")
OUTPUT_PATH <- here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_EquityCapital_raw.rds")

gss_full <- haven::read_sav(USER_GSS_SAV_FILE_PATH)

# Flexible renaming for all 9 indicators to ensure they exist for the Prepare script
gss_subset <- gss_full %>%
  rename(
    ID       = any_of(c("id", "ID", "id_", "ID_", "CASEID", "caseid")),
    YEAR     = any_of(c("year", "YEAR", "gssyear", "GSSYEAR")),
    # Opportunity
    GETAHEAD = any_of(c("getahead", "GETAHEAD")),
    RANK     = any_of(c("rank", "RANK")),
    DEGREE   = any_of(c("degree", "DEGREE")),
    # Resources
    REALINC  = any_of(c("realinc", "REALINC")),
    FINRELA  = any_of(c("finrela", "FINRELA")),
    CONBUS   = any_of(c("conbus", "CONBUS")),
    # Outcomes
    HEALTH   = any_of(c("health", "HEALTH")),
    HAPPY    = any_of(c("happy", "HAPPY")),
    SATJOB   = any_of(c("satjob", "SATJOB")),
    # Anchor & Weight
    INCOME16 = any_of(c("income16", "INCOME16")),
    WTSSNRPS = any_of(c("wtssnrps", "WTSSNRPS"))
  ) %>%
  select(ID, YEAR, WTSSNRPS, INCOME16, GETAHEAD, RANK, DEGREE, REALINC, FINRELA, CONBUS, HEALTH, HAPPY, SATJOB) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(WTSSNRPS) & WTSSNRPS > 0)

saveRDS(gss_subset, OUTPUT_PATH)
message("SUCCESS: Raw subset saved with standardized indicator names.")