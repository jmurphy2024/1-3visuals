## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: gss_LifeSatisfaction_acquire.R
## Purpose: Ingests raw GSS .sav and keeps variables for Life Satisfaction analysis.
## Author: Janica Murphy, Max Goshert, EPAG / Gemini
## Created: January 21, 2026

rm(list = ls()); gc()
library(dplyr); library(here); library(haven) 

USER_GSS_SAV_FILE_PATH <- here::here("02_Scripts", "III- Templates", "GSS", "GSS2024.sav")
OUTPUT_PATH <- here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_LifeSatisfaction_raw.rds")

gss_full <- haven::read_sav(USER_GSS_SAV_FILE_PATH)

gss_subset <- gss_full %>%
  rename(
    ID       = any_of(c("id", "ID", "id_", "ID_", "CASEID", "caseid")),
    YEAR     = any_of(c("year", "YEAR", "gssyear", "GSSYEAR")),
    # Infrastructure Inputs
    SATJOB   = any_of(c("satjob", "SATJOB")),     # Economic/Job
    FINRELA  = any_of(c("finrela", "FINRELA")),   # Economic/Perception
    CONBUS   = any_of(c("conbus", "CONBUS")),     # Institutional
    # Health/Social Outcomes
    HEALTH   = any_of(c("health", "HEALTH")),     # Physical/Environmental
    HAPPY    = any_of(c("happy", "HAPPY")),       # Overall Cognitive Assessment
    SOCFREND = any_of(c("socfrend", "SOCFREND")), # Social Infrastructure
    # Anchor & Weight
    INCOME16 = any_of(c("income16", "INCOME16")),
    WTSSNRPS = any_of(c("wtssnrps", "WTSSNRPS"))
  ) %>%
  select(ID, YEAR, WTSSNRPS, INCOME16, SATJOB, FINRELA, CONBUS, HEALTH, HAPPY, SOCFREND) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(WTSSNRPS) & WTSSNRPS > 0)

saveRDS(gss_subset, OUTPUT_PATH)
message("SUCCESS: Life Satisfaction raw subset saved.")