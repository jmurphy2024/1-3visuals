## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: GSS_communitytrust_acquire.R
## Purpose: Ingests raw GSS .sav with diverse "Third Place" social indicators.
## Author: Janica Murphy / Gemini
## Created: January 21, 2026

rm(list = ls()); gc()
library(dplyr); library(here); library(haven) 

USER_GSS_SAV_FILE_PATH <- here::here("02_Scripts", "III- Templates", "GSS", "GSS2024.sav")
OUTPUT_PATH <- here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_ThirdPlaces_raw.rds")

gss_subset <- haven::read_sav(USER_GSS_SAV_FILE_PATH) %>%
  rename(
    ID       = any_of(c("id", "ID", "CASEID")),
    # Diverse "Third Place" & Social Infrastructure
    SOCBAR   = any_of(c("socbar", "SOCBAR")),     # Neighborhood Bar/Tavern
    ATTEND   = any_of(c("attend", "ATTEND")),     # Religious House/Services
    SOCFREND = any_of(c("socfrend", "SOCFREND")), # Socializing with Friends
    SOCREL = any_of(c("socrel", "SOCREL")), # Socializing with Relatives
    SOCOMMUN= any_of(c("socommun", "SOCOMMUN")), # Socializing with Neighbors
    # Social Cohesion (The core Trust assessment)
    TRUST    = any_of(c("trust", "TRUST")),       # Can people be trusted?
    FAIR     = any_of(c("fair", "FAIR")),         # Do people try to be fair?
    HELPFUL  = any_of(c("helpful", "HELPFUL")),   # Are people helpful?
    # Anchors
    INCOME16 = any_of(c("income16", "INCOME16")),
    WTSSNRPS = any_of(c("wtssnrps", "WTSSNRPS"))
  ) %>%
  select(ID, WTSSNRPS, INCOME16, SOCBAR, ATTEND, SOCFREND, SOCREL, SOCOMMUN, TRUST, FAIR, HELPFUL) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(WTSSNRPS) & WTSSNRPS > 0)

saveRDS(gss_subset, OUTPUT_PATH)