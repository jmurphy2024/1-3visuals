## WD location: 1-3 Visualization Analysis/Phase 3/Shared WD
## Script: GSS_worklife_acquire.R
## Purpose: Ingest raw GSS .sav and normalize indicators for Work-Life Balance.
## Author: Janica Murphy, Gemini / User
## Date Created: 2026-01-27
## Dependencies: haven, dplyr, here

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(haven); library(dplyr); library(here) 

# ==== 1. PARAMETERS ====
# Ensure here::here() is used for all file paths
USER_GSS_SAV_FILE_PATH <- here::here("02_Scripts", "III- Templates", "GSS", "GSS2024.sav")
OUTPUT_PATH <- here::here("01_Data", "Standardized", "GSS_Microdata", "gss_2024_cross", "gss_wlb_raw.rds")
# ==== 2. DATA INGESTION & VARIABLE MAPPING ====
gss_full <- haven::read_sav(USER_GSS_SAV_FILE_PATH)

gss_subset <- gss_full %>%
  rename(
    ID       = any_of(c("id", "ID", "id_", "ID_", "CASEID", "caseid")),
    YEAR     = any_of(c("year", "YEAR", "gssyear", "GSSYEAR")),
    # Opportunity Pillar
    HRS1     = any_of(c("hrs1", "HRS1")),         
    WRKSCHED = any_of(c("wrksched", "WRKSCHED", "WRKSCHDL")), 
    # Resources Pillar
    REALINC  = any_of(c("realinc", "REALINC")),   
    MARITAL  = any_of(c("marital", "MARITAL")),   
    # Outcomes Pillar
    SATCITY  = any_of(c("satcity", "SATCITY")), # Ensure this is mapped!
    HAPPY    = any_of(c("happy", "HAPPY")),       
    # Anchor & Weight
    INCOME16 = any_of(c("income16", "INCOME16")),
    WTSSNRPS = any_of(c("wtssnrps", "WTSSNRPS"))
  ) %>%
  select(ID, YEAR, WTSSNRPS, INCOME16, 
         any_of(c("HRS1", "WRKSCHED", "REALINC", "MARITAL", "SATCITY", "HAPPY"))) %>%
  # FORCE-CREATE placeholders for missing metrics
  mutate(
    WRKSCHED = if (!"WRKSCHED" %in% names(.)) as.numeric(NA) else WRKSCHED,
    SATCITY  = if (!"SATCITY" %in% names(.)) as.numeric(NA) else SATCITY
  ) %>%
  mutate(across(everything(), as.numeric)) %>%
  filter(!is.na(WTSSNRPS) & WTSSNRPS > 0)

# ==== 3. SAVE ====
saveRDS(gss_subset, OUTPUT_PATH)
message("SUCCESS: WLB Raw subset saved. n=", nrow(gss_subset), " observations.")