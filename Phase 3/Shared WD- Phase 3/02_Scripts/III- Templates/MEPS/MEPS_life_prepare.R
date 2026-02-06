# ==== 0. ABOUT ====
## WD location: 02_Scripts/II- Prepare/MEPS
## Script: meps_life_expectancy_prepare.R
## Purpose: Synchronized relational join fixing 'ADDEV' and 'MHLTHRD' errors.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr); library(ipumsr)

# ==== 1. LOAD AND SPLIT ====
raw_data <- readRDS(here::here("01_data", "raw", "IPUMS_MEPS", "meps_life_expectancy", "raw_meps_life_expectancy_2022.rds"))
raw_df   <- if (is.list(raw_data) && !is.data.frame(raw_data)) raw_data[[1]] else raw_data

# Use the weight-based split confirmed by your screenshot
ds_person <- raw_df %>% filter(!is.na(PERWEIGHT))
ds_event  <- raw_df %>% filter(is.na(PERWEIGHT))

# ==== 2. EVENT SUMMARIZATION ====
mh_event_summary <- ds_event %>%
  filter(!is.na(MEPSID)) %>%
  group_by(MEPSID) %>%
  summarise(
    # Corrected to MHLTHRD to match Acquire script
    ind_mh_event = if_else(any(!is.na(MHLTHRD)), 1, 0),
    .groups = "drop"
  )

# ==== 3. PILLAR CONSTRUCTION & JOIN ====
prepared_data <- ds_person %>%
  left_join(mh_event_summary, by = "MEPSID") %>%
  filter(PERWEIGHT > 0) %>%
  mutate(
    # Health Status Proxy
    ind_high_health_status = if_else(as.numeric(HEALTH) %in% c(1, 2), 1, 0),
    
    # Chronic Burden using ALL screenshot variables
    ind_chronic_risk = if_else(
      as.numeric(HYPERTENEV) == 2 | as.numeric(STROKEV)    == 2 | 
        as.numeric(CHEARTDIEV) == 2 | as.numeric(ANGIPECEV)  == 2 | 
        as.numeric(CANCEREV)   == 2 | as.numeric(ADDEV)      == 2 |
        as.numeric(ASTHMAEV)   == 2 | as.numeric(CHOLHIGHEV) == 2 |
        as.numeric(ARTHGLUPEV) == 2 | as.numeric(DIABETICEV) == 2, 1, 0),
    
    # Any limitation reported
    ind_functional_limit = if_else(as.numeric(ANYLMT) == 1, 1, 0),
    
    ind_mh_event = replace_na(ind_mh_event, 0),
    income_clean = if_else(as.numeric(INCWAGE) < 0, NA_real_, as.numeric(INCWAGE))
  ) %>%
  select(PERWEIGHT, income_clean, starts_with("ind_"))

# ==== 4. SAVE ====
saveRDS(prepared_data, here::here("01_data", "processed", "IPUMS_MEPS", "prepared_meps_life_expectancy_2022.rds"))
message("SUCCESS: All variables synchronized and prepared.")