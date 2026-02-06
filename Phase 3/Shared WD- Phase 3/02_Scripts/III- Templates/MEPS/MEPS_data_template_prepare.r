## WD location: 02_Scripts/II- Prepare/MEPS
## Script: MEPS_Infrastructure_prepare.R
## Purpose: Categorizes catalysts into 5 Pillars using COVERTYPE and VISITCTGRY.
## Author: Janica Murphy, Maxwell Goshert, Gemini Thought Partner

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(here); library(tidyr)

# --- 1. LOAD AND BRIDGE IDENTIFIERS ---
file_path <- here::here("01_data", "raw", "IPUMS_MEPS", 
                        "meps_Medical_Infrastructure_and_Life_Outcomes", 
                        "raw_meps_hierarchical_2023.rds")

raw_list <- readRDS(file_path)

# Separate records
person_data <- raw_list[[which(sapply(raw_list, function(x) any(as.character(x$RECTYPE) %in% c("100", "P"))))]]
event_data  <- raw_list[[which(sapply(raw_list, function(x) any(as.character(x$RECTYPE) %in% c("200", "E"))))]]

# ROBUST ID BRIDGE: Finds MEPSID or reconstructs it
bridge_meps_id <- function(df) {
  # 1. Check for standard IPUMS name
  if ("MEPSID" %in% names(df)) return(df)
  
  # 2. Check for common typos/alternatives
  if ("MEPSIDE" %in% names(df)) return(df %>% rename(MEPSID = MEPSIDE))
  if ("DUPERSID" %in% names(df)) return(df %>% rename(MEPSID = DUPERSID))
  
  # 3. Reconstruct if DUID and PID are present
  if (all(c("DUID", "PID") %in% names(df))) {
    message("Constructing MEPSID from DUID and PID...")
    # For 2018+ MEPSID is concatenating PANEL + DUID + PID
    # If PANEL isn't there, we use what's available to create a unique key
    df <- df %>% mutate(MEPSID = paste0(as.character(DUID), as.character(PID)))
    return(df)
  }
  
  stop("CRITICAL: No unique identifier (MEPSID, DUPERSID, or DUID/PID) found in the data.")
}

person_data <- bridge_meps_id(person_data)
event_data  <- bridge_meps_id(event_data)
# --- 2. CATEGORIZE CLINICAL CATALYSTS (PERSON-LEVEL) ---
# Transitioning from event counts to binary presence markers
person_event_summary <- event_data %>%
  group_by(MEPSID) %>%
  summarise(
    # Primary Care Visit: 01 (General checkup) or 09 (Well child exam)
    has_primary = any(as.numeric(VISITCTGRY) %in% c(1, 9), na.rm = TRUE),
    # ER Visit: 03 (Emergency medical events)
    has_er      = any(as.numeric(VISITCTGRY) == 3, na.rm = TRUE),
    # Specialized Care: Codes 02, 04, 05, 06, 07, 08, 10
    has_spec    = any(as.numeric(VISITCTGRY) %in% c(2, 4, 5, 6, 7, 8, 10), na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. POPULATION SHIFT & PILLAR CONSTRUCTION ---
# Establishing the community-based National Container
prepared_data <- person_data %>%
  filter(PERWEIGHT > 0) %>% 
  left_join(person_event_summary, by = "MEPSID") %>%
  mutate(
    # Fill non-utilizers with 0 to ensure correct national prevalence
    across(starts_with("has_"), ~replace_na(., 0)),
    
    # Mutually Exclusive Insurance Pillars from COVERTYPE
    ind_private_ins = if_else(as.numeric(COVERTYPE) == 1, 1, 0),
    ind_public_ins  = if_else(as.numeric(COVERTYPE) == 2, 1, 0),
    
    # Clinical Catalyst Pillars
    ind_primary_visit     = as.numeric(has_primary),
    ind_acute_visit       = as.numeric(has_er),
    ind_specialized_visit = as.numeric(has_spec),
    
    # Economic Geography Anchor
    income_clean = if_else(as.numeric(INCWAGE) < 0, NA_real_, as.numeric(INCWAGE))
  ) %>%
  select(MEPSID, PERWEIGHT, income_clean, starts_with("ind_"), STRATANN, PSUANN)

# --- 4. SAVE ---
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_MEPS")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)
saveRDS(prepared_data, file.path(PROCESSED_DIR, "prepared_meps_infrastructure_2023.rds"))

message("SUCCESS: MEPS Pillars prepared with five-variable categorization.")