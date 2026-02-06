# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr)

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #
USER_IPUMS_SAMPLE_ID  <- "ih2014"  # Correct sample with Mortality variables
USER_INDICATOR_NAME   <- "Life_Expectancy"

USER_HHID_VAR          <- "SERIAL"
USER_PERSON_WEIGHT_VAR <- "MORTWT" # Crucial: Use Mortality Weight
USER_INCOME_VAR        <- "INCFAM07ON"

# ================================================================= #
# ==== 2. PREPARATION LOGIC ====
# ================================================================= #

RAW_DATA_DIR <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("nhis_", USER_IPUMS_SAMPLE_ID))
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)

raw_data <- readRDS(file.path(RAW_DATA_DIR, "raw_data.rds"))

# --- 2.3. CLEAN AND CALCULATE SURVIVAL TIME ---
message("Calculating survival times...")

# FIX: Find the latest VALID death year (filtering out 9999/NIU)
valid_death_years <- as.numeric(raw_data$MORTDODY)
# We assume valid years are < 2030. 9999 is the code for Missing/NIU.
REAL_CENSOR_YEAR <- max(valid_death_years[valid_death_years < 2030], na.rm = TRUE)

message(paste("Censorship Cutoff CORRECTED to:", REAL_CENSOR_YEAR))

cleaned_data <- raw_data %>%
  # 1. Filter: Must be eligible for linkage
  filter(MORTELIG == 1) %>%
  
  mutate(
    # Clean Income: Codes 96-99 are unknown/refused
    # INCFAM07ON is categorical (10, 20..), but rank-order is preserved in numeric conversion.
    # We map this to 'HHINCOME' so generic scripts can understand it.
    HHINCOME = if_else(as.numeric(INCFAM07ON) >= 96, NA_real_, as.numeric(INCFAM07ON)),
    
    # Clean Weights
    MORTWT = as.numeric(MORTWT),
    
    # 2. Determine End Year
    # If Dead (MORTSTAT=1) AND valid year (<2030), use Death Year.
    # Otherwise, use the REAL_CENSOR_YEAR (e.g., 2019).
    end_year = if_else(MORTSTAT == 1 & as.numeric(MORTDODY) < 2030, 
                       as.numeric(MORTDODY), 
                       REAL_CENSOR_YEAR),
    
    # 3. Calculate Age at Event
    # Age at Survey + (End Year - Survey Year)
    age_at_event = as.numeric(AGE) + (end_year - as.numeric(YEAR)),
    
    # 4. Status Flag (1 = Event/Death, 0 = Censored/Alive)
    status_flag = if_else(MORTSTAT == 1 & as.numeric(MORTDODY) < 2030, 1, 0),
    
    # 5. Entry Age (For Left Truncation)
    age_entry = as.numeric(AGE)
  )

# --- 2.5. SAVE ---
essential_cols <- c(
  "SERIAL", "MORTWT", "HHINCOME",
  "age_entry", "age_at_event", "status_flag"
)

final_prepared_data <- cleaned_data %>% select(any_of(essential_cols))
saveRDS(final_prepared_data, file = file.path(PROCESSED_DIR, paste0("prepared_NHIS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds")))
message("Data preparation complete.")