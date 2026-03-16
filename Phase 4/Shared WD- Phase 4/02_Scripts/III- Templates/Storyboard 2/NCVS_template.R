# ==============================================================================
# SCRIPT: NCVS_Victimization_Skyline_Final.R
# Purpose: Generate 3-Country Skyline for Victimization Rates
# Logic: Dynamic RDA Load + 3-Way Join + Spatial RPP + Temporal Inflation
# ==============================================================================
rm(list = ls()); gc()
library(dplyr); library(here); library(stringr); library(tidyr); library(ggplot2)

# 1. SOURCE MASTER LOGIC
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))
set.seed(123) 

# Helper to extract numeric codes from labeled NCVS data
extract_code <- function(x) { as.numeric(str_extract(as.character(x), "\\d+")) }

# 2. DATA ACQUISITION (Dynamic Object Loading)
HH_FILE   <- here::here("01_data", "raw", "NCVS", "ncvs_household_2023.rda")
PER_FILE  <- here::here("01_data", "raw", "NCVS", "ncvs_person_2023.rda")
INC_FILE  <- here::here("01_data", "raw", "NCVS", "ncvs_extract_2023.rda")

if (!file.exists(HH_FILE) | !file.exists(PER_FILE) | !file.exists(INC_FILE)) {
  stop("NCVS .rda files not found. Please verify the exact filenames.")
}

message("--- Loading NCVS RDA Files ---")
# Capture the original ICPSR object names upon loading
hh_obj_name  <- load(HH_FILE)
per_obj_name <- load(PER_FILE)
inc_obj_name <- load(INC_FILE)

# Assign them to our standard variables using get()
ds2_raw <- get(hh_obj_name[1])
ds3_raw <- get(per_obj_name[1])
ds5_raw <- get(inc_obj_name[1])

# Free up RAM by removing the original ICPSR objects
rm(list = c(hh_obj_name, per_obj_name, inc_obj_name))
gc()

# 3. SPATIAL & TEMPORAL CONFIGURATION
region_rpp_lookup <- tibble(
  REGION_ID = c(1, 2, 3, 4),
  REG_RPP   = c(105.2, 92.8, 95.4, 104.1) # NE, MW, S, W
)

# Fetch the 2023 multiplier based on your II-D function
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

# 4. DEDUPLICATION
ds2_unique <- ds2_raw %>%
  group_by(IDHH) %>%
  slice(1) %>% 
  ungroup()

ds3_unique <- ds3_raw %>%
  group_by(IDHH, IDPER) %>%
  slice(1) %>% 
  ungroup()

# 5. THE UNIFIED PIPELINE: JOINING, RECODING, AND ADJUSTMENTS
prepared_data <- ds3_unique %>%
  # --- A. Relational Joins ---
  # Join Household Data (Income = V2026, Region = V2127B) to Persons
  left_join(ds2_unique %>% select(IDHH, V2026, V2127B), by = "IDHH") %>%
  
  # Join Incidents to Persons (One-to-Many)
  left_join(ds5_raw %>% select(IDHH, IDPER, V4529), 
            by = c("IDHH", "IDPER"),
            relationship = "one-to-many") %>%
  
  # --- B. Base Variables & Flags ---
  mutate(
    income_code   = extract_code(V2026),
    PERWT         = as.numeric(as.character(WGTPERCY)),
    crime_code    = extract_code(V4529),
    MAPPED_REGION = as.numeric(V2127B), # NCVS codes match 1=NE, 2=MW, 3=S, 4=W
    
    # Victimization Flags
    ind_violent   = if_else(!is.na(crime_code) & crime_code >= 1 & crime_code <= 20, 1, 0),
    ind_property  = if_else(!is.na(crime_code) & crime_code >= 31 & crime_code <= 59, 1, 0)
  ) %>%
  
  # --- C. Collapse to Person-Level ---
  group_by(IDHH, IDPER) %>%
  summarise(
    PERWT         = first(PERWT),
    income_code   = first(income_code),
    MAPPED_REGION = first(MAPPED_REGION),
    ind_violent   = max(ind_violent, na.rm = TRUE),
    ind_property  = max(ind_property, na.rm = TRUE),
    .groups       = "drop"
  ) %>%
  
  # --- D. Jittering & Adjustments ---
  # Filter missing income/weights before applying jitter logic
  filter(!is.na(income_code) & income_code < 99 & PERWT > 0) %>% 
  mutate(
    raw_dollars = case_when(
      income_code == 1  ~ runif(n(), 0, 4999), 
      income_code == 2  ~ runif(n(), 5000, 9999),
      income_code == 3  ~ runif(n(), 10000, 14999),
      income_code == 4  ~ runif(n(), 15000, 24999),
      income_code == 5  ~ runif(n(), 25000, 34999),
      income_code == 6  ~ runif(n(), 35000, 49999),
      income_code == 7  ~ runif(n(), 50000, 74999),
      income_code == 17 ~ runif(n(), 75000, 250000), 
      TRUE              ~ runif(n(), 35000, 49999) 
    )
  ) %>%
  
  # Attach Spatial RPP multiplier
  left_join(region_rpp_lookup, by = c("MAPPED_REGION" = "REGION_ID")) %>%
  
  mutate(
    # Apply Temporal (Inflation) and Spatial (RPP) math
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)),
    
    # Map to the Three Countries
    Country = case_when(
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  
  # --- E. Final Cleanup ---
  filter(!is.na(REAL_INCOME), !is.na(Country))

# 6. VISUALIZATION
# Plotting Violent Crime Rate. Change indicator_var to "ind_property" for property crime.
plot_economic_skyline(
  data = prepared_data, 
  indicator_var = "ind_violent", 
  weight_var = "PERWT", 
  y_axis_label = "Violent Victimization Rate (%)",
  plot_title = "NCVS_2023_Violent_Crime_Adjusted"
)