# ==============================================================================
# SCRIPT: NORC_GSS_Criminalization_Cumulative.R
# Purpose: Generate 3-Country Skyline for Criminalization (Arrest History) Rates
# Logic: Dynamic Year Selection + Strict SPSS Label Handling + CONINC
# ==============================================================================
rm(list = ls()); gc()
library(haven); library(dplyr); library(here); library(ggplot2)

# 1. SOURCE MASTER LOGIC
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R")) 
set.seed(123) 

# 2. DATA ACQUISITION
TARGET_FILE <- here::here("01_data", "raw", "GSShistorical.sav")
message("--- Loading GSS Cumulative Data ---")
raw_data <- read_sav(TARGET_FILE)

# 3. SPATIAL CONFIGURATION
region_rpp_lookup <- tibble(
  REGION_ID = c(1, 2, 3, 4),
  REG_RPP   = c(105.2, 92.8, 95.4, 104.1) 
)

# 4. DYNAMIC RECODING & FILTERING
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  mutate(
    # Handle SPSS labels: Only 1 (Yes) and 2 (No) are valid. 
    # Everything else (0=IAP, 8=DK, 9=NA) becomes NA.
    arrest_num = as.numeric(ARREST),
    target_indicator = case_when(
      arrest_num == 1 ~ 1,
      arrest_num == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # Cumulative file has CONINC (Constant Income). No jittering needed!
    income_num = as.numeric(CONINC),
    
    # Bulletproof Weight: Scans for whichever weight variable is populated
    PERWT = coalesce(as.numeric(WTSSALL), as.numeric(WTSSNRPS), as.numeric(WTSSCOMP), 1),
    
    # RPP CROSSWALK: Collapse GSS 9 Regions to 4 Regions
    MAPPED_REGION = case_when(
      as.numeric(REGION) %in% c(1, 2) ~ 1,
      as.numeric(REGION) %in% c(3, 4) ~ 2,
      as.numeric(REGION) %in% c(5, 6, 7) ~ 3,
      as.numeric(REGION) %in% c(8, 9) ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  # Filter to ONLY respondents with valid income, valid weights, and a Yes/No to the arrest question
  filter(!is.na(income_num), income_num > 0, !is.na(target_indicator), PERWT > 0)

# --- DYNAMIC YEAR SELECTION ---
# Find the most recent year in this cleaned dataset
LATEST_YEAR <- max(prepared_data$YEAR, na.rm = TRUE)
message(">> Dynamically selected year: ", LATEST_YEAR)

# Calculate inflation based on the dynamically selected year
INFLATION_ADJ <- get_inflation_multiplier(data_year = LATEST_YEAR, base_year = 2023)

# 5. FINAL SPATIAL & TEMPORAL ADJUSTMENT
final_data <- prepared_data %>%
  filter(YEAR == LATEST_YEAR) %>% # Lock the data down to that specific year
  left_join(region_rpp_lookup, by = c("MAPPED_REGION" = "REGION_ID")) %>%
  mutate(
    # Apply Inflation adjustment FIRST, then Spatial RPP adjustment
    REAL_INCOME = (income_num * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)),
    
    # Assign to the Three Countries
    Country = case_when(
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(REAL_INCOME), !is.na(Country))

# 6. VISUALIZATION
plot_economic_skyline(
  data = final_data, 
  indicator_var = "target_indicator", 
  weight_var = "PERWT", 
  y_axis_label = "Lifetime Arrest / Police Contact Rate (%)",
  plot_title = paste0("GSS_", LATEST_YEAR, "_Criminalization_Skyline")
)