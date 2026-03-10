# ==============================================================================
# SCRIPT: NCVS_Incidence_Skyline_2.R
# Purpose: Generate 3-Country Skyline for Crime INCIDENCE (Total crimes per 100 pop)
# Logic: Uses sum() to count all incidents per person + Dynamic V2 Borders
# ==============================================================================
rm(list = ls()); gc()
library(dplyr); library(here); library(stringr); library(tidyr); library(ggplot2)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)
set.seed(123) 

extract_code <- function(x) { as.numeric(str_extract(as.character(x), "\\d+")) }

# 2. DATA ACQUISITION (Corrected Safe Load)
HH_FILE   <- here::here("01_data", "raw", "NCVS", "ncvs_household_2023.rda")
PER_FILE  <- here::here("01_data", "raw", "NCVS", "ncvs_person_2023.rda")
INC_FILE  <- here::here("01_data", "raw", "NCVS", "ncvs_extract_2023.rda")

if (!file.exists(HH_FILE) | !file.exists(PER_FILE) | !file.exists(INC_FILE)) {
  stop("NCVS .rda files not found. Please verify the exact filenames.")
}

message("--- Loading NCVS RDA Files ---")
hh_obj_name  <- load(HH_FILE)
per_obj_name <- load(PER_FILE)
inc_obj_name <- load(INC_FILE)

ds2_raw <- get(hh_obj_name[1])
ds3_raw <- get(per_obj_name[1])
ds5_raw <- get(inc_obj_name[1])

rm(list = c(hh_obj_name, per_obj_name, inc_obj_name))
gc()

# 3. SPATIAL & TEMPORAL CONFIGURATION
region_rpp_lookup <- tibble(REGION_ID = c(1, 2, 3, 4), REG_RPP = c(105.2, 92.8, 95.4, 104.1)) 
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023) 

# 4. DEDUPLICATION
ds2_unique <- ds2_raw %>% group_by(IDHH) %>% slice(1) %>% ungroup() 
ds3_unique <- ds3_raw %>% group_by(IDHH, IDPER) %>% slice(1) %>% ungroup() 

# 5. THE UNIFIED PIPELINE (INCIDENCE LOGIC)
prepared_data <- ds3_unique %>%
  left_join(ds2_unique %>% select(IDHH, V2026, V2127B), by = "IDHH") %>% 
  left_join(ds5_raw %>% select(IDHH, IDPER, V4529), by = c("IDHH", "IDPER"), relationship = "one-to-many") %>% 
  mutate(
    income_code   = extract_code(V2026), 
    PERWT         = as.numeric(as.character(WGTPERCY)), 
    crime_code    = extract_code(V4529), 
    MAPPED_REGION = as.numeric(V2127B), 
    Violent_Crime = if_else(!is.na(crime_code) & crime_code >= 1 & crime_code <= 20, 1, 0), 
    Property_Crime= if_else(!is.na(crime_code) & crime_code >= 31 & crime_code <= 59, 1, 0) 
  ) %>%
  group_by(IDHH, IDPER) %>%
  summarise(
    PERWT         = first(PERWT), 
    income_code   = first(income_code), 
    MAPPED_REGION = first(MAPPED_REGION), 
    
    # INCIDENCE: Uses sum() to count ALL incidents against this person
    Violent_Crime = sum(Violent_Crime, na.rm = TRUE), 
    Property_Crime= sum(Property_Crime, na.rm = TRUE), 
    .groups       = "drop" 
  ) %>%
  filter(!is.na(income_code) & income_code < 99 & PERWT > 0) %>% 
  mutate(
    raw_dollars = case_when(
      income_code == 1 ~ runif(n(),0,4999),       income_code == 2 ~ runif(n(),5000,9999),
      income_code == 3 ~ runif(n(),10000,14999),  income_code == 4 ~ runif(n(),15000,24999),
      income_code == 5 ~ runif(n(),25000,34999),  income_code == 6 ~ runif(n(),35000,49999),
      income_code == 7 ~ runif(n(),50000,74999),  income_code == 17 ~ runif(n(),75000,250000), 
      TRUE             ~ runif(n(),35000,49999) 
    )
  ) %>%
  left_join(region_rpp_lookup, by = c("MAPPED_REGION" = "REGION_ID")) %>% 
  mutate(
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)), 
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third", 
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third", 
      TRUE ~ "Top Third" 
    )
  ) %>%
  filter(!is.na(REAL_INCOME), !is.na(Country)) 

# 6. VISUALIZATION (With Incidence Caption)
# ------------------------------------------------------------------------------
plot_data <- prepared_data %>% rename(`Property Crime` = Property_Crime, `Violent Crime` = Violent_Crime)

# Multi-line note formatting
incidence_note <- paste0(
  "Incidence Rate: Measures the total volume of crimes committed per 100 persons.\n",
  "1. Methodology: Sums all reported incidents to capture the full burden of crime.\n",
  "2. Inclusion: Explicitly accounts for 'repeat victimization' (multiple crimes per person).\n",
  "3. Reading: A 10% rate indicates 10 total crimes occurred for every 100 people."
)

p <- plot_economic_skyline_2(
  data           = plot_data, 
  indicator_vars = c("Property Crime", "Violent Crime"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Incidents per 100 Persons", 
  plot_title     = "NCVS_Incidence_Skyline_2",
  caption_text   = incidence_note
)

print(p)