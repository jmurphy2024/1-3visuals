# ==============================================================================
# SCRIPT: II-C_Person_Weighted_Full_Borders_AutoDownload_V2.R
# Purpose: Single-click solution. Downloads data AND calculates aggregated borders.
# Logic:   1. NATIVE AGGREGATION: Sums INCTOT by SERIAL to bypass Census HHINCOME.
#          2. Zero-Floor Incomes (Negatives -> 0).
#          3. Population Scaling (Target 342M).
#          4. Generates Version 2 Map Cutoffs (RDS) and Chart Borders (CSV)
#             without overwriting original legacy files.
# ==============================================================================

rm(list = ls()); gc() 
library(ipumsr); library(dplyr); library(readr); library(srvyr)
library(survey); library(rlang); library(tibble); library(here); library(scales)

# --- 1. CONFIGURATION ---
# Target Population for Re-weighting (114M * 3 = 342M)
TARGET_US_POPULATION <- 342000000 
ACS_SAMPLE_ID        <- "us2023c"  # 5-Year Sample

# Paths (UPDATED TO PRESERVE LEGACY FILES)
processed_data_dir <- here::here("01_data", "processed")
raw_data_path      <- file.path(processed_data_dir, "ipums_data_raw2.rds")
main_cutoffs_file  <- file.path(processed_data_dir, "main_tercile_cutoffs_person_inclusive2.rds")
borders_csv_file   <- file.path(processed_data_dir, "within_tercile_quantile_borders_person_inclusive2.csv")

# Ensure directories exist
if(!dir.exists(processed_data_dir)) dir.create(processed_data_dir, recursive = TRUE)


# ==============================================================================
# SECTION A: AUTO-DOWNLOAD LOGIC
# ==============================================================================

if (file.exists(raw_data_path)) {
  message("✓ Valid V2 raw data found. Loading from disk...")
  ipums_data <- readRDS(raw_data_path)
} else {
  message("⚠ V2 Raw data NOT found. Initiating download from IPUMS...")
  
  if (Sys.getenv("IPUMS_API_KEY") == "") {
    stop("Error: IPUMS_API_KEY not found. Please set your API key in .Renviron or use set_ipums_api_key().")
  }
  
  # 1. Define Extract (Pulls INCTOT and SERIAL for native aggregation)
  vars_needed <- c("SERIAL", "INCTOT", "HHWT", "PERWT", "STATEFIP", "PUMA", "ADJUST", "PERNUM")
  
  extract_def <- define_extract_micro(
    collection = "usa",
    description = "Auto-Download: Aggregated Person-Level Income Analysis (V2)",
    samples = ACS_SAMPLE_ID,
    variables = vars_needed
  )
  
  # 2. Submit & Wait
  message("Submitting extract request...")
  submitted <- submit_extract(extract_def)
  message(paste("Extract submitted. Number:", submitted$number))
  
  ready <- wait_for_extract(submitted)
  
  # 3. Download
  download_dir <- here::here("01_data", "raw")
  if(!dir.exists(download_dir)) dir.create(download_dir, recursive = TRUE)
  
  message("Downloading DDI and Data...")
  files <- download_extract(ready, download_dir = download_dir, overwrite = TRUE)
  
  # 4. Load & Save RDS
  ddi_file <- files[grep("\\.xml$", files)]
  data_raw <- read_ipums_micro(read_ipums_ddi(ddi_file), verbose = FALSE)
  
  saveRDS(data_raw, raw_data_path)
  message(paste("✓ Data saved to:", raw_data_path))
  
  ipums_data <- data_raw
}


# ==============================================================================
# SECTION B: AGGREGATION, PREPARATION & WEIGHT SCALING
# ==============================================================================
message("\n--- Processing Data (Aggregating Household Incomes) ---")

# RPP Lookup Table
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

# Step 1: Construct True Household Income from Individual INCTOT
hh_aggregated <- ipums_data %>%
  mutate(inctot_num = as.numeric(INCTOT)) %>%
  filter(inctot_num != 9999999) %>% # Remove N/A codes
  group_by(SERIAL) %>%
  summarise(AGGREGATED_HHINCOME = sum(inctot_num, na.rm = TRUE), .groups = "drop")

# Step 2: Merge, Clamp, and Apply Adjustments
data_intermediate <- ipums_data %>%
  left_join(hh_aggregated, by = "SERIAL") %>%
  mutate(
    income_raw = AGGREGATED_HHINCOME,
    
    # LOGIC: Clamp Negatives to 0 | Drop Missing
    income_clamped = case_when(
      is.na(income_raw) ~ NA_real_, 
      income_raw < 0 ~ 0,               
      TRUE ~ income_raw                 
    ),
    
    adj_factor = if_else(as.numeric(ADJUST) > 100, as.numeric(ADJUST) / 1000000, as.numeric(ADJUST)),
    STATEFIP = as.numeric(STATEFIP)
  ) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>% 
  mutate(REAL_INCOME = (income_clamped * adj_factor) * (100 / coalesce(STATE_RPP, 100))) %>%
  filter(!is.na(REAL_INCOME), !is.na(PERWT), PERWT > 0)

# Step 3: Population Scaling
current_total_pop <- sum(data_intermediate$PERWT)
pop_scalar        <- TARGET_US_POPULATION / current_total_pop

data_inclusive <- data_intermediate %>%
  mutate(FINAL_WEIGHT = PERWT * pop_scalar)

message(paste("Population Scaled to Target:", comma(TARGET_US_POPULATION)))
message(paste("Scalar Applied:", round(pop_scalar, 5)))


# ==============================================================================
# SECTION C: CALCULATE BORDERS
# ==============================================================================
message("\n--- Calculating Tercile Borders ---")

person_design <- survey::svydesign(ids = ~1, weights = ~FINAL_WEIGHT, data = data_inclusive)

main_q <- survey::svyquantile(
  ~REAL_INCOME, 
  design = person_design, 
  quantiles = c(1/3, 2/3), 
  na.rm = TRUE, 
  ci = FALSE
)

main_cutoffs <- list(
  main_cutoff1 = as.numeric(main_q$REAL_INCOME[1]), 
  main_cutoff2 = as.numeric(main_q$REAL_INCOME[2])
)

saveRDS(main_cutoffs, main_cutoffs_file)


# ==============================================================================
# SECTION D: SUMMARY TABLE
# ==============================================================================
message("\n--- Generating Summary Table ---")

summary_table <- data_inclusive %>%
  mutate(income_tercile = case_when(
    REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "T1: Bottom Third",
    REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "T2: Middle Third",
    TRUE ~ "T3: Top Third"
  )) %>%
  group_by(income_tercile) %>%
  summarise(
    Population   = sum(FINAL_WEIGHT),
    Min_Real     = min(REAL_INCOME, na.rm = TRUE),
    Max_Real     = max(REAL_INCOME, na.rm = TRUE),
    Min_Raw      = min(income_clamped, na.rm = TRUE),
    Max_Raw      = max(income_clamped, na.rm = TRUE)
  ) %>%
  mutate(
    `Pop %`      = percent(Population / sum(Population), accuracy = 0.1),
    Population   = comma(Population),
    `Real Range` = paste0(dollar(Min_Real), " - ", dollar(Max_Real)),
    `Raw Range`  = paste0(dollar(Min_Raw), " - ", dollar(Max_Raw))
  ) %>%
  select(income_tercile, Population, `Pop %`, `Real Range`, `Raw Range`)

print(as.data.frame(summary_table))


# ==============================================================================
# SECTION E: DETAILED VENTILES (CSV)
# ==============================================================================
message("\n--- Calculating 5% Ventiles (Please Wait) ---")

detailed_borders <- data_inclusive %>%
  mutate(income_tercile_group = case_when(
    REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Tercile 1 (Bottom)",
    REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Tercile 2 (Middle)",
    TRUE ~ "Tercile 3 (Top)"
  )) %>%
  group_by(income_tercile_group) %>%
  group_modify(~ {
    tdesign <- survey::svydesign(ids = ~1, weights = ~FINAL_WEIGHT, data = .x)
    q_probs <- seq(0.05, 0.95, by = 0.05) 
    
    # ci = FALSE prevents size errors
    q_vals_raw <- survey::svyquantile(~REAL_INCOME, tdesign, quantiles = q_probs, na.rm = TRUE, ci = FALSE)
    numeric_cutoffs <- as.numeric(q_vals_raw[[1]])
    
    tibble(QuantileGroup = "Groups_20", QuantileProbability = q_probs, CutoffValue = numeric_cutoffs)
  }) %>%
  ungroup()

write_csv(detailed_borders, borders_csv_file)

message("\n--- SUCCESS: All V2 Files Created ---")
message(paste("Cutoffs RDS:", main_cutoffs_file))
message(paste("Borders CSV:", borders_csv_file))