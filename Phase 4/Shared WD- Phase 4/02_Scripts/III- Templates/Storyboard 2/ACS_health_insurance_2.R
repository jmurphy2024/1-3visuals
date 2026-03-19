# ==============================================================================
# SCRIPT: ACS_health_insurance_private_vs_public.R
# Purpose: Generate 3-Country Skyline for Private vs Public Health Coverage
# Logic:   ACS Microdata + Macro Insurance Types + Working-Age (18-64)
# Engine:  data.table for high-speed 15M+ row aggregation
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tibble)
library(data.table); library(scales); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT (2023 ACS)
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 

# Core variables for Demographics, Income, and Macro-Level Insurance
VARS_NEEDED <- c(
  "SERIAL", "PERWT", "INCTOT", "STATEFIP", "AGE", 
  "HCOVANY",  # 1 = Uninsured, 2 = Insured
  "HCOVPUB",  # 1 = No Public, 2 = Has Public Coverage
  "HCOVPRIV"  # 1 = No Private, 2 = Has Private Coverage
)

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_health_macro")
TARGET_FILE <- file.path(TARGET_DIR, "acs_health_macro_raw.rds")

# Optional: Uncomment the line below if you ever need to force a fresh API pull
# unlink(TARGET_DIR, recursive = TRUE)

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", samples = USER_SAMPLE, variables = VARS_NEEDED,
    description = "Three Countries ACS Private vs Public Coverage (Working-Age)"
  )
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  raw_data <- readRDS(TARGET_FILE)
}

# 3. CONSTRUCT TRUE HOUSEHOLD INCOME
# ------------------------------------------------------------------------------
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

# ACS uses 9999999 for Missing/NIU INCTOT 
hh_aggregated <- as_tibble(dt_raw[INCTOT != 9999999, .(AGGREGATED_HHINCOME = sum(INCTOT, na.rm = TRUE)), by = SERIAL])
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

# 4. NORMALIZATION & PILLAR LOGIC
# ------------------------------------------------------------------------------
prepared_data <- as_tibble(dt_raw) %>%
  left_join(hh_aggregated, by = "SERIAL") %>%
  mutate(
    PERWT = as.numeric(PERWT),
    
    # --- HEALTHCARE PILLAR LOGIC (IPUMS standard: 1 = No, 2 = Yes) ---
    
    # Tier 1: Any Private Insurance (Employer, ACA, Union, Direct Purchase)
    `Private Coverage` = if_else(as.numeric(HCOVPRIV) == 2, 1, 0),
    
    # Tier 2: Any Public Insurance (Medicaid, Medicare, VA, State-specific plans)
    `Public Coverage` = if_else(as.numeric(HCOVPUB) == 2, 1, 0),
    
    # Tier 3: The Coverage Gap
    `Uninsured` = if_else(as.numeric(HCOVANY) == 1, 1, 0),
    
    # Composite Target: The Uninsured Rate
    Health_Uninsured_Composite = `Uninsured`,
    
    # --- ONE-STEP REAL INCOME NORMALIZATION ---
    REAL_INCOME = AGGREGATED_HHINCOME * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # UNIVERSE FILTER: Working-age adults (18-64)
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), 
         as.numeric(AGE) >= 18, as.numeric(AGE) <= 64)

# 5. SUMMARY STATISTICS
# ------------------------------------------------------------------------------
message("\n=== HEALTHCARE ACCESS SUMMARY (PRIVATE VS PUBLIC) ===")
print(as.data.frame(get_country_summary(prepared_data, "Health_Uninsured_Composite", "PERWT")))

# 6. VISUALIZATION
# ------------------------------------------------------------------------------
plot_caption <- paste0(
  "The Architecture of Healthcare Access (Working-Age Adults Only, 18-64):\n",
  "Comparing macro-level reliance on Private vs. Public health insurance.\n  ",
  "'Private Coverage' includes employer-sponsored and direct-purchase plans.\n ",
  "'Public Coverage' includes Medicaid, Medicare, and VA/Military care.\n ",
  "Note: Categories are not mutually exclusive, as some individuals hold dual coverage."
)

# THE REVISED FIX: 
# Add newlines to the END of Public Coverage to push it UP.
# Leave Uninsured alone so it rests naturally on the line without hitting the axis.
plot_data <- prepared_data %>%
  rename(`Public Coverage\n\n` = `Public Coverage`)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data, 
  indicator_vars = c("Private Coverage", "Public Coverage\n\n", "Uninsured"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Percentage (%)",
  plot_title     = "ACS_health_private_vs_public_pillars",
  caption_text   = stringr::str_wrap(plot_caption, width = 130)
)
print(p_pillars)

composite_caption <- paste0(
  "Note: The Healthcare Gap Index represents the baseline percentage of working-age adults (18-64) ",
  "who lack any form of recognized health insurance coverage."
)

p_index <- plot_economic_skyline(
  data          = prepared_data, 
  indicator_var = "Health_Uninsured_Composite", 
  weight_var    = "PERWT", 
  y_axis_label  = "Uninsured Rate (%)",
  plot_title    = "ACS_health_uninsured_working_age_composite",
  caption_text  = stringr::str_wrap(composite_caption, width = 130)
)
print(p_index)