# ==============================================================================
# SCRIPT: SCF_net_assets_resilience.R
# Purpose: Generate 3-Country Skyline for Net Assets & Wealth Resilience
# Logic:   Fed Survey of Consumer Finances (SCF) -> Terciles -> Wealth Pillars
# Engine:  dplyr, haven, data.table, plot_economic_skyline_2
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit for heavy ggplot rendering
options(expressions = 500000)

library(dplyr); library(here); library(scales); library(data.table); 
library(stringr); library(tidyr); library(ggplot2); library(httr); library(haven)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & CACHE SETUP (2022 Federal Reserve SCF)
# ------------------------------------------------------------------------------
TARGET_DIR  <- here::here("01_data", "raw", "Federal_Reserve_SCF")
TARGET_FILE <- file.path(TARGET_DIR, "scf_summary_2022.rds")

# The Federal Reserve provides a public summary zip of all macro wealth variables
SCF_URL <- "https://www.federalreserve.gov/econres/files/scfp2022s.zip"

# 3. DATA ACQUISITION
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Downloading Federal Reserve Survey of Consumer Finances (2022) ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  temp_zip <- tempfile(fileext = ".zip")
  
  # Download the zip file
  res <- tryCatch({
    GET(SCF_URL, write_disk(temp_zip, overwrite = TRUE), timeout(120))
  }, error = function(e) {
    stop(paste("Failed to download SCF data:", e$message))
  })
  
  # Unzip and identify the specific file inside
  extracted_file <- unzip(temp_zip, list = TRUE)$Name[1]
  unzip(temp_zip, files = extracted_file, exdir = TARGET_DIR)
  
  full_path <- file.path(TARGET_DIR, extracted_file)
  
  # Dynamically read the file based on the format the Fed provided
  if (grepl("\\.dta$", extracted_file, ignore.case = TRUE)) {
    message(paste("-> Detected Stata format:", extracted_file, "- Reading via haven..."))
    raw_data <- haven::read_dta(full_path)
  } else if (grepl("\\.csv$", extracted_file, ignore.case = TRUE)) {
    message(paste("-> Detected CSV format:", extracted_file, "- Reading via data.table..."))
    raw_data <- data.table::fread(full_path)
  } else {
    stop("Unknown file format extracted from ZIP.")
  }
  
  saveRDS(raw_data, TARGET_FILE)
  
  # Clean up temp files
  unlink(temp_zip)
  unlink(full_path)
  message("--- SCF Acquisition Complete and Cached ---")
  
} else {
  message("--- Loading cached SCF data from local disk ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 4. NORMALIZATION & PILLAR LOGIC (PERSON-LEVEL)
# ------------------------------------------------------------------------------
message("Calculating Wealth and Asset Thresholds at the Person Level...")

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2022, base_year = 2023)

prepared_data <- as_tibble(raw_data) %>%
  rename_with(toupper, everything()) %>%  
  mutate(
    # --- CONVERT TO PERSON-LEVEL WEIGHTS ---
    # 1. Calculate the number of adults (MARRIED=1 means 2 adults, otherwise 1)
    Adults = if_else(MARRIED == 1, 2, 1),
    
    # 2. Total Family Size = Adults + Children
    PEU_SIZE = Adults + KIDS,
    
    # 3. Multiply the Household Weight by the Family Size to get Total People
    PERWT = WGT * PEU_SIZE,
    
    # Adjust Income to 2023 dollars
    REAL_INCOME = INCOME * INFLATION_ADJ,
    
    # 1. Positive Net Worth (Total Assets > Total Debt)
    `Positive Net Worth` = if_else(NETWORTH > 0, 1, 0),
    
    # 2. Liquid Savings Buffer (Checking, Savings, Money Market > $5,000)
    `Liquid Savings (>$5k)` = if_else((LIQ * INFLATION_ADJ) >= 5000, 1, 0),
    
    # 3. Homeownership (Has >$0 value in primary residence)
    `Homeowner` = if_else(HOUSES > 0, 1, 0),
    
    # Composite: Has all three foundations of baseline wealth
    Wealth_Resilience_Index = if_else(`Positive Net Worth` == 1 & 
                                        `Liquid Savings (>$5k)` == 1 & 
                                        `Homeowner` == 1, 1, 0),
    
    # Categorize into the Three Countries (SCF lacks state RPPs, so we use national cutoffs)
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(PERWT > 0, !is.na(Country), !is.na(REAL_INCOME))

# 5. SUMMARY STATISTICS (PERSON-LEVEL: TERCILES & QUARTILES)
# ------------------------------------------------------------------------------
if(!require(Hmisc)) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

message("\n=== NET ASSETS & WEALTH RESILIENCE SUMMARY (PERSON LEVEL) ===")

# Step 1: Calculate the overall stats for each of the Three Countries
overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Population = sum(PERWT, na.rm = TRUE),
    
    `Wealth Resilience Index (%)` = round((sum(Wealth_Resilience_Index * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Median Net Worth ($)` = median(rep(NETWORTH * INFLATION_ADJ, times = pmax(1, round(PERWT / 100))), na.rm = TRUE),
    `Positive Net Worth (%)` = round((sum(`Positive Net Worth` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Liquid Savings >$5k (%)` = round((sum(`Liquid Savings (>$5k)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Homeowner (%)`          = round((sum(Homeowner * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  )

# Step 2: Dynamically calculate weighted income quartiles WITHIN each Tercile
prepared_data_q <- prepared_data %>%
  group_by(Country) %>%
  mutate(
    q25 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.25, na.rm = TRUE),
    q50 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.50, na.rm = TRUE),
    q75 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.75, na.rm = TRUE),
    Quartile = case_when(
      REAL_INCOME <= q25 ~ "Q1 (Bottom 25%)",
      REAL_INCOME > q25 & REAL_INCOME <= q50 ~ "Q2",
      REAL_INCOME > q50 & REAL_INCOME <= q75 ~ "Q3",
      TRUE ~ "Q4 (Top 25%)"
    )
  ) %>%
  ungroup()

# Step 3: Calculate the exact same stats for those inner Quartiles
quartile_stats <- prepared_data_q %>%
  group_by(Country, Quartile) %>%
  summarise(
    Subgroup = first(Quartile),
    Total_Population = sum(PERWT, na.rm = TRUE),
    
    `Wealth Resilience Index (%)` = round((sum(Wealth_Resilience_Index * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Median Net Worth ($)` = median(rep(NETWORTH * INFLATION_ADJ, times = pmax(1, round(PERWT / 100))), na.rm = TRUE),
    `Positive Net Worth (%)` = round((sum(`Positive Net Worth` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Liquid Savings >$5k (%)` = round((sum(`Liquid Savings (>$5k)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Homeowner (%)`          = round((sum(Homeowner * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  ) %>%
  select(-Quartile)

# Step 4: Bind everything together, format, and print the master table
macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(
    Total_Population = scales::comma(Total_Population),
    `Median Net Worth ($)` = scales::dollar(round(`Median Net Worth ($)`, 0))
  )

print(as.data.frame(macro_summary))

# 6. VISUALIZATIONS
# ------------------------------------------------------------------------------
message("\nGenerating Skyline Plots...")
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

# --- PILLAR PLOT ---
plot_caption <- paste0(
  "The Architecture of Wealth Resilience (Federal Reserve SCF 2022, Person-Level):\n",
  "'Positive Net Worth' indicates individuals living in households where total assets exceed total debts. ",
  "'Liquid Savings (>$5k)' indicates living in a household with at least $5,000 in highly liquid accounts (checking, savings). ",
  "'Homeowner' represents living in an owned primary residence. Dollar amounts adjusted to 2023 baseline."
)

plot_data <- prepared_data %>%
  rename(`Homeowner\n\n` = Homeowner)

p_pillars <- plot_economic_skyline_2(
  data           = plot_data, 
  indicator_vars = c("Positive Net Worth", "Homeowner\n\n", "Liquid Savings (>$5k)"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Population Percentage (%)",
  plot_title     = "SCF_Net_Assets_Resilience_Pillars",
  caption_text   = stringr::str_wrap(plot_caption, width = 130)
)
print(p_pillars)
ggsave(here::here("03_output", "visualizations_final", "SCF_Net_Assets_Resilience_Pillars.png"), p_pillars, width = 12, height = 7, dpi = 300)

# --- COMPOSITE INDEX PLOT ---
composite_caption <- paste0(
  "Note: The Wealth Resilience Index represents the percentage of the population that is buffered by all three ",
  "foundational pillars simultaneously: Positive Household Net Worth, >$5,000 in liquid savings, and Homeownership."
)

p_index <- plot_economic_skyline(
  data          = prepared_data, 
  indicator_var = "Wealth_Resilience_Index", 
  weight_var    = "PERWT", 
  y_axis_label  = "Wealth Resilience Index (%)",
  plot_title    = "SCF_Net_Assets_Resilience_Composite",
  caption_text  = stringr::str_wrap(composite_caption, width = 130)
)
print(p_index)
ggsave(here::here("03_output", "visualizations_final", "SCF_Net_Assets_Resilience_Composite.png"), p_index, width = 12, height = 7, dpi = 300)

message("Processing & Visualizations Complete!")