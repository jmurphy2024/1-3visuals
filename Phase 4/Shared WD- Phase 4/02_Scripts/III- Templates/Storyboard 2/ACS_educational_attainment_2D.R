# ==============================================================================
# SCRIPT: ACS_educational_attainment_2D.R
# Purpose: Generate 3-Country Skyline for Educational Attainment (BA+)
# Logic:   Native HHINCOME, Negative Households Dropped, Person-Weighted
# Engine:  Minimalist 2D (LOESS Smoothed Trend Lines)
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit for heavy ggplot rendering
options(expressions = 500000)

library(ipumsr)
library(dplyr)
library(here)
library(ggplot2)
library(tibble)
library(data.table)
library(scales)
library(tidyr)
library(gridExtra)
library(grid)
library(Hmisc)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
# Pointing to the new Skyline 2D engine for LOESS smoothing
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2D.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 
VARS_NEEDED <- c("SERIAL", "PERWT", "HHINCOME", "STATEFIP", "AGE", "EDUC")

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_educational_attainment")
TARGET_FILE <- file.path(TARGET_DIR, "acs_educational_attainment_native_hh.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "usa", 
    samples = USER_SAMPLE,
    variables = VARS_NEEDED,
    description = "Three Countries ACS Skyline - Educational Attainment (Native HHINCOME)"
  )
  
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  message("--- Loading existing IPUMS ACS API data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 3. DATA ENGINEERING & CLEANING
# ------------------------------------------------------------------------------
message("Cleaning data and dropping negative households...")
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

dt_raw[, HHINCOME_clean := as.numeric(HHINCOME)]
dt_raw[HHINCOME_clean == 9999999, HHINCOME_clean := NA_real_]
dt_filtered <- dt_raw[HHINCOME_clean >= 0]

# 4. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
message("Applying normalization and dynamic cutoffs...")

# Extract year from string (e.g., "us2023c" -> 2023)
INFLATION_ADJ <- get_inflation_multiplier(data_year = as.numeric(substring(USER_SAMPLE, 3, 6)), base_year = 2023)

prepared_data <- as_tibble(dt_filtered) %>%
  mutate(
    PERWT = as.numeric(PERWT),
    # IPUMS code 10 and above equals a Bachelor's Degree or higher
    target_indicator = if_else(as.numeric(EDUC) >= 10, 1, 0),
    
    # --- ONE-STEP REAL INCOME NORMALIZATION ---
    REAL_INCOME = HHINCOME_clean * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    
    # Calculate Deciles Safely
    decile = ntile(REAL_INCOME, 10),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # Filter strictly for adults 25 and older
  filter(
    !is.na(PERWT), 
    PERWT > 0, 
    !is.na(REAL_INCOME), 
    !is.na(Country), 
    !is.na(target_indicator), 
    as.numeric(AGE) >= 25
  )

# ==============================================================================
# 5. SUMMARY STATISTICS (TERCILES & QUARTILES WITHIN)
# ==============================================================================
message("\n=== EDUCATIONAL ATTAINMENT SUMMARY (BA+ FOR ADULTS 25+) ===")

# Step 1: Calculate the overall stats for each of the Three Countries
overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Bachelor's Degree or Higher (%)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
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
    `Bachelor's Degree or Higher (%)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  ) %>%
  select(-Quartile)

# Step 4: Bind everything together, format, and print the master table
macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(
    Total_Population = scales::comma(Total_Population)
  )

print(as.data.frame(macro_summary))

# Step 5: Export Table to PNG
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

table_grob <- tableGrob(macro_summary, rows = NULL, theme = ttheme_default(
  core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)), 
  colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))
))

ggsave(
  filename = here::here("03_output", "visualizations_final", "ACS_educational_attainment_Summary_Table_2D.png"), 
  plot = table_grob, 
  width = 10, 
  height = 6, 
  bg = "white", 
  dpi = 300
)

# ==============================================================================
# 6. VISUALIZATION EXECUTION (Minimalist 2D Format)
# ==============================================================================
message("\nGenerating Minimalist Skyline Plot...")

# Call the Master Visualization Engine (now applying LOESS math via the 2D script)
p_chart <- plot_economic_skyline_2C(
  data          = prepared_data, 
  indicator_var = "target_indicator", 
  weight_var    = "PERWT"
)

# Save the trend plot
ggsave(
  filename = here::here("03_output", "visualizations_final", "ACS_educational_attainment_2D.png"), 
  plot = p_chart, 
  width = 10, 
  height = 7, 
  dpi = 300, 
  bg = "white"
)

message("Processing & Visualizations Complete!")
print (p_chart)
