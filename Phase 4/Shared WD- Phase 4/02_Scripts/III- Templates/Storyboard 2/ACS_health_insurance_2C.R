# ==============================================================================
# SCRIPT: ACS_health_insurance_2C.R
# Purpose: Generate 3-Country Skyline for Private vs Public Health Coverage
# Logic:   Native HHINCOME, Negative Households Dropped, Person-Weighted
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit for heavy ggplot rendering
options(expressions = 500000)

library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tibble)
library(data.table); library(scales); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2C.R"))

# ==============================================================================
# VISUAL THEME STANDARDIZATION (MINIMALIST / NO CUTOFFS)
# ==============================================================================
apply_standard_theme <- function(p) {
  p_updated <- p +
    theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      text              = element_text(family = "serif", size = 11), 
      legend.position   = "none", # Removed legend to match the clean image style
      panel.grid        = element_blank(), # Removed all gridlines
      axis.text.x       = element_blank(), # Removed X-axis text
      axis.text.y       = element_text(color = "black", size = 11, family = "serif", margin = margin(r = 5)), 
      axis.line.x       = element_line(color = "black", linewidth = 1.2), # Bolded axis lines
      axis.line.y       = element_line(color = "black", linewidth = 1.2), 
      axis.title        = element_blank(), # Removed axis titles
      plot.title        = element_blank(), # Removed plot title
      plot.caption      = element_blank(), # Removed plot caption
      plot.margin       = margin(t = 30, r = 100, b = 30, l = 20), # Extended right margin for direct line labels
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA)
    )
  
  return(p_updated)
}

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT (2023 ACS)
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 

VARS_NEEDED <- c(
  "SERIAL", "PERWT", "HHINCOME", "STATEFIP", "AGE", 
  "HCOVANY",  # 1 = Uninsured, 2 = Insured
  "HCOVPUB",  # 1 = No Public, 2 = Has Public Coverage
  "HCOVPRIV"  # 1 = No Private, 2 = Has Private Coverage
)

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_health_macro")
TARGET_FILE <- file.path(TARGET_DIR, "acs_health_macro_native_hh.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", 
    description = "Three Countries ACS Private vs Public Coverage (Native HHINCOME)",
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED
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

# 3. DATA ENGINEERING (Native HHINCOME & Drop Negatives)
# ------------------------------------------------------------------------------
message("Cleaning data and dropping negative households...")
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

dt_raw[, HHINCOME_clean := as.numeric(HHINCOME)]
dt_raw[HHINCOME_clean == 9999999, HHINCOME_clean := NA_real_]
dt_filtered <- dt_raw[HHINCOME_clean >= 0]

# 4. NORMALIZATION & PILLAR LOGIC
# ------------------------------------------------------------------------------
message("Applying normalization and dynamic cutoffs...")

INFLATION_ADJ <- get_inflation_multiplier(data_year = as.numeric(substring(USER_SAMPLE, 3, 6)), base_year = 2023)

prepared_data <- as_tibble(dt_filtered) %>%
  mutate(
    PERWT = as.numeric(PERWT),
    
    # --- HEALTHCARE PILLAR LOGIC (IPUMS standard: 1 = No, 2 = Yes) ---
    `Private Coverage` = if_else(as.numeric(HCOVPRIV) == 2, 1, 0),
    `Public Coverage` = if_else(as.numeric(HCOVPUB) == 2, 1, 0),
    `Uninsured` = if_else(as.numeric(HCOVANY) == 1, 1, 0),
    Health_Uninsured_Composite = `Uninsured`,
    
    # --- ONE-STEP REAL INCOME NORMALIZATION ---
    REAL_INCOME = HHINCOME_clean * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # UNIVERSE FILTER: Working-age adults (18-64)
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), 
         as.numeric(AGE) >= 18, as.numeric(AGE) <= 64)

# ==============================================================================
# 5. SUMMARY STATISTICS (TERCILES & QUARTILES WITHIN)
# ==============================================================================
if(!require(Hmisc)) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

message("\n=== HEALTHCARE ACCESS SUMMARY (WORKING-AGE ADULTS 18-64) ===")

overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Private Coverage (%)` = round((sum(`Private Coverage` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Public Coverage (%)`  = round((sum(`Public Coverage` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Uninsured (%)`        = round((sum(`Uninsured` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  )

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

quartile_stats <- prepared_data_q %>%
  group_by(Country, Quartile) %>%
  summarise(
    Subgroup = first(Quartile),
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Private Coverage (%)` = round((sum(`Private Coverage` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Public Coverage (%)`  = round((sum(`Public Coverage` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Uninsured (%)`        = round((sum(`Uninsured` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  ) %>%
  select(-Quartile)

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(
    Total_Population = scales::comma(Total_Population)
  )

print(as.data.frame(macro_summary))

# ==============================================================================
# 6. VISUALIZATION EXECUTION
# ==============================================================================
message("\nGenerating Skyline Plots...")
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

plot_data <- prepared_data %>%
  rename(`Public Coverage` = `Public Coverage`)

# Generate Pillars Chart
p_pillars <- plot_economic_skyline_2(
  data           = plot_data, 
  indicator_vars = c("Private Coverage", "Public Coverage", "Uninsured"), 
  weight_var     = "PERWT", 
  y_axis_label   = "",
  plot_title     = "",
  caption_text   = ""
)
p_pillars <- apply_standard_theme(p_pillars)
print(p_pillars)
ggsave(here::here("03_output", "visualizations_final", "ACS_health_private_vs_public_pillars_2C.png"), p_pillars, width = 10, height = 7, dpi = 300, bg = "white")

# Generate Composite Index Chart
p_index <- plot_economic_skyline(
  data          = prepared_data, 
  indicator_var = "Health_Uninsured_Composite", 
  weight_var    = "PERWT", 
  y_axis_label  = "",
  plot_title    = "",
  caption_text  = ""
)
p_index <- apply_standard_theme(p_index)
print(p_index)
ggsave(here::here("03_output", "visualizations_final", "ACS_health_uninsured_working_age_composite_2C.png"), p_index, width = 10, height = 7, dpi = 300, bg = "white")

message("Processing Complete! Minimalist graphs saved to 03_output/visualizations_final/")