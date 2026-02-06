# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: IPUMS_ACS_Template.R
## Purpose: A standardized template for downloading, cleaning, and preparing
##          IPUMS microdata to generate a 1/3 Country visualization. This script
##          is designed to be copied and modified for each new indicator.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-09-25
## Last Modified: 2025-09-25 (Consolidated user inputs; added codebook generation to data folder)
## Dependencies: ipumsr, dplyr, readr, here, srvyr, survey, rlang, tidyr, stringr, ggplot2
## Input: User-defined IPUMS variables and parameters.
##        - 01_data/processed/main_tercile_cutoffs.rds
##        - 01_data/processed/within_tercile_quantile_borders.csv
## Output: An RDS file with summary statistics for the specified indicator, saved to
##         `01_data/processed/`. A visualization PNG saved to `03_output/`.


### IMPORTANT NOTE: THIS SCRIPT HAS NOT BEEN TESTED ###


# ==== 0. SETUP ====
# This section loads necessary libraries and sources the shared functions from Stage II.
# It should not need to be modified for a standard analysis.

# ===== 0.1. Clear Environment (Optional) =====
rm(list = ls())
gc()

# ===== 0.2. Load Libraries =====
library(ipumsr)
library(dplyr)
library(readr)
library(here)
library(srvyr)
library(survey)
library(rlang)
library(tidyr)
library(stringr)
library(ggplot2)

# ===== 0.3. Source Shared Functions =====
# Load the reusable functions for utilities and visualizations.
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

message("Setup complete. Libraries and shared functions loaded.")


# ==== 1. USER CONFIGURATION ====
# This is the primary section to modify for each new analysis.
# Fill in the parameters below to define the data, variables, and plot details.
# User will also need to fill in sections 3.1 and 3.2

# ===== 1.1. IPUMS Extract Definition =====
# Define the IPUMS collection, sample, and all variables needed for your analysis.
# Refer to the IPUMS website for variable names and sample availability.
USER_IPUMS_COLLECTION <- "usa"
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_VARIABLES_NEEDED <- c(
  # --- Core variables (MUST BE INCLUDED) ---
  "SERIAL", "HHWT", "PERWT", "HHINCOME", "AGE",
  # --- Indicator-specific variables (ADD YOURS HERE) ---
  "EMPSTAT"
)

# ===== 1.2. Core Variable Names =====
# Tell the script which variables to use for essential operations.
# These names must be present in USER_VARIABLES_NEEDED above.
USER_HHID_VAR          <- "SERIAL"
USER_PERSON_WEIGHT_VAR <- "PERWT"
USER_HH_WEIGHT_VAR     <- "HHWT"
USER_INCOME_VAR        <- "HHINCOME"

# ===== 1.3. Analysis & File Naming =====
# Define a short, descriptive name for your indicator. This will be used in output filenames.
USER_INDICATOR_NAME      <- "Employment_Rate"
USER_FINE_GROUP_LEVEL    <- "Groups_20" # E.g., "Groups_20" for ventiles, "Groups_4" for quartiles

# ===== 1.4. Plotting Parameters =====
# Define the titles, labels, and output filename for the final visualization.
USER_PLOT_TITLE       <- "Employment Rate by Position in Income Distribution"
USER_Y_AXIS_LABEL     <- "Employment Rate (Age 25-65)"
USER_PLOT_FILENAME    <- paste0("plot_", USER_INDICATOR_NAME, ".png")

# ===== 1.5. (Optional) Skip Download =====
# If you have already downloaded the data, provide the full path to the .xml DDI file below.
# Otherwise, leave it as NULL to perform the download.
USER_DDI_FILE_PATH <- NULL
# Example: USER_DDI_FILE_PATH <- here::here("01_data", "raw", "ipums_downloads", "USA_EXTRACT_1_2025-09-25", "usa_00001.xml")


# ==== 2. DATA ACQUISITION ====
# This section downloads data from IPUMS and generates a codebook unless a DDI file is provided.

if (is.null(USER_DDI_FILE_PATH) || !file.exists(USER_DDI_FILE_PATH)) {
  message("--- DDI file path not provided or invalid, proceeding with IPUMS download ---")
  
  extract_def <- define_extract_micro(
    collection = USER_IPUMS_COLLECTION,
    samples = USER_IPUMS_SAMPLE_ID,
    variables = unique(USER_VARIABLES_NEEDED)
  )
  
  submitted_extract <- submit_extract(extract_def)
  downloadable_extract <- wait_for_extract(submitted_extract)
  
  download_dir <- here::here("01_data", "raw", "ipums_downloads", paste0(toupper(USER_IPUMS_COLLECTION), "_EXTRACT_", submitted_extract$number, "_", Sys.Date()))
  dir.create(download_dir, showWarnings = FALSE, recursive = TRUE)
  
  downloaded_files <- download_extract(downloadable_extract, download_dir = download_dir)
  ddi_file_path <- downloaded_files[grep("\\.xml$", downloaded_files)]
  
} else {
  message(paste("--- Skipping IPUMS download. Using provided DDI file:", USER_DDI_FILE_PATH, "---"))
  ddi_file_path <- USER_DDI_FILE_PATH
  download_dir <- dirname(ddi_file_path)
}

# ===== 2.1. Generate and Save Codebook =====
# A text codebook is generated and saved in the same folder as the raw data.
codebook_path <- file.path(download_dir, paste0("codebook_", USER_INDICATOR_NAME, ".txt"))
generate_codebook_from_ddi(ddi_file_path, USER_VARIABLES_NEEDED, codebook_path)

# ===== 2.2. Load Data =====
raw_data <- read_ipums_micro(ddi = ddi_file_path, verbose = FALSE)
message("... IPUMS data loaded successfully.")


# ==== 3. DATA CLEANING & PREPARATION ====
# This section is for all data cleaning, variable recoding, and derived variable creation.
# The logic here will be specific to the variables you are analyzing.

# ===== 3.1. Handle Missing/Special Values =====
message("Cleaning raw data by recoding special values to NA...")
cleaned_data <- raw_data %>%
  mutate(
    # --- USER: Add your variable cleaning logic below ---
    # Use if_else() or case_when() to recode NIU, missing, etc. to NA.
    # Refer to the IPUMS codebook for your variables to find the correct codes.
    # Example from the old ACS script:
    # HHINCOME = if_else(HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME)),
    # EMPSTAT = if_else(EMPSTAT %in% c(0, 9), NA_integer_, as.integer(EMPSTAT)),
    # HISPAN = if_else(HISPAN == 9, NA_integer_, as.integer(HISPAN)),
    # CITIZEN = if_else(CITIZEN %in% c(0, 8, 9), NA_integer_, as.integer(CITIZEN))
  )
message("...Data cleaning complete.")

# ===== 3.2. Create Derived Indicator Variable =====
message("Creating derived variables for analysis...")
prepared_data <- cleaned_data %>%
  mutate(
    # --- USER: Add your derived variable logic below ---
    # Create the final variable you intend to plot and give it a clear name.
    # Example for Employment Rate:
    indicator_to_plot = if_else(EMPSTAT == 1 & AGE >= 25 & AGE <= 65, 1, 0, missing = NA_real_)
    
    # --- Example for Housing Burden ---
    # is_burdened_flag = if_else(
    #   OWNERSHP == 2 & !is.na(HHINCOME) & HHINCOME > 0,
    #   (RENTGRS * 12) / HHINCOME > 0.3,
    #   NA
    # ),
    # indicator_to_plot = if_else(is_burdened_flag, 1, 0, missing = NA_real_)
  )
message("...Derived variables created.")


# ==== 4. INCOME GROUP ASSIGNMENT ====
# This section loads the pre-calculated income borders and assigns income groups.

# ===== 4.1. Load Border Files =====
main_cutoffs <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
within_tercile_borders <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# ===== 4.2. Assign Income Groups =====
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df = within_tercile_borders,
  income_var_name = USER_INCOME_VAR,
  detail_level = USER_FINE_GROUP_LEVEL,
  main_cutoff1 = main_cutoffs$main_cutoff1,
  main_cutoff2 = main_cutoffs$main_cutoff2
)

message("Income groups assigned successfully.")


# ==== 5. CALCULATE SUMMARY STATISTICS ====
# Calculate the final summary statistics for your indicator, grouped by income.
message("Calculating final summary statistics...")
summary_stats <- data_with_groups %>%
  filter(!is.na(income_tercile) & !is.na(fine_income_group)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    # --- USER: MODIFY THE CALCULATION BELOW FOR YOUR INDICATOR ---
    # The `weighted.mean()` function will calculate a proportion if your `indicator_to_plot`
    # is a binary 0/1 variable, or an average if it's continuous.
    indicator_value = weighted.mean(indicator_to_plot, w = !!sym(USER_PERSON_WEIGHT_VAR), na.rm = TRUE),
    
    # Keep these for context
    n_unweighted = n(),
    n_weighted = sum(!!sym(USER_PERSON_WEIGHT_VAR), na.rm = TRUE),
    .groups = "drop"
  )

message("Summary statistics calculated successfully.")
print(head(summary_stats))


# ==== 6. VISUALIZATION ====
# This section calls the shared plotting function to generate the final visualization.
message("Generating visualization...")
create_final_plot(
  data_to_plot = summary_stats,
  x_var = "fine_income_group",
  y_var = "indicator_value",
  plot_type = "line",
  plot_title = USER_PLOT_TITLE,
  x_axis_label = "Population Distribution by Household Income",
  y_axis_label = USER_Y_AXIS_LABEL
)

# Save the plot
ggsave(
  filename = here::here("03_output", USER_PLOT_FILENAME),
  width = 10,
  height = 7,
  dpi = 300
)
message(paste("Visualization saved to:", here::here("03_output", USER_PLOT_FILENAME)))


# ==== 7. SAVE SUMMARY DATA (OPTIONAL) ====
# Save the summary statistics to an RDS file for later use or aggregation.
saveRDS(
  summary_stats,
  file = here::here("01_data", "processed", paste0("summary_", USER_INDICATOR_NAME, ".rds"))
)
message(paste("Summary data saved to:", here::here("01_data", "processed", paste0("summary_", USER_INDICATOR_NAME, ".rds"))))


# ==== 8. CLEAN UP MEMORY ====
rm(list = c("raw_data", "cleaned_data", "prepared_data", "data_with_groups", "summary_stats"))
gc()
message("Script complete. Large data objects removed from memory.")

