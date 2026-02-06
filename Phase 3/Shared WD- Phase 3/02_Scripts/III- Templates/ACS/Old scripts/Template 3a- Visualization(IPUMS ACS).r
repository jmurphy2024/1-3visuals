# ===================================================================
# TEMPLATE 3a: VISUALIZATION (IPUMS ACS)
# ===================================================================
# Project: 1/3 Country Project Visualizations
# Author: Janica Murphy/ Gemini
# Date: 2025-10-01
#
# Purpose:
# This script loads the final ACS summary statistics checkpoint (.rds file)
# created by Template 2a and generates visualizations.
# ===================================================================

# ==== 0. SETUP & PARAMETERS ====

# ---- 0.1 Load Core Packages ----
library(ggplot2); library(dplyr); library(readr); library(here); library(ggtext); library(grid)

# ---- 0.2 Source Visualization Functions ----
source(here::here("V2_Visualization_Functions (JM).r"))

# ---- 0.3 USER-DEFINED PARAMETERS & PATHS (ACS) ----
# This path should point to the .rds file created by the COMBINE script (e.g., IV2_Combine_Summaries)
COMBINED_SUMMARY_DATA_PATH <- here::here("data", "summary_outputs", "combined_summary_stats_final_G_groups.rds")
BORDERS_CSV_PATH <- here::here("data", "summary_outputs", "within_tercile_quantile_borders.csv")
PLOT_OUTPUT_DIR <- here::here("output", "plots_acs")
dir.create(PLOT_OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 0.4 Load Data ----
if (!file.exists(COMBINED_SUMMARY_DATA_PATH)) stop("FATAL ERROR: Combined summary data file not found.")
summary_data <- readRDS(COMBINED_SUMMARY_DATA_PATH)


# ==== 1. GENERATE VISUALIZATION FOR ACS DATA ====
data_for_plot <- summary_data %>% filter(source_dataset == "acs")
if (nrow(data_for_plot) == 0) stop("No ACS data found in the combined summary file.")

# ---- 1.2 Define Plotting Arguments ----
# >>> ACTION REQUIRED: Change these variables to columns in your ACS summary file. <<<
Y1_VARIABLE_COLUMN <- "prop_non_citizen"
Y2_VARIABLE_COLUMN <- "prop_burdened_owner"
if (!all(c(Y1_VARIABLE_COLUMN, Y2_VARIABLE_COLUMN) %in% names(data_for_plot))) {
  stop("Error: One or more specified Y-variable columns not found in the ACS data.")
}

# ---- 1.3 Call the Plotting Function ----
print("Generating visualization for ACS data...")
create_dual_axis_quartile_plot(
  combined_data = data_for_plot,
  borders_data_path = BORDERS_CSV_PATH,
  y1_var_cols = Y1_VARIABLE_COLUMN, y2_var_col = Y2_VARIABLE_COLUMN,
  y1_category_labels = "Non-Citizen Population", y2_category_label = "Cost-Burdened Homeowners",
  y1_axis_label = "Proportion Non-Citizen", y2_axis_label = "Proportion Cost-Burdened",
  plot_title = "Citizenship and Housing Burden Distribution (ACS)",
  legend_title = "Metric",
  y1_axis_format = "percent", y2_axis_format = "percent",
  output_filename = "ACS_Plot_Citizenship_vs_Housing.png",
  output_dir = PLOT_OUTPUT_DIR,
  include_points = TRUE
)

print(paste("ACS plot saved to:", PLOT_OUTPUT_DIR))
print("--- ACS Visualization Template Finished ---")
