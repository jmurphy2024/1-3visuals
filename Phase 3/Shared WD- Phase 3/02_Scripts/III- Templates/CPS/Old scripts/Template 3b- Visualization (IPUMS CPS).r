# ===================================================================
# TEMPLATE 3b: VISUALIZATION (IPUMS CPS)
# ===================================================================
# Project: 1/3 Country Project Visualizations
# Author: Gemini
# Date: 2025-10-01
#
# Purpose:
# This script loads the final CPS summary statistics checkpoint (.rds file)
# created by the COMBINE script (e.g. IV2) and generates visualizations.
# ===================================================================

# ==== 0. SETUP & PARAMETERS ====
library(ggplot2); library(dplyr); library(readr); library(here); library(ggtext); library(grid)
source(here::here("V2_Visualization_Functions (JM).r"))

# ---- 0.3 USER-DEFINED PARAMETERS & PATHS (CPS) ----
COMBINED_SUMMARY_DATA_PATH <- here::here("data", "summary_outputs", "combined_summary_stats_final_G_groups.rds")
BORDERS_CSV_PATH <- here::here("data", "summary_outputs", "within_tercile_quantile_borders.csv")
PLOT_OUTPUT_DIR <- here::here("output", "plots_cps")
dir.create(PLOT_OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# ---- 0.4 Load Data ----
if (!file.exists(COMBINED_SUMMARY_DATA_PATH)) stop("FATAL ERROR: Combined summary data file not found.")
summary_data <- readRDS(COMBINED_SUMMARY_DATA_PATH)


# ==== 1. GENERATE VISUALIZATION FOR CPS DATA ====
DATA_SOURCE_FILTER <- "cps_edsupp"
data_for_plot <- summary_data %>% filter(source_dataset == DATA_SOURCE_FILTER)
if (nrow(data_for_plot) == 0) stop(paste("No data found for source:", DATA_SOURCE_FILTER))

# ---- 1.2 Define Plotting Arguments (Replicating Plot C2) ----
Y1_VARIABLE_COLUMN <- "prop_child_enrolled_5_17"
Y2_VARIABLE_COLUMN <- "n_weighted_children_age_5_to_17"
if (!all(c(Y1_VARIABLE_COLUMN, Y2_VARIABLE_COLUMN) %in% names(data_for_plot))) {
  stop("Error: Y-variable columns not found in the CPS data.")
}

# ---- 1.3 Call the Plotting Function ----
print("Generating visualization for CPS data (replicating Plot C2)...")
create_dual_axis_quartile_plot(
  combined_data = data_for_plot,
  borders_data_path = BORDERS_CSV_PATH,
  y1_var_cols = Y1_VARIABLE_COLUMN, y2_var_col = Y2_VARIABLE_COLUMN,
  y1_category_labels = "Enrollment Rate (Age 5-17)",
  y2_category_label = "Weighted Child Population Count (Age 5-17)",
  y1_axis_label = "Enrollment Rate (Age 5-17)",
  y2_axis_label = "Weighted Child Population Count",
  plot_title = "Child Enrollment Rate and Population by Household Income (CPS)",
  legend_title = "Metric",
  y1_axis_format = "percent", y2_axis_format = "comma",
  output_filename = "CPS_Plot_C2_Child_Enrollment.png",
  output_dir = PLOT_OUTPUT_DIR,
  include_points = FALSE, use_html_x_axis = FALSE,
  line1_palette = c("#1B9E77"), line2_color = "#D95F02"
)

print(paste("CPS plot saved to:", PLOT_OUTPUT_DIR))
print("--- CPS Visualization Template Finished ---")
