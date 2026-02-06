## WD location: 02_Scripts/IV- Visual Scripts/UIED
## Script: UIED_gfc_viz.R
## Purpose: Generates a dual-axis visualization for Advanced Coursework
##          and Fiber Internet rates from UIED data.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Last Modified: 2025-10-02 (Corrected argument names in function call)

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(ggplot2); library(purrr)
library(stringr); library(ggtext); library(glue); library(grid); library(gridExtra); library(scales)
library(ggnewscale); library(cowplot); library(tidyr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. LOAD DATA ====
message("--- Loading Data Files ---")
main_cutoffs <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
within_tercile_borders_df <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)
USER_FINE_GROUP_LEVEL <- "Groups_20" # Using ventiles for smooth plotting

USER_INDICATOR_NAME <- "gfc"
PREPARED_DATA_FILE <- here::here("01_data", "processed", "Finalized RDS outputs", "Urban Institute Education Data", paste0("uied_district_data_with_income_", USER_INDICATOR_NAME, "_", "2017", ".rds"))
district_data <- readRDS(PREPARED_DATA_FILE)

# ============================================================================ #
# ==== 2. GENERATE DUAL-AXIS PLOT (FIBER & ADVANCED COURSEWORK) ====
# ============================================================================ #
message("\n--- Generating Dual-Axis Plot ---")

# --- 2.1. Configure Plot ---
WEIGHT_VARIABLE <- "total_enrollment_crdc"
Y1_VAR_COLS       <- c("advanced_coursework_rate")
Y2_VAR_COL        <- "fiber_internet_rate"
Y1_LABELS         <- c("Advanced Coursework Enrollment")
Y2_LABEL          <- "Fiber Internet"
PLOT_TITLE        <- "Rates of Advanced Coursework Enrollment and Fiber Internet Access"
Y1_AXIS_LABEL     <- "Advanced Coursework Rate"
Y2_AXIS_LABEL     <- "Fiber Internet Rate"
LEGEND_TITLE      <- "Indicator"
OUTPUT_PLOT_FILENAME <- paste0("plot_", USER_INDICATOR_NAME, "_dual_axis_", "2017", ".png")

# --- 2.2. Process Data for Plot ---
data_with_groups <- assign_income_groups(
  data_to_process = district_data, borders_df = within_tercile_borders_df,
  income_var_name = "median_household_income_final", detail_level = USER_FINE_GROUP_LEVEL,
  main_cutoff1 = main_cutoffs$main_cutoff1, main_cutoff2 = main_cutoffs$main_cutoff2
)

summary_stats <- data_with_groups %>%
  filter(!is.na(income_tercile) & !is.na(fine_income_group)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    across(all_of(c(Y1_VAR_COLS, Y2_VAR_COL)), ~ weighted.mean(.x, w = .data[[WEIGHT_VARIABLE]], na.rm = TRUE)),
    .groups = "drop"
  )

# --- 2.3. Generate and Display Plot ---
# Using create_dual_axis_plot as it's the correct function from shared visuals
final_plot <- create_dual_axis_plot(
  summary_data = summary_stats,
  borders_data_path = here::here("01_data", "processed", "within_tercile_quantile_borders.csv"),
  y1_vars = Y1_VAR_COLS,
  y2_var = Y2_VAR_COL,
  y1_labels = Y1_LABELS, # MODIFICATION: Corrected argument name
  y2_label = Y2_LABEL,   # MODIFICATION: Corrected argument name
  y1_axis_label = Y1_AXIS_LABEL,
  y2_axis_label = Y2_AXIS_LABEL,
  plot_title = PLOT_TITLE,
  legend_title = LEGEND_TITLE,
  output_filename = OUTPUT_PLOT_FILENAME,
  output_dir = here("03_output", "visualizations_final"),
  y1_axis_format = "percent",
  y2_axis_format = "percent",
  source_filter = NULL, # Data is already processed
  line1_palette = c("#2980B9"), # Blue for coursework
  line2_color = "#27AE60",     # Green for fiber
  border_t1_t2 = main_cutoffs$main_cutoff1,
  border_t2_t3 = main_cutoffs$main_cutoff2,
  intra_tercile_quantile_group = USER_FINE_GROUP_LEVEL
)

print(final_plot)

message("\nVisualization script complete.")

