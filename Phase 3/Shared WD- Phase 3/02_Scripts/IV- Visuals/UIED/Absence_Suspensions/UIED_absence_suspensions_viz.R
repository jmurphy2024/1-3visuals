## WD location: 02_Scripts/IV Variable Visual Scripts/Urban Institute Education Data
## Script: UEID_absence_suspensions_viz.R
## Purpose: Loads prepared district-level data, assigns income groups, calculates
##          summary statistics, and generates the final dual-axis visualization
##          for chronic absenteeism and suspensions.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-01
## Last Modified: 2025-10-01 (Finalized with improved shared function)
## Dependencies: dplyr, readr, here, rlang, ggplot2, purrr, stringr, ggtext, glue, grid, cowplot
## Input: `01_data/processed/Finalized RDS outputs/Urban Institute Education Data/uied_district_data_with_income_[YEAR].rds`
##        `01_data/processed/main_tercile_cutoffs.rds`
##        `01_data/processed/within_tercile_quantile_borders.csv`
## Output: An RDS file with summary statistics and a dual-axis visualization PNG.

# ==== 0. SETUP ====
# ===== 0.1. Clear Environment =====
rm(list = ls())
gc()

# ===== 0.2. Load Libraries =====
library(dplyr)
library(readr)
library(here)
library(rlang)
library(ggplot2)
library(purrr)
library(stringr)
library(ggtext)
library(glue)
library(grid)
library(gridExtra)
library(scales)
library(ggnewscale)
library(cowplot)
library(tidyr)

# ===== 0.3. Source Shared Functions =====
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

message("Setup complete. Environment cleared and libraries loaded.")


# ==== 1. USER CONFIGURATION ====
USER_YEAR              <- 2017
USER_FINE_GROUP_LEVEL  <- "Groups_20"
USER_INDICATOR_NAME    <- "Chronic_Absence_and_Suspensions"

USER_PLOT_TITLE        <- "Chronic Absence and Suspension Days by Income Position"
USER_Y1_AXIS_LABEL     <- "Chronic Absence Rate"
USER_Y2_AXIS_LABEL     <- "Average Suspension Days per Student"
USER_PLOT_FILENAME     <- paste0("plot_", USER_INDICATOR_NAME, "_", USER_YEAR, ".png")

PREPARED_DATA_FILE <- here::here("01_data", "processed", "Finalized RDS outputs", "Urban Institute Education Data", paste0("uied_district_data_with_income_", USER_YEAR, ".rds"))


# ==== 2. LOAD PREPARED DATA & BORDERS ====
district_data_with_income <- readRDS(PREPARED_DATA_FILE)
main_cutoffs <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
within_tercile_borders <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)


# ==== 3. ASSIGN INCOME GROUPS ====
data_with_groups <- assign_income_groups(
  data_to_process = district_data_with_income,
  borders_df = within_tercile_borders,
  income_var_name = "median_household_income_final",
  detail_level = USER_FINE_GROUP_LEVEL,
  main_cutoff1 = main_cutoffs$main_cutoff1,
  main_cutoff2 = main_cutoffs$main_cutoff2
)


# ==== 4. CALCULATE FINAL SUMMARY STATISTICS FOR PLOTTING ====
summary_stats <- data_with_groups %>%
  filter(!is.na(income_tercile) & !is.na(fine_income_group)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    chronic_absent_rate = weighted.mean(chronic_absent_rate, w = total_enrollment, na.rm = TRUE),
    avg_suspension_days_per_student = weighted.mean(avg_suspension_days_per_student, w = total_enrollment, na.rm = TRUE),
    .groups = "drop"
  )

saveRDS(
  summary_stats,
  file = here::here("01_data", "processed", paste0("summary_stats_", USER_INDICATOR_NAME, "_", USER_YEAR, ".rds"))
)


# ==== 5. GENERATE AND SAVE VISUALIZATION ====

final_plot <- create_dual_axis_plot(
  summary_data      = summary_stats,
  y1_vars           = "chronic_absent_rate",
  y2_var            = "avg_suspension_days_per_student",
  y1_labels         = "Chronic Absence Rate",
  y2_label          = "Avg. Suspension Days",
  plot_title        = "Chronic Absence and Suspension Days by Income Position",
  y1_axis_label     = "Chronic Absence Rate",
  y2_axis_label     = "Average Suspension Days per Student",
  legend_title      = "Indicator",
  output_filename   = "plot_Chronic_Absence_and_Suspensions_2017.png",
  fine_group_level  = "Groups_20",
  y1_axis_format    = "percent",
  y2_axis_format    = "number",
  # Pass common theme parameters as additional arguments
  border_t1_t2      = 64400,
  border_t2_t3      = 130000
)

final_plot

message(paste("Visualization saved to:", here::here("03_output", "visualizations_final", USER_PLOT_FILENAME)))
message("Script complete.")