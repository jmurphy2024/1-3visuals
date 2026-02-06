# ==== 0. ABOUT ====
## WD location: 02_Scripts/IV-Visuals/ACS & CPS
## Script: visualize_dual_axis_ChildEnrollment.R
## Purpose: Creates a dual-axis plot comparing child enrollment rate (from CPS)
##          with the child population count (from ACS) by income group.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Dependencies: All plotting libraries, dplyr, readr, here.
## Input: Summary RDS files for CPS enrollment and ACS population.
## Output: A final visualization PNG file in `03_output/visualizations_final/`.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(purrr); library(stringr)
library(ggplot2); library(ggtext); library(glue); library(grid); library(gridExtra)
library(scales); library(ggnewscale); library(cowplot); library(tidyr)
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. PARAMETERS & DATA LOADING ====
USER_FINE_GROUP_LEVEL <- "Groups_20"
PLOT_TITLE <- "Child School Enrollment Rate and Population by Income Position"
OUTPUT_FILENAME <- "plot_ChildEnrollment_vs_Population_DualAxis.png"

summary_enroll <- readRDS(here::here("01_data", "processed", "summary_cps_child_enrollment.rds"))
summary_pop <- readRDS(here::here("01_data", "processed", "summary_acs_child_population.rds"))
main_cutoffs <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

# ==== 2. MERGE AND VISUALIZE ====
combined_summary <- full_join(summary_enroll, summary_pop, by = c("income_tercile", "fine_income_group"))

# Create the plot, modifying the secondary axis labels
create_dual_axis_plot(
  summary_data      = combined_summary,
  y1_vars           = "prop_child_enrolled",
  y2_var            = "n_children_weighted",
  y1_labels         = "Child Enrollment Rate (CPS)",
  y2_label          = "Number of Children (ACS)",
  plot_title        = PLOT_TITLE,
  y1_axis_label     = "Enrollment Rate (Age < 18)",
  y2_axis_label     = "Number of Children",
  legend_title      = "Indicator",
  output_filename   = OUTPUT_FILENAME,
  fine_group_level  = USER_FINE_GROUP_LEVEL,
  y1_axis_format    = "percent",
  y2_axis_format    = "number",
  line1_palette     = c("#1A5276"),
  line2_color       = "#A93226",
  border_t1_t2      = main_cutoffs$main_cutoff1,
  border_t2_t3      = main_cutoffs$main_cutoff2,
  # Custom argument for secondary axis scaling for millions
  y2_axis_label_formatter = function(x) scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = "M")(x)
)

message("\n--- Dual-axis visualization script complete. ---")