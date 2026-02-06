# ==== 0. ABOUT ====
## Script: ACS_poverty_viz.R
## Purpose: Visualize housing precariousness across income terciles.

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(gridExtra); library(here)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# --- 1. LOAD DATA ---
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "Poverty_Living_Arrangements"
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

prepared_data <- readRDS(PREPARED_DATA_FILE)
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign groups
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "HHINCOME",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# --- 2. SUMMARIZE & PLOT ---
summary_stats <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Doubled-Up" = weighted.mean(ind_doubled_up, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_single_line_plot(
  summary_data       = summary_stats,
  y_var              = "Doubled-Up",
  plot_title         = "Prevalence of Doubled-Up Households (Poverty + Non-Relative Status)",
  y_axis_label       = "Prevalence (%)",
  y_axis_format      = "percent",
  output_filename    = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level   = "Groups_20",
  border_t1_t2       = main_cutoffs$main_cutoff1,
  border_t2_t3       = main_cutoffs$main_cutoff2
)

grid.draw(final_plot)