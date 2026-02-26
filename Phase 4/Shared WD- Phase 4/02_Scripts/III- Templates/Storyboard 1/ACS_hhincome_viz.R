# ==== 0. ABOUT ====
## WD location:02_Scripts/III-Templates/ ACS
## Script: ACS_hhincome_viz
## Purpose: # Purpose: Visualizes Average Household Income (Bar + Dot + Line Chart).
#          - LOGIC: Person-Weighted (PERWT).
#          - DATA: Includes all household members (Inclusive).
## Author: Gemini / User
## Date Created: 2026-02-09
## Updated: 2026/02/12
## Dependencies: dplyr, readr, here, ggplot2, cowplot, scales


rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(cowplot); library(scales)

# Source shared functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# --- 1. CONFIG ---
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")
BORDERS_CSV_FILE  <- here::here("01_data", "processed", "within_tercile_quantile_borders_person_inclusive.csv")

USER_INCOME_VAR   <- "REAL_INCOME"  
USER_WEIGHT_VAR   <- "PERWT"        # Weight by Humans
USER_DETAIL_LEVEL <- "Groups_20"    # 20 bars = 5% of population each

# --- 2. LOAD & PROCESS ---
acs_data     <- readRDS(INPUT_DATA_FILE)
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)
borders_df   <- read_csv(BORDERS_CSV_FILE, show_col_types = FALSE) %>%
  rename(any_of(c(MainTercile = "income_tercile_group")))

# Assign ventiles using shared utility
data_grouped <- assign_income_groups(
  data_to_process = acs_data,
  borders_df      = borders_df,
  income_var_name = USER_INCOME_VAR,
  detail_level    = USER_DETAIL_LEVEL,
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# Calculate weighted means per ventile
summary_stats <- data_grouped %>%
  filter(!is.na(fine_income_group), !is.na(income_tercile)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    avg_val = weighted.mean(.data[[USER_INCOME_VAR]], w = .data[[USER_WEIGHT_VAR]], na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. PLOT ---
create_bar_dot_line_plot(
  summary_data     = summary_stats,
  y_var            = "avg_val",
  plot_title       = "Average Shared Household Income",
  y_axis_label     = "Real Household Income (Adjusted)",
  output_filename  = "Income_Distribution_Person_Weighted.png",
  fine_group_level = USER_DETAIL_LEVEL,
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2,
  t1_color = "#C0392B", t2_color = "#F5B041", t3_color = "#27AE60"
)

message("Success: Visualization saved to 03_output/visualizations_final/")