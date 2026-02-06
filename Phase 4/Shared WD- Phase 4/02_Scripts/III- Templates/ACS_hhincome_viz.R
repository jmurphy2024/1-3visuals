# ==== 0. ABOUT ====
## Script: ACS_hhincome_viz.R
## Logic: Household Universe (PERNUM 1) | Weight: HHWT

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# --- 1. CONFIGURATION ---
USER_IPUMS_SAMPLE_ID  <- "us2023b"
USER_INDICATOR_NAME   <- "avg_hh_income"
USER_WEIGHT_VARIABLE  <- "HHWT" 
PLOT_SUBFOLDER        <- "plots"
OUTPUT_PLOT_NAME      <- paste0("plot_", USER_INDICATOR_NAME, "_2023.png")

# Load Data and Borders ($15,904.48 version)
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))
prepared_data      <- readRDS(PREPARED_DATA_FILE)
main_cutoffs       <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds"))
borders_df         <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders_2023.csv"), show_col_types = FALSE)

# Assign groups using REAL_INCOME
data_with_groups <- assign_income_groups(prepared_data, borders_df, "REAL_INCOME", "Groups_20", 
                                         main_cutoffs$main_cutoff1, main_cutoffs$main_cutoff2)

# --- 2. SUMMARIZE & PLOT ---
summary_stats <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(avg_income = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE), .groups = "drop")

# Capture the ACTUAL plot object
# If your function saves and returns a string, we must wrap it or modify the function
final_plot <- create_single_line_plot(
  summary_data       = summary_stats,
  y_var              = "avg_income",
  plot_title         = "Average Household Income: The Three Countries (2019-2023)",
  y_axis_label       = "Real Household Income (2023 $)",
  y_axis_format      = "dollar",
  output_filename    = OUTPUT_PLOT_NAME,
  fine_group_level   = "Groups_20",
  border_t1_t2       = main_cutoffs$main_cutoff1,
  border_t2_t3       = main_cutoffs$main_cutoff2
)

# --- 3. RENDER CHECK ---
if (is.character(final_plot)) {
  message("Function returned a file path. Re-loading plot object for display...")
  # If the function only returns a path, we usually need to find where it saved it 
  # OR ensure the function actually returns the ggplot object at the end.
} else {
  grid::grid.newpage()
  grid::grid.draw(final_plot)
}

ggsave(here::here("03_output", PLOT_SUBFOLDER, OUTPUT_PLOT_NAME), width = 11, height = 7)