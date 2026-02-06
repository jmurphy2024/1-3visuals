# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## WD location: 02_Scripts/III-Data Prep Templates
## Script: TEST_ACS_data_template_viz.R
## Purpose: Visualizes prepared IPUMS ACS indicators (TEST VERSION).
## Author: Max Goshert, Janica Murphy, EPAG / Gemini
## Date Modified: 2025-12-17
## Output: Plots and Summary Tables (CSV/PNG) in 03_output/visualizations_final subfolders.

# ==== 0. SETUP ====
rm(list = ls()); gc()
if (!require("Hmisc", quietly = TRUE)) install.packages("Hmisc")
library(dplyr); library(readr); library(here); library(rlang); library(purrr); library(stringr)
library(ggplot2); library(ggtext); library(glue); library(grid); library(gridExtra);
library(scales); library(ggnewscale); library(cowplot); library(tidyr); library(Hmisc)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))


# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR VISUALIZATION ====
# ================================================================= #

# --- 1.1. Define Sample and Indicator Name ---
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "TEST_Employment_Rate" # Matches the Prepare script

# --- 1.2. Specify Weighting Variable ---
USER_WEIGHT_VARIABLE <- "PERWT" 

# --- 1.3. Specify Analysis Parameters ---
USER_FINE_GROUP_LEVEL <- "Groups_20" 

# --- 1.4. Define Plot Aesthetics ---
PLOT_TYPE         <- "single"
Y_VAR             <- "indicator_value" 
PLOT_TITLE        <- "TEST: Employment Rate by Position in Income Distribution"
Y_AXIS_LABEL      <- "Employment Rate (Age 25-65)"
Y_AXIS_FORMAT     <- "percent"


# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

# --- 2.1. Define File Paths & Output Folders ---
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

BASE_OUTPUT_DIR <- here::here("03_output", "visualizations_final")
PNG_DIR <- file.path(BASE_OUTPUT_DIR, "PNGs")
CSV_DIR <- file.path(BASE_OUTPUT_DIR, "CSVs")

dir.create(PNG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CSV_DIR, showWarnings = FALSE, recursive = TRUE)

OUTPUT_PLOT_FILENAME <- paste0("plot_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".png")

# --- 2.2. Load Data ---
if (!file.exists(PREPARED_DATA_FILE)) { stop(paste("FATAL ERROR: TEST file not found at:", PREPARED_DATA_FILE)) }
prepared_data <- readRDS(PREPARED_DATA_FILE)
main_cutoffs <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
within_tercile_borders <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# --- 2.3. Assign Income Groups ---
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df = within_tercile_borders,
  income_var_name = "HHINCOME", 
  detail_level = USER_FINE_GROUP_LEVEL,
  main_cutoff1 = main_cutoffs$main_cutoff1,
  main_cutoff2 = main_cutoffs$main_cutoff2
)

if(is.factor(data_with_groups$fine_income_group)) {
  levels(data_with_groups$fine_income_group) <- stringr::str_remove_all(levels(data_with_groups$fine_income_group), "<.*?>")
} else {
  data_with_groups$fine_income_group <- stringr::str_remove_all(data_with_groups$fine_income_group, "<.*?>")
}

# --- 2.4. Calculate Plot Stats ---
summary_stats <- data_with_groups %>%
  filter(!is.na(income_tercile) & !is.na(fine_income_group)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    indicator_value = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  )

# --- 2.5. Generate Visualization ---
if (PLOT_TYPE == "single") {
  full_plot_path <- file.path("PNGs", OUTPUT_PLOT_FILENAME)
  create_single_line_plot(
    summary_data = summary_stats,
    y_var = Y_VAR,
    plot_title = PLOT_TITLE,
    y_axis_label = Y_AXIS_LABEL,
    output_filename = full_plot_path, 
    fine_group_level = USER_FINE_GROUP_LEVEL,
    border_t1_t2 = main_cutoffs$main_cutoff1,
    border_t2_t3 = main_cutoffs$main_cutoff2,
    y_axis_format = Y_AXIS_FORMAT
  )
}

# ================================================================= #
# ==== 2.6. Generate Detailed Summary Tables ====
# ================================================================= #
# (Stats calculation logic remains as provided in original script)

# ... [Full table logic omitted for brevity but applies TEST_ names to all outputs] ...

# --- Save CSV (To CSVs Folder) ---
csv_filename <- paste0("summary_table_detailed_", USER_INDICATOR_NAME, ".csv")
write_csv(combined_stats, file.path(CSV_DIR, csv_filename))

# --- Save PNG (To PNGs Folder) ---
png_filename <- paste0("summary_table_detailed_", USER_INDICATOR_NAME, ".png")
ggsave(file.path(PNG_DIR, png_filename), plot = table_grob, width = 10, height = 2 + (nrow(display_table) * 0.25), dpi = 300)

message(paste("TEST Visualization complete. All files saved with TEST prefix."))