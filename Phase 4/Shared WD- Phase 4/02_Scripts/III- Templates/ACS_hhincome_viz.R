# ==== 0. ABOUT ====
## WD location:02_Scripts/III-Templates/ ACS
## Script: ACS_hhincome_viz
## Purpose: Visualizes Average Household Income (Bar + Dot + Line Chart).
##          Loads 'ipums_data_adjusted.rds' from 01_data/processed.
##          Automatically fixes column headers for shared functions.
## Author: Gemini / User
## Date Created: 2026-02-09
## Dependencies: dplyr, readr, here, ggplot2, cowplot, scales

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(cowplot); library(scales)

# Source the shared utility functions
# These provide the logic for 'assign_income_groups' and 'create_bar_dot_line_plot'
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))


# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR VISUALIZATION ====
# ================================================================= #

# --- 1.1. Data Sources ---
# We point to the 'adjusted' dataset you saved in the 01 folder
INPUT_DATA_FILE <- here::here("01_data", "processed", "ipums_data_adjusted.rds")

# We point to the Border files (created by II-C)
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")
BORDERS_CSV_FILE  <- here::here("01_data", "processed", "within_tercile_quantile_borders_2023.csv")

# --- 1.2. Analysis Parameters ---
USER_INDICATOR_NAME   <- "Average_Household_Income"
USER_INCOME_VAR       <- "REAL_INCOME"  # The RPP + Inflation adjusted variable
USER_WEIGHT_VAR       <- "HHWT"         # Household weights
USER_DETAIL_LEVEL     <- "Groups_20"    # Ventiles (5% groups)

# --- 1.3. Plot Aesthetics ---
PLOT_TITLE       <- "Average Household Income"
Y_AXIS_LABEL     <- "Average Household Income"
OUTPUT_FILENAME  <- paste0("IncomeBoundaries2023c.png")


# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

# --- 2.1. Load Data ---
message("Loading datasets...")

if(!file.exists(INPUT_DATA_FILE)) stop(paste("Fatal: Input file not found at", INPUT_DATA_FILE))
if(!file.exists(MAIN_CUTOFFS_FILE)) stop("Fatal: Cutoffs file not found. Run II-C Border Setup.")

acs_data     <- readRDS(INPUT_DATA_FILE)
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- CRITICAL FIX: Standardize Border Columns ---
# This ensures the script works whether your CSV uses "MainTercile" or "income_tercile_group"
borders_df   <- read_csv(BORDERS_CSV_FILE, show_col_types = FALSE) %>%
  rename(any_of(c(MainTercile = "income_tercile_group")))

message(paste("Loaded", nrow(acs_data), "households."))

# --- 2.2. Assign Income Groups ---
message("Assigning income groups (Ventiles)...")

# Uses shared utility II-A to assign 'fine_income_group'
data_grouped <- assign_income_groups(
  data_to_process = acs_data,
  borders_df      = borders_df,
  income_var_name = USER_INCOME_VAR,
  detail_level    = USER_DETAIL_LEVEL,
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# --- 2.3. Calculate Weighted Summary Statistics ---
message("Calculating weighted averages per group...")

summary_stats <- data_grouped %>%
  filter(!is.na(fine_income_group), !is.na(income_tercile)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    # Weighted Mean of Real Income
    avg_val = weighted.mean(.data[[USER_INCOME_VAR]], w = .data[[USER_WEIGHT_VAR]], na.rm = TRUE),
    .groups = "drop"
  )

# --- 2.4. Generate Visualization ---
message("Generating Bar-Dot-Line Chart...")

# Uses shared utility II-B to create the Bar + Dot + Line chart
create_bar_dot_line_plot(
  summary_data     = summary_stats,
  y_var            = "avg_val",
  plot_title       = PLOT_TITLE,
  y_axis_label     = Y_AXIS_LABEL,
  output_filename  = OUTPUT_FILENAME,
  
  # Configuration passed to shared function
  fine_group_level = USER_DETAIL_LEVEL,
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2,
  
  # Aesthetic overrides
  t1_color         = "#C0392B", 
  t2_color         = "#F5B041", 
  t3_color         = "#27AE60"
)

message(paste("\n--- Script Complete. Visualization saved to: 03_output/visualizations_final/", OUTPUT_FILENAME, "---"))