# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## Script: UIED_absence_suspensions_viz.R
## Purpose: Visualizes Chronic Absenteeism by District Income.
## Author: Max Goshert, Janica Murphy, EPAG / Gemini
## Date Created: 2025-10-02
## Date Modified: 2025-12-15


# ==== 0. SETUP ====
rm(list = ls()); gc()
if (!require("Hmisc", quietly = TRUE)) install.packages("Hmisc")
library(dplyr); library(readr); library(here); library(ggplot2); library(Hmisc); library(stringr)
library(ggtext); library(glue); library(grid); library(gridExtra); library(scales); library(cowplot)
library(gtable); library(tidyr)

# Source Shared Functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #
USER_YEAR             <- 2017
USER_INDICATOR_NAME   <- "Chronic_Absence_Rate"
USER_WEIGHT_VARIABLE  <- "PERWT"    # Student Enrollment
USER_INCOME_VAR_NAME  <- "HHINCOME" # District Median Income

# --- Analysis Parameters ---
USER_FINE_GROUP_LEVEL <- "Groups_20" 

# --- Plot Aesthetics ---
PLOT_TYPE         <- "single"
Y_VAR             <- "indicator_value" 
PLOT_TITLE        <- "Chronic Student Absenteeism by District Income (2017)"
Y_AXIS_LABEL      <- "Chronic Absence Rate"
Y_AXIS_FORMAT     <- "percent" 

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

# --- 2.1. Define File Paths & Subfolders ---
# Smart detection for data folder
base_data_path <- here::here("01_data")
if (!dir.exists(base_data_path)) base_data_path <- here::here("01_Data")

PROCESSED_DIR    <- file.path(base_data_path, "processed", "Urban Institute Education Data")
BORDER_FILES_DIR <- file.path(base_data_path, "processed") 

# DEFINE OUTPUT DIRECTORY (Same as Shared Function default)
OUTPUT_DIR <- here::here("03_output", "visualizations_final")

# Define Subfolders
PNG_DIR <- file.path(OUTPUT_DIR, "PNGs")
CSV_DIR <- file.path(OUTPUT_DIR, "CSVs")

# Create folders if they don't exist
dir.create(PNG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CSV_DIR, showWarnings = FALSE, recursive = TRUE)

PREPARED_DATA_FILE   <- file.path(PROCESSED_DIR, paste0("prepared_UIED_Absence_", USER_YEAR, ".rds"))
MAIN_CUTOFFS_FILE    <- file.path(BORDER_FILES_DIR, "main_tercile_cutoffs.rds")
WITHIN_BORDERS_FILE  <- file.path(BORDER_FILES_DIR, "within_tercile_quantile_borders.csv")
OUTPUT_PLOT_FILENAME <- paste0("plot_UIED_", USER_INDICATOR_NAME, "_", USER_YEAR, ".png")

# --- 2.2. Load Data ---
if (!file.exists(PREPARED_DATA_FILE)) { stop("Prepared data file not found.") }
prepared_data <- readRDS(PREPARED_DATA_FILE)

if (!file.exists(MAIN_CUTOFFS_FILE)) { stop("FATAL: ACS main cutoffs not found.") }
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

if (!file.exists(WITHIN_BORDERS_FILE)) { stop("FATAL: ACS borders file not found.") }
within_tercile_borders <- readr::read_csv(WITHIN_BORDERS_FILE, show_col_types = FALSE)

message("Data and Shared Cutoffs loaded.")

# --- 2.3. Assign Income Groups ---
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df = within_tercile_borders,
  income_var_name = USER_INCOME_VAR_NAME, 
  detail_level = USER_FINE_GROUP_LEVEL,
  main_cutoff1 = main_cutoffs$main_cutoff1,
  main_cutoff2 = main_cutoffs$main_cutoff2
)

# Clean Labels
if(is.factor(data_with_groups$fine_income_group)) {
  levels(data_with_groups$fine_income_group) <- str_remove_all(levels(data_with_groups$fine_income_group), "<.*?>")
} else {
  data_with_groups$fine_income_group <- str_remove_all(data_with_groups$fine_income_group, "<.*?>")
}

# --- 2.4. Calculate Plot Stats ---
summary_stats <- data_with_groups %>%
  filter(!is.na(income_tercile) & !is.na(fine_income_group)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    indicator_value = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  )

# --- 2.5. Generate Plot ---
message("Generating Plot...")

# Override default filename to include subfolder path for shared function
full_plot_path <- file.path("PNGs", OUTPUT_PLOT_FILENAME)

create_single_line_plot(
  summary_data = summary_stats,
  y_var = Y_VAR,
  plot_title = PLOT_TITLE,
  y_axis_label = Y_AXIS_LABEL,
  output_filename = full_plot_path,
  fine_group_level = USER_FINE_GROUP_LEVEL,
  
  # PASS THE LOADED SHARED CUTOFFS
  border_t1_t2 = main_cutoffs$main_cutoff1,
  border_t2_t3 = main_cutoffs$main_cutoff2,
  
  t1_color = "#C0392B", 
  t2_color = "#F5B041", 
  t3_color = "#27AE60"
)

# ================================================================= #
# ==== 2.6. Generate Detailed Summary Tables (Added) ====
# ================================================================= #
message("\n--- Generating Detailed Summary Tables ---")

# --- A. Setup & Helpers ---
summary_input <- data_with_groups %>%
  filter(!is.na(fine_income_group) & !is.na(indicator_to_plot)) %>%
  mutate(
    tercile_code = str_extract(fine_income_group, "^T\\d"),
    Tercile_Label = case_when(
      tercile_code == "T1" ~ "Bottom Third",
      tercile_code == "T2" ~ "Middle Third",
      tercile_code == "T3" ~ "Top Third",
      TRUE ~ "Unknown"
    )
  ) %>% filter(Tercile_Label != "Unknown")

# --- B. Calculate Statistics ---
# 1. National
stats_national <- summary_input %>%
  summarise(
    Tercile_Label = " National", 
    sort_key      = "00",
    Group_Name    = "All Districts",
    Mean          = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Count_N       = n()
  )

# 2. Global Quartiles
w_var <- summary_input[[USER_WEIGHT_VARIABLE]]
q_breaks_global <- Hmisc::wtd.quantile(summary_input[[USER_INCOME_VAR_NAME]], weights = w_var, probs = seq(0, 1, 0.25))

summary_input$Global_Quartile <- cut(
  summary_input[[USER_INCOME_VAR_NAME]], 
  breaks = q_breaks_global, 
  labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"), 
  include.lowest = TRUE
)

stats_global_quartiles <- summary_input %>%
  filter(!is.na(Global_Quartile)) %>%
  group_by(Global_Quartile) %>%
  summarise(
    Tercile_Label = " Quartiles", 
    sort_key      = as.character(as.integer(first(Global_Quartile))), 
    Group_Name    = as.character(first(Global_Quartile)),
    Mean          = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Count_N       = n(),
    .groups = "drop"
  ) %>% select(-Global_Quartile)

# 3. Terciles
stats_tercile <- summary_input %>%
  group_by(Tercile_Label) %>%
  summarise(
    sort_key   = "00", 
    Group_Name = "Overall (Tercile Avg)",
    Mean       = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Count_N    = n(),
    .groups = "drop"
  )

# --- C. Combine & Output ---
combined_stats <- bind_rows(stats_national, stats_global_quartiles, stats_tercile) %>%
  mutate(Tercile_Label = factor(Tercile_Label, levels = c(" National", " Quartiles", "Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Tercile_Label, sort_key) %>% 
  select(-sort_key)

display_table <- combined_stats %>%
  mutate(
    Mean = scales::percent(Mean, accuracy = 0.1),
    Count_N = scales::comma(Count_N)
  ) %>%
  rename(
    "Income Tier" = Tercile_Label,
    "Sub-Group" = Group_Name,
    "Rate" = Mean,
    "Sample Size (Districts)" = Count_N
  )

# Visual Table
custom_theme <- gridExtra::ttheme_default(
  core = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 9), bg_params = list(fill = "white", col = NA)),
  colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold", fontsize = 10), bg_params = list(fill = "white", col = NA))
)
table_grob <- gridExtra::tableGrob(display_table, rows = NULL, theme = custom_theme)

# Add Academic Lines
bottom_row_index <- nrow(display_table) + 1
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 3)), t = 1, b = 1, l = 1, r = ncol(display_table))
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 1)), t = 1, b = 1, l = 1, r = ncol(display_table))
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 3)), t = bottom_row_index, b = bottom_row_index, l = 1, r = ncol(display_table))

# --- Save CSV (To CSVs Folder) ---
csv_filename <- paste0("summary_table_detailed_", USER_INDICATOR_NAME, ".csv")
write_csv(combined_stats, file.path(CSV_DIR, csv_filename))

# --- Save PNG (To PNGs Folder) ---
png_filename <- paste0("summary_table_detailed_", USER_INDICATOR_NAME, ".png")
ggsave(file.path(PNG_DIR, png_filename), plot = table_grob, width = 10, height = 2 + (nrow(display_table) * 0.25), dpi = 300)

message(paste("Success! Files saved to separated PNGs/CSVs folders."))
message("\n--- Visualization script complete. ---")