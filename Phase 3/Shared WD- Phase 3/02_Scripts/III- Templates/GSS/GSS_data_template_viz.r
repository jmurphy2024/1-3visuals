# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## WD location: 02_Scripts/III-Data Prep Templates/GSS
## Script: GSS_data_template_viz.R
## Purpose: Visualizes GSS indicator and generates summary tables.
##          (UPDATED: Saves PNGs and CSVs to separate subfolders)
## Author: Max Goshert, Janica Murphy, EPAG / Gemini
## Date Created: 2025-10-02
## Date Modified: 2025-12-15
## Dependencies: dplyr, readr, here, rlang, ggplot2, tidyr, Hmisc
## Input: A processed RDS file from the Prepare script.
## Output: Final visualization PNG and Summary Table CSV/PNG in subfolders.

# ==== 0. SETUP ====
rm(list = ls()); gc()

# Check for and install Hmisc if missing
if (!require("Hmisc", quietly = TRUE)) install.packages("Hmisc")

library(dplyr); library(readr); library(here); library(rlang); library(purrr); library(stringr)
library(ggplot2); library(ggtext); library(glue); library(grid); library(gridExtra);
library(scales); library(ggnewscale); library(cowplot); library(tidyr); library(Hmisc)

# Source Shared Functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))


# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR VISUALIZATION ====
# ================================================================= #

USER_GSS_YEAR_ID     <- "GSS2024"
USER_INDICATOR_NAME  <- "Happiness" 
USER_WEIGHT_VARIABLE <- "wtssnrps"
USER_GSS_INCOME_CAT_VAR <- "real_inc_approx"
USER_FINE_GROUP_LEVEL <- "Groups_20" 

PLOT_TYPE         <- "single" 
Y_VAR             <- "indicator_value" 
PLOT_TITLE        <- "Happiness Rate by Income Quantile (GSS 2024)"
X_AXIS_LABEL      <- "Income Quantile Group"
Y_AXIS_LABEL      <- "Proportion Reporting 'Very Happy'"
Y_AXIS_FORMAT     <- "percent" 


# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

# --- 2.1. Define File Paths & Subfolders ---
PREPARED_DATA_DIR <- here::here("01_data", "processed", "GSS_Data")
BORDER_FILES_DIR <- here::here("01_data", "processed") 

# Define Output Folders
BASE_OUTPUT_DIR <- here::here("03_output", "visualizations_final")
PNG_DIR <- file.path(BASE_OUTPUT_DIR, "PNGs")
CSV_DIR <- file.path(BASE_OUTPUT_DIR, "CSVs")

# Create folders if missing
dir.create(PNG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CSV_DIR, showWarnings = FALSE, recursive = TRUE)

PREPARED_DATA_FILE <- file.path(PREPARED_DATA_DIR, paste0("prepared_", USER_GSS_YEAR_ID, "_", USER_INDICATOR_NAME, ".rds"))
MAIN_CUTOFFS_FILE <- file.path(BORDER_FILES_DIR, "main_tercile_cutoffs.rds")
WITHIN_BORDERS_FILE <- file.path(BORDER_FILES_DIR, "within_tercile_quantile_borders.csv")

OUTPUT_PLOT_FILENAME <- paste0("plot_", USER_INDICATOR_NAME, "_Quantile_", USER_GSS_YEAR_ID, ".png")

# --- 2.2. Load Data ---
if (!file.exists(PREPARED_DATA_FILE)) { stop(paste("FATAL ERROR: Prepared GSS data file not found at:", PREPARED_DATA_FILE)) }
prepared_data <- readRDS(PREPARED_DATA_FILE)
names(prepared_data) <- tolower(names(prepared_data)) 

if (!file.exists(MAIN_CUTOFFS_FILE)) { stop("FATAL ERROR: Main tercile cutoffs file not found.") }
if (!file.exists(WITHIN_BORDERS_FILE)) { stop("FATAL ERROR: Within-tercile borders file not found.") }
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)
within_tercile_borders <- readr::read_csv(WITHIN_BORDERS_FILE, show_col_types = FALSE)

# --- 2.3. Assign Income Groups ---
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df = within_tercile_borders,
  income_var_name = USER_GSS_INCOME_CAT_VAR, 
  detail_level = USER_FINE_GROUP_LEVEL,
  main_cutoff1 = main_cutoffs$main_cutoff1,
  main_cutoff2 = main_cutoffs$main_cutoff2
)

# --- 2.4. Calculate Plot Stats ---
summary_stats_grouped <- data_with_groups %>%
  filter(!is.na(income_tercile) & !is.na(fine_income_group) &
           !is.na(indicator_to_plot) & !is.na(.data[[USER_WEIGHT_VARIABLE]])) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    indicator_value = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  )

# --- 2.5. Generate Visualization ---
if (PLOT_TYPE == "single") {
  
  # Override default path to save into the 'PNGs' subfolder
  full_plot_path <- file.path("PNGs", OUTPUT_PLOT_FILENAME)
  
  create_single_line_plot(
    summary_data = summary_stats_grouped,
    y_var = Y_VAR, 
    plot_title = PLOT_TITLE,
    x_axis_label = X_AXIS_LABEL,
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
message("\n--- Generating Detailed Summary Tables ---")

# --- A. Setup & Helpers ---
summary_input <- data_with_groups %>%
  filter(!is.na(fine_income_group) & !is.na(indicator_to_plot) & !is.na(.data[[USER_WEIGHT_VARIABLE]])) %>%
  mutate(
    tercile_code = str_extract(fine_income_group, "^T\\d"),
    Tercile_Label = case_when(
      tercile_code == "T1" ~ "Bottom Third",
      tercile_code == "T2" ~ "Middle Third",
      tercile_code == "T3" ~ "Top Third",
      TRUE ~ "Unknown"
    )
  ) %>%
  filter(Tercile_Label != "Unknown")

# --- B. Calculate Statistics ---
stats_national <- summary_input %>%
  summarise(
    Tercile_Label = " National", 
    sort_key      = "00",
    Group_Name    = "Entire Sample",
    Mean          = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Count_N       = n()
  )

inc_var <- USER_GSS_INCOME_CAT_VAR
w_var   <- summary_input[[USER_WEIGHT_VARIABLE]]
q_breaks_global <- Hmisc::wtd.quantile(summary_input[[inc_var]], weights = w_var, probs = seq(0, 1, 0.25))

summary_input$Global_Quartile <- cut(
  summary_input[[inc_var]], 
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
    "Sample Size" = Count_N
  )

# Visual Table
custom_theme <- gridExtra::ttheme_default(
  core = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 9), bg_params = list(fill = "white", col = NA)),
  colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold", fontsize = 10), bg_params = list(fill = "white", col = NA))
)
table_grob <- gridExtra::tableGrob(display_table, rows = NULL, theme = custom_theme)

library(gtable); library(grid)
bottom_row_index <- nrow(display_table) + 1
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 3)), t = 1, b = 1, l = 1, r = ncol(display_table))
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 1)), t = 1, b = 1, l = 1, r = ncol(display_table))
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 3)), t = bottom_row_index, b = bottom_row_index, l = 1, r = ncol(display_table))

# Save Outputs
csv_filename <- paste0("summary_table_detailed_", USER_INDICATOR_NAME, ".csv")
write_csv(combined_stats, file.path(CSV_DIR, csv_filename))

png_filename <- paste0("summary_table_detailed_", USER_INDICATOR_NAME, ".png")
ggsave(file.path(PNG_DIR, png_filename), plot = table_grob, width = 10, height = 2 + (nrow(display_table) * 0.25), dpi = 300)

message(paste("Success! Files saved to separated PNGs/CSVs folders."))
message("\n--- Visualization script complete. ---")

rm(list=setdiff(ls(), c("summary_stats_grouped", lsf.str()))); gc()