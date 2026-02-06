# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: 03_visualize_income_wealth.R
## Purpose: Visualizes the "Big 4" Quality of Life Metrics.
##          (Updated: Plots 4 curves for Security, Balance, & Connection).
## Author: 1/3 Country Project Assistant
## Date Created: 2026-01-08

# ==== 0. SETUP ====
rm(list = ls()); gc()

if (!require("Hmisc", quietly = TRUE)) install.packages("Hmisc")
if (!require("ggtext", quietly = TRUE)) install.packages("ggtext")

library(dplyr); library(readr); library(here); library(rlang); library(purrr); library(stringr);
library(ggplot2); library(ggtext); library(glue); library(grid); library(gridExtra);
library(scales); library(ggnewscale); library(cowplot); library(tidyr); library(Hmisc)
library(gtable)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))


# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR VISUALIZATION ====
# ================================================================= #

# --- 1.1. Define Sample ID and Indicator Name ---
USER_ASEC_SAMPLE_ID <- "cps2023_03s"
USER_INDICATOR_NAME <- "Quality_of_Life_Composite" # Matches the Prepare script output

# --- 1.2. Specify Weighting Variable ---
USER_WEIGHT_VARIABLE <- "COMMON_WEIGHT" 

# --- 1.3. Specify Analysis Parameters ---
USER_FINE_GROUP_LEVEL <- "Groups_20" 

# --- 1.4. Define Plot Aesthetics ---
PLOT_TYPE         <- "multi"

# UPDATED: The 4 Key Metrics
Y_VARS            <- c("val_flag_food_secure", 
                       "val_flag_time_wealth", 
                       "val_flag_thriving_worker", 
                       "val_flag_reciprocal_neighbor") 

# UPDATED: Descriptive Labels
Y_LABELS          <- c("Secure Foundation (Food Security)", 
                       "Time Wealth (Balanced Work)", 
                       "Thriving Worker (Health + Purpose)", 
                       "Reciprocal Neighbor (Active Bonds)") 

# --- 1.5. Define Table Parameters ---
TABLE_VAR         <- "val_flag_thriving_worker" 
TABLE_VAR_LABEL   <- "Thriving Worker Rate (%)" 

# --- 1.6. Titles & Output Name ---
PLOT_TITLE        <- "Quality of Life: Security, Balance, and Connection"
Y_AXIS_LABEL      <- "Percent of Population"
LEGEND_TITLE      <- "Metric"
Y_AXIS_FORMAT     <- "percent" # Expects 0-1 input

# UPDATED: File saved as 'quality_of_life_4_curves'
OUTPUT_PLOT_FILENAME <- "quality_of_life_4_curves.png"


# ================================================================= #
# ==== 2. GENERIC LOGIC (NO CHANGES BELOW THIS LINE) ====
# ================================================================= #

# --- 2.1. Define File Paths & Subfolders ---
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_CPS_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))
BASE_OUTPUT_DIR <- here::here("03_output", "visualizations_final")
PNG_DIR <- file.path(BASE_OUTPUT_DIR, "PNGs")
CSV_DIR <- file.path(BASE_OUTPUT_DIR, "CSVs")
dir.create(PNG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CSV_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 2.2. Load Data and Borders ---
if (!file.exists(PREPARED_DATA_FILE)) { stop(paste("FATAL ERROR: Prepared data file not found at:", PREPARED_DATA_FILE)) }
prepared_data <- readRDS(PREPARED_DATA_FILE)
main_cutoffs <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
within_tercile_borders <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# --- 2.3. Assign Income Groups ---
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data, borders_df = within_tercile_borders,
  income_var_name = "HHINCOME", detail_level = USER_FINE_GROUP_LEVEL,
  main_cutoff1 = main_cutoffs$main_cutoff1, main_cutoff2 = main_cutoffs$main_cutoff2
)

# --- 2.4. Calculate Final Summary Statistics ---
summary_stats <- data_with_groups %>%
  filter(!is.na(income_tercile) & !is.na(fine_income_group)) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    across(all_of(Y_VARS), ~ weighted.mean(.x, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE)),
    .groups = "drop"
  )

# --- 2.5. Generate Visualization ---
if (PLOT_TYPE == "multi") {
  
  full_plot_path <- file.path("PNGs", OUTPUT_PLOT_FILENAME)
  
  create_multi_line_plot(
    summary_data = summary_stats, y_vars = Y_VARS, y_labels = Y_LABELS,
    plot_title = PLOT_TITLE, y_axis_label = Y_AXIS_LABEL, legend_title = LEGEND_TITLE,
    output_filename = full_plot_path, fine_group_level = USER_FINE_GROUP_LEVEL,
    border_t1_t2 = main_cutoffs$main_cutoff1, border_t2_t3 = main_cutoffs$main_cutoff2
  )
} else {
  message("This template is pre-configured for a 'multi' line plot. Adjust as needed.")
}

message("\n--- Visualization script complete. ---")


# ================================================================= #
# ==== 2.6. Generate Detailed Summary Tables ====
# ================================================================= #
message("\n--- Generating Detailed Summary Tables ---")

summary_input <- data_with_groups %>%
  filter(!is.na(fine_income_group)) %>%
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

total_weighted_sum <- sum(summary_input[[TABLE_VAR]] * summary_input[[USER_WEIGHT_VARIABLE]], na.rm = TRUE)

stats_national <- summary_input %>%
  summarise(
    Tercile_Label = " National", 
    sort_key      = "00",
    Group_Name    = "Entire Country",
    Mean          = weighted.mean(.data[[TABLE_VAR]], w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Weighted_N    = sum(.data[[TABLE_VAR]] * .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Share_Total   = Weighted_N / total_weighted_sum,
    N             = n()
  ) %>% select(-Weighted_N)

inc_var <- "HHINCOME" 
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
    Mean          = weighted.mean(.data[[TABLE_VAR]], w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Weighted_N    = sum(.data[[TABLE_VAR]] * .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Share_Total   = Weighted_N / total_weighted_sum,
    N             = n(),
    .groups = "drop"
  ) %>% select(-Global_Quartile, -Weighted_N)

stats_tercile <- summary_input %>%
  group_by(Tercile_Label) %>%
  summarise(
    sort_key      = "00", 
    Group_Name    = "Overall (Tercile Avg)",
    Mean          = weighted.mean(.data[[TABLE_VAR]], w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Weighted_N    = sum(.data[[TABLE_VAR]] * .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    Share_Total   = Weighted_N / total_weighted_sum,
    N             = n(),
    .groups = "drop"
  ) %>% select(-Weighted_N)

combined_stats <- bind_rows(stats_national, stats_global_quartiles, stats_tercile) %>%
  mutate(
    Tercile_Label = factor(Tercile_Label, levels = c(" National", " Quartiles", "Bottom Third", "Middle Third", "Top Third"))
  ) %>%
  arrange(Tercile_Label, sort_key) %>% 
  select(-sort_key)

display_table <- combined_stats %>%
  mutate(
    Mean = scales::percent(Mean, accuracy = 0.1), 
    Share_Total = scales::percent(Share_Total, accuracy = 0.1),
    N    = scales::comma(N)
  ) %>%
  rename(
    "Income Tier" = Tercile_Label,
    "Sub-Group" = Group_Name,
    !!TABLE_VAR_LABEL := Mean, 
    "Share of Total" = Share_Total,
    "Sample Size" = N
  )

custom_theme <- gridExtra::ttheme_default(
  core = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 9), bg_params = list(fill = "white", col = NA)),
  colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold", fontsize = 10), bg_params = list(fill = "white", col = NA))
)
table_grob <- gridExtra::tableGrob(display_table, rows = NULL, theme = custom_theme)

header_row_idx <- 1
last_data_row_idx <- nrow(display_table) + 1 
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 3)), t = header_row_idx, b = header_row_idx, l = 1, r = ncol(display_table))
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 1)), t = header_row_idx, b = header_row_idx, l = 1, r = ncol(display_table))
table_grob <- gtable_add_grob(table_grob, grobs = segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 3)), t = last_data_row_idx, b = last_data_row_idx, l = 1, r = ncol(display_table))

csv_filename <- paste0("summary_table_detailed_CPS_", USER_INDICATOR_NAME, ".csv")
write_csv(combined_stats, file.path(CSV_DIR, csv_filename))

ggsave(file.path(PNG_DIR, "quality_of_life_metrics_table.png"), plot = table_grob, width = 10, height = 2 + (nrow(display_table) * 0.25), dpi = 300)

message("Success! Files saved to separated PNGs/CSVs folders.")