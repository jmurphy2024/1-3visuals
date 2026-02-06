# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## Script: NHIS_data_template_viz.R
## Purpose: Visualize NHIS 2018 data using Survey Design.
##          (UPDATED: Saves PNGs and CSVs to separate subfolders)
## Author: Max Goshert, Janica Murphy, EPAG / Gemini
## Date Created: 2025-10-02
## Date Modified: 2025-12-15
# ==== 0. SETUP ====
rm(list = ls()); gc()

if (!require("Hmisc", quietly = TRUE)) install.packages("Hmisc")
if (!require("survey", quietly = TRUE)) install.packages("survey")

library(dplyr); library(readr); library(here); library(ggplot2); library(survey); library(Hmisc)
library(ggtext); library(glue); library(grid); library(gridExtra);
library(scales); library(ggnewscale); library(cowplot); library(tidyr)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #
USER_IPUMS_COLLECTION <- "nhis"
USER_IPUMS_SAMPLES    <- c("ih2018") 
USER_INDICATOR_NAME   <- "Health_Insurance_Coverage"
USER_WEIGHT_VARIABLE  <- "WTFA_A"
USER_INCOME_VAR       <- "INC"    
USER_FINE_GROUP_LEVEL <- "Groups_20" 
PLOT_TYPE             <- "single"
Y_VAR                 <- "indicator_value" 
PLOT_TITLE            <- "Uninsured Rate by Family Income (NHIS 2018)"
Y_AXIS_LABEL          <- "Percent Uninsured"
Y_AXIS_FORMAT         <- "percent"

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #

# --- 2.1. File Paths & Subfolders ---
SAMPLES_TAG <- paste(USER_IPUMS_SAMPLES, collapse = "_")
PREPARED_DATA_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
BORDER_FILES_DIR <- here::here("01_data", "processed")

# Define Output Folders
BASE_OUTPUT_DIR <- here::here("03_output", "visualizations_final")
PNG_DIR <- file.path(BASE_OUTPUT_DIR, "PNGs")
CSV_DIR <- file.path(BASE_OUTPUT_DIR, "CSVs")

# Create folders if missing
dir.create(PNG_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(CSV_DIR, showWarnings = FALSE, recursive = TRUE)

PREPARED_DATA_FILE <- file.path(PREPARED_DATA_DIR, paste0("prepared_NHIS_", USER_INDICATOR_NAME, "_", SAMPLES_TAG, ".rds"))
OUTPUT_PLOT_FILENAME <- paste0("plot_NHIS_", USER_INDICATOR_NAME, "_", SAMPLES_TAG, ".png")

# --- 2.2. Load Data ---
if (!file.exists(PREPARED_DATA_FILE)) { stop("FATAL ERROR: Prepared data not found.") }
prepared_data <- readRDS(PREPARED_DATA_FILE)
main_cutoffs <- readRDS(file.path(BORDER_FILES_DIR, "main_tercile_cutoffs.rds"))
within_borders <- readr::read_csv(file.path(BORDER_FILES_DIR, "within_tercile_quantile_borders.csv"), show_col_types = FALSE)
message("Data loaded.")

# --- 2.3. Assign Income Groups ---
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df = within_borders,
  income_var_name = USER_INCOME_VAR, 
  detail_level = USER_FINE_GROUP_LEVEL,
  main_cutoff1 = main_cutoffs$main_cutoff1,
  main_cutoff2 = main_cutoffs$main_cutoff2
)

# --- 2.4. Stats (Survey Design) ---
message("Calculating stats...")
clean_input <- data_with_groups %>%
  filter(!is.na(income_tercile) & !is.na(fine_income_group) & 
           !is.na(indicator_to_plot) & !is.na(.data[[USER_WEIGHT_VARIABLE]]) &
           !is.na(PPSU) & !is.na(PSTRAT))

nhissvy <- svydesign(id = ~PPSU, strata = ~PSTRAT, nest = TRUE, weights = ~get(USER_WEIGHT_VARIABLE), data = clean_input)

summary_stats <- svyby(formula = ~indicator_to_plot, by = ~income_tercile + fine_income_group, design = nhissvy, FUN = svymean, na.rm = TRUE) %>%
  rename(indicator_value = indicator_to_plot)

# --- 2.5. Visualize ---
message("Generating Plot...")
if (PLOT_TYPE == "single") {
  
  # Override default path by passing subfolder explicitly
  full_plot_path <- file.path("PNGs", OUTPUT_PLOT_FILENAME)
  
  create_single_line_plot(
    summary_data = summary_stats,
    y_var = Y_VAR,
    plot_title = PLOT_TITLE,
    x_axis_label = "Income Quantile Group",
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
message("\n--- Generating Summary Tables ---")

# 1. Setup Base Data
summary_input <- clean_input %>%
  mutate(
    tercile_code = str_extract(fine_income_group, "^T\\d"),
    Tercile_Label = case_when(
      tercile_code == "T1" ~ "Bottom Third",
      tercile_code == "T2" ~ "Middle Third",
      tercile_code == "T3" ~ "Top Third",
      TRUE ~ "Unknown"
    )
  ) %>% filter(Tercile_Label != "Unknown")

# 2. Calculate Stats Groups
stats_national <- summary_input %>%
  summarise(Tercile_Label = " National", sort_key = "00", Group_Name = "Entire Sample",
            Mean = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE), N = n())

inc_var <- USER_INCOME_VAR
w_var   <- summary_input[[USER_WEIGHT_VARIABLE]]
q_breaks <- Hmisc::wtd.quantile(summary_input[[inc_var]], weights = w_var, probs = seq(0, 1, 0.25))
summary_input$Global_Quartile <- cut(summary_input[[inc_var]], breaks = q_breaks, labels = c("Q1 (Lowest)", "Q2", "Q3", "Q4 (Highest)"), include.lowest = TRUE)

stats_quartiles <- summary_input %>% filter(!is.na(Global_Quartile)) %>% group_by(Global_Quartile) %>%
  summarise(Tercile_Label = " Quartiles", sort_key = as.character(as.integer(first(Global_Quartile))), 
            Group_Name = as.character(first(Global_Quartile)),
            Mean = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE), N = n(), .groups = "drop") %>% select(-Global_Quartile)

stats_tercile <- summary_input %>% group_by(Tercile_Label) %>%
  summarise(sort_key = "00", Group_Name = "Overall (Tercile Avg)",
            Mean = weighted.mean(indicator_to_plot, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE), N = n(), .groups = "drop")

# 3. Combine & Output
combined_stats <- bind_rows(stats_national, stats_quartiles, stats_tercile) %>%
  mutate(Tercile_Label = factor(Tercile_Label, levels = c(" National", " Quartiles", "Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Tercile_Label, sort_key) %>% select(-sort_key)

display_table <- combined_stats %>%
  mutate(Mean = scales::percent(Mean, accuracy = 0.1), N = scales::comma(N)) %>%
  rename("Income Tier" = Tercile_Label, "Sub-Group" = Group_Name, "Rate" = Mean, "Sample Size" = N)

# 4. Visual Table Logic
custom_theme <- gridExtra::ttheme_default(
  core = list(fg_params = list(hjust = 0, x = 0.05, fontsize = 9), bg_params = list(fill = "white", col = NA)),
  colhead = list(fg_params = list(hjust = 0, x = 0.05, fontface = "bold", fontsize = 10), bg_params = list(fill = "white", col = NA))
)
table_grob <- gridExtra::tableGrob(display_table, rows = NULL, theme = custom_theme)

bottom_row_index <- nrow(display_table) + 1
table_grob <- gtable::gtable_add_grob(table_grob, grobs = grid::segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 3)), t = 1, b = 1, l = 1, r = ncol(display_table))
table_grob <- gtable::gtable_add_grob(table_grob, grobs = grid::segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 1)), t = 1, b = 1, l = 1, r = ncol(display_table))
table_grob <- gtable::gtable_add_grob(table_grob, grobs = grid::segmentsGrob(y0 = unit(0,"npc"), y1 = unit(0,"npc"), gp = gpar(lwd = 3)), t = bottom_row_index, b = bottom_row_index, l = 1, r = ncol(display_table))

# 5. Save Outputs
csv_filename <- paste0("summary_table_detailed_NHIS_", USER_INDICATOR_NAME, ".csv")
write_csv(combined_stats, file.path(CSV_DIR, csv_filename))

png_filename <- paste0("summary_table_detailed_NHIS_", USER_INDICATOR_NAME, ".png")
ggsave(file.path(PNG_DIR, png_filename), plot = table_grob, width = 10, height = 2 + (nrow(display_table) * 0.25), dpi = 300)

message("Success! Files saved to separated PNGs/CSVs folders.")