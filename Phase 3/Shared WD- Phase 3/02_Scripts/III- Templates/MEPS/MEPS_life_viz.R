# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/MEPS
## Script: MEPS_life_expectancy_viz.R
## Purpose: Final Viz fixing the "visualizations_final" path error and using 'subtables'.
## Author: Janica Murphy / Gemini Assistant
## Last Modified: 2026-01-27

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable)

# Load shared visual and utility functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP PATHS & DATA ====
# Aligning with NCVS folder structure
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"

# Ensure directories exist physically in 03_output
if (!dir.exists(here::here("03_output", PLOT_SUBFOLDER))) {
  dir.create(here::here("03_output", PLOT_SUBFOLDER), recursive = TRUE)
}
if (!dir.exists(here::here("03_output", TABLE_SUBFOLDER))) {
  dir.create(here::here("03_output", TABLE_SUBFOLDER), recursive = TRUE)
}

# Define filenames
OUTPUT_PLOT_NAME  <- "plot_meps_life_expectancy_pillars_2022.png"
OUTPUT_TABLE_NAME <- "table_meps_life_expectancy_summary_2022.png"

# FIX: Use an absolute path for the plotting function to avoid "jump-back" errors
ABS_PLOT_PATH  <- here::here("03_output", PLOT_SUBFOLDER, OUTPUT_PLOT_NAME)
ABS_TABLE_PATH <- here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME)

# Load prepared 2022 data
prepared_data <- readRDS(here::here("01_data", "processed", "IPUMS_MEPS", "prepared_meps_life_expectancy_2022.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign households to income groups
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "income_clean",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
) %>% filter(!is.na(income_tercile))

# ==== 2. DATA SUMMARIZATION ====

# 2.1. National Baseline
total_pop <- data_with_groups %>%
  summarise(
    Country = "MEPS 2022 National Sample",
    n = n(),
    "High Status"    = weighted.mean(ind_high_health_status, w = PERWEIGHT, na.rm = TRUE),
    "Chronic Burden" = weighted.mean(ind_chronic_risk, w = PERWEIGHT, na.rm = TRUE),
    "MH Provider"    = weighted.mean(ind_mh_event, w = PERWEIGHT, na.rm = TRUE)
  )

# 2.2. Country Summaries (The Three Countries)
country_summary <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(
    n = n(),
    "High Status"    = weighted.mean(ind_high_health_status, w = PERWEIGHT, na.rm = TRUE),
    "Chronic Burden" = weighted.mean(ind_chronic_risk, w = PERWEIGHT, na.rm = TRUE),
    "MH Provider"    = weighted.mean(ind_mh_event, w = PERWEIGHT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>%
  select(Country, n, "High Status", "Chronic Burden", "MH Provider")

# Formatting for Table
table_data_final <- bind_rows(total_pop, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.2f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

colnames(table_data_final) <- c(
  "bold('Country')", "bold(italic('n'))", 
  "bold('High Health Status')", "bold('Chronic Burden')", "bold('MH Provider Visit')"
)

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "High Status"    = weighted.mean(ind_high_health_status, w = PERWEIGHT, na.rm = TRUE),
    "Chronic Burden" = weighted.mean(ind_chronic_risk, w = PERWEIGHT, na.rm = TRUE),
    "MH Provider"    = weighted.mean(ind_mh_event, w = PERWEIGHT, na.rm = TRUE),
    .groups = "drop"
  )



final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("High Status", "Chronic Burden", "MH Provider"),
  y_labels         = c("High Health Status", "Chronic Risk Burden", "Mental Health Event"),
  plot_title       = "Life Expectancy Pillars: Health Outcomes and Care (2022)",
  y_axis_label     = "Weighted Population Prevalence (%)",
  y_axis_format    = "percent",
  # Providing the corrected Jump-Back path
  output_filename  = JUMP_BACK_PLOT_PATH, 
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final); cols_n <- ncol(table_data_final)
adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n); adj_fontface[1, ] <- "bold"

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(
    base_family = "serif",
    core = list(fg_params = list(fontface = adj_fontface, hjust = adj_hjust, x = adj_x, fontsize = 9)),
    colhead = list(fg_params = list(fontsize = 10, parse = TRUE))
  )
)

notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.90),
  textGrob("1. Source: 2022 IPUMS MEPS Hierarchical Records.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.78),
  textGrob("2. Chronic Burden: Aggregated physical and ADHD indicators.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.66),
  textGrob("3. MH Event: Persons with at least one MH-coded provider visit.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.54)
)

final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(8, "lines")))

# Save table using Absolute Path
png(ABS_TABLE_PATH, width = 1100, height = 600, res = 150)
grid.draw(final_layout); dev.off()

message("SUCCESS: Visuals exported to 03_output/subtables.")