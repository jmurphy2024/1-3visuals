## WD location: 02_Scripts/III- Templates
## Script: NCVS_victimizationrates_viz.R
## Purpose: Generates Prevalence-based multi-line plot and summary table.
## Author: Janica Murphy, Maxwell Goshert EPAG/ Gemini
## Date Created: 2026-01-08
## Last Modified: 2026-01-14

# ==== 0. ABOUT ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP ====
# Define subfolder names relative to 03_output
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"
OUTPUT_PLOT_NAME  <- "plot_victimization.png"
OUTPUT_TABLE_NAME <- "table_victimization.png"

# Create the subdirectories inside 03_output if they don't exist
if (!dir.exists(here::here("03_output", PLOT_SUBFOLDER))) {
  dir.create(here::here("03_output", PLOT_SUBFOLDER), recursive = TRUE)
}
if (!dir.exists(here::here("03_output", TABLE_SUBFOLDER))) {
  dir.create(here::here("03_output", TABLE_SUBFOLDER), recursive = TRUE)
}

# Load the 3-Way Join prevalence dataset
prepared_data <- readRDS(here::here("01_data", "processed", "NCVS_Microdata", "prepared_NCVS_Prevalence_2023.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

OUTPUT_PLOT_NAME  <- "plot_victimization.png"
OUTPUT_TABLE_NAME <- "table_victimization.png"

# Assign households to the Three Countries based on Income
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "HHINCOME",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# ==== 2. DATA SUMMARIZATION (Prevalence Rates) ====

# 2.1. U.S. Population Baseline
total_population <- data_with_groups %>%
  summarise(
    Country = "NCVS 2023 Sample",
    n = n(),
    "Violent Prev."    = weighted.mean(ind_violent, w = PERWT, na.rm = TRUE),
    "Property Prev."   = weighted.mean(ind_property, w = PERWT, na.rm = TRUE)
  )

# 2.2. Country Summaries
country_summary <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(
    n = n(),
    "Violent Prev."    = weighted.mean(ind_violent, w = PERWT, na.rm = TRUE),
    "Property Prev."   = weighted.mean(ind_property, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>%
  select(Country, n, contains("Prev."), contains("Interaction"))

# 2.3. Formatting for Table
table_data_final <- bind_rows(total_population, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.2f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

# Rename headers for the tableGrob parser
colnames(table_data_final) <- c(
  "bold('Country')", 
  "bold(italic('n'))", 
  "bold('Violent Crime Prevalence')", 
  "bold('Property Crime Prevalence')" 
)

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Violent Victimization"  = weighted.mean(ind_violent, w = PERWT, na.rm = TRUE),
    "Property Victimization" = weighted.mean(ind_property, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  )

# REVISION: Navigate out of the hardcoded 'visualizations_final' folder
final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Violent Victimization", "Property Victimization"),
  y_labels         = c("Violent Victimization", "Property Victimization"),
  plot_title       = "Victimization Prevalence",
  y_axis_label     = "Weighted Percentage of Population",
  y_axis_format    = "percent",
  output_filename  = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# REVISION 1: Display plot in RStudio Plot View
grid.newpage(); grid.draw(final_plot)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final)
cols_n <- ncol(table_data_final)

adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n)
adj_fontface[1, ] <- "bold"

table_grob <- tableGrob(
  table_data_final, 
  rows = NULL, 
  theme = ttheme_minimal(
    base_family = "serif",
    core = list(fg_params = list(fontface = adj_fontface, hjust = adj_hjust, x = adj_x, fontsize = 10)),
    colhead = list(fg_params = list(fontsize = 11, parse = TRUE))
  )
)
# UPDATED NOTES SECTION: Two-line version with expanded vertical spacing
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.95),
  
  # Line 1: Data Source
  textGrob("1. Data: 2023 National Crime Victimization Survey (NCVS) DS2, DS3, and DS5.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.82), # Increased gap from 0.95
  
  # Line 2: Interpretation (Consolidated)
  textGrob("2. Interpretation: Percentages represent the population weighted risk of victimization; includes non-victims for a national denominator.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.69)  # Increased gap from 0.82
)

final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(5, "lines")))

# REVISION 1: Display table in RStudio Plot View
grid.newpage(); grid.draw(final_layout)

# REVISION 2 & 3: Save table into the tables subfolder using here logic
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 1000, height = 480, res = 120)
grid.draw(final_layout)
dev.off()