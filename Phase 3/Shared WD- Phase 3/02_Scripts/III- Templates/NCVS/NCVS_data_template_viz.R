## WD location: 02_Scripts/III- Templates
## Script: NCVS_data_template_viz.R
## Purpose: Generates Prevalence-based multi-line plot and summary table with 150k observations.
## Author: Janica Murphy, Maxwell Goshert EPAG/ Gemini
## Date Created: 2026-01-08
## Last Modified: 2026-01-08 4:06

# ==== 0. ABOUT ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP ====
# Load the 3-Way Join prevalence dataset
prepared_data <- readRDS(here::here("01_data", "processed", "NCVS_Microdata", "prepared_NCVS_Prevalence_2023.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

OUTPUT_PLOT_NAME  <- "plot_national_safety_gap_2023.png"
OUTPUT_TABLE_NAME <- "table_national_prevalence_summary_2023.png"

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
    "Property Prev."   = weighted.mean(ind_property, w = PERWT, na.rm = TRUE),
    "Police Interaction" = weighted.mean(ind_police, w = PERWT, na.rm = TRUE)
  )

# 2.2. Country Summaries
country_summary <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(
    n = n(),
    "Violent Prev."    = weighted.mean(ind_violent, w = PERWT, na.rm = TRUE),
    "Property Prev."   = weighted.mean(ind_property, w = PERWT, na.rm = TRUE),
    "Police Interaction" = weighted.mean(ind_police, w = PERWT, na.rm = TRUE),
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
  "bold('Property Crime Prevalence')", 
  "bold('Police Reporting Rate')"
)

# ==== 3. GENERATE TREND PLOT ====
# Summarize by 60 fine-grained income groups for a smooth trend line
summary_plotting <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Violent Victimization"  = weighted.mean(ind_violent, w = PERWT, na.rm = TRUE),
    "Property Victimization" = weighted.mean(ind_property, w = PERWT, na.rm = TRUE),
    "Police Interaction"     = weighted.mean(ind_police, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Violent Victimization", "Property Victimization", "Police Interaction"),
  y_labels         = c("Violent Victimization", "Property Victimization", "Police Interaction"),
  plot_title       = "Victimization Prevalence and Police Interaction",
  y_axis_label     = "Weighted Percentage of Population",
  y_axis_format    = "percent",
  output_filename  = OUTPUT_PLOT_NAME,
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# Display plot in RStudio
grid.newpage(); grid.draw(final_plot)

# ==== 4. EXPORT SUMMARY TABLE ====
# (Uses standard ttheme_minimal styling and 5-line project notes)
# Define table formatting
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

# Combine table with descriptive notes for final report
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.95),
  textGrob("1. 2023 National Crime Victimization Survey (NCVS) DS2 (Household), DS3 (Person), and DS5 (Incident)", gp = gpar(fontface = "italic", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.82),
  textGrob("2. Percentages represent the population weighted risk of victimization.", gp = gpar(fontface = "italic", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.69),
  textGrob("3. Analysis includes non-victims to establish a true national denominator.", gp = gpar(fontface = "italic", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.56)
)

final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(5, "lines")))

png(here::here("03_output", OUTPUT_TABLE_NAME), width = 1000, height = 480, res = 120)
grid.draw(final_layout)
dev.off()