## WD location: 02_Scripts/III- Templates
## Script: NCVS_criminal_viz.R
## Purpose: Combined Prevalence for Criminal Incidents and Police Reporting behavior.
## Author: Janica Murphy, Maxwell Goshert EPAG/ Gemini

# ==== 0. ABOUT ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP ====
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"
OUTPUT_PLOT_NAME  <- "plot_criminal.png"
OUTPUT_TABLE_NAME <- "table_criminal.png"

# Ensure directories exist
if (!dir.exists(here::here("03_output", PLOT_SUBFOLDER))) {
  dir.create(here::here("03_output", PLOT_SUBFOLDER), recursive = TRUE)
}
if (!dir.exists(here::here("03_output", TABLE_SUBFOLDER))) {
  dir.create(here::here("03_output", TABLE_SUBFOLDER), recursive = TRUE)
}

# Load the prevalence dataset and tercile parameters
prepared_data <- readRDS(here::here("01_data", "processed", "NCVS_Microdata", "prepared_NCVS_Prevalence_2023.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign households to the Three Countries
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "HHINCOME",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# ==== 2. DATA SUMMARIZATION ====

# 2.1. Calculate Combined Incident Indicator
data_with_groups <- data_with_groups %>%
  mutate(ind_criminal_incident = if_else(ind_violent == 1 | ind_property == 1, 1, 0, missing = 0))

# 2.2. U.S. Population Baseline
total_population <- data_with_groups %>%
  summarise(
    Country = "NCVS 2023 Sample",
    n = n(),
    "Criminal Incidents" = weighted.mean(ind_criminal_incident, w = PERWT, na.rm = TRUE),
    "Police Reporting"   = weighted.mean(ind_reported, w = PERWT, na.rm = TRUE)
  )

# 2.3. Country Summaries
country_summary <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(
    n = n(),
    "Criminal Incidents" = weighted.mean(ind_criminal_incident, w = PERWT, na.rm = TRUE),
    "Police Reporting"   = weighted.mean(ind_reported, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>%
  select(Country, n, "Criminal Incidents", "Police Reporting")

# 2.4. Formatting for Table
table_data_final <- bind_rows(total_population, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.2f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

colnames(table_data_final) <- c(
  "bold('Country')", "bold(italic('n'))", 
  "bold('Total Criminal Incidents')", "bold('Police Reporting Prevalence')"
)

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Total Incidents" = weighted.mean(ind_criminal_incident, w = PERWT, na.rm = TRUE),
    "Reporting Rate"  = weighted.mean(ind_reported, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Total Incidents", "Reporting Rate"),
  y_labels         = c("Total Criminal Incidents", "Crimes Reported to Police"),
  plot_title       = "Criminal Incidents vs. Reporting Behavior",
  y_axis_label     = "Weighted Percentage of Population",
  y_axis_format    = "percent",
  output_filename  = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# Render plot in RStudio
grid.newpage(); grid.draw(final_plot)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final)
cols_n <- ncol(table_data_final)
adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n); adj_fontface[1, ] <- "bold"

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(
    base_family = "serif",
    core = list(fg_params = list(fontface = adj_fontface, hjust = adj_hjust, x = adj_x, fontsize = 10)),
    colhead = list(fg_params = list(fontsize = 11, parse = TRUE))
  )
)

# Detailed notes section
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.90),
  textGrob("1. Data: 2023 NCVS DS2, DS3, and DS5 files.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.75),
  textGrob("2. Total Incidents: Combined prevalence of violent and property crime indicators.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.60),
  textGrob("3. Reporting: Population-weighted percentage of individuals who reported any incident to law enforcement.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.45)
)

final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(6, "lines")))

# Export Table
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 950, height = 500, res = 120)
grid.draw(final_layout)
dev.off()

message("SUCCESS: Overall Incident Viz script complete.")