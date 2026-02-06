## WD location: 02_Scripts/III- Visuals/MEPS
## Script: MEPS_Infrastructure_viz.R
## Purpose: National Infrastructure Analysis (Split Insurance: Public vs Private).
##          Generates: (1) 60-Group Loess Trend Plot, (2) Detailed Summary Table.
## Author: Janica Murphy, Maxwell Goshert, Gemini Thought Partner

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable)

# Load shared 1/3 Country Project functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. DATA INGESTION & BORDER ALIGNMENT ====
# NOTE: Ensure the PREPARE script with the MEPSID bridge has been run first
prepared_data <- readRDS(here::here("01_data", "processed", "IPUMS_MEPS", "prepared_meps_infrastructure_2023.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

OUTPUT_PLOT_NAME  <- "plot_meps_infrastructure_trend_2023.png"
OUTPUT_TABLE_NAME <- "table_meps_infrastructure_summary_2023.png"

# Assign households to the Three Countries using 60 fine-grained income groups
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "income_clean",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# ==== 2. DATA SUMMARIZATION (National Prevalence) ====

# 2.1. U.S. Population Baseline
total_population <- data_with_groups %>%
  summarise(
    Country = "MEPS 2023 National",
    n = n(),
    "Private/Employer" = weighted.mean(ind_private_ins, w = PERWEIGHT, na.rm = TRUE),
    "Public Only"      = weighted.mean(ind_public_ins, w = PERWEIGHT, na.rm = TRUE),
    "Primary Care"     = weighted.mean(ind_primary_visit, w = PERWEIGHT, na.rm = TRUE),
    "ER Visit"         = weighted.mean(ind_acute_visit, w = PERWEIGHT, na.rm = TRUE),
    "Specialized Care" = weighted.mean(ind_specialized_visit, w = PERWEIGHT, na.rm = TRUE)
  )

# 2.2. Country Summaries
country_summary <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(
    n = n(),
    "Private/Employer" = weighted.mean(ind_private_ins, w = PERWEIGHT, na.rm = TRUE),
    "Public Only"      = weighted.mean(ind_public_ins, w = PERWEIGHT, na.rm = TRUE),
    "Primary Care"     = weighted.mean(ind_primary_visit, w = PERWEIGHT, na.rm = TRUE),
    "ER Visit"         = weighted.mean(ind_acute_visit, w = PERWEIGHT, na.rm = TRUE),
    "Specialized Care" = weighted.mean(ind_specialized_visit, w = PERWEIGHT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>%
  select(Country, n, `Private/Employer`, `Public Only`, `Primary Care`, `ER Visit`, `Specialized Care`)

# 2.3. Formatting for Table
table_data_final <- bind_rows(total_population, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.2f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

colnames(table_data_final) <- c(
  "bold('Geography')", "bold(italic('n'))", 
  "bold('Private/Employer')", "bold('Public Only')", 
  "bold('Primary Care')", "bold('ER Visit')", "bold('Specialized Care')"
)

# ==== 3. GENERATE TREND PLOT ====
# Loess smoothing visualizes the framework across the 'National Container'
summary_plotting <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Private/Employer Insurance" = weighted.mean(ind_private_ins, w = PERWEIGHT, na.rm = TRUE),
    "Government Insurance"       = weighted.mean(ind_public_ins, w = PERWEIGHT, na.rm = TRUE),
    "Primary Care Visit"         = weighted.mean(ind_primary_visit, w = PERWEIGHT, na.rm = TRUE),
    "ER Visit"                   = weighted.mean(ind_acute_visit, w = PERWEIGHT, na.rm = TRUE),
    "Specialized Care Visit"     = weighted.mean(ind_specialized_visit, w = PERWEIGHT, na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Private/Employer Insurance", "Government Insurance", "Primary Care Visit", "ER Visit", "Specialized Care Visit"),
  y_labels         = c("Private/Employer Insurance", "Government Insurance", "Primary Care Visit", "ER Visit", "Specialized Care Visit"),
  plot_title       = "Medical Infrastructure Catalyst Prevalence",
  y_axis_label     = "Weighted National Prevalence (%)",
  y_axis_format    = "percent",
  output_filename  = OUTPUT_PLOT_NAME,
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# ==== 4. EXPORT SUMMARY TABLE & NOTES ====
rows_n <- nrow(table_data_final); cols_n <- ncol(table_data_final)
adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n); adj_fontface[1, ] <- "bold"

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(
    base_family = "serif",
    core = list(fg_params = list(fontface = adj_fontface, hjust = adj_hjust, x = adj_x, fontsize = 9)),
    colhead = list(fg_params = list(fontsize = 10, parse = TRUE), bg_params = list(fill = "#F2F2F2", col = "white"))
  )
)

# Comprehensive Institutional Notes
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.95),
  textGrob("1. Data Source: 2023 IPUMS MEPS Full-Year Consolidated and Event Records.", gp = gpar(fontface = "italic", fontsize = 8.2, family = "serif"), hjust = 0, x = 0.05, y = 0.86),
  textGrob("2. Universe: The estimated 2023 U.S. civilian non-institutionalized population, which excludes persons residing in nursing homes, prisons, or on active military duty.", gp = gpar(fontface = "italic", fontsize = 8.2, family = "serif"), hjust = 0, x = 0.05, y = 0.77),
  textGrob("3. Percentages reflect weighted prevalence across the entire population segment, including non-utilizers.", gp = gpar(fontface = "italic", fontsize = 8.2, family = "serif"), hjust = 0, x = 0.05, y = 0.68),
  textGrob("4. Private/Employer Insurance: Any private health insurance, including mixed coverage (COVERTYPE=1).", gp = gpar(fontface = "italic", fontsize = 8.2, family = "serif"), hjust = 0, x = 0.05, y = 0.59),
  textGrob("5. Public Only Insurance: Government coverage (Medicare/Medicaid) for those with no private insurance (COVERTYPE=2).", gp = gpar(fontface = "italic", fontsize = 8.2, family = "serif"), hjust = 0, x = 0.05, y = 0.50),
  textGrob("6. Primary Care Visit: General checkup (01) or Well child exam (09). ER Visit: Emergency medical events (03).", gp = gpar(fontface = "italic", fontsize = 8.2, family = "serif"), hjust = 0, x = 0.05, y = 0.41),
  textGrob("7. Specialized Care Visit: Includes Diagnosis/Treatment, Psychotherapy, Follow-up, Immunizations, Vision, Pregnancy, or Laser surgery.", gp = gpar(fontface = "italic", fontsize = 8.0, family = "serif"), hjust = 0, x = 0.05, y = 0.32)
)

final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(10, "lines")))

dir.create(here::here("03_output"), showWarnings = FALSE, recursive = TRUE)
png(here::here("03_output", OUTPUT_TABLE_NAME), width = 1450, height = 700, res = 150)
grid.draw(final_layout); dev.off()

message("SUCCESS: Unified MEPS Viz with split COVERTYPE insurance and universe notes exported.")