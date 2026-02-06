# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: CPS_employment_viz.R
## Purpose: Generates Prevalence-based line plot and summary table for Employment.
##          Denominator: Total working-age population (16+).
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-23

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP PATHS & DATA ====
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"

# Ensure directories exist
if (!dir.exists(here::here("03_output", PLOT_SUBFOLDER))) {
  dir.create(here::here("03_output", PLOT_SUBFOLDER), recursive = TRUE)
}
if (!dir.exists(here::here("03_output", TABLE_SUBFOLDER))) {
  dir.create(here::here("03_output", TABLE_SUBFOLDER), recursive = TRUE)
}

USER_IPUMS_SAMPLE_ID  <- "cps2023_03s"
USER_INDICATOR_NAME   <- "Employment_Rate"
USER_WEIGHT_VARIABLE  <- "PERWT" # Mapped from ASECWT in Prepare script

OUTPUT_PLOT_NAME  <- "plot_employmentrate.png"
OUTPUT_TABLE_NAME <- "table_employmentrate.png"

# Load prepared data
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_CPS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

prepared_data <- readRDS(PREPARED_DATA_FILE)
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign income groups
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "HHINCOME",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# LOGIC VALIDATION: Filter out NA income groups to maintain "Country" population integrity
data_with_groups <- data_with_groups %>%
  filter(!is.na(income_tercile))

# ==== 2. DATA SUMMARIZATION ====

# 2.1. U.S. Population Baseline
total_pop <- data_with_groups %>%
  summarise(
    Country = "CPS ASEC 2023 Universe",
    n = n(),
    "Employment Rate" = weighted.mean(ind_employed, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE)
  )

# 2.2. Country Summaries (Tercile Breakdown)
country_summary <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(
    n = n(),
    "Employment Rate" = weighted.mean(ind_employed, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>%
  select(Country, n, "Employment Rate")

# 2.3. Formatting for tableGrob
table_data_final <- bind_rows(total_pop, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.1f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

colnames(table_data_final) <- c("bold('Country')", "bold(italic('n'))", "bold('Employment Rate')")

# ==== 3. GENERATE TREND PLOT ====
summary_stats <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Employment Rate" = weighted.mean(ind_employed, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_single_line_plot(
  summary_data       = summary_stats,
  y_var              = "Employment Rate",
  plot_title         = "Employment Prevalence by Household Income",
  y_axis_label       = "Employment Rate (%)",
  y_axis_format      = "percent",
  output_filename    = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level   = "Groups_20",
  border_t1_t2       = main_cutoffs$main_cutoff1,
  border_t2_t3       = main_cutoffs$main_cutoff2
)

# DISPLAY IN RSTUDIO
grid.newpage(); grid.draw(final_plot)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final)
cols_n <- ncol(table_data_final)

adj_hjust <- matrix(rep(c(0, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n)
adj_fontface[1, ] <- "bold"

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(
    base_family = "serif",
    core = list(fg_params = list(fontface = adj_fontface, hjust = adj_hjust, x = adj_x, fontsize = 10)),
    colhead = list(fg_params = list(fontsize = 11, parse = TRUE), bg_params = list(fill = "#F2F2F2", col = "white"))
  )
)

# Standardized Population-Based Notes
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.90),
  textGrob("1. Source: 2023 IPUMS CPS ASEC (Annual Social and Economic Supplement).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.75),
  textGrob("2. Universe: Civilian non-institutionalized population age 16+ with known income.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.60),
  textGrob("3. Interpret: Denominator includes all adults (Employed, Unemployed, and Not in Labor Force).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.45),
  textGrob("4. Employment: Includes those 'at work' or 'with a job but not at work' during the survey week.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.30)
)

final_table_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(9, "lines")))

# DISPLAY TABLE IN RSTUDIO
grid.newpage(); grid.draw(final_table_layout)

# SAVE TABLE TO PNG
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 550, res = 120)
grid.draw(final_table_layout)
dev.off()

message("SUCCESS: CPS Employment visuals exported and displayed.")