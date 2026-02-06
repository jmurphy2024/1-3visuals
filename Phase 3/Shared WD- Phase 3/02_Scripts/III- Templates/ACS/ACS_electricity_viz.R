# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: Electricity_Metrics_viz.R
## Purpose: Generates Prevalence-based line plot and summary table for Electricity Insecurity.
## Author: Gemini / User
## Last Modified: 2026-01-27


# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP PATHS & DATA ====
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"

USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "Electricity_Metrics"
USER_WEIGHT_VARIABLE  <- "HHWT" 

OUTPUT_PLOT_NAME  <- "plot_electricity_insecurity.png"
OUTPUT_TABLE_NAME <- "table_electricity_insecurity.png"

# Load prepared data
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

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

data_with_groups <- data_with_groups %>% filter(!is.na(income_tercile))

# ==== 2. DATA SUMMARIZATION ====
# Removed Steal Proxy; focusing on No Elec and At-Risk
total_pop <- data_with_groups %>%
  summarise(
    Country = paste("ACS", gsub("us", "", USER_IPUMS_SAMPLE_ID), "Sample"),
    n = n(),
    "No Elec" = weighted.mean(ind_no_electricity, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    "At-Risk" = weighted.mean(ind_at_risk, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE)
  )

country_summary <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(
    n = n(),
    "No Elec" = weighted.mean(ind_no_electricity, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    "At-Risk" = weighted.mean(ind_at_risk, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>%
  select(Country, n, "No Elec", "At-Risk")

# Formatting for tableGrob
table_data_final <- bind_rows(total_pop, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.2f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

colnames(table_data_final) <- c("bold('Country')", "bold(italic('n'))", "bold('No Elec')", "bold('At-Risk')")

# ==== 3. GENERATE TREND PLOT ====
summary_stats <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "At-Risk" = weighted.mean(ind_at_risk, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_single_line_plot(
  summary_data       = summary_stats,
  y_var              = "At-Risk",
  plot_title         = "Prevalence of Electricity Insecurity (Energy Burden > 10%)",
  y_axis_label       = "Prevalence (%)",
  y_axis_format      = "percent",
  output_filename    = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level   = "Groups_20",
  border_t1_t2       = main_cutoffs$main_cutoff1,
  border_t2_t3       = main_cutoffs$main_cutoff2
)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final)
cols_n <- ncol(table_data_final)
# Adjusted matrix dimensions for 4 columns instead of 5
adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
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

notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.90),
  textGrob("1. Source: IPUMS ACS 2023 1-Year Sample", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.75),
  textGrob("2. No Electricity: Proxy based on households reporting no heating fuel source.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.60),
  textGrob("3. At-Risk: Energy burden exceeds 10% of gross household income.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.45)
)

final_table_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(7, "lines")))

# SAVE TABLE TO PNG
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 850, height = 500, res = 120)
grid.draw(final_table_layout)
dev.off()

# Display plots in RStudio
grid.newpage(); grid.draw(final_plot)
grid.newpage(); grid.draw(final_table_layout)

message("SUCCESS: Visuals exported without Steal Proxy.")