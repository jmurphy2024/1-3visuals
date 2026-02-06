# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: CPS_ASEC_poverty_viz.R
## Purpose: Generates Prevalence-based multi-line plot and summary table for Poverty.
## Universe: All individuals with valid Weight + Income information.
## Author: Janica Murphy, Max Goshert, EPAG / Gemini
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

USER_INDICATOR_NAME  <- "poverty_rate"
USER_ASEC_SAMPLE_ID  <- "cps2023_03s"
USER_WEIGHT_VARIABLE <- "ASECWT" 

OUTPUT_PLOT_NAME  <- "plot_povertyrate.png"
OUTPUT_TABLE_NAME <- "table_povertyrate.png"

# Load prepared data
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))

if (!file.exists(PREPARED_DATA_FILE)) stop(paste("File not found at:", PREPARED_DATA_FILE))
prepared_data <- readRDS(PREPARED_DATA_FILE)

main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign households to income groups
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "HHINCOME",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
) %>% filter(!is.na(income_tercile))

# ==== 2. DATA SUMMARIZATION ====

# 2.1. U.S. Population Baseline
total_pop <- data_with_groups %>% 
  summarise(
    Country = "bold('CPS ASEC 2023 Universe')",
    n = n(),
    "Poverty Rate" = weighted.mean(ind_poverty, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE)
  )

# 2.2. Country Summaries (Tercile Breakdown)
country_summary <- data_with_groups %>% 
  group_by(income_tercile) %>% 
  summarise(
    n = n(),
    "Poverty Rate" = weighted.mean(ind_poverty, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>% 
  select(Country, n, everything(), -income_tercile)

# 2.3. Formatting for tableGrob
table_data_final <- bind_rows(total_pop, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.1f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

colnames(table_data_final) <- c("bold('Country')", "bold(italic('n'))", "bold('Poverty Rate')")

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Poverty Rate" = weighted.mean(ind_poverty, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Poverty Rate"),
  y_labels         = c("Poverty Prevalence"),
  plot_title       = "Poverty Rate by Household Income",
  y_axis_label     = "Weighted Prevalence (%)",
  y_axis_format    = "percent",
  # Path Logic: Standardized to match employment/housing scripts
  output_filename  = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final)
cols_n <- ncol(table_data_final)

# Formatting: Bold the first row (Universe)
parse_logic <- matrix(FALSE, nrow = rows_n, ncol = cols_n); parse_logic[1, ] <- TRUE 

adj_hjust <- matrix(rep(c(0, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5), each = rows_n), nrow = rows_n)

table_grob <- tableGrob(
  table_data_final, 
  rows = NULL, 
  theme = ttheme_minimal(
    base_family = "serif",
    core = list(
      fg_params = list(
        hjust = adj_hjust, 
        x = adj_x, 
        fontsize = 9.5,
        parse = parse_logic
      )
    ),
    colhead = list(
      fg_params = list(fontsize = 10.5, parse = TRUE), 
      bg_params = list(fill = "#F2F2F2", col = "white")
    )
  )
)

# Standardized Notes
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.90),
  textGrob("1. Source: 2023 IPUMS CPS ASEC Supplement.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.78),
  textGrob("2. Universe: All individuals with valid weight and household income information.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.66),
  textGrob("3. Interpret: Percentages represent the population prevalence of individuals below the poverty threshold.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.54),
  textGrob("4. Categories: Poverty identified as individuals in households below the threshold (POVERTY code 10).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.42)
)

final_table_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(8, "lines")))

# EXPORT AND DISPLAY
grid.newpage(); grid.draw(final_plot)
grid.newpage(); grid.draw(final_table_layout)

png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 550, res = 120)
grid.draw(final_table_layout)
dev.off()

message("SUCCESS: Poverty Rate visuals exported.")