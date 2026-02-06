## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: gss_LifeSatisfaction_viz.R
## Purpose: Generates plots for Overall Life Satisfaction Infrastructure.
## Author: Janica Murphy, Max Goshert, EPAG / Gemini
## Created: January 21, 2026

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(scales)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. PARAMETERS & DIRECTORIES ====
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"
OUTPUT_PLOT_NAME  <- "plot_lifesatisfaction.png"
OUTPUT_TABLE_NAME <- "table_lifesatisfaction.png"


# Ensure directories exist per NCVS template style
if (!dir.exists(here::here("03_output", PLOT_SUBFOLDER))) {
  dir.create(here::here("03_output", PLOT_SUBFOLDER), recursive = TRUE)
}
if (!dir.exists(here::here("03_output", TABLE_SUBFOLDER))) {
  dir.create(here::here("03_output", TABLE_SUBFOLDER), recursive = TRUE)
}

# Load the prepared prevalence dataset (n=3,309)
prepared_data <- readRDS(here::here("01_Data", "processed", "GSS_Microdata", "prepared_gss_LifeSatisfaction_2024.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

OUTPUT_PLOT_NAME  <- "plot_lifesatisfaction.png"
OUTPUT_TABLE_NAME <- "table_lifesatisfaction.png"

# ==== 2. DATA SUMMARIZATION ====

# Helper for weighted prevalence across infrastructures
summary_func <- function(df) {
  df %>% summarise(
    n = n(),
    "Life Satisfaction" = weighted.mean(ind_life_sat_index, w = WTSSNRPS, na.rm = TRUE),
    "Economic Infra"    = weighted.mean(ind_econ_infra, w = WTSSNRPS, na.rm = TRUE),
    "Social Infra"      = weighted.mean(ind_soc_infra, w = WTSSNRPS, na.rm = TRUE),
    "Physical Infra"    = weighted.mean(ind_phys_infra, w = WTSSNRPS, na.rm = TRUE)
  )
}

# 2.1. U.S. Population Baseline
total_population <- summary_func(prepared_data) %>% 
  mutate(Country = "GSS 2024 Sample")

# 2.2. Country Summaries (Tercile breakdown)
country_summary <- prepared_data %>%
  group_by(income_tercile) %>%
  summary_func() %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  ))

# 2.3. Formatting for Table
table_data_final <- bind_rows(total_population, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.1f%%", . * 100))) %>%
  mutate(n = scales::comma(n)) %>%
  select(Country, n, "Life Satisfaction", "Economic Infra", "Social Infra", "Physical Infra")

# Rename headers for tableGrob
colnames(table_data_final) <- c(
  "bold('Country')", "bold(italic('n'))", "bold('Life Satisfaction')", 
  "bold('Economic')", "bold('Social')", "bold('Physical')"
)

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- prepared_data %>% 
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Overall Satisfaction" = weighted.mean(ind_life_sat_index, w = WTSSNRPS, na.rm = TRUE),
    "Economic" = weighted.mean(ind_econ_infra, w = WTSSNRPS, na.rm = TRUE),
    "Social"   = weighted.mean(ind_soc_infra, w = WTSSNRPS, na.rm = TRUE),
    "Physical" = weighted.mean(ind_phys_infra, w = WTSSNRPS, na.rm = TRUE), 
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Overall Satisfaction", "Economic", "Social", "Physical"),
  y_labels         = c("Overall Life Satisfaction", "Economic Infrastructure", "Social Infrastructure", "Physical Infrastructure"),
  plot_title       = "Life Satisfaction: The Combined Effectiveness of Infrastructures",
  y_axis_label     = "Weighted Percentage of Population",
  y_axis_format    = "percent",
  output_filename  = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# Display plot
grid.newpage(); grid.draw(final_plot)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final); cols_n <- ncol(table_data_final)

adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n); adj_fontface[1, ] <- "bold"

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(base_family = "serif",
                         core = list(fg_params = list(fontface = adj_fontface, hjust = adj_hjust, x = adj_x, fontsize = 9)),
                         colhead = list(fg_params = list(fontsize = 10, parse = TRUE)))
)

# EXPANDED NOTES: Detailing the "High Status" infrastructure definitions
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.95),
  
  # Line 1: Data Source (Moved to top to match Community Trust example)
  textGrob("1. Data: 2024 General Social Survey", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.86),
  
  # Line 2: Interpretation/Composite Definition
  textGrob("2. Interpretation: Overall Life Satisfaction is a restrictive composite requiring 'High Status' in all three pillars simultaneously.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.77),
  
  # Line 3: Infrastructure Definitions (Consolidated/Sequential)
  textGrob("3. Economic & Social: High Status if satisfied with job/finances (Economic) or frequency of social time/happiness (Social).", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.68),
  
  # Line 4: Physical Infrastructure
  textGrob("4. Physical Infrastructure: High Status if self-reported health is 'Excellent' or 'Good', reflecting health system effectiveness.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.59)
)

# INCREASED HEIGHTS: unit(12, "lines") provides more vertical room for the expanded notes
final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(12, "lines")))

# INCREASED PNG HEIGHT: 850px ensures the entire layout fits without compression
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 1100, height = 850, res = 120)
grid.draw(final_layout); dev.off()

message("SUCCESS: Life Satisfaction Viz script complete with expanded infrastructure notes.")
