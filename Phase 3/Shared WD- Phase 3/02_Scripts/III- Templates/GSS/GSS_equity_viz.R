## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: gss_EquityCapital_viz.R
## Purpose: Generates formatted Summary Tables and Multi-line Plots for Equity Capital.
## Author: Janica Murphy, Data Scientist Assistant / Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(scales)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. PARAMETERS ====
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"
OUTPUT_PLOT_NAME  <- "plot_equity.png"
OUTPUT_TABLE_NAME <- "table_equity.png"

# Load the prepared prevalence dataset (n=3,309) and borders
prepared_data <- readRDS(here::here("01_Data", "processed", "GSS_Microdata", "prepared_gss_EquityCapital_2024.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

# ==== 2. DATA SUMMARIZATION ====

# Use WTSSNRPS for weighted prevalence
summary_func <- function(df) {
  df %>% summarise(
    n = n(),
    "Equity Index"  = weighted.mean(ind_equity_index, w = WTSSNRPS, na.rm = TRUE),
    "Opportunity"   = weighted.mean(ind_opportunity, w = WTSSNRPS, na.rm = TRUE),
    "Resources"     = weighted.mean(ind_resources, w = WTSSNRPS, na.rm = TRUE),
    "Outcomes"      = weighted.mean(ind_outcomes, w = WTSSNRPS, na.rm = TRUE)
  )
}

total_pop <- summary_func(prepared_data) %>% mutate(Country = "GSS 2024 Sample")

tercile_summ <- prepared_data %>% 
  group_by(income_tercile) %>% 
  summary_func() %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  ))

# Formatting for tableGrob
table_data_final <- bind_rows(total_pop, tercile_summ) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.1f%%", . * 100))) %>%
  mutate(n = scales::comma(n)) %>% 
  select(Country, n, "Equity Index", "Opportunity", "Resources", "Outcomes")

colnames(table_data_final) <- c(
  "bold('Country')", "bold(italic('n'))", "bold('Equity Index')", 
  "bold('Opportunity')", "bold('Resources')", "bold('Outcomes')"
)

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- prepared_data %>% 
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Equity Index" = weighted.mean(ind_equity_index, w = WTSSNRPS, na.rm = TRUE),
    "Opportunity"  = weighted.mean(ind_opportunity, w = WTSSNRPS, na.rm = TRUE),
    "Resources"    = weighted.mean(ind_resources, w = WTSSNRPS, na.rm = TRUE),
    "Outcomes"     = weighted.mean(ind_outcomes, w = WTSSNRPS, na.rm = TRUE), 
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Equity Index", "Opportunity", "Resources", "Outcomes"),
  y_labels         = c("Composite Equity Index", "Opportunity Pillar", "Resources Pillar", "Outcomes Pillar"),
  plot_title       = "Opportunity, Resources, and Outcomes",
  y_axis_label     = "Weighted Percentage of Population",
  y_axis_format    = "percent",
  output_filename  = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# Render plot in RStudio Plot Panel
grid.newpage(); grid.draw(final_plot)

# ==== 4. EXPORT SUMMARY TABLE IMAGE ====
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

# REWRITTEN DESCRIPTIVE NOTES
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.95),
  
  # Line 1: Data Source (Standardized per GSS_communitytrust_viz.R)
  textGrob("1. Data: 2024 General Social Survey (GSS).", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.86),
  
  # Line 2: Interpretation
  textGrob("2. How to Read: Percentages represent the weighted share of that income group achieving a 'High Capital State'.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.77),
  
  # Line 3: Opportunity
  textGrob("3. Opportunity High Capital: Achieved if respondent has a Bachelor's+ (DEGREE), ranks social standing >= 6/10 (RANK), or believes hard work gets you ahead (GETAHEAD).", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.68),
  
  # Line 4: Resources
  textGrob("4. Resources High Capital: Achieved if family income >= $75k (REALINC), perceived finance is above average (FINRELA), or has confidence in major companies (CONBUS).", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.59),
  
  # Line 5: Outcomes
  textGrob("5. Outcomes High Capital: Achieved if reported health is Good/Excellent (HEALTH), is Pretty/Very Happy (HAPPY), or is satisfied with current job (SATJOB).", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.50),
  
  # Line 6: Equity Index
  textGrob("6. Equity Index: A restrictive composite metric requiring a 'High Capital State' in all three pillars simultaneously (Opp. AND Res. AND Out.).", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.41)
)

# INCREASED HEIGHTS: unit(12, "lines") provides more vertical room for the notes
final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(12, "lines")))

# INCREASED PNG HEIGHT: 850px ensures the entire layout fits without compression
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 1100, height = 850, res = 120)
grid.draw(final_layout); dev.off()

message("SUCCESS: Viz script complete. Plot and Table generated for full n=3,309.")