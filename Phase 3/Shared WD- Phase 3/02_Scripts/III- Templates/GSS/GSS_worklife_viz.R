## WD location: 1-3 Visualization Analysis/Phase 3/Shared WD
## Script: GSS_worklife_viz.R
## Purpose: Generate weighted summary tables and multi-line plots for WLB metrics.
## Author: Janica Murphy, Gemini / User
## Date Created: 2026-01-27
## Dependencies: dplyr, ggplot2, grid, gridExtra, here, scales

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(scales)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. PARAMETERS ====
PLOT_SUBFOLDER    <- "plots"
TABLE_SUBFOLDER   <- "tables"
OUTPUT_PLOT_NAME  <- "plot_worklife.png"
OUTPUT_TABLE_NAME <- "table_worklife.png"

# Load the prepared prevalence dataset
prepared_data <- readRDS(here::here("01_Data", "processed", "GSS_Microdata", "prepared_gss_wlb_2024.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

# ==== 2. DATA SUMMARIZATION ====

# Use WTSSNRPS for weighted prevalence
summary_func <- function(df) {
  df %>% summarise(
    n = n(),
    "WLB Index"   = weighted.mean(ind_wlb_index, w = WTSSNRPS, na.rm = TRUE),
    "Sovereignty" = weighted.mean(ind_sovereignty, w = WTSSNRPS, na.rm = TRUE),
    "Support"     = weighted.mean(ind_support, w = WTSSNRPS, na.rm = TRUE),
    "Fulfillment" = weighted.mean(ind_fulfillment, w = WTSSNRPS, na.rm = TRUE)
  )
}

total_pop <- summary_func(prepared_data) %>% mutate(Country = "Full GSS Sample")

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
  select(Country, n, "WLB Index", "Sovereignty", "Support", "Fulfillment")

colnames(table_data_final) <- c(
  "bold('Country')", "bold(italic('n'))", "bold('WLB Index')", 
  "bold('Sovereignty')", "bold('Support')", "bold('Fulfillment')"
)

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- prepared_data %>% 
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "WLB Index"   = weighted.mean(ind_wlb_index, w = WTSSNRPS, na.rm = TRUE),
    "Sovereignty" = weighted.mean(ind_sovereignty, w = WTSSNRPS, na.rm = TRUE),
    "Support"     = weighted.mean(ind_support, w = WTSSNRPS, na.rm = TRUE),
    "Fulfillment" = weighted.mean(ind_fulfillment, w = WTSSNRPS, na.rm = TRUE), 
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("WLB Index", "Sovereignty", "Support", "Fulfillment"),
  y_labels         = c("Work-Life Balance Index", "Sovereignty Pillar", "Support Pillar", "Fulfillment Pillar"),
  plot_title       = "Work-Life Balance",
  y_axis_label     = "Weighted Percentage of Population",
  y_axis_format    = "percent",
  output_filename  = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

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

# REWRITTEN DESCRIPTIVE NOTES (Universal Logic)

# ==== 4. EXPORT SUMMARY TABLE IMAGE ====

# FIXED: Vertical spacing and structure following the NCVS template
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.90),
  
  # Note 1: Data Source
  textGrob("1. Data: 2024 General Social Survey (GSS).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.78),
  
  # Note 2: Interpretation Logic
  textGrob("2. Interpretation: Percentages reflect the weighted share of each income group achieving a 'High Capital State'.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.66),
  
  # Note 3: Sovereignty Pillar
  textGrob("3. Sovereignty: High if respondent works <= 40 hrs OR has a regular daytime schedule.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.54),
  
  # Note 4: Support Pillar
  textGrob("4. Support: High if family income >= $75k OR Married.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.42),
  
  # Note 5: Fulfillment Pillar
  textGrob("5. Fulfillment: High if satisfied with residence OR Very Happy.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.30),
  
  # Note 6: Index Logic
  textGrob("6. WLB Index: A composite metric requiring High Capital in all three pillars (AND logic).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.18)
)

# Increase height of notes section to 10 lines to accommodate the extra pillars
final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(10, "lines")))

# Save table using the standard width and optimized height
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 600, res = 120)
grid.draw(final_layout)
dev.off()

message("SUCCESS: WLB Viz script complete.")