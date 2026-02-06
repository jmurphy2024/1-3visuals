## WD location: /Users/jamurph8/ASU Dropbox/Janica Murphy/1-3 Visualization Analysis/Phase 3/Shared WD
## Script: GSS_communitytrust_viz.R
## Purpose: Generates Prevalence-based multi-line plot and summary table for Community Trust.
## Author: Janica Murphy / Gemini
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

# Ensure directories exist per NCVS template style
if (!dir.exists(here::here("03_output", PLOT_SUBFOLDER))) {
  dir.create(here::here("03_output", PLOT_SUBFOLDER), recursive = TRUE)
}
if (!dir.exists(here::here("03_output", TABLE_SUBFOLDER))) {
  dir.create(here::here("03_output", TABLE_SUBFOLDER), recursive = TRUE)
}

# Load the prepared deconstructed dataset
prepared_data <- readRDS(here::here("01_Data", "processed", "GSS_Microdata", "prepared_gss_CommunityTrust_2024.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))

OUTPUT_PLOT_NAME  <- "plot_communitytrust.png"
OUTPUT_TABLE_NAME <- "table_communitytrust.png"

# ==== 2. DATA SUMMARIZATION ====

# Helper for weighted prevalence (Individual Lines for each Marker)
summary_func <- function(df) {
  df %>% summarise(
    n = n(),
    "Integration" = weighted.mean(ind_social_integration, w = WTSSNRPS, na.rm = TRUE),
    "Trustworthy" = weighted.mean(ind_trust, w = WTSSNRPS, na.rm = TRUE),
    "Fair"        = weighted.mean(ind_fair, w = WTSSNRPS, na.rm = TRUE),
    "Helpful"     = weighted.mean(ind_helpful, w = WTSSNRPS, na.rm = TRUE)
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
  select(Country, n, "Integration", "Trustworthy", "Fair", "Helpful")

# Rename headers for tableGrob (NCVS format)
colnames(table_data_final) <- c(
  "bold('Country')", "bold(italic('n'))",  
  "bold('Integration')", "bold('Trustworthy')", "bold('Fair')", "bold('Helpful')"
)

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- prepared_data %>% 
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Integration" = weighted.mean(ind_social_integration, w = WTSSNRPS, na.rm = TRUE),
    "Trustworthy" = weighted.mean(ind_trust, w = WTSSNRPS, na.rm = TRUE),
    "Fair"        = weighted.mean(ind_fair, w = WTSSNRPS, na.rm = TRUE),
    "Helpful"     = weighted.mean(ind_helpful, w = WTSSNRPS, na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Integration", "Trustworthy", "Fair", "Helpful"),
  y_labels         = c("Social Integration", "Trustworthiness", "Fairness", "Helpfulness"),
  plot_title       = "Community Trust",
  y_axis_label     = "Weighted Percentage of Population",
  y_axis_format    = "percent",
  output_filename  = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final); cols_n <- ncol(table_data_final)
adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n); adj_fontface[1, ] <- "bold"

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(base_family = "serif",
                         core = list(fg_params = list(fontface = adj_fontface, hjust = adj_hjust, x = adj_x, fontsize = 9)),
                         colhead = list(fg_params = list(fontsize = 10, parse = TRUE)))
)

# REORDERED NOTES (NCVS Standard)
# UPDATED NOTES SECTION: Consolidated Single-Line Interpretation & Criteria
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.95),
  
  # Line 1: Data Source
  textGrob("1. Data: 2024 General Social Survey", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.86),
  
  # Line 2: Explicit Interpretation of "High Status"
  textGrob("2. Interpretation: Percentages reflect the prevalence of individuals achieving 'High Status' benchmarks within the total income group.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.77),
  
  # Line 3: Integration Criteria (Consolidated)
  textGrob("3. Integration: Weekly+ engagement with neighbors, friends, relatives, bars, or religious services.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.68),
  
  # Line 4: Attitudinal Criteria (Consolidated)
  textGrob("4. Attitudinal Markers: Positive perception on trustworthiness, fairness, and helpfulness.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.59)
 
)

# Adjusted heights for the consolidated single-line layout
final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(9, "lines")))

final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(9, "lines")))

# Save table image
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 1200, height = 850, res = 120)
grid.draw(final_layout); dev.off()

message("SUCCESS: Community Trust Deconstructed Viz complete.")