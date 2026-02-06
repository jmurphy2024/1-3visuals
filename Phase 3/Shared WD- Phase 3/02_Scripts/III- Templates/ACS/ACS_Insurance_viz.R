# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: ACS_Health_Coverage_Dual_viz.R
## Purpose: Generate weighted summary tables and multi-line plots for Health Coverage.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-29

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(scales); library(readr); library(tidyr); library(gtable)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. PARAMETERS ====
PLOT_SUBFOLDER    <- "plots"
TABLE_SUBFOLDER   <- "tables"

USER_IPUMS_SAMPLE_ID  <- "us2024a" 
USER_INDICATOR_NAME   <- "Health_Coverage_Dual"
USER_WEIGHT_VARIABLE  <- "PERWT"

OUTPUT_PLOT_NAME  <- paste0("plot_", USER_INDICATOR_NAME, ".png")
OUTPUT_TABLE_NAME <- paste0("table_", USER_INDICATOR_NAME, ".png")

# Load prepared data
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

if (!file.exists(PREPARED_DATA_FILE)) stop(paste("File not found:", PREPARED_DATA_FILE))
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

# LOGIC VALIDATION: Filter out NA income groups
data_with_groups <- data_with_groups %>% filter(!is.na(income_tercile))

# ==== 2. DATA SUMMARIZATION ====

# Define summary function for the 2 coverage types
summary_func <- function(df) {
  df %>% summarise(
    n = n(),
    "Private Coverage" = weighted.mean(ind_private_coverage, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    "Public Coverage"  = weighted.mean(ind_public_coverage, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE)
  )
}

total_pop <- summary_func(data_with_groups) %>% mutate(Country = "ACS 2024 Sample")

tercile_summ <- data_with_groups %>% 
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
  select(Country, n, "Private Coverage", "Public Coverage")

colnames(table_data_final) <- c(
  "bold('Country')", "bold(italic('n'))", 
  "bold('Private Coverage')", "bold('Public Coverage')"
)

# ==== 3. GENERATE TREND PLOT (Using Multi-Line Function) ====

summary_plotting <- data_with_groups %>% 
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Private Coverage" = weighted.mean(ind_private_coverage, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    "Public Coverage"  = weighted.mean(ind_public_coverage, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE), 
    .groups = "drop"
  )

# Using create_multi_line_plot to generate 2 curves with legend
final_plot <- create_multi_line_plot(
  summary_data       = summary_plotting,
  y_vars             = c("Private Coverage", "Public Coverage"),
  y_labels           = c("Private Health Insurance", "Public Health Insurance"),
  plot_title         = "Health Insurance Coverage by Income",
  y_axis_label       = "Coverage Rate (%)",
  y_axis_format      = "percent",
  output_filename    = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level   = "Groups_20",
  border_t1_t2       = main_cutoffs$main_cutoff1,
  border_t2_t3       = main_cutoffs$main_cutoff2
)

# DISPLAY PLOT IN RSTUDIO
grid.newpage(); grid.draw(final_plot)

# ==== 4. EXPORT SUMMARY TABLE IMAGE ====
rows_n <- nrow(table_data_final); cols_n <- ncol(table_data_final)
# Adjusted matrices for 4 columns
adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n); adj_fontface[1, ] <- "bold"

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(base_family = "serif",
                         core = list(fg_params = list(fontface = adj_fontface, hjust = adj_hjust, x = adj_x, fontsize = 9)),
                         colhead = list(fg_params = list(fontsize = 10, parse = TRUE)))
)

# REWRITTEN DESCRIPTIVE NOTES (ACS Logic)
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.90),
  
  textGrob("1. Data: 2024 IPUMS ACS (American Community Survey).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.78),
  
  textGrob("2. Universe: All persons in households with reported Total Family Income (FTOTINC).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.66),
  
  textGrob("3. Private Coverage: Includes employer-based, direct-purchase, and TRICARE plans.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.54),
  
  textGrob("4. Public Coverage: Includes Medicare, Medicaid, VA, and other government programs.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.42),
  
  textGrob("5. Interpret: Percentages reflect the share of each income group with that specific coverage type.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.30)
)

final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(10, "lines")))

# Save table using the standard width
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 600, res = 120)
grid.draw(final_layout)
dev.off()

# Force final display
grid.newpage(); grid.draw(final_layout)
grid.newpage(); grid.draw(final_plot)

message("SUCCESS: ACS Dual-Coverage Viz script complete.")