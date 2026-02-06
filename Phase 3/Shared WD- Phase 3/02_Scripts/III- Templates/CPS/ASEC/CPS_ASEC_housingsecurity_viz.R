## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: ASEC_Housing_viz.R
## Purpose: Generates Prevalence-based multi-line plot and summary table for Housing Pillars.
## Universe: All individuals with valid Housing + Income information.
## Author: Janica Murphy, Max Goshert, EPAG / Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable)

# Source shared functions for grouping and plotting
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP PATHS & DATA ====
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"

# Ensure directories exist according to project structure
if (!dir.exists(here::here("03_output", PLOT_SUBFOLDER))) {
  dir.create(here::here("03_output", PLOT_SUBFOLDER), recursive = TRUE)
}
if (!dir.exists(here::here("03_output", TABLE_SUBFOLDER))) {
  dir.create(here::here("03_output", TABLE_SUBFOLDER), recursive = TRUE)
}

USER_INDICATOR_NAME <- "housing_security"
USER_ASEC_SAMPLE_ID <- "cps2023_03s"

OUTPUT_PLOT_NAME  <- "plot_housingsecurity.png"
OUTPUT_TABLE_NAME <- "table_housingsecurity.png"

# Load data prepared with the inclusive universe logic
prepared_data <- readRDS(here::here("01_data", "processed", "IPUMS_Microdata", "prepared_housing_2023.rds"))
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign households to income groups (60 fine-grained bins)
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "HHINCOME",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
) %>% filter(!is.na(income_tercile))

# ==== 2. DATA SUMMARIZATION ====

# Helper function for weighted housing statistics
summarize_housing <- function(df) {
  summarise(df,
            n = n(),
            "Homeowners"       = weighted.mean(ind_homeowner, w = ASECWT, na.rm = TRUE),
            "Private Renters"  = weighted.mean(ind_private_renter, w = ASECWT, na.rm = TRUE),
            "Assisted Renters" = weighted.mean(ind_housing_assist, w = ASECWT, na.rm = TRUE))
}

# ==== 3. GENERATE SUMMARY TABLE ====

# 3.1 Statistics
# UPDATED: Wrapping the label in bold() for the tableGrob parser
total_pop <- data_with_groups %>% 
  mutate(Country = "bold('ASEC 2023 Universe')") %>% 
  group_by(Country) %>% 
  summarize_housing()

country_summary <- data_with_groups %>% 
  group_by(income_tercile) %>% 
  summarize_housing() %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>% 
  select(Country, n, everything(), -income_tercile)

# 3.2 Formatting
table_data_final <- bind_rows(total_pop, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.1f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

# Column headers also use the bold() syntax
colnames(table_data_final) <- c(
  "bold('Country')", "bold(italic('n'))", 
  "bold('Homeowners')", "bold('Private Renters')", "bold('Assisted Renters')"
)

# ==== 3. GENERATE TREND PLOT ====
summary_plotting <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Homeowners"       = weighted.mean(ind_homeowner, w = ASECWT, na.rm = TRUE),
    "Private Renters"  = weighted.mean(ind_private_renter, w = ASECWT, na.rm = TRUE),
    "Assisted Renters" = weighted.mean(ind_housing_assist, w = ASECWT, na.rm = TRUE),
    .groups = "drop"
  )

final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Homeowners", "Private Renters", "Assisted Renters"),
  y_labels         = c("Homeownership", "Private Market Renter", "Housing Assistance"),
  plot_title       = "Housing Tenure Distribution",
  y_axis_label     = "Weighted Prevalence (%)",
  y_axis_format    = "percent",
  output_filename  = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# ==== 4. EXPORT SUMMARY TABLE ====
rows_n <- nrow(table_data_final)
cols_n <- ncol(table_data_final)

# Create a parsing logic matrix: TRUE for Row 1 (Universe), FALSE for others
# This forces the bold() expression to render as actual bold text
parse_logic <- matrix(FALSE, nrow = rows_n, ncol = cols_n)
parse_logic[1, ] <- TRUE 

adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)

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
        parse = parse_logic # KEY FIX: Apply conditional parsing
      )
    ),
    colhead = list(
      fg_params = list(fontsize = 10.5, parse = TRUE), 
      bg_params = list(fill = "#F2F2F2", col = "white")
    )
  )
)
# UPDATED: Universe statement as Note #2
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.90),
  textGrob("1. Source: 2023 IPUMS CPS ASEC.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.78),
  textGrob("2. Universe: All individuals with valid housing and household income information.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.66),
  textGrob("3. Interpret: Percentages represent the weighted share of the population in mutually exclusive housing states.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.54),
  textGrob("4. Categories: Homeowners (Owned/Buying), Assisted (Public/Subsidized), Private (Market Rent).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.42)
)

final_table_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(9, "lines")))
final_table_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(8, "lines")))

# EXPORT AND DISPLAY
grid.newpage(); grid.draw(final_plot)
grid.newpage(); grid.draw(final_table_layout)

png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 550, res = 120)
grid.draw(final_table_layout)
dev.off()

message("SUCCESS: Full Universe Housing visuals exported to 03_output.")