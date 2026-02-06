# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: NHIS_Life_Expectancy_viz.R
## Purpose: Generates Survival-based line plot and summary table for Life Expectancy.
##          Method: Restricted Mean Survival Time (RMST) capped at Age 90.
## Author: Janica Murphy, Arpit Gaba, Max Goshert, EPAG / Gemini
## Last Modified: 2026-01-28

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable); library(survival); library(broom)

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

USER_IPUMS_SAMPLE_ID  <- "ih2014"
USER_INDICATOR_NAME   <- "Life_Expectancy"
USER_WEIGHT_VARIABLE  <- "MORTWT" # Mortality Weight (Crucial)

OUTPUT_PLOT_NAME  <- paste0("plot_NHIS_", USER_INDICATOR_NAME, ".png")
OUTPUT_TABLE_NAME <- paste0("table_NHIS_", USER_INDICATOR_NAME, ".png")

# Load prepared data
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_NHIS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

prepared_data <- readRDS(PREPARED_DATA_FILE)

# NOTE: NHIS uses local grouping because income codes are not compatible with external ACS cutoffs
message("Assigning income groups locally...")

# Assign income groups (Local Logic)
data_with_groups <- prepared_data %>%
  filter(!is.na(HHINCOME)) %>%
  mutate(
    # 1. Force data into 20 evenly sized ranks (5% groups)
    fine_income_group_rank = ntile(HHINCOME, 20),
    
    # 2. Create labels (X-Axis)
    fine_income_group = factor(fine_income_group_rank, levels = 1:20, labels = paste0(seq(5, 100, 5), "%")),
    
    # 3. Create broad terciles for coloring
    income_tercile = case_when(
      fine_income_group_rank <= 7  ~ "Tercile 1 (Bottom)",
      fine_income_group_rank <= 14 ~ "Tercile 2 (Middle)",
      TRUE                         ~ "Tercile 3 (Top)"
    )
  )

# ==== 2. DATA SUMMARIZATION (SURVIVAL ANALYSIS) ====

# Custom Function for RMST (Life Expectancy)
calculate_rmst <- function(df) {
  # Fix "Zero Time" error
  df <- df %>% mutate(age_at_event = ifelse(age_at_event <= age_entry, age_entry + 0.1, age_at_event))
  
  fit <- tryCatch(
    survfit(Surv(age_entry, age_at_event, status_flag) ~ 1, weights = MORTWT, data = df),
    error = function(e) return(NULL)
  )
  
  if (is.null(fit)) return(NA)
  
  tryCatch(
    summary(fit, rmean = 90)$table["rmean"],
    error = function(e) return(NA)
  )
}

# 2.1. U.S. Population Baseline
total_stats <- calculate_rmst(data_with_groups)
total_n     <- sum(data_with_groups$MORTWT, na.rm = TRUE)

total_pop <- tibble(
  Country = "NHIS 2014 Sample",
  n = total_n,
  "Life Expectancy" = total_stats
)

# 2.2. Country Summaries (Tercile Breakdown)
country_summary <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(
    n = sum(MORTWT, na.rm = TRUE),
    "Life Expectancy" = calculate_rmst(cur_data()),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>%
  select(Country, n, "Life Expectancy")

# 2.3. Formatting for tableGrob
table_data_final <- bind_rows(total_pop, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.1f Years", .))) %>%
  mutate(n = scales::comma(round(n))) # Round weights for display

colnames(table_data_final) <- c("bold('Country')", "bold(italic('n'))", "bold('Avg Life Expectancy')")

# ==== 3. GENERATE TREND PLOT ====

# Calculate Life Expectancy for each of the 20 buckets
summary_stats <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Life_Expectancy" = calculate_rmst(cur_data()),
    .groups = "drop"
  ) %>%
  filter(!is.na(Life_Expectancy))

# Create Plot
final_plot <- create_single_line_plot(
  summary_data       = summary_stats,
  y_var              = "Life_Expectancy",
  plot_title         = "Life Expectancy by Income Level",
  y_axis_label       = "Estimated Life Expectancy (Years)",
  y_axis_format      = "number",
  output_filename    = paste0("../", PLOT_SUBFOLDER, "/", OUTPUT_PLOT_NAME),
  fine_group_level   = "Groups_20",
  
  # Note: These cutoffs are purely for coloring since we used local groups
  border_t1_t2       = 7.5, 
  border_t2_t3       = 14.5
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
  textGrob("1. Source: IPUMS NHIS Linked Mortality File (2014 Sample, Linked to 2019 Deaths).", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.75),
  textGrob("2. Universe: Civilian non-institutionalized population eligible for mortality linkage.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.60),
  textGrob("3. Metric: Restricted Mean Survival Time (RMST). Estimates average lifespan up to age 90.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.45),
  textGrob("4. Income: Ranked by family income groups (20 bins) due to NHIS categorical data limitations.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.30)
)

final_table_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(9, "lines")))

# DISPLAY TABLE IN RSTUDIO
grid.newpage(); grid.draw(final_table_layout)

# SAVE TABLE TO PNG
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 550, res = 120)
grid.draw(final_table_layout)
dev.off()

message("SUCCESS: NHIS Life Expectancy visuals exported and displayed.")