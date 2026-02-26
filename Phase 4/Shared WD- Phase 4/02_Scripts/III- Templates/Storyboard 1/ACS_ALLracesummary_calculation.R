# ==============================================================================
# SCRIPT: ACS_race_summary_table.R
# Purpose: Exports a formatted summary table of Racial Composition by Country.
# Updates: Includes both Population Counts (M) and Percentage Shares (%).
# Logic:   Uses Master Prepared Data (342M Scaled Population).
# Output:  03_output/visualizations_final/Table_Race_Composition_Summary.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(here); library(scales); library(gridExtra); library(grid); library(ggplot2); library(tidyr)

# --- 1. SETUP & LOAD ---
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(INPUT_DATA_FILE)) stop("Master Prepared Data not found.")
acs_data     <- readRDS(INPUT_DATA_FILE)
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- 2. AGGREGATE DATA ---
message("Calculating racial counts and shares per economic country...")

# Map terciles and calculate population stats
race_stats <- acs_data %>%
  mutate(
    tercile_group = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  group_by(tercile_group, Race_Ethnicity) %>%
  summarise(Population = sum(PERWT), .groups = "drop") %>%
  group_by(tercile_group) %>%
  mutate(
    Share = Population / sum(Population),
    # Create combined label: "XX.XM (XX.X%)"
    Label = paste0(number(Population / 1e6, accuracy = 0.1, suffix = "M"), 
                   " (", percent(Share, accuracy = 0.1), ")")
  ) %>%
  ungroup()

# --- 3. FORMAT FOR TABLE ---
# Convert to wide format so races are columns
table_data <- race_stats %>%
  select(tercile_group, Race_Ethnicity, Label) %>%
  pivot_wider(names_from = Race_Ethnicity, values_from = Label) %>%
  rename(`Economic Country` = tercile_group)

# Add a "National Average" row for comparison
national_avg <- acs_data %>%
  group_by(Race_Ethnicity) %>%
  summarise(Population = sum(PERWT), .groups = "drop") %>%
  mutate(
    Share = Population / sum(Population),
    Label = paste0(number(Population / 1e6, accuracy = 0.1, suffix = "M"), 
                   " (", percent(Share, accuracy = 0.1), ")")
  ) %>%
  select(Race_Ethnicity, Label) %>%
  pivot_wider(names_from = Race_Ethnicity, values_from = Label) %>%
  mutate(`Economic Country` = "NATIONAL TOTAL")

final_table_df <- bind_rows(table_data, national_avg)

# --- 4. EXPORT AS STYLED PNG ---
message("Formatting and saving table...")

# Define theme (Matching your project executive summary style)
table_theme <- ttheme_minimal(
  core = list(
    bg_params = list(fill = c(rep(c("white", "#f9f9f9"), length.out = nrow(table_data)), "#D5DBDB")),
    fg_params = list(fontface = c(rep("plain", nrow(table_data)), "bold"), cex = 0.8) # Slightly smaller font for density
  ),
  colhead = list(
    bg_params = list(fill = "#2C3E50"), 
    fg_params = list(col = "white", fontface = "bold", cex = 0.9)
  )
)

# Create Grob
t_grob <- tableGrob(final_table_df, rows = NULL, theme = table_theme)

# Add Header
title <- textGrob("Race & Ethnicity Composition by Economic Country (Count & %)", 
                  gp = gpar(fontsize = 16, fontface = "bold"))
padding <- unit(5, "mm")
final_plot <- gtable::gtable_add_rows(t_grob, heights = grobHeight(title) + padding, pos = 0)
final_plot <- gtable::gtable_add_grob(final_plot, title, 1, 1, 1, ncol(final_plot), clip = "off")

# Save - increased width to accommodate dual labels
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "Table_Race_Composition_Summary.png"), 
       final_plot, width = 15, height = 4.5, dpi = 300, bg = "white")

message("Success: Race Summary Table saved.")