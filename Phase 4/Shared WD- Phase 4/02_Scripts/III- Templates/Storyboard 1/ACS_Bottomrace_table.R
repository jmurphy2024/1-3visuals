# ==============================================================================
# SCRIPT: ACS_race_bottom_third_notitle.R
# Purpose: Focused summary table (Bottom Third vs National) without plot titles.
# Updates: Removed textGrob titles; kept Bottom Country Red styling.
# Logic:   Uses Master Prepared Data.
# Output:  03_output/visualizations_final/Table_Race_Bottom_Third_NoTitle.png
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
message("Calculating racial stats for Bottom Third and National Total...")

# Aggregate for Bottom Third (Low-Income Country)
bottom_stats <- acs_data %>%
  filter(REAL_INCOME <= main_cutoffs$main_cutoff1) %>%
  group_by(Race_Ethnicity) %>%
  summarise(Population = sum(PERWT), .groups = "drop") %>%
  mutate(
    Share = Population / sum(Population),
    Label = paste0(number(Population / 1e6, accuracy = 0.1, suffix = "M"), 
                   " (", percent(Share, accuracy = 0.1), ")"),
    `Economic Group` = "Bottom Third (Low-Income)"
  )

# Aggregate for National Total
national_stats <- acs_data %>%
  group_by(Race_Ethnicity) %>%
  summarise(Population = sum(PERWT), .groups = "drop") %>%
  mutate(
    Share = Population / sum(Population),
    Label = paste0(number(Population / 1e6, accuracy = 0.1, suffix = "M"), 
                   " (", percent(Share, accuracy = 0.1), ")"),
    `Economic Group` = "NATIONAL TOTAL"
  )

# Combine and Format for Table
final_table_df <- bind_rows(bottom_stats, national_stats) %>%
  select(`Economic Group`, Race_Ethnicity, Label) %>%
  pivot_wider(names_from = Race_Ethnicity, values_from = Label)

# --- 3. EXPORT AS STYLED PNG (NO TITLE) ---
message("Applying Bottom Country Red styling...")

# Define Red-themed Table Style
table_theme <- ttheme_minimal(
  core = list(
    bg_params = list(fill = c("#FDEDEC", "#D5DBDB")), # Light red for Bottom, Grey for National
    fg_params = list(fontface = c("bold", "bold"), cex = 0.85)
  ),
  colhead = list(
    bg_params = list(fill = "#C0392B"), # Project "Bottom Country" Red
    fg_params = list(col = "white", fontface = "bold", cex = 1.0)
  )
)

# Create Grob (Directly save the table without adding a title grob)
t_grob <- tableGrob(final_table_df, rows = NULL, theme = table_theme)

# Save
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Reduced height since the title block is removed
ggsave(file.path(out_dir, "Table_Race_Bottom_Third_NoTitle.png"), 
       t_grob, width = 15, height = 2.5, dpi = 300, bg = "white")

message("Success: Clean Bottom Third Table (No Title) saved.")