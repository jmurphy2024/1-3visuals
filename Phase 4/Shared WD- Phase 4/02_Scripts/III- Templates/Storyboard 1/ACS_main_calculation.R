# ==============================================================================
# SCRIPT: ACS_export_summary_table_v2.R
# Purpose: Exports an expanded Executive Summary as a PNG.
# Logic:   Includes Tercile stats AND National Statistics.
#          Uses 342M Scaled Weights, $0 Floor Income, and RPP.
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(here); library(scales); library(gridExtra); library(grid); library(ggplot2)

# --- 1. LOAD DATA & CUTOFFS ---
data_path    <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(data_path)) stop("Data not found. Run ACS_master_prepare.R first.")
if(!file.exists(cutoffs_path)) stop("Cutoffs not found. Run II-C Border Setup.R first.")

acs_data     <- readRDS(data_path)
main_cutoffs <- readRDS(cutoffs_path)

# --- 2. CALCULATE STATISTICS ---
message("Processing summary statistics...")

# A. Base Data Classification
data_classified <- acs_data %>%
  mutate(
    tercile_group = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom Third (T1)",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle Third (T2)",
      TRUE ~ "Top Third (T3)"
    )
  )

# B. Calculate Stats by Tercile
stats_tercile <- data_classified %>%
  group_by(tercile_group) %>%
  summarise(
    Population       = sum(PERWT),
    Min_Real         = min(REAL_INCOME, na.rm = TRUE),
    Max_Real         = max(REAL_INCOME, na.rm = TRUE),
    Min_Unadj        = min(income_clamped, na.rm = TRUE),
    Max_Unadj        = max(income_clamped, na.rm = TRUE),
    Metro_Share      = weighted.mean(is_metro, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(`Economic Country` = tercile_group)

# C. Calculate National Stats (Total US)
stats_total <- data_classified %>%
  summarise(
    `Economic Country` = "TOTAL US POPULATION",
    Population       = sum(PERWT),
    Min_Real         = min(REAL_INCOME, na.rm = TRUE),
    Max_Real         = max(REAL_INCOME, na.rm = TRUE),
    Min_Unadj        = min(income_clamped, na.rm = TRUE),
    Max_Unadj        = max(income_clamped, na.rm = TRUE),
    Metro_Share      = weighted.mean(is_metro, w = PERWT, na.rm = TRUE)
  )

# D. Combine and Format for Table
table_data <- bind_rows(stats_tercile, stats_total) %>%
  mutate(
    `Population`         = comma(Population),
    `Real Range (RPP)`   = paste0(dollar(Min_Real), " - ", dollar(Max_Real)),
    `Unadjusted Range`   = paste0(dollar(Min_Unadj), " - ", dollar(Max_Unadj)),
    `Metro Share %`      = percent(Metro_Share, accuracy = 0.1)
  ) %>%
  select(
    `Economic Country`, 
    `Population`, 
    `Real Range (RPP)`, 
    `Unadjusted Range`, 
    `Metro Share %`
  )

# --- 3. CREATE STYLED PNG TABLE ---
message("Rendering table to PNG...")

output_file <- here::here("03_output", "visualizations_final", "ACS_income_metro.png")
if(!dir.exists(dirname(output_file))) dir.create(dirname(output_file), recursive = TRUE)

# Table Styling (Highlighting the Total row with a different background)
table_theme <- ttheme_minimal(
  core = list(
    bg_params = list(fill = c(rep(c("white", "#f2f2f2"), length.out = 3), "#D5DBDB"), col = "white"),
    fg_params = list(fontsize = 11, fontface = c(rep("plain", 3), "bold"))
  ),
  colhead = list(
    bg_params = list(fill = "#2C3E50"),
    fg_params = list(col = "white", fontface = "bold", fontsize = 12)
  )
)

# Create Grob
t_grob <- tableGrob(table_data, rows = NULL, theme = table_theme)

# Title & Subtitle
title <- textGrob("1/3 Country Project: Population & Income Summary", gp = gpar(fontsize = 16, fontface = "bold"))
subtitle <- textGrob(
  paste("Includes Tercile Breakdown and National Benchmarks | Inclusive 342M Target"), 
  gp = gpar(fontsize = 9, fontitalic = TRUE, col = "gray30")
)

# Combine elements
final_table <- grid.arrange(
  title, subtitle, t_grob,
  heights = unit.c(unit(2, "lines"), unit(1, "lines"), unit(1, "null"))
)

# Save high-resolution PNG
ggsave(output_file, final_table, width = 11, height = 5, bg = "white", dpi = 300)

message(paste("Success! Complete summary table with National stats saved to:", output_file))