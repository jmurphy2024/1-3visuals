# ==============================================================================
# SCRIPT: ACS_racestacked_viz.R
# Purpose: Stacked Bar Chart of Race/Ethnicity composition per Income Tercile.
# Design:  Matches project template styling (position = "fill").
# Logic:   Uses Master Prepared Data (342M Target, $0 Floor, RPP).
# Output:  03_output/visualizations_final/plot_Race_Composition_by_Tercile.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(cowplot)

# --- 1. SETUP & PATHS ---
# Using the single master file created in your master workflow
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(INPUT_DATA_FILE)) stop("Master Prepared Data not found. Run ACS_master_prepare.R.")
if(!file.exists(MAIN_CUTOFFS_FILE)) stop("Cutoffs not found. Run II-C Border Setup.R.")

acs_data     <- readRDS(INPUT_DATA_FILE)
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- 2. ASSIGN TERCILES & AGGREGATE ---
message("Assigning Terciles and calculating composition...")

plot_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # Group by Tercile and the Race categories defined in Master Prep
  group_by(income_tercile, Race_Ethnicity) %>%
  summarise(count = sum(PERWT), .groups = "drop") %>%
  # Calculate percentage within each tercile
  group_by(income_tercile) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup() %>%
  # Ensure the X-axis terciles are in the correct order
  mutate(income_tercile = factor(income_tercile, 
                                 levels = c("Bottom Third", "Middle Third", "Top Third")))

# --- 3. VISUALIZATION ---
message("Generating Stacked Bar Chart...")

# Match the template color palette
# Note: Categories adjusted to match those created in ACS_master_prepare.R
race_colors <- c(
  "White (NH)"      = "#2C3E50", # Dark Blue
  "Hispanic"        = "#E67E22", # Orange
  "Black (NH)"      = "#2980B9", # Blue
  "Asian/PI (NH)"   = "#8E44AD", # Purple
  "Other/Multi (NH)" = "#95A5A6"  # Grey
)

p <- ggplot(plot_data, aes(x = income_tercile, y = percentage, fill = Race_Ethnicity)) +
  
  # Stacked Bar (Proportional)
  geom_col(width = 0.7, position = "fill") +
  
  # Labels (Percentage) - Only show if > 3% to avoid clutter per template
  geom_text(aes(label = ifelse(percentage > 0.03, percent(percentage, accuracy = 1), "")), 
            position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 4) +
  
  # Scales & Formatting
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = race_colors) +
  
  labs(
    title = "Racial Composition",
    subtitle = "Percentage of each Economic Country by Race/Ethnicity",
    x = NULL,
    y = "Percentage of Group Population",
    fill = "Race / Ethnicity",
    caption = "Source: ACS 5-Year Inclusive Estimates | Weighted by PERWT (342M Target)"
  ) +
  
  # Template Theme Settings
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30", margin = margin(b = 20)),
    axis.title.y = element_text(face = "bold", size = 12),
    axis.text.x = element_text(face = "bold", size = 12, color = "black"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# --- 4. SAVE ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "plot_Race_Composition_by_Tercile.png"), 
       p, width = 10, height = 8, dpi = 300, bg = "white")

message("Success: Race Composition Stacked Bar saved.")