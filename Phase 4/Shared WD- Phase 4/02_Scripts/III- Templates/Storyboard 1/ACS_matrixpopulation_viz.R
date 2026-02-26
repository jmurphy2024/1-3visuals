# ==============================================================================
# SCRIPT: ACS_metro_heatmap_viz.R
# Purpose: Heatmap of population counts (Metro vs Non-Metro) across countries.
# Design:  Matrix style with explicit Million (M) labels.
# Output:  03_output/visualizations_final/plot_Metro_Heatmap_by_Tercile.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(here); library(ggplot2); library(scales); library(tidyr)

# --- 1. SETUP & DATA LOADING ---
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(INPUT_DATA_FILE)) stop("Master Prepared Data not found. Run ACS_master_prepare.R.")

acs_data     <- readRDS(INPUT_DATA_FILE)
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- 2. DATA AGGREGATION ---
message("Summarizing population for heatmap...")

heatmap_data <- acs_data %>%
  mutate(
    # Assign the Three Countries
    tercile_name = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    ),
    # Map Metro status
    location = if_else(is_metro == 1, "Metropolitan", "Non-Metropolitan")
  ) %>%
  group_by(tercile_name, location) %>%
  summarise(total_pop = sum(PERWT), .groups = "drop") %>%
  mutate(
    tercile_name = factor(tercile_name, levels = c("Bottom Third", "Middle Third", "Top Third")),
    pop_m = total_pop / 1e6 # Convert to Millions for labels
  )

# --- 3. GENERATE HEATMAP ---
message("Generating Heatmap...")

p <- ggplot(heatmap_data, aes(x = tercile_name, y = location, fill = total_pop)) +
  # Create the heatmap tiles
  geom_tile(color = "white", size = 0.5) +
  
  # Add explicit text labels (e.g., 95.4M)
  geom_text(aes(label = paste0(round(pop_m, 1), "M")), 
            color = "white", fontface = "bold", size = 6) +
  
  # Color scale: Using a high-contrast gradient
  # You can use scale_fill_gradient for a professional blue look
  scale_fill_gradient(low = "#D5DBDB", high = "#2C3E50", labels = label_number(scale = 1e-6, suffix = "M")) +
  
  # Layout
  scale_x_discrete(position = "top") + # Put Tercile labels at the top
  
  labs(
    title = "Population Density: Metro vs. Non-Metro",
    subtitle = "Explicit population counts (Millions) across the Three Countries",
    x = NULL,
    y = NULL,
    fill = "Population",
    caption = "Source: ACS 5-Year Inclusive Estimates | 342M Target Population"
  ) +
  
  # Theme
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30", margin = margin(b = 20)),
    axis.text = element_text(face = "bold", size = 12, color = "black"),
    legend.position = "right",
    panel.grid = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# --- 4. SAVE ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "plot_Metro_Heatmap_by_Tercile.png"), 
       p, width = 10, height = 6, dpi = 300, bg = "white")

message("Success: Metro Heatmap saved.")