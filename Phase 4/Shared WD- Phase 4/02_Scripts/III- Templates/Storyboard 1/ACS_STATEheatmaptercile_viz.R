# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_heatmaptercile_viz.R
# Purpose: Maps the percentage of each state's population that falls into 
#          the Bottom, Middle, and Top income terciles.
# Update:  Added auto-installation for 'usmap'.
# Output:  03_output/visualizations_final/map_Tercile_Concentration_[1,2,3].png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(cowplot)

# --- 1. SETUP & INSTALLATION ---
# Check if 'usmap' is installed. If not, install it automatically.
if(!require(usmap)) {
  message("Installing missing package 'usmap'...")
  install.packages("usmap")
  library(usmap)
}

PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_Race_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. CALCULATE STATE CONCENTRATIONS ---
message("Calculating State-level Stats...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

# A. Assign Terciles
state_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Tercile 1 (Bottom)",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Tercile 2 (Middle)",
      REAL_INCOME >= limit_2 ~ "Tercile 3 (Top)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_tercile))

# B. Calculate % of Each State in Each Tercile
# Formula: (Count of People in State X in Tercile Y) / (Total Population of State X)
map_data <- state_data %>%
  group_by(STATEFIP, income_tercile) %>%
  summarise(count = sum(PERWT), .groups = "drop_last") %>%
  mutate(pct_of_state = count / sum(count)) %>%
  ungroup() %>%
  # Prepare for usmap (needs 'fips' column)
  mutate(fips = sprintf("%02d", STATEFIP)) %>%
  select(fips, income_tercile, pct_of_state)

# --- 3. CONFIGURATION ---
tercile_config <- list(
  "1" = list(name = "Bottom Third", filter_match = "Tercile 1 (Bottom)", 
             low_col = "#FADBD8", high_col = "#943126", title_col = "#C0392B"), # Red Scale
  
  "2" = list(name = "Middle Third", filter_match = "Tercile 2 (Middle)", 
             low_col = "#FCF3CF", high_col = "#B7950B", title_col = "#F5B041"), # Gold Scale
  
  "3" = list(name = "Top Third",    filter_match = "Tercile 3 (Top)",    
             low_col = "#D5F5E3", high_col = "#196F3D", title_col = "#27AE60")  # Green Scale
)

# --- 4. GENERATE MAPS ---
message("Generating Maps...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  message(paste("Mapping:", config$name))
  
  # Filter Data
  current_map_data <- map_data %>%
    filter(income_tercile == config$filter_match)
  
  # Plot using plot_usmap
  p <- plot_usmap(data = current_map_data, values = "pct_of_state", color = "white") +
    
    # Color Scales
    scale_fill_gradient(
      low = config$low_col, 
      high = config$high_col, 
      name = "% of State Pop", 
      labels = scales::percent
    ) +
    
    # Labels
    labs(
      title = paste0("Geographic Concentration: ", config$name),
      #subtitle = "States darker in color have a higher percentage of their population in this income group",
      caption = "Source: IPUMS USA 2023 ACS"
    ) +
    
    # Theme Adjustments
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = config$title_col),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
      legend.position = "right",
      legend.title = element_text(face = "bold")
    )
  
  filename <- paste0("map_Tercile_Concentration_", i, ".png")
  ggsave(file.path(out_dir, filename), plot = p, width = 12, height = 8, bg = "white")
  message(paste("  -> Saved:", filename))
  
  print(p)
  Sys.sleep(0.5)
}

message("\n--- All geographic maps generated. ---")