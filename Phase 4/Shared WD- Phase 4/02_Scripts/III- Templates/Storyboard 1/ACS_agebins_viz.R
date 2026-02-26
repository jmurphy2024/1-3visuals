# ==============================================================================
# SCRIPT: ACS_age_explicit_viz.R
# Purpose: Generates 3 separate bar charts showing EVERY age year (0-95+).
# Template: Matches the style of ACS_agedecade_viz.R
# Output:  03_output/visualizations_final/plot_Age_Years_Tercile_[1,2,3].png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales)

# --- 1. SETUP & PATHS ---
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(INPUT_DATA_FILE)) stop("Master Prepared Data not found. Run ACS_master_prepare.R.")
if(!file.exists(MAIN_CUTOFFS_FILE)) stop("Cutoffs not found. Run II-C Border Setup.R.")

acs_data     <- readRDS(INPUT_DATA_FILE)
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- 2. TERCILE ASSIGNMENT & DATA AGGREGATION ---
message("Assigning Terciles and calculating counts...")

age_data <- acs_data %>%
  mutate(
    tercile_id = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ 1,
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ 2,
      TRUE ~ 3
    )
  ) %>%
  group_by(tercile_id, AGE) %>%
  summarise(total_pop = sum(PERWT), .groups = "drop") %>%
  mutate(
    is_decade = if_else(AGE %% 10 == 0 & AGE != 0, "Decade", "Standard"),
    age_clean = as.numeric(AGE)
  )

# --- 3. PLOTTING CONFIGURATION (Matching Template Colors) ---
tercile_configs <- list(
  "1" = list(name = "Bottom Third (T1)", col_light = "#E6B0AA", col_dark = "#C0392B", file = "plot_Age_Years_Tercile_1.png"),
  "2" = list(name = "Middle Third (T2)", col_light = "#FAD7A0", col_dark = "#F5B041", file = "plot_Age_Years_Tercile_2.png"),
  "3" = list(name = "Top Third (T3)",    col_light = "#ABEBC6", col_dark = "#27AE60", file = "plot_Age_Years_Tercile_3.png")
)

# --- 4. GENERATE INDIVIDUAL PLOTS ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

for (t_id in names(tercile_configs)) {
  config <- tercile_configs[[t_id]]
  message(paste("Generating plot for:", config$name))
  
  plot_data <- age_data %>% filter(tercile_id == as.numeric(t_id))
  
  # Find a nice round upper limit for Y axis based on max count across all terciles for consistency
  # Or use local max for better detail. Template usually uses local max.
  upper_limit <- max(plot_data$total_pop) * 1.1
  y_breaks    <- pretty(c(0, upper_limit), n = 5)
  
  p <- ggplot(plot_data, aes(x = age_clean, y = total_pop, fill = is_decade)) +
    # Bar Chart
    geom_col(width = 0.9) +
    
    # Use Decade vs Standard colors
    scale_fill_manual(values = c("Decade" = config$col_dark, "Standard" = config$col_light)) +
    
    # Y-Axis in Millions
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = "M"), 
      breaks = y_breaks,
      limits = c(0, max(y_breaks)),
      expand = c(0, 0)
    ) +
    
    # X-Axis every 10 years
    scale_x_continuous(
      breaks = seq(0, 100, 10), 
      expand = c(0.01, 0)
    ) +
    
    labs(
      title = paste0("Population by Age: ", config$name),
      x = "Age (Years)",
      y = "Population (Millions)"
    ) +ß
    
    # Template Theme Settings
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black", margin = margin(b = 10)),
      axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
      axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
      axis.text = element_text(size = 10, color = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.margin = margin(20, 20, 20, 20)
    )
  
  # Save
  ggsave(file.path(out_dir, config$file), p, width = 12, height = 6, dpi = 300, bg = "white")
}

message("Success: All 3 Age Distribution plots saved to 03_output/visualizations_final/")