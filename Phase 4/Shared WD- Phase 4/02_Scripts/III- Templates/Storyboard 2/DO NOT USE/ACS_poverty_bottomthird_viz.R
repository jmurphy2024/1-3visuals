# ==============================================================================
# SCRIPT: ACS_poverty_bottomthird_viz.R
# Purpose: Professional high-resolution line chart for Bottom Third.
# Logic:   Internal 10-decile depth with improved axis aesthetics.
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(here); library(scales); library(tidyr)

# --- 1. SETUP ---
data_path    <- here::here("01_data", "processed", "prepared_ACS_poverty.rds")
cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(data_path)) stop("Data not found.")
prepared_data <- readRDS(data_path)
main_cutoffs  <- readRDS(cutoffs_path)

# --- 2. DATA PROCESSING ---
# Isolate Bottom Third and calculate internal 10-decile intensity
red_viz_data <- prepared_data %>%
  filter(REAL_INCOME <= main_cutoffs$main_cutoff1) %>%
  mutate(decile = ntile(REAL_INCOME, 10)) %>%
  group_by(decile) %>%
  summarise(
    # Depth of poverty (continuous scale)
    avg_poverty_depth = weighted.mean(as.numeric(POVERTY), w = PERWT, na.rm = TRUE),
    # Representative Income label for X-axis
    avg_income = weighted.mean(REAL_INCOME, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. GENERATE REFINED VISUAL ---
p_refined <- ggplot(red_viz_data, aes(x = avg_income, y = avg_poverty_depth)) +
  
  # Shadow/Glow effect (Mandatory #FDEDEC)
  geom_line(linewidth = 4, color = "#FDEDEC", alpha = 0.8) +
  
  # Main Intensity Line (Updated to master #9B2226)
  geom_line(linewidth = 1, color = "#9B2226", linejoin = "round", lineend = "round") +
  
  # Professional Scales & Labels
  scale_y_continuous(
    name = "Income-to-Poverty Ratio (%)",
    labels = label_number(suffix = "%"),
    expand = c(0.02, 0)
  ) +
  scale_x_continuous(
    name = "Household Income (RPP Adjusted)",
    labels = label_dollar(),
    breaks = breaks_pretty(n = 5),
    expand = c(0.01, 0)
  ) +
  
  # Framework Theme Standards
  theme_minimal(base_family = "sans") +
  theme(
    # Adjusted Axis Line thickness to match master standard (1.5)
    axis.line       = element_line(color = "black", linewidth = 1.5), 
    axis.title      = element_text(face = "bold", size = 14),
    axis.text       = element_text(color = "black", size = 11),
    panel.grid      = element_blank(), # Seamless logic (no gridlines)
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(20, 20, 20, 20)
  )

# --- 4. OUTPUT ---
print(p_refined)

out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ggsave(file.path(out_dir, "Poverty_Bottom_Third_Refined.png"), p_refined, width = 11, height = 6.5, dpi = 300)

message("Refined visualization complete with standard axis weights and labels.")