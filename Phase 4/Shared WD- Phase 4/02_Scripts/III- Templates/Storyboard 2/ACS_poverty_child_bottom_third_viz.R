# ==============================================================================
# SCRIPT: ACS_child_poverty_intensity_bottom_third.R
# Purpose: Granular 20-ventile focus on Child Poverty Intensity (Red Country).
# Logic:   Isolates children (Age < 18) and maps Income-to-Poverty Ratio.
# Branding: Red (#C0392B) with shadow effect (#FDEDEC).
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(here); library(scales); library(tidyr)

# --- 1. SETUP & DATA LOAD ---
data_path    <- here::here("01_data", "processed", "prepared_ACS_poverty.rds")
cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(data_path)) stop("Master data not found. Run preparation script first.")
data    <- readRDS(data_path)
cutoffs <- readRDS(cutoffs_path)

# --- 2. BOTTOM-THIRD CHILD INTENSITY PROCESSING ---
# Isolating Red Country children and calculating 20 internal ventiles
red_child_intensity <- data %>%
  filter(
    as.numeric(AGE) < 18, 
    REAL_INCOME <= cutoffs$main_cutoff1
  ) %>%
  # Recalculate 20 ventiles specifically for this population segment
  mutate(internal_ventile = ntile(REAL_INCOME, 20)) %>%
  group_by(internal_ventile) %>%
  summarise(
    # Continuous Intensity: Income-to-Poverty Ratio (e.g., 50 = 50% of threshold)
    avg_poverty_intensity = weighted.mean(as.numeric(POVERTY), w = PERWT, na.rm = TRUE),
    # Weighted average income for X-axis labels
    avg_income = weighted.mean(REAL_INCOME, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  )

# --- 3. GENERATE REFINED INTENSITY VISUAL ---
p_red_child_int <- ggplot(red_child_intensity, aes(x = avg_income, y = avg_poverty_intensity)) +
  
  # Shadow/Glow effect (Mandatory Red Country shading: #FDEDEC)
  geom_line(size = 4, color = "#FDEDEC", alpha = 0.8) +
  
  # Main Intensity Line (#C0392B)
  geom_line(size = 1, color = "#C0392B", linejoin = "round", lineend = "round") +
  
  # Professional Scales & Fixed Y-Axis Limit
  scale_y_continuous(
    name = "Income-to-Poverty Ratio (%)",
    labels = label_number(suffix = "%"),
    # FIXED LIMIT: Extends to 200% to ensure the line does not hit the top border
    limits = c(0, 200), 
    breaks = seq(0, 200, 50),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = "Household Income",
    labels = label_dollar(),
    breaks = seq(0, 60000, 10000),
    expand = c(0.01, 0)
  ) +
  # Framework Theme Standards
  theme_minimal(base_family = "sans") +
  theme(
    axis.line       = element_line(color = "black", size = 0.8), # Professional weight
    axis.title      = element_text(face = "bold", size = 14),
    axis.text       = element_text(color = "black", size = 11),
    panel.grid      = element_blank(), # Seamless logic: no internal gridlines
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(20, 20, 20, 20)
  )

# --- 4. OUTPUT ---
print(p_red_child_int)

out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ggsave(file.path(out_dir, "Child_Poverty_Intensity_Bottom_Third.png"), p_red_child_int, width = 11, height = 6.5, dpi = 300)

message("Child Poverty Intensity visualization for the Bottom Third complete.")