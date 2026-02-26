# ==============================================================================
# SCRIPT: ACS_poverty_viz_line.R
# Purpose: Generates segmented line charts for Poverty Rate (Skyline & T1 Focus).
# Format: Line charts with 60 ventiles (Full) and 20 ventiles (T1 focus).
# Logic: Uses prepared_ACS_inclusive.rds and Master ACS Borders.
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(here); library(scales); library(tidyr)

# --- 1. SETUP & DATA LOAD ---
# Using the inclusive RDS and the master cutoffs defined in the framework
INCLUSIVE_FILE <- here::here("01_data", "processed", "prepared_ACS_poverty.rds")
CUTOFFS_FILE   <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(INCLUSIVE_FILE)) stop("Input data not found. Run preparation script first.")
if(!file.exists(CUTOFFS_FILE)) stop("Master Cutoffs not found.")

prepared_data <- readRDS(INCLUSIVE_FILE)
main_cutoffs  <- readRDS(CUTOFFS_FILE)

# Ensure is_poverty flag exists (Poverty Logic: POVERTY < 100 is below the line)
if(!"is_poverty" %in% names(prepared_data)) {
  prepared_data <- prepared_data %>%
    mutate(is_poverty = if_else(as.numeric(POVERTY) > 0 & as.numeric(POVERTY) < 100, 1, 0))
}

# --- 2. DATA PROCESSING (60-VENTILE DISTRIBUTION) ---
# We calculate 20 ventiles internally within each Country to create the skyline (cite: 3, 5)
viz_full <- prepared_data %>%
  mutate(
    country = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(country)) %>%
  group_by(country) %>%
  mutate(ventile = ntile(REAL_INCOME, 20)) %>%
  group_by(country, ventile) %>%
  summarise(
    poverty_rate = weighted.mean(is_poverty, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(country = factor(country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(country, ventile) %>%
  mutate(x_id = row_number())

# --- 3. PLOT 1: ALL INCOME GROUPS (SKYLINE) ---
# Brand Colors: Red (#C0392B), Orange (#F5B041), Green (#27AE60) (cite: 2)
p1 <- ggplot(viz_full, aes(x = x_id, y = poverty_rate, color = country, group = country)) +
  # Visual "Glow/Shadow" effect from reference image
  geom_line(aes(y = poverty_rate), size = 4, alpha = 0.15) +
  # Main Segmented Line
  geom_line(size = 1.5, linejoin = "round", lineend = "round") +
  scale_color_manual(values = c(
    "Bottom Third" = "#C0392B", 
    "Middle Third" = "#F5B041", 
    "Top Third"    = "#27AE60"
  )) +
  scale_y_continuous(labels = label_percent(), expand = c(0.05, 0.05)) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  labs(x = "Household Income", y = NULL) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x     = element_blank(), # Clean design: no x-axis labels (cite: 2)
    #axis.title.x    = element_text(face = "bold", size = 18, margin = margin(t = 20)),
   # axis.text.y     = element_text(size = 12, color = "black"),
    axis.line.x     = element_line(color = "black", size = 1.5), # Bold axis line per image
    axis.line.y     = element_line(color = "black", size = 1.5),
    panel.grid      = element_blank(), # Remove all internal gridlines (cite: 2)
    plot.background = element_rect(fill = "white", color = NA)
  )

# Output to RStudio Plot Pane
print(p1)

# --- 4. PLOT 2: BOTTOM THIRD ONLY ---
viz_bottom <- viz_full %>% filter(country == "Bottom Third")

p2 <- ggplot(viz_bottom, aes(x = x_id, y = poverty_rate)) +
  geom_line(aes(y = poverty_rate), size = 4, alpha = 0.15, color = "#C0392B") +
  geom_line(size = 1.5, color = "#C0392B", linejoin = "round") +
  scale_y_continuous(labels = label_percent(), expand = c(0.05, 0.05)) +
  scale_x_continuous(expand = c(0.01, 0.01)) +
  labs(x = "Household Income", y = NULL) +
  theme_minimal() +
  theme(
    axis.text.x     = element_blank(),
   # axis.title.x    = element_text(face = "bold", size = 18, margin = margin(t = 20)),
    # axis.text.y     = element_text(face = "bold", size = 12, color = "black"),
    axis.line.x     = element_line(color = "black", size = 1.5),
    axis.line.y     = element_line(color = "black", size = 1.5),
    panel.grid      = element_blank(),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Output to RStudio Plot Pane
print(p2)

# --- 5. SAVE OUTPUTS ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "Poverty_Skyline_All.png"), p1, width = 10, height = 6, dpi = 300)
ggsave(file.path(out_dir, "Poverty_Skyline_Bottom_Third.png"), p2, width = 10, height = 6, dpi = 300)

message("Success: All Countries and Bottom Third visualizations generated.")