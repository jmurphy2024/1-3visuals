# ==== 0. ABOUT ====
## WD location:02_Scripts/III-Templates/ ACS
## Script: ACS_hhincome_viz
## Purpose: Visualizes Average Household Income (Dot + Line Chart).
#          - LOGIC: Person-Weighted (PERWT).
#          - DATA: Includes all household members (Inclusive).
#          - V2 FIX: Native Skyline decile calculation (Bypasses assign_income_groups)
## Updated: 2026-04-10
## Dependencies: dplyr, readr, here, ggplot2, scales, Hmisc

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales)

# Install/Load Hmisc for weighted quantiles
if(!require(Hmisc)) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

# Source shared utilities (UPDATED TO V2)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))

# --- 1. CONFIG ---
# Pulls the newly cleaned file from the Master Setup script
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_native_hh.rds") 
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds") 

USER_INCOME_VAR   <- "REAL_INCOME"  
USER_WEIGHT_VAR   <- "PERWT"        # Weight by Humans

# --- 2. LOAD & PROCESS ---
message("Loading data and calculating dynamic Skyline deciles...")
acs_data     <- readRDS(INPUT_DATA_FILE)
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# Dynamically group the population and calculate weighted averages
summary_stats <- acs_data %>%
  # Step 1: Assign the Three Countries based on exact cutoffs
  mutate(
    income_tercile = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > main_cutoffs$main_cutoff1 & REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(income_tercile), !is.na(PERWT), PERWT > 0) %>%
  
  # Step 2: Chop each Country into 10 equal weighted deciles (30 points total) for a smooth curve
  group_by(income_tercile) %>%
  mutate(
    decile = as.integer(cut(
      REAL_INCOME, 
      breaks = c(-Inf, wtd.quantile(REAL_INCOME, weights = PERWT, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE), Inf),
      labels = 1:10, 
      include.lowest = TRUE
    ))
  ) %>%
  
  # Step 3: Calculate the average income of each decile
  group_by(income_tercile, decile) %>%
  summarise(
    avg_val = weighted.mean(.data[[USER_INCOME_VAR]], w = .data[[USER_WEIGHT_VAR]], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(income_tercile, decile) %>%
  mutate(
    income_tercile = factor(income_tercile, levels = c("Bottom Third", "Middle Third", "Top Third")),
    x_id = row_number() # 1 to 30 continuous scale across the X-axis
  )

# --- 3. CREATE CUTOFF MARKERS ---
# These sit exactly on the borders between the 3 sections (x = 10.5 and 20.5)
cutoff_markers <- tibble::tibble(
  x_pos = c(10.5, 20.5),
  y_val = c(main_cutoffs$main_cutoff1, main_cutoffs$main_cutoff2),
  income_tercile = factor(c("Bottom Third", "Middle Third"), levels = c("Bottom Third", "Middle Third", "Top Third")),
  label = c(
    paste0("Cutoff:\n", scales::dollar(main_cutoffs$main_cutoff1, accuracy = 1)),
    paste0("Cutoff:\n", scales::dollar(main_cutoffs$main_cutoff2, accuracy = 1))
  )
)

# --- 4. PLOT (Custom Dot & Line Chart) ---
message("Generating Line Graph...")
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

# NEW: Dynamically calculate the top y-axis break (stepping by 50k) 
# so the dashed lines stop exactly at the top of the y-axis line.
top_y_val <- ceiling(max(summary_stats$avg_val, na.rm = TRUE) / 50000) * 50000

p_chart <- ggplot() +
  # Thicker background line for a slight glow effect
  geom_line(data = summary_stats, aes(x = x_id, y = avg_val, color = income_tercile, group = 1), linewidth = 3, alpha = 0.2) + 
  # Main solid line connecting the deciles
  geom_line(data = summary_stats, aes(x = x_id, y = avg_val, color = income_tercile, group = 1), linewidth = 1, linejoin = "round") +
  # Add the standard data points back in on top of the lines
  geom_point(data = summary_stats, aes(x = x_id, y = avg_val, color = income_tercile), size = 2) +
  
  # UPDATED: Added show.legend = FALSE so dashed lines don't clutter the legend
  geom_segment(data = cutoff_markers, aes(x = x_pos, xend = x_pos, y = 0, yend = top_y_val, color = income_tercile), 
               linetype = "dashed", linewidth = 1, alpha = 0.8, show.legend = FALSE) +
  # UPDATED: Added show.legend = FALSE so text characters don't clutter the legend
  geom_text(data = cutoff_markers, aes(x = x_pos, y = y_val, label = label, color = income_tercile), 
            hjust = 1.15, vjust = -0.2, fontface = "bold", size = 11 / .pt, family = "serif", show.legend = FALSE) +
  
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  
  # Format Y-Axis: Start at $0, step by $50,000, let max auto-scale
  scale_y_continuous(
    labels = scales::label_dollar(), 
    breaks = scales::breaks_width(50000), 
    limits = c(0, NA), 
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  theme_minimal() +
  theme(
    text              = element_text(family = "serif", size = 11), 
    
    # UPDATED: Legend styling and positioning
    legend.position   = "bottom",
    legend.title      = element_blank(), # Removes the "income_tercile" title
    legend.text       = element_text(family = "serif", size = 11),
    legend.margin     = margin(t = 20),  # Adds space between the x-axis and the legend
    
    panel.grid.minor  = element_blank(),
    panel.grid.major.x= element_blank(),
    axis.text.x       = element_blank(), 
    axis.text.y       = element_text(color = "black", size = 11, family = "serif"), 
    axis.line.x       = element_line(color = "black", linewidth = 0.5),
    axis.line.y       = element_line(color = "black", linewidth = 0.5), 
    
    # Center the title and add space below it
    plot.title        = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 40), family = "serif"),
    
    # Maintain the padding around the overall chart
    plot.margin       = margin(t = 30, r = 20, b = 30, l = 20),
    
    plot.background   = element_rect(fill = "white", color = NA)
  ) +
  labs(
    title = "Average Household Income", 
    x = NULL,                           
    y = NULL
  )

print(p_chart)

# Output image
out_path <- here::here("03_output", "visualizations_final", "ACS_hhincome_lineplot.png")
ggsave(out_path, p_chart, width = 10, height = 7, dpi = 300, bg = "white")

message("Processing Complete! Graph saved to 03_output/visualizations_final/")