# ==============================================================================
# SCRIPT 3: VISUALIZE (Race by Tercile)
# Script: 03_ACS_Race_Viz.R
# Purpose: Stacked Bar Chart of Race/Ethnicity composition per Income Tercile.
# Output:  03_output/visualizations_final/plot_Race_Composition_by_Tercile.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(cowplot)

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_Race_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Race data not found. Run Script 2.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. ASSIGN TERCILES ---
message("Assigning Terciles...")
limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

plot_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Bottom Third",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Middle Third",
      REAL_INCOME >= limit_2 ~ "Top Third",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_tercile), !is.na(race_ethnicity)) %>%
  # Sum weights by Tercile and Race
  group_by(income_tercile, race_ethnicity) %>%
  summarise(total_pop = sum(PERWT), .groups = "drop_last") %>%
  # Calculate Proportions
  mutate(percentage = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  # Reorder Factors for cleaner plotting
  mutate(
    income_tercile = factor(income_tercile, levels = c("Bottom Third", "Middle Third", "Top Third")),
    race_ethnicity = factor(race_ethnicity, levels = c("White", "Hispanic", "Black", "Asian", "Other/Multiracial", "Native American"))
  )

# --- 3. GENERATE CHART ---
message("Generating Plot...")

# Custom Colors
race_colors <- c(
  "White" = "#34495E",               # Dark Grey/Blue
  "Hispanic" = "#E67E22",            # Orange
  "Black" = "#E74C3C",               # Red
  "Asian" = "#F1C40F",               # Yellow
  "Other/Multiracial" = "#95A5A6",   # Grey
  "Native American" = "#16A085"      # Teal
)

p <- ggplot(plot_data, aes(x = income_tercile, y = percentage, fill = race_ethnicity)) +
  
  # Stacked Bar
  geom_col(width = 0.7, position = "fill") +
  
  # Labels (Percentage) - Only show if > 3% to avoid clutter
  geom_text(aes(label = ifelse(percentage > 0.03, percent(percentage, accuracy = 1), "")), 
            position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 4) +
  
  # Scales
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = race_colors) +
  
  # Labels
  labs(
    title = "Racial Composition",
    #subtitle = "Proportion of population within each income tercile by race/ethnicity",
    x = NULL,
    y = "Percentage of Group Population",
    fill = "Race / Ethnicity"
  ) +
  
  # Theme
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
    axis.text.x = element_text(face = "bold", size = 12),
    legend.position = "right",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# --- 4. SAVE ---
out_path <- here::here("03_output", "visualizations_final", "Race by Tercile.png")
ggsave(out_path, plot = p, width = 10, height = 7, bg = "white")
message(paste("Saved plot to:", out_path))