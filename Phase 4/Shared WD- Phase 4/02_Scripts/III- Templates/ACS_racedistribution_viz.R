# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: 03_ACS_Race_Viz_Distribution.R
# Purpose: Shows where each racial group falls within the economy.
#          Calculates: "X% of [Race] population belongs to [Tercile]"
# "What percentage of all White people fall into the Bottom, Middle, and Top Thirds?"
# Output:  03_output/visualizations_final/plot_Race_Distribution_Within_Group.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(cowplot); library(stringr)

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_Race_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Race data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. ASSIGN TERCILES ---
message("Assigning Income Terciles...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

race_levels <- c("White", "Asian", "Hispanic", "Black", "Native American", "Other/Multiracial")

plot_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Bottom Third",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Middle Third",
      REAL_INCOME >= limit_2 ~ "Top Third",
      TRUE ~ NA_character_
    ),
    race_ethnicity = factor(race_ethnicity, levels = race_levels)
  ) %>%
  filter(!is.na(income_tercile), !is.na(race_ethnicity)) %>%
  
  # Group by RACE first, then Tercile
  group_by(race_ethnicity, income_tercile) %>%
  summarise(count = sum(PERWT), .groups = "drop_last") %>%
  
  # Calculate % of THAT RACE in each tercile
  mutate(percentage = count / sum(count)) %>%
  ungroup() %>%
  
  # Reorder Terciles for Stacking (Bottom -> Middle -> Top)
  mutate(income_tercile = factor(income_tercile, levels = c("Top Third", "Middle Third", "Bottom Third")))

# --- 3. GENERATE CHART ---
message("Generating Distribution Plot...")

# Tercile Colors
tercile_colors <- c(
  "Bottom Third" = "#C0392B",  # Red
  "Middle Third" = "#F5B041",  # Gold
  "Top Third"    = "#27AE60"   # Green
)

p <- ggplot(plot_data, aes(x = race_ethnicity, y = percentage, fill = income_tercile)) +
  
  # Stacked Bar
  geom_col(width = 0.75, color = "white", alpha = 0.95) +
  
  # Add Percentage Labels inside bars
  geom_text(aes(label = percent(percentage, accuracy = 1)), 
            position = position_stack(vjust = 0.5), 
            color = "white", fontface = "bold", size = 4) +
  
  # Horizontal Line at 33% (Equality Benchmark)
  geom_hline(yintercept = 0.33, linetype = "dashed", color = "white", alpha = 0.5) +
  geom_hline(yintercept = 0.66, linetype = "dashed", color = "white", alpha = 0.5) +
  
  # Scales
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = tercile_colors, breaks = c("Top Third", "Middle Third", "Bottom Third")) +
  
  # Labels
  labs(
    title = "Economic Status by Race/Ethnicity",
    subtitle = "How the population of each group is distributed across income terciles",
    x = NULL,
    y = "Percentage of Group",
    fill = "Income Group"
  ) +
  
  # Theme
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.title.y = element_text(face = "bold", size = 12),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

# --- 4. SAVE & DISPLAY ---
out_path <- here::here("03_output", "visualizations_final", "plot_Race_Distribution_Within_Group.png")
dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)

ggsave(out_path, plot = p, width = 11, height = 7, bg = "white")
message(paste("Saved plot to:", out_path))

print(p)