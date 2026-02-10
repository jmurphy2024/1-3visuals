# ==============================================================================
# SCRIPT 3: VISUALIZE (Race by Quartile - CUSTOM LABELS)
# Script: 03_ACS_Race_Viz_Quartiles_Individual_Ceilings.R
# Purpose: Generates 3 separate charts showing racial composition.
# Update:  Breaks each Tercile into 4 QUARTILES (instead of 20 Ventiles).
# Output:  03_output/visualizations_final/plot_Race_Quartiles_Ceilings_Tercile_[1,2,3].png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(cowplot); library(stringr)

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_Race_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Race data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. ASSIGN TERCILES AND QUARTILES ---
message("Assigning Income Terciles and Quartiles...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

# A. Assign Main Terciles
clean_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Tercile 1 (Bottom)",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Tercile 2 (Middle)",
      REAL_INCOME >= limit_2 ~ "Tercile 3 (Top)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_tercile), !is.na(race_ethnicity))

# B. Assign Quartiles (1-4) WITHIN each Tercile
# We use 'ntile' weighted by PERWT to ensure population-representative groups
# Note: ntile doesn't natively support weights well, so we use a robust weighted rank approach 
# or simpler unweighted if weights are consistent. For ACS, we'll use a robust weighted approach:

data_grouped <- clean_data %>%
  group_by(income_tercile) %>%
  arrange(REAL_INCOME) %>%
  mutate(
    cum_weight = cumsum(PERWT),
    total_weight = sum(PERWT),
    # Calculate percentile position (0 to 1)
    percentile = cum_weight / total_weight,
    # Assign Quartile based on percentile (1=0-25%, 2=25-50%, etc.)
    quartile_group = ceiling(percentile * 4)
  ) %>%
  ungroup() %>%
  # Ensure max group is 4 (handles tiny floating point edges)
  mutate(quartile_group = pmin(quartile_group, 4))

# --- 3. CALCULATE INCOME CEILINGS ---
message("Calculating Income Ceilings for each Quartile...")

# We determine the max income for each quartile to use as the label
quartile_ceilings <- data_grouped %>%
  group_by(income_tercile, quartile_group) %>%
  summarise(max_income = max(REAL_INCOME), .groups = "drop") %>%
  # Fix the ceiling for the very last group of Bottom/Middle to match main cutoffs exactly
  mutate(
    max_income = case_when(
      quartile_group == 4 & str_detect(income_tercile, "Tercile 1") ~ limit_1,
      quartile_group == 4 & str_detect(income_tercile, "Tercile 2") ~ limit_2,
      TRUE ~ max_income
    )
  )

# --- 4. PREPARE PLOT DATA ---
message("Calculating Racial Composition...")

race_levels <- c("White", "Hispanic", "Black", "Asian", "Native American", "Other/Multiracial")

plot_data_full <- data_grouped %>%
  mutate(race_ethnicity = factor(race_ethnicity, levels = race_levels)) %>%
  group_by(income_tercile, quartile_group, race_ethnicity) %>%
  summarise(count = sum(PERWT), .groups = "drop_last") %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup() %>%
  # Join with Ceilings
  left_join(quartile_ceilings, by = c("income_tercile", "quartile_group"))

# --- 5. CONFIGURATION ---
tercile_config <- list(
  "1" = list(name = "Bottom Third", filter_match = "Tercile 1 (Bottom)", title_col = "#C0392B"),
  "2" = list(name = "Middle Third", filter_match = "Tercile 2 (Middle)", title_col = "#F5B041"),
  "3" = list(name = "Top Third",    filter_match = "Tercile 3 (Top)",    title_col = "#27AE60")
)

race_colors <- c(
  "White" = "#34495E", "Hispanic" = "#E67E22", "Black" = "#E74C3C",
  "Asian" = "#F1C40F", "Native American" = "#16A085", "Other/Multiracial" = "#95A5A6"
)

# --- 6. GENERATE PLOTS ---
message("Generating Individual Quartile Plots...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  message(paste("Processing:", config$name))
  
  current_plot_data <- plot_data_full %>%
    filter(income_tercile == config$filter_match)
  
  if(nrow(current_plot_data) == 0) next
  
  # Create Labels: Divide by 1,000 and remove suffix -> "$116.1"
  current_labels <- current_plot_data %>%
    select(quartile_group, max_income) %>%
    distinct() %>%
    arrange(quartile_group) %>%
    mutate(
      label = if_else(
        # Check if it's the absolute max of Top Tercile (keep as "Max" or value?)
        # For consistency, we label all. If distinct logic needed for top, add here.
        is.na(max_income), "Max", 
        label_dollar(scale = 0.001, suffix = "", accuracy = 0.1)(max_income)
      )
    )
  
  p <- ggplot(current_plot_data, aes(x = quartile_group, y = percentage, fill = race_ethnicity)) +
    
    geom_col(width = 0.85, position = "fill", alpha = 0.95) +
    
    # Text Labels (Show if > 4%)
    geom_text(aes(label = ifelse(percentage > 0.04, percent(percentage, accuracy = 1), "")), 
              position = position_fill(vjust = 0.5), 
              color = "white", fontface = "bold", size = 4) +
    
    scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
    
    # X-Axis: 1-4 with Ceiling Labels
    scale_x_continuous(
      breaks = current_labels$quartile_group,
      labels = current_labels$label,
      name = "Income Ceiling of Quartile ($ Thousands)",
      expand = c(0.01, 0)
    ) +
    
    scale_fill_manual(values = race_colors) +
    
    labs(
      title = paste0("Racial Composition by Quartile: ", config$name),
      #subtitle = "Breakdown of this income third into 4 sub-groups (Quartiles)",
      y = "Percentage of Population",
      fill = "Race / Ethnicity"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = config$title_col, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
      axis.text.x = element_text(size = 11, face = "bold"),
      axis.title = element_text(face = "bold", size = 12),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  filename <- paste0("plot_Race_Quartiles_Ceilings_Tercile_", i, ".png")
  ggsave(file.path(out_dir, filename), plot = p, width = 12, height = 8, bg = "white")
  message(paste("  -> Saved:", filename))
  
  print(p)
  Sys.sleep(0.5)
}

message("\n--- All quartile charts generated successfully. ---")