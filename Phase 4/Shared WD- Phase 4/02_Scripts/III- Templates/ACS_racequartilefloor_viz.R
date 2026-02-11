# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: 03_ACS_Race_Viz_Quartiles_Individual_Floors.R
# Purpose: Generates 3 separate charts showing racial composition.
# Update:  X-Axis now uses the FLOOR (Minimum) income of each group.
# Output:  03_output/visualizations_final/plot_Race_Quartiles_Floors_Tercile_[1,2,3].png
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
# Using weighted rank calculation
data_grouped <- clean_data %>%
  group_by(income_tercile) %>%
  arrange(REAL_INCOME) %>%
  mutate(
    cum_weight = cumsum(PERWT),
    total_weight = sum(PERWT),
    percentile = cum_weight / total_weight,
    quartile_group = ceiling(percentile * 4)
  ) %>%
  ungroup() %>%
  mutate(quartile_group = pmin(quartile_group, 4))

# --- 3. CALCULATE INCOME FLOORS (MINIMUMS) ---
message("Calculating Income Floors for each Quartile...")

# Instead of MAX, we calculate MIN (Floor)
quartile_floors <- data_grouped %>%
  group_by(income_tercile, quartile_group) %>%
  summarise(min_income = min(REAL_INCOME), .groups = "drop") %>%
  # Fix the "Floor" for the 1st group of Middle/Top to match main cutoffs exactly
  mutate(
    min_income = case_when(
      quartile_group == 1 & str_detect(income_tercile, "Tercile 2") ~ limit_1,
      quartile_group == 1 & str_detect(income_tercile, "Tercile 3") ~ limit_2,
      TRUE ~ min_income
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
  # Join with FLOORS
  left_join(quartile_floors, by = c("income_tercile", "quartile_group"))

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
  
  # Create Labels: "$116.1+" using FLOOR
  current_labels <- current_plot_data %>%
    select(quartile_group, min_income) %>%
    distinct() %>%
    arrange(quartile_group) %>%
    mutate(
      label = paste0(
        label_dollar(scale = 0.001, suffix = "", accuracy = 0.1)(min_income),
        "+"
      )
    )
  
  p <- ggplot(current_plot_data, aes(x = quartile_group, y = percentage, fill = race_ethnicity)) +
    
    geom_col(width = 0.85, position = "fill", alpha = 0.95) +
    
    # Text Labels (Show if > 4%)
    geom_text(aes(label = ifelse(percentage > 0.04, percent(percentage, accuracy = 1), "")), 
              position = position_fill(vjust = 0.5), 
              color = "white", fontface = "bold", size = 4) +
    
    scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
    
    # X-Axis: 1-4 with FLOOR Labels
    scale_x_continuous(
      breaks = current_labels$quartile_group,
      labels = current_labels$label,
      name = "Income Floor of Quartile ($ Thousands)",
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
  
  filename <- paste0("plot_Race_Quartiles_Floors_Tercile_", i, ".png")
  ggsave(file.path(out_dir, filename), plot = p, width = 12, height = 8, bg = "white")
  message(paste("  -> Saved:", filename))
  
  print(p)
  Sys.sleep(0.5)
}

message("\n--- All quartile charts (Floors) generated successfully. ---")