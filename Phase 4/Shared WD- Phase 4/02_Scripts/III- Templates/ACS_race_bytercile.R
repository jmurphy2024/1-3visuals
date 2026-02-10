# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: 03_ACS_Race_Viz_Individual.R
# Purpose: Generates 3 separate bar charts showing Racial Composition.
# "What is the racial makeup of the Bottom Third?"
# Updates: Fixed 'theme' duplicate argument error.
# Output:  03_output/visualizations_final/plot_Race_Composition_Tercile_[1,2,3].png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(cowplot); library(stringr)

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_Race_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Race data not found. Please run the Acquire/Prepare scripts for Race.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. ASSIGN TERCILES ---
message("Assigning Income Terciles...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

race_levels <- c("White", "Hispanic", "Black", "Asian", "Native American", "Other/Multiracial")

clean_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Tercile 1",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Tercile 2",
      REAL_INCOME >= limit_2 ~ "Tercile 3",
      TRUE ~ NA_character_
    ),
    race_ethnicity = factor(race_ethnicity, levels = race_levels)
  ) %>%
  filter(!is.na(income_tercile), !is.na(race_ethnicity))

# --- 3. CONFIGURATION ---
tercile_config <- list(
  "1" = list(name = "Bottom Third", filter_match = "Tercile 1", title_col = "#C0392B"),
  "2" = list(name = "Middle Third", filter_match = "Tercile 2", title_col = "#F5B041"),
  "3" = list(name = "Top Third",    filter_match = "Tercile 3", title_col = "#27AE60")
)

race_colors <- c(
  "White" = "#34495E",               
  "Hispanic" = "#E67E22",            
  "Black" = "#E74C3C",               
  "Asian" = "#F1C40F",               
  "Native American" = "#16A085",     
  "Other/Multiracial" = "#95A5A6"    
)

# --- 4. LOOP AND GENERATE PLOTS ---
message("Generating Individual Race Charts...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  message(paste("Processing:", config$name))
  
  plot_data <- clean_data %>%
    filter(income_tercile == config$filter_match) %>%
    group_by(race_ethnicity) %>%
    summarise(total_pop = sum(PERWT), .groups = "drop") %>%
    mutate(percentage = total_pop / sum(total_pop))
  
  p <- ggplot(plot_data, aes(x = race_ethnicity, y = percentage, fill = race_ethnicity)) +
    
    geom_col(width = 0.75, color = "white", alpha = 0.95) +
    
    geom_text(aes(label = percent(percentage, accuracy = 0.1)), 
              vjust = -0.5, size = 4.5, fontface = "bold", color = "grey30") +
    
    scale_y_continuous(labels = percent_format(), limits = c(0, max(plot_data$percentage) * 1.15), expand = c(0, 0)) +
    scale_fill_manual(values = race_colors) +
    
    labs(
      title = paste0("Racial Composition: ", config$name),
      subtitle = "Proportion of population within this income bracket",
      x = NULL,
      y = "Percentage of Population"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = config$title_col, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
      axis.text.x = element_text(size = 11, face = "bold", angle = 15, hjust = 1),
      axis.title.y = element_text(face = "bold", size = 12),
      legend.position = "none",
      # FIX: Removed the duplicate 'panel.grid.major.x' line below
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  filename <- paste0("plot_Race_Composition_Tercile_", i, ".png")
  ggsave(file.path(out_dir, filename), plot = p, width = 10, height = 7, bg = "white")
  message(paste("  -> Saved:", filename))
  
  print(p)
  Sys.sleep(0.5) 
}

message("\n--- All race charts generated and displayed. ---")