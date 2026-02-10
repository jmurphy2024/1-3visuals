# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_agebins_viz.R
# Purpose: Generates 3 separate bar charts using specific Census age ranges.
# Updates: Y-axis limit is now explicitly calculated (Max Bar * 1.15) to guarantee headroom.
# Output:  03_output/visualizations_final/plot_Age_Ranges_Tercile_[1,2,3].png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(cowplot); library(scales); library(stringr)

# --- 1. SETUP ---
SAMPLE_ID <- "us2023c"
INDICATOR <- "Population_Age"
PREP_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_ACS_", INDICATOR, "_", SAMPLE_ID, ".rds"))
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Age data not found.")
if(!file.exists(CUTOFFS_FILE)) stop("Cutoffs file not found.")

acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. MANUAL TERCILE ASSIGNMENT & AGE GROUPING ---
message("Assigning Income Terciles and Age Groups...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

age_levels <- c(
  "Under 5 years", "5 to 17 years", "18 to 24 years", 
  "25 to 34 years", "35 to 44 years", "45 to 64 years", 
  "65 to 84 years", "85 to 99 years", "100 years and over"
)

clean_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Tercile 1",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Tercile 2",
      REAL_INCOME >= limit_2 ~ "Tercile 3",
      TRUE ~ NA_character_
    ),
    age_group = case_when(
      age_clean < 5  ~ "Under 5 years",
      age_clean >= 5  & age_clean <= 17 ~ "5 to 17 years",
      age_clean >= 18 & age_clean <= 24 ~ "18 to 24 years",
      age_clean >= 25 & age_clean <= 34 ~ "25 to 34 years",
      age_clean >= 35 & age_clean <= 44 ~ "35 to 44 years",
      age_clean >= 45 & age_clean <= 64 ~ "45 to 64 years",
      age_clean >= 65 & age_clean <= 84 ~ "65 to 84 years",
      age_clean >= 85 & age_clean <= 99 ~ "85 to 99 years",
      age_clean >= 100 ~ "100 years and over"
    )
  ) %>%
  filter(!is.na(income_tercile), !is.na(age_group)) %>%
  mutate(age_group = factor(age_group, levels = age_levels))

# --- 3. LOOP AND GENERATE PLOTS ---
message("Generating Individual Range Charts...")

tercile_config <- list(
  "1" = list(name = "Bottom Third", color = "#C0392B", filter_match = "Tercile 1"),
  "2" = list(name = "Middle Third", color = "#F5B041", filter_match = "Tercile 2"),
  "3" = list(name = "Top Third",    color = "#27AE60", filter_match = "Tercile 3")
)

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  message(paste("Processing:", config$name))
  
  # Aggregate Data
  plot_data <- clean_data %>%
    filter(income_tercile == config$filter_match) %>%
    group_by(age_group) %>%
    summarise(total_pop = sum(PERWT), .groups = "drop")
  
  # CRITICAL FIX: Explicitly calculate max height + 15% buffer
  max_y_val <- max(plot_data$total_pop)
  y_limit   <- max_y_val * 1.15
  
  # Generate Plot
  p <- ggplot(plot_data, aes(x = age_group, y = total_pop)) +
    geom_col(fill = config$color, color = "white", width = 0.8, alpha = 0.95) +
    
    geom_text(aes(label = label_number(accuracy = 0.1, scale = 1e-6, suffix = "M")(total_pop)), 
              vjust = -0.5, size = 4, fontface = "bold", color = "grey30") +
    
    # Apply Explicit Limit
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = "M"), 
      limits = c(0, y_limit),  # <--- Forces the headroom
      expand = c(0, 0)         # Removes default padding since we set limits manually
    ) +
    
    labs(
      title = paste0("Population by Age Group: ", config$name),
      subtitle = "Weighted count using Census Bureau age brackets",
      x = NULL,
      y = "Population (Millions)"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black"),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
      axis.title.y = element_text(face = "bold", size = 12),
      axis.text.x = element_text(size = 11, face = "bold", angle = 20, hjust = 1),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  filename <- paste0("plot_Age_Ranges_Tercile_", i, ".png")
  ggsave(file.path(out_dir, filename), plot = p, width = 12, height = 8, bg = "white")
  message(paste("  -> Saved:", filename))
}

message("\n--- All range charts generated successfully. ---")