# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_agedecade_viz.R
# Purpose: Generates 3 separate bar charts showing EVERY age year (0-100).
# Fix:     Reduced vertical whitespace between bars and title.
# Output:  03_output/visualizations_final/plot_Age_Years_Tercile_[1,2,3].png
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

# --- 2. MANUAL TERCILE ASSIGNMENT ---
message("Assigning Income Terciles...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

clean_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Tercile 1",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Tercile 2",
      REAL_INCOME >= limit_2 ~ "Tercile 3",
      TRUE ~ NA_character_
    ),
    is_decade = if_else(age_clean %% 10 == 0, "Decade", "Standard")
  ) %>%
  filter(!is.na(income_tercile), age_clean <= 100)

# --- 3. CONFIGURATION ---
tercile_config <- list(
  "1" = list(name = "Bottom Third", filter_match = "Tercile 1", col_dark = "#C0392B", col_light = "#E6B0AA"),
  "2" = list(name = "Middle Third", filter_match = "Tercile 2", col_dark = "#F5B041", col_light = "#FDEBD0"),
  "3" = list(name = "Top Third",    filter_match = "Tercile 3", col_dark = "#27AE60", col_light = "#A9DFBF")
)

# --- 4. LOOP AND GENERATE PLOTS ---
message("Generating Individual Year Charts...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  message(paste("Processing:", config$name))
  
  plot_data <- clean_data %>%
    filter(income_tercile == config$filter_match) %>%
    group_by(age_clean, is_decade) %>%
    summarise(total_pop = sum(PERWT), .groups = "drop")
  
  # --- TIGHTER AXIS FIX ---
  max_val <- max(plot_data$total_pop)
  
  # Reduce headroom buffer to 5% (was 15%)
  upper_limit <- max_val * 1.05 
  
  # Calculate breaks that land just above the bars
  y_breaks <- pretty(c(0, upper_limit), n = 5)
  
  p <- ggplot(plot_data, aes(x = age_clean, y = total_pop, fill = is_decade)) +
    
    geom_col(width = 0.9) +
    
    scale_fill_manual(values = c("Decade" = config$col_dark, "Standard" = config$col_light)) +
    
    scale_y_continuous(
      labels = label_number(scale = 1e-6, suffix = "M"), 
      breaks = y_breaks,
      limits = c(0, max(y_breaks)),
      expand = c(0, 0)
    ) +
    
    scale_x_continuous(
      breaks = seq(0, 100, 10), 
      expand = c(0.01, 0)
    ) +
    
    labs(
      title = paste0("Population by Age: ", config$name),
      # Removed subtitle to save vertical space (optional)
      x = "Age (Years)",
      y = "Population (Millions)"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      # REDUCED MARGINS: Tighter spacing between Title and Plot
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "black", margin = margin(b = 10)),
      
      axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
      axis.text.x = element_text(size = 10, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  filename <- paste0("plot_Age_Years_Tercile_", i, ".png")
  ggsave(file.path(out_dir, filename), plot = p, width = 14, height = 8, bg = "white")
  message(paste("  -> Saved:", filename))
}

message("\n--- All year-by-year charts generated successfully. ---")