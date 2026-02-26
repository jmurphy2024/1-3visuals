# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_age_viz.R
# Purpose: Generates THREE separate histograms (Bottom, Middle, Top).
# FIX:     Uses manual assignment (case_when) to guarantee 0% duplication.
# Output:  03_output/visualizations_final/plot_Age_Histogram_Tercile_[1,2,3].png
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

# --- 2. MANUAL TERCILE ASSIGNMENT (FAIL-SAFE) ---
message("Assigning Income Terciles Manually...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

# We create a clean dataset with explicit tercile labels
clean_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Tercile 1",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Tercile 2",
      REAL_INCOME >= limit_2 ~ "Tercile 3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_tercile), age_clean <= 90)

# --- 3. LOOP AND GENERATE PLOTS ---
message("Generating Individual Histograms...")

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
  
  # Filter & Aggregate
  # We aggregate by Age first to ensure 1-year bars are perfectly clean
  plot_data <- clean_data %>%
    filter(income_tercile == config$filter_match) %>%
    group_by(age_clean) %>%
    summarise(total_pop = sum(PERWT), .groups = "drop")
  
  # Generate Plot
  p <- ggplot(plot_data, aes(x = age_clean, y = total_pop)) +
    
    # Histogram (Stat Identity because we pre-calculated sums)
    geom_col(fill = config$color, color = "white", width = 0.95, alpha = 0.95) +
    
    # Formatting
    scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"), expand = c(0, 0)) +
    scale_x_continuous(breaks = seq(0, 90, 10), expand = c(0.01, 0)) +
    
    labs(
      title = paste0("Population Age Structure: ", config$name),
      subtitle = paste0("Total Weighted Population: ", comma(sum(plot_data$total_pop))),
      x = "Age (Years)",
      y = "Population (Millions)"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = config$color),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40"),
      axis.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )
  
  # Save Individual File
  filename <- paste0("AgeHistogram_Tercile_", i, "_Corrected.png")
  output_path <- file.path(out_dir, filename)
  
  ggsave(output_path, plot = p, width = 10, height = 7, bg = "white")
  message(paste("  -> Saved:", filename))
}

message("\n--- All histograms generated successfully. ---")