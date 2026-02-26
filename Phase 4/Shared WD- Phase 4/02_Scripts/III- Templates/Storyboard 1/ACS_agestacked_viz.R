# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_agestacked_viz.R
# Purpose: Generates a SINGLE stacked bar chart.
# FIX:     Manually assigns Terciles using cutoffs to guarantee 0% duplication.
# Output:  03_output/visualizations_final/plot_Age_Histogram_Stacked.png
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

# Extract numeric cutoffs
limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

plot_data_clean <- acs_data %>%
  # 1. Assign Group directly (No Joins = No Duplication)
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Tercile 1 (Bottom)",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Tercile 2 (Middle)",
      REAL_INCOME >= limit_2 ~ "Tercile 3 (Top)",
      TRUE ~ NA_character_
    )
  ) %>%
  # 2. Filter valid data
  filter(!is.na(income_tercile), age_clean <= 90)

# --- 3. DIAGNOSTIC CHECK ---
# Verify the total matches US Population (~335M)
total_pop_verify <- sum(plot_data_clean$PERWT)
message(paste("\n>>> VERIFICATION: Total US Population in Chart:", comma(total_pop_verify)))

if(total_pop_verify > 350000000) warning("WARNING: Population still seems too high!")

# --- 4. SUMMARIZE ---
plot_data_summary <- plot_data_clean %>%
  group_by(age_clean, income_tercile) %>%
  summarise(total_pop = sum(PERWT), .groups = "drop") %>%
  # Order: Top on bottom (foundation), then Middle, then Bottom
  mutate(income_tercile = factor(income_tercile, levels = c("Tercile 1 (Bottom)", "Tercile 2 (Middle)", "Tercile 3 (Top)")))

# --- 5. GENERATE CHART ---
message("Generating Final Plot...")

T1_COLOR <- "#C0392B"
T2_COLOR <- "#F5B041"
T3_COLOR <- "#27AE60"

p <- ggplot(plot_data_summary, aes(x = age_clean, y = total_pop, fill = income_tercile)) +
  
  # Stacked Bar
  geom_bar(stat = "identity", position = "stack", width = 0.9, alpha = 0.95) +
  
  # Manual Colors
  scale_fill_manual(
    values = c(
      "Tercile 1 (Bottom)" = T1_COLOR, 
      "Tercile 2 (Middle)" = T2_COLOR, 
      "Tercile 3 (Top)"    = T3_COLOR
    ),
    labels = c("Bottom Third", "Middle Third", "Top Third")
  ) +
  
  # Scales
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = "M"), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 90, 10), expand = c(0.01, 0)) +
  
  labs(
    title = "US Population by Age and Income Group",
    subtitle = paste0("Total Weighted Population: ", comma(round(total_pop_verify))),
    x = "Age (Years)",
    y = "Population (Millions)",
    fill = "Household Income"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank()
  )

# --- 6. SAVE ---
output_path <- here::here("03_output", "visualizations_final", "plot_Age_Histogram_Stacked_Corrected.png")
dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)

ggsave(output_path, plot = p, width = 12, height = 8, bg = "white")
message(paste("Final Corrected Chart saved to:", output_path))