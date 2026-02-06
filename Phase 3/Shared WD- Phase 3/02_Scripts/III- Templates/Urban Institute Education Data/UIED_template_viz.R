# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## Script: URBAN_lifelong_master_viz.R
## Purpose: Multi-indicator dashboard showing population-based prevalence.
##          Calculates weighted means across 20 income groups.
## Author: Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(tidyr); library(scales)

# Source Shared functions for income groups and UIED theme
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ================================================================= #
# ==== 1. CONFIGURATION ====
# ================================================================= #
USER_INCOME_VAR <- "HHINCOME"
INDICATORS      <- c("K12_Graduation", "Total_Enrollment", "Adult_Learners", "CTE_Completion")
PROCESSED_DIR   <- here::here("01_data", "processed", "Urban_Education")

# ================================================================= #
# ==== 2. DATA CONSOLIDATION ====
# ================================================================= #
message("Loading and cleaning prepared datasets...")

consolidated_data <- INDICATORS %>%
  purrr::map_df(~ {
    file_path <- file.path(PROCESSED_DIR, paste0("prepared_", .x, ".rds"))
    if(file.exists(file_path)) {
      readRDS(file_path) %>% 
        rename_with(stringr::str_trim) %>% # Robustness against whitespace
        mutate(indicator_name = .x)
    } else { NULL }
  })

if (nrow(consolidated_data) == 0) stop("FATAL: No prepared data found. Run Prep script first.")

# ================================================================= #
# ==== 3. INCOME ASSIGNMENT (SHARED BORDERS) ====
# ================================================================= #
main_cutoffs   <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
within_borders <- read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

data_with_groups <- assign_income_groups(
  data_to_process = consolidated_data,
  borders_df      = within_borders,
  income_var_name = USER_INCOME_VAR,
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# ================================================================= #
# ==== 4. PREVALENCE CALCULATION (WEIGHTED MEAN) ====
# ================================================================= #
message("Calculating weighted prevalence per income group...")

summary_stats <- data_with_groups %>%
  filter(!is.na(income_tercile), !is.na(fine_income_group)) %>%
  group_by(indicator_name, fine_income_group) %>%
  summarise(
    # WEIGHTED MEAN: This is the actual population-based prevalence (%)
    # w = PERWT ensures districts with 10k students weigh more than districts with 100
    prevalence = weighted.mean(indicator_to_plot, w = PERWT, na.rm = TRUE),
    total_pop  = sum(PERWT, na.rm = TRUE), # For internal validation
    .groups = "drop"
  )

# ================================================================= #
# ==== 5. VISUALIZATION ====
# ================================================================= #
final_plot <- ggplot(summary_stats, aes(x = fine_income_group, y = prevalence, 
                                        color = indicator_name, group = indicator_name)) +
  # Add visual vertical breaks for Terciles T1 | T2 | T3
  geom_vline(xintercept = c(6.5, 13.5), linetype = "dashed", color = "gray80") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c(
    "K12_Graduation"   = "#1696d2", # Urban Blue
    "Total_Enrollment" = "#000000", # Black
    "Adult_Learners"   = "#fdbf11", # Urban Gold
    "CTE_Completion"   = "#d2d2d2"  # Gray
  )) +
  theme_minimal(base_family = "serif") +
  labs(
    title    = "Lifelong Learning Pipeline: Population Prevalence",
    subtitle = "Weighted Participation Rates by District Median Income Group (2022)",
    x        = "Income Group (Low T1 to High T3)",
    y        = "Weighted Prevalence (%)",
    color    = "Milestone"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save output
ggsave(here::here("03_output", "visualizations", "lifelong_prevalence_pipeline.png"), 
       final_plot, width = 12, height = 7)

print(final_plot)