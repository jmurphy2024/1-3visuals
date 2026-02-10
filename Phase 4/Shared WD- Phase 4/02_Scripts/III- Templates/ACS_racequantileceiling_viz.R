# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: 03_ACS_Race_Viz_Quantiles_Individual_Ceilings.R
# Purpose: Generates 3 separate charts showing racial composition. 
# "What is the racial makeup of the Bottom Third?"
# Update:  FIXED 'NA' labels by manually assigning main cutoffs to the 20th group.
# Output:  03_output/visualizations_final/plot_Race_Quantiles_Ceilings_Tercile_[1,2,3].png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(cowplot); library(stringr)

# Source Shared Utilities
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_Race_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")
BORDERS_FILE <- here::here("01_data", "processed", "within_tercile_quantile_borders_2023.csv")

if(!file.exists(PREP_FILE)) stop("Prepared Race data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# Load Borders
borders_df <- read_csv(BORDERS_FILE, show_col_types = FALSE)

# --- 2. ROBUST COLUMN RENAMING ---
# Ensure the border column is named 'CutoffValue' for the function
possible_names <- c("border", "value", "income_border", "max_income", "CutoffValue")
found_col <- intersect(names(borders_df), possible_names)[1]

if(is.na(found_col)) {
  numeric_cols <- names(select(borders_df, where(is.numeric)))
  found_col <- tail(numeric_cols, 1)
}

borders_df <- borders_df %>%
  rename(CutoffValue = all_of(found_col)) %>%
  rename(any_of(c(MainTercile = "income_tercile_group")))

# --- 3. ASSIGN GROUPS ---
message("Assigning 20 Groups per Tercile...")

data_grouped <- assign_income_groups(
  data_to_process = acs_data,
  borders_df      = borders_df,
  income_var_name = "REAL_INCOME",
  detail_level    = "Groups_20", 
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
)

# --- 4. PREPARE DATA ---
message("Calculating Composition...")

race_levels <- c("White", "Hispanic", "Black", "Asian", "Native American", "Other/Multiracial")

# Step A: Summarize Race Data
plot_data_summary <- data_grouped %>%
  filter(!is.na(fine_income_group), !is.na(race_ethnicity)) %>%
  mutate(race_ethnicity = factor(race_ethnicity, levels = race_levels)) %>%
  group_by(income_tercile, fine_income_group, race_ethnicity) %>%
  summarise(count = sum(PERWT), .groups = "drop_last") %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup() %>%
  mutate(group_num = as.numeric(str_extract(fine_income_group, "\\d+$")))

# Step B: Get Ceiling Values
if("detail_level" %in% names(borders_df)) {
  borders_subset <- borders_df %>% filter(detail_level == "Groups_20")
} else {
  borders_subset <- borders_df
}

ceiling_labels <- borders_subset %>%
  select(MainTercile, CutoffValue) %>%
  distinct() %>%
  group_by(MainTercile) %>%
  arrange(CutoffValue) %>%
  mutate(group_num = row_number()) %>%
  ungroup() %>%
  select(income_tercile = MainTercile, group_num, max_income = CutoffValue)

# Merge
plot_data_full <- plot_data_summary %>%
  left_join(ceiling_labels, by = c("income_tercile", "group_num"))

# --- 5. FIX "NA" LABELS (CRITICAL STEP) ---
# We manually overwrite the missing ceilings for the last group (20) of each tercile.
message("Fixing missing NA labels using main cutoffs...")

plot_data_full <- plot_data_full %>%
  mutate(
    max_income = case_when(
      # If NA in Bottom Third -> Use Cutoff 1
      is.na(max_income) & str_detect(income_tercile, "Tercile 1") ~ main_cutoffs$main_cutoff1,
      # If NA in Middle Third -> Use Cutoff 2
      is.na(max_income) & str_detect(income_tercile, "Tercile 2") ~ main_cutoffs$main_cutoff2,
      # If NA in Top Third -> Keep as NA (or use max(REAL_INCOME) if preferred)
      TRUE ~ max_income
    )
  )

# --- 6. CONFIGURATION ---
tercile_config <- list(
  "1" = list(name = "Bottom Third", filter_match = "Tercile 1 (Bottom)", title_col = "#C0392B"),
  "2" = list(name = "Middle Third", filter_match = "Tercile 2 (Middle)", title_col = "#F5B041"),
  "3" = list(name = "Top Third",    filter_match = "Tercile 3 (Top)",    title_col = "#27AE60")
)

race_colors <- c(
  "White" = "#34495E", "Hispanic" = "#E67E22", "Black" = "#E74C3C",
  "Asian" = "#F1C40F", "Native American" = "#16A085", "Other/Multiracial" = "#95A5A6"
)

# --- 7. GENERATE PLOTS ---
message("Generating Individual Plots with Custom Labels...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  message(paste("Processing:", config$name))
  
  current_plot_data <- plot_data_full %>%
    filter(income_tercile == config$filter_match)
  
  if(nrow(current_plot_data) == 0) next
  
  # Create Labels: 
  # Divide by 1,000 (scale = 0.001) and remove suffix to get "$116.1"
  current_labels <- current_plot_data %>%
    select(group_num, max_income) %>%
    distinct() %>%
    arrange(group_num) %>%
    mutate(
      label = if_else(
        is.na(max_income), 
        "Max", 
        label_dollar(scale = 0.001, suffix = "", accuracy = 0.1)(max_income)
      )
    )
  
  p <- ggplot(current_plot_data, aes(x = group_num, y = percentage, fill = race_ethnicity)) +
    
    geom_col(width = 0.9, position = "fill", alpha = 0.95) +
    
    # Text Labels
    geom_text(aes(label = ifelse(percentage > 0.04, percent(percentage, accuracy = 1), "")), 
              position = position_fill(vjust = 0.5), 
              color = "white", fontface = "bold", size = 3) +
    
    scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
    
    scale_x_continuous(
      breaks = current_labels$group_num,
      labels = current_labels$label,
      name = "Income Ceiling of Ventile ($ Thousands)", # Clarified unit in axis title
      expand = c(0.01, 0)
    ) +
    
    scale_fill_manual(values = race_colors) +
    
    labs(
      title = paste0("Racial Composition by Income: ", config$name),
     #subtitle = "X-axis shows the maximum household income for each ventile",
      y = "Percentage of Population",
      fill = "Race / Ethnicity"
    ) +
    
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = config$title_col, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1, face = "bold"),
      axis.title = element_text(face = "bold", size = 12),
      legend.position = "bottom",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  filename <- paste0("plot_Race_Quantiles_Ceilings_Tercile_", i, ".png")
  ggsave(file.path(out_dir, filename), plot = p, width = 14, height = 8, bg = "white")
  message(paste("  -> Saved:", filename))
  
  print(p)
  Sys.sleep(0.5)
}

message("\n--- All plots generated with updated labels. ---")