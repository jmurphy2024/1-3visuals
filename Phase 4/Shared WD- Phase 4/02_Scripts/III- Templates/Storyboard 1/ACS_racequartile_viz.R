# ==============================================================================
# SCRIPT: ACS_race_quartile_stacked_viz.R
# Purpose: 12-Group Racial Composition (Quartiles within Bottom, Middle, Top).
# Updates: Colored X-axis labels, Dash-format (Bottom-1), and % >= 4% labels only.
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(tidyr)

# --- 1. SETUP & DATA LOADING ---
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(INPUT_DATA_FILE)) stop("Master Prepared Data not found.")
acs_data     <- readRDS(INPUT_DATA_FILE)
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- 2. ASSIGN 12 GROUPS & AGGREGATE ---
message("Splitting 3 Countries into 12 groups (Bottom-1, etc)...")

plot_data <- acs_data %>%
  mutate(
    tercile_name = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle",
      TRUE ~ "Top"
    )
  ) %>%
  group_by(tercile_name) %>%
  mutate(
    quartile = ntile(REAL_INCOME, 4),
    # FORMAT: [Tercile]-[Quartile] e.g., Bottom-1
    group_label = factor(paste0(tercile_name, "-", quartile),
                         levels = c(paste0("Bottom-", 1:4), 
                                    paste0("Middle-", 1:4), 
                                    paste0("Top-",    1:4)))
  ) %>%
  group_by(group_label, Race_Ethnicity) %>%
  summarise(count = sum(PERWT), .groups = "drop") %>%
  group_by(group_label) %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

# --- 3. CONFIGURATION (Colors) ---

# A. Segment Colors (Bars)
race_colors <- c(
  "White (NH)"           = "#2C3E50", # Dark Blue
  "Hispanic"             = "#E67E22", # Orange
  "Black (NH)"           = "#2980B9", # Blue
  "Asian & PI (NH)"      = "#8E44AD", # Purple
  "Native American (NH)" = "#16A085", # Teal
  "Multiracial (NH)"     = "#95A5A6", # Grey
  "Other (NH)"           = "#D5DBDB"  # Light Grey
)

# B. Label Colors (X-Axis Text)
label_colors <- c(
  rep("#C0392B", 4), # Bottom (Red)
  rep("#F5B041", 4), # Middle (Yellow/Orange)
  rep("#27AE60", 4)  # Top (Green)
)

# --- 4. VISUALIZATION ---
message("Generating the 12-group chart with % labels >= 4%...")

p <- ggplot(plot_data, aes(x = group_label, y = percentage, fill = Race_Ethnicity)) +
  
  # Stacked Bar
  geom_col(position = "fill", width = 0.8) +
  
  # Percentage Labels: Shown ONLY if >= 4% (0.04)
  geom_text(aes(label = ifelse(percentage >= 0.04, percent(percentage, accuracy = 1), "")), 
            position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 2.5) +
  
  # Scales
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = race_colors) +
  
  labs(
    title = "Racial Composition",
    subtitle = "Quartile segments within the Bottom, Middle, and Top Thirds",
    x = NULL, 
    y = "Percentage of Population",
    fill = "Race / Ethnicity",
    #caption = "Source: ACS 5-Year Inclusive Estimates | Weighted by PERWT (342M Target)"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30", margin = margin(b = 15)),
    
    # Colored X-Axis labels matching the Economic Country
    axis.text.x = element_text(color = label_colors, face = "bold", size = 10, angle = 0, hjust = 0.5),
    
    axis.title.y = element_text(face = "bold", size = 11),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    plot.margin = margin(20, 20, 20, 20)
  )

# --- 5. SAVE ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "Race_12_Group_Stacked_FilteredLabels.png"), 
       p, width = 12, height = 7, dpi = 300, bg = "white")

message("Success: 12-Group Stacked Bar (Labels >= 4%) saved.")