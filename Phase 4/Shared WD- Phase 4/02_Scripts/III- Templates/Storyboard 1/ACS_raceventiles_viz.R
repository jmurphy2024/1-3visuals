# ==============================================================================
# SCRIPT: ACS_race_ventile_continuous_final.R
# Purpose: Racial Composition with high-distinction monochromatic seamless bars.
# Logic:   7 colors in the same tone as #C0392B (2nd darkest). 
#          No borders, adaptive labels, wide horizontal legend.
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(tidyr); library(haven)

# --- 1. SETUP & DATA LOADING ---
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(INPUT_DATA_FILE)) stop("Master Prepared Data not found.")

acs_data <- readRDS(INPUT_DATA_FILE) %>% haven::zap_labels()
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- 2. DATA PREPARATION ---
race_mapping <- c(
  "White (NH)"           = "White",
  "Black (NH)"           = "Black",
  "Native American (NH)" = "Indigenous",
  "Asian & PI (NH)"      = "Asian", 
  "Other (NH)"           = "Other",
  "Multiracial (NH)"     = "Multiracial",
  "Hispanic"             = "Latino"
)

bottom_third <- acs_data %>%
  filter(REAL_INCOME <= main_cutoffs$main_cutoff1)

plot_data <- bottom_third %>%
  mutate(ventile = ntile(REAL_INCOME, 20)) %>%
  group_by(ventile, Race_Ethnicity) %>%
  summarise(count = sum(PERWT), .groups = "drop") %>%
  group_by(ventile) %>%
  mutate(
    percentage = count / sum(count),
    x_mid = (ventile - 1) * 5 + 2.5,
    race_label = factor(recode(Race_Ethnicity, !!!race_mapping), 
                        levels = c("Latino", "Multiracial", "Other", "Asian", "Indigenous", "Black", "White")),
    # Adaptive Text Contrast: Only the darkest 3 shades use white text
    text_color = ifelse(race_label %in% c("White", "Black", "Indigenous"), "white", "black")
  ) %>%
  ungroup()

# --- 3. INCOME RULER (X-AXIS) ---
income_targets <- seq(0, floor(main_cutoffs$main_cutoff1 / 10000) * 10000, by = 10000)
label_positions <- sapply(income_targets, function(val) {
  mean(bottom_third$REAL_INCOME < val, na.rm = TRUE) * 100
})
final_labels <- data.frame(
  pos = c(label_positions, 100),
  text = c(paste0("$", income_targets / 1000), paste0("$", round(main_cutoffs$main_cutoff1 / 1000)))
)

# --- 4. COLOR CONFIGURATION (MONOCHROMATIC #C0392B TONES) ---
# Strictly monochromatic: Anchor #C0392B is the 2nd darkest.
race_colors <- c(
  "White"       = "#641E16", # Darkest Shade
  "Black"       = "#C0392B", # 2nd Darkest Anchor
  "Indigenous"  = "#D94E41", # Progressive tint
  "Asian"       = "#E67C71", # Progressive tint
  "Other"       = "#F0A69F", # Progressive tint
  "Multiracial" = "#F9D0CC", # Progressive tint
  "Latino"      = "#FDF2F1"  # Lightest Tint
)

# --- 5. VISUALIZATION ---
p <- ggplot(plot_data, aes(x = x_mid, y = percentage, fill = race_label)) +
  
  # SEAMLESS BARS: Removed all white borders
  geom_col(position = "fill", width = 5, color = NA) +
  
  # PERCENTAGE LABELS: Visible if >= 4%, with adaptive contrast
  geom_text(aes(label = ifelse(percentage >= 0.04, percent(percentage, accuracy = 1), ""),
                color = text_color), 
            position = position_fill(vjust = 0.5), 
            fontface = "bold", size = 3) +
  
  scale_color_identity() +
  
  scale_x_continuous(
    breaks = final_labels$pos,
    labels = final_labels$text,
    expand = c(0, 0), 
    limits = c(0, 100)
  ) +
  
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = race_colors) +
  
  labs(title = NULL, subtitle = NULL, x = "Household Income (Thousands)", y = "Percentage of Population", fill = NULL) +
  
  theme_minimal(base_family = "sans") +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(color = "black", face = "bold", size = 10),
    axis.title.x = element_text(face = "bold", size = 13, margin = margin(t = 15)),
    axis.title.y = element_text(face = "bold", size = 12),
    
    # STRETCHED HORIZONTAL LEGEND (Single Row)
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.key.width = unit(4.5, "line"), 
    legend.text = element_text(size = 10, margin = margin(r = 10)),
    plot.margin = margin(20, 25, 20, 25)
  ) +
  guides(fill = guide_legend(nrow = 1, label.position = "bottom"))

# --- 6. SAVE ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "Race_Low_Income_Monochromatic.png"), 
       p, width = 16, height = 8, dpi = 300, bg = "white")