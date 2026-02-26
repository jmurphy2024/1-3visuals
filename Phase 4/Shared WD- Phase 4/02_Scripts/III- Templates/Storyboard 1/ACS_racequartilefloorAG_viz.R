# ==============================================================================
# WD location: 02_Scripts/III-Templates/ACS
# Script: 03_ACS_Race_Viz_Thirds_and_Table.R
# Purpose: 1. Visualizes Racial Composition by Income Third (3 Bars).
#          2. Exports a detailed summary table (Total + 3 Thirds).
# Output:  plot_Race_By_Third.png, table_Race_Summary.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); 
library(scales); library(cowplot); library(stringr); library(grid); library(gridExtra)

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_Race_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Race data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. ASSIGN TERCILES ---
message("Assigning Income Terciles...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

clean_data <- acs_data %>%
  filter(!is.na(REAL_INCOME), !is.na(race_ethnicity)) %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Bottom Third",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Middle Third",
      REAL_INCOME >= limit_2 ~ "Top Third",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_tercile))

# --- 3. PREPARE AGGREGATED DATA ---
message("Calculating Racial Composition...")

# Define Race Order for Consistency
race_levels <- c("White", "Hispanic", "Black", "Asian", "Native American", "Other/Multiracial")

# A. Data for PLOT (By Third)
plot_data <- clean_data %>%
  mutate(race_ethnicity = factor(race_ethnicity, levels = race_levels)) %>%
  group_by(income_tercile, race_ethnicity) %>%
  summarise(count = sum(PERWT), .groups = "drop_last") %>%
  mutate(percentage = count / sum(count)) %>%
  ungroup()

# B. Data for TABLE (Total + Thirds)
# 1. Calculate 'Total' Row
total_stats <- clean_data %>%
  group_by(race_ethnicity) %>%
  summarise(count = sum(PERWT), .groups = "drop") %>%
  mutate(
    income_tercile = "Total Population",
    percentage = count / sum(count)
  )

# 2. Combine with Thirds
table_data_raw <- bind_rows(plot_data, total_stats) %>%
  select(income_tercile, race_ethnicity, count, percentage)

# ==============================================================================
# ==== 4. GENERATE PLOT (3 BARS) ====
# ==============================================================================
message("Generating Plot...")

OUT_DIR <- here::here("03_output", "visualizations_final")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Color Palette
race_colors <- c(
  "White" = "#34495E", "Hispanic" = "#E67E22", "Black" = "#E74C3C",
  "Asian" = "#F1C40F", "Native American" = "#16A085", "Other/Multiracial" = "#95A5A6"
)

# Ensure Thirds are ordered correctly on X-axis
plot_data$income_tercile <- factor(plot_data$income_tercile, 
                                   levels = c("Bottom Third", "Middle Third", "Top Third"))

p <- ggplot(plot_data, aes(x = income_tercile, y = percentage, fill = race_ethnicity)) +
  geom_col(width = 0.7, position = "fill", alpha = 0.95) +
  
  # Text Labels: Show if > 0.5%, Round to 1 decimal place
  geom_text(aes(label = ifelse(percentage > 0.005, 
                               sprintf("%.1f%%", percentage * 100), 
                               "")), 
            position = position_fill(vjust = 0.5), 
            color = "white", fontface = "bold", size = 3.5) +
  
  scale_y_continuous(labels = percent_format(), expand = c(0, 0)) +
  scale_fill_manual(values = race_colors) +
  
  labs(
    title = "Racial Composition by Income Third",
    subtitle = "Distribution of race/ethnicity across the Bottom, Middle, and Top income tiers",
    y = "Percentage of Population",
    x = NULL,
    fill = "Race / Ethnicity"
  ) +
  
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
    axis.text.x = element_text(size = 12, face = "bold", color = "black"),
    legend.position = "bottom",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(file.path(OUT_DIR, "plot_Race_By_Third.png"), plot = p, width = 10, height = 7, bg = "white")
message(" -> Saved Plot: plot_Race_By_Third.png")

# ==============================================================================
# ==== 5. GENERATE SUMMARY TABLE ====
# ==============================================================================
message("Generating Summary Table...")

# A. Pivot Data to Wide Format
table_wide <- table_data_raw %>%
  mutate(
    # Format Percentage for display (1 decimal)
    pct_label = sprintf("%.1f%%", percentage * 100)
  ) %>%
  select(income_tercile, race_ethnicity, pct_label) %>%
  tidyr::pivot_wider(names_from = race_ethnicity, values_from = pct_label, values_fill = "0.0%") 

# B. Get Population Counts (N)
pop_counts <- table_data_raw %>%
  group_by(income_tercile) %>%
  summarise(N = sum(count), .groups = "drop") %>%
  mutate(N_label = scales::comma(N))

# C. Join and Arrange
table_final <- pop_counts %>%
  left_join(table_wide, by = "income_tercile") %>%
  select(income_tercile, N_label, all_of(race_levels)) %>%
  # Order Rows: Total first, then Bottom -> Top
  mutate(sorter = case_when(
    income_tercile == "Total Population" ~ 1,
    income_tercile == "Bottom Third" ~ 2,
    income_tercile == "Middle Third" ~ 3,
    income_tercile == "Top Third" ~ 4
  )) %>%
  arrange(sorter) %>%
  select(-sorter) %>%
  rename(
    "Income Group" = income_tercile,
    "Population (N)" = N_label
  )

# --- D. GRIDEXTRA TABLE GENERATION (Using your style) ---

rows_n <- nrow(table_final)
cols_n <- ncol(table_final)

# Formatting Matrices
adj_hjust <- matrix(0.5, nrow = rows_n, ncol = cols_n)
adj_hjust[, 1] <- 0 # Left align first column
adj_x     <- matrix(0.5, nrow = rows_n, ncol = cols_n)
adj_x[, 1]     <- 0.05 # Indent first column

adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n)
adj_fontface[1, ] <- "bold" # Bold the "Total" row

# Create Table Grob
table_grob <- tableGrob(
  table_final, rows = NULL, 
  theme = ttheme_minimal(
    base_family = "sans",
    core = list(fg_params = list(fontface = adj_fontface, hjust = as.vector(adj_hjust), x = as.vector(adj_x), fontsize = 10)),
    colhead = list(fg_params = list(fontsize = 11, fontface = "bold"), bg_params = list(fill = "#F2F2F2", col = "white"))
  )
)

# Notes Section
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 9, family = "sans"), 
           hjust = 0, x = 0.02, y = 0.90),
  
  textGrob("1. Source: IPUMS ACS 2023. Universe: Total Population.", 
           gp = gpar(fontface = "italic", fontsize = 8.5, family = "sans"), 
           hjust = 0, x = 0.02, y = 0.70),
  
  textGrob("2. Income Thirds are calculated based on national thresholds.", 
           gp = gpar(fontface = "italic", fontsize = 8.5, family = "sans"), 
           hjust = 0, x = 0.02, y = 0.50),
  
  textGrob("3. Percentages may not sum to 100% due to rounding.", 
           gp = gpar(fontface = "italic", fontsize = 8.5, family = "sans"), 
           hjust = 0, x = 0.02, y = 0.30)
)

# Layout: Table gets most space, notes get bottom slice
final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(1.5, "in")))

# Save Table PNG
png_path <- file.path(OUT_DIR, "table_Race_Summary.png")
png(png_path, width = 1000, height = 400, res = 120)
grid.draw(final_layout)
dev.off()

message(" -> Saved Table: table_Race_Summary.png")
message("\n--- Script Complete ---")