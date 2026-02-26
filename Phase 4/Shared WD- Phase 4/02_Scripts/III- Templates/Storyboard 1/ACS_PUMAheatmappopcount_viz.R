# ==============================================================================
# WD: 02_Scripts
# Script: 21_Map_Bottom_Third_Count_CENTERED.R
# Purpose: Maps the Bottom Third Population Count.
#          - FIX: Removes Puerto Rico (State 72) to fix centering/alignment.
#          - STYLE: Exact match to your reference script.
# Output:  03_output/visualizations_final/Map_Bottom_Third_Count_Centered.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(sf); library(tidyr)
if(!require(tigris)) { install.packages("tigris"); library(tigris) }
if(!require(scales)) { install.packages("scales"); library(scales) }

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared PUMA Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)
names(acs_data) <- toupper(names(acs_data))

# --- 2. CALCULATE POPULATION COUNTS ---
message("Calculating Bottom Third Population Counts...")
limit_1 <- main_cutoffs$main_cutoff1

puma_counts <- acs_data %>%
  filter(REAL_INCOME < limit_1) %>%
  group_by(PUMA_GEOID) %>%
  summarise(bottom_third_count = sum(PERWT)) %>%
  ungroup()

# --- 3. PREPARE MAP (THE ALIGNMENT FIX) ---
message("Loading Map & Fixing Alignment...")

# Load PUMA boundaries
us_pumas <- tigris::pumas(year = 2020, cb = TRUE, progress_bar = FALSE)

# FILTER: Remove Puerto Rico (72) to prevent the map from shifting off-center
# We also ensure we only have the 50 States + DC for a clean national map.
us_pumas_clean <- us_pumas %>%
  filter(!STATEFP20 %in% c("72")) # 72 = Puerto Rico

# SHIFT: Move Alaska & Hawaii to the bottom-left
us_pumas_shifted <- us_pumas_clean %>%
  shift_geometry(position = "below") %>% 
  st_simplify(dTolerance = 100, preserveTopology = TRUE)

# --- 4. MERGE DATA ---
message("Merging data...")
plot_ready_data <- us_pumas_shifted %>%
  left_join(puma_counts, by = c("GEOID20" = "PUMA_GEOID")) %>%
  mutate(bottom_third_count = replace_na(bottom_third_count, 0))

# --- 5. PLOT (EXACT FORMAT MATCH) ---
message("Generating Map...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Cap outliers for better color spread
max_val <- quantile(plot_ready_data$bottom_third_count, 0.99, na.rm=TRUE)

p <- ggplot(plot_ready_data) +
  
  # Geometry
  geom_sf(aes(fill = bottom_third_count), color = NA) +
  
  # Color Scale
  scale_fill_viridis_c(
    option = "rocket",
    direction = 1,
    name = "Population Count",
    limits = c(0, max_val),
    na.value = "grey10",
    labels = scales::comma
  ) +
  
  # Labels
  labs(
    title = "Where the Bottom Third Lives",
    subtitle = "Total population count of lowest-income households per PUMA",
    caption = "Source: ACS 2023 Microdata | Unit: Public Use Microdata Areas (PUMAs)",
    x = NULL, y = NULL
  ) +
  
  # THEME: Exact dimensions and margins from your snippet
  theme_void() + 
  theme(
    plot.title    = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
    legend.position = "right",
    legend.title    = element_text(face = "bold", size = 10, color = "#2C3E50"),
    legend.text     = element_text(size = 9, color = "#2C3E50"),
    plot.caption    = element_text(size = 8, color = "#95A5A6", margin = margin(t = 20)),
    plot.margin     = margin(20, 20, 20, 20)
  )

# --- 6. SAVE OUTPUT ---
out_file <- file.path(out_dir, "Map_Bottom_Third_Count_Centered.png")

# Save as PNG with White Background
ggsave(out_file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")

message(paste("Map saved successfully to:", out_file))