# ==============================================================================
# SCRIPT 7: VISUALIZE (Predominant Group with 3-LEVEL GRANULARITY)
# Script: 07_ACS_Map_Predominant_Tercile_3_Level.R
# Purpose: Maps the "Winner" using a 3-color scale (Low/Med/High Intensity).
# Update:  Reduced granularity from 5 levels to 3 for clarity.
# Output:  03_output/visualizations_final/map_Predominant_3_Level.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(sf); library(tidyr)

# --- 1. SETUP ---
if(!require(tigris)) { install.packages("tigris"); library(tigris) }

PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared PUMA Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. LOAD MAP SHAPEFILE ---
map_url  <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_puma20_500k.zip"
dest_zip <- here::here("01_data", "raw", "cb_2020_us_puma20_500k.zip")
shp_dir  <- here::here("01_data", "raw", "puma_shapefiles_2020")

if(!file.exists(dest_zip)) {
  tryCatch({ options(timeout = 300); download.file(map_url, dest_zip, mode = "wb") }, 
           error = function(e) stop("Download failed."))
}
if(!dir.exists(shp_dir)) unzip(dest_zip, exdir = shp_dir)

shp_file <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
us_pumas_raw <- st_read(shp_file, quiet = TRUE)

message("Shifting geometry...")
us_pumas <- us_pumas_raw %>% shift_geometry() %>% st_make_valid() 

# --- 3. DEFINE THE 3-COLOR PALETTES ---
# We pick Light, Medium, and Dark shades for each group
palettes <- list(
  # Red Scale: Pale Pink -> Bright Red -> Deep Crimson
  "Bottom Third" = c("#FDEDEC", "#E74C3C", "#641E16"), 
  
  # Yellow Scale: Pale Yellow -> Deep Yellow -> Amber
  "Middle Third" = c("#FEF9E7", "#FBC02D", "#F57F17"), 
  
  # Green Scale: Pale Green -> Emerald -> Dark Forest
  "Top Third"    = c("#EAFAF1", "#27AE60", "#0B3B24")  
)

# --- 4. CALCULATE WINNER & INTENSITY ---
message("Calculating Win Strength and assigning colors...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

# A. Aggregate
puma_stats <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Bottom Third",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Middle Third",
      REAL_INCOME >= limit_2 ~ "Top Third",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(income_tercile)) %>%
  group_by(puma_geoid, income_tercile) %>%
  summarise(count = sum(PERWT), .groups = "drop")

# B. Determine Winner, Intensity, and Specific Color
dominant_data <- puma_stats %>%
  pivot_wider(names_from = income_tercile, values_from = count, values_fill = 0) %>%
  rowwise() %>%
  mutate(
    total = sum(`Bottom Third`, `Middle Third`, `Top Third`),
    
    # 1. Identify Winner
    winner = case_when(
      `Bottom Third` >= `Middle Third` & `Bottom Third` >= `Top Third` ~ "Bottom Third",
      `Middle Third` >= `Bottom Third` & `Middle Third` >= `Top Third` ~ "Middle Third",
      `Top Third` >= `Bottom Third` & `Top Third` >= `Middle Third` ~ "Top Third"
    ),
    
    # 2. Calculate Intensity (Percentage Share)
    win_pct = max(c(`Bottom Third`, `Middle Third`, `Top Third`)) / total
  ) %>%
  ungroup() %>%
  
  # 3. ASSIGN 3-LEVEL COLORS
  # We group by the Winner, then bin the 'win_pct' into 3 quantiles (Low/Med/High strength)
  group_by(winner) %>%
  mutate(
    # Break intensity into 3 equal buckets relative to that group
    strength_bin = ntile(win_pct, 3), 
    
    # Map (Winner + Bin) -> Hex Code
    fill_color = case_when(
      winner == "Bottom Third" ~ palettes[["Bottom Third"]][strength_bin],
      winner == "Middle Third" ~ palettes[["Middle Third"]][strength_bin],
      winner == "Top Third"    ~ palettes[["Top Third"]][strength_bin]
    )
  ) %>%
  ungroup() %>%
  select(puma_geoid, winner, win_pct, fill_color)

# --- 5. MERGE ---
message("Merging data with map...")
plot_ready_data <- us_pumas %>%
  inner_join(dominant_data, by = c("GEOID20" = "puma_geoid"))

# --- 6. PLOT (Using Identity Scale) ---
message("Generating 3-Level Map...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

p <- ggplot(plot_ready_data) +
  
  # Use the pre-calculated hex code for fill
  geom_sf(aes(fill = fill_color), color = NA) + 
  
  # scale_fill_identity() tells ggplot: "The data IS the color code"
  scale_fill_identity() +
  
  labs(
    title = "The Economic Geography of the US",
    subtitle = "Light = Mixed Area | Medium = Strong Majority | Dark = Highly Segregated",
    x = NULL, y = NULL
  ) +
  
  theme_void() + 
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
    
    # Ensure no legend
    legend.position = "none"
  )

# Save
filename <- "map_Predominant_3_Level.png"
ggsave(file.path(out_dir, filename), plot = p, width = 16, height = 10, bg = "white")
message(paste("  -> Saved:", filename))

# Display
print(p)

message("\n--- 3-Level Map generated. ---")