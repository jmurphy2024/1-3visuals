# ==============================================================================
# SCRIPT 4: VISUALIZE (PUMA Heatmap - HIGH CONTRAST)
# Script: 04_ACS_Map_PUMA_High_Contrast.R
# Purpose: Maps income tercile concentration by PUMA using "Magma-style" scales.
#          Uses the 2020 Census Shapefile (Direct Download).
# Output:  03_output/visualizations_final/map_PUMA_Tercile_[1,2,3].png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(sf)

# --- 1. SETUP ---
# We need 'tigris' ONLY for the 'shift_geometry' function (moves AK/HI).
if(!require(tigris)) { install.packages("tigris"); library(tigris) }

PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared PUMA Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. DIRECT DOWNLOAD OF MAP SHAPES ---
# Use the 2020 Vintage file, which is the standard for current ACS data.
map_url  <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_puma20_500k.zip"
dest_zip <- here::here("01_data", "raw", "cb_2020_us_puma20_500k.zip")
shp_dir  <- here::here("01_data", "raw", "puma_shapefiles_2020")

# A. Download
if(!file.exists(dest_zip)) {
  message("Downloading National PUMA Shapefile (2020 Census Base)...")
  tryCatch({
    options(timeout = 300) 
    download.file(map_url, dest_zip, mode = "wb")
  }, error = function(e) stop("Download failed. Check internet connection."))
} else {
  message("Using existing ZIP file.")
}

# B. Unzip
if(!dir.exists(shp_dir)) {
  message("Unzipping shapefiles...")
  unzip(dest_zip, exdir = shp_dir)
}

# C. Load Shapefile
message("Loading map geometry...")
shp_file <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
us_pumas_raw <- st_read(shp_file, quiet = TRUE)

# D. Shift Geometry (Move AK/HI for compact view)
message("Shifting geometry...")
us_pumas <- us_pumas_raw %>%
  shift_geometry() %>%
  st_make_valid() 

# --- 3. CALCULATE PUMA STATS ---
message("Calculating PUMA-level Stats...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

# A. Assign Terciles
puma_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Tercile 1 (Bottom)",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Tercile 2 (Middle)",
      REAL_INCOME >= limit_2 ~ "Tercile 3 (Top)",
      TRUE ~ NA_character_
    )
  )

# B. Aggregate by PUMA
map_data <- puma_data %>%
  group_by(puma_geoid, income_tercile) %>%
  summarise(count = sum(PERWT), .groups = "drop_last") %>%
  mutate(pct_of_puma = count / sum(count)) %>%
  ungroup() %>%
  select(puma_geoid, income_tercile, pct_of_puma)

# --- 4. MERGE DATA WITH SHAPES ---
message("Merging data with map shapes...")
# The 2020 shapefile uses column 'GEOID20' for the PUMA ID
plot_ready_data <- us_pumas %>%
  inner_join(map_data, by = c("GEOID20" = "puma_geoid"))

# --- 5. GENERATE MAPS (WITH MAGMA SCALES) ---
message("Generating Maps...")

tercile_config <- list(
  "1" = list(
    name = "Bottom Third", 
    filter_match = "Tercile 1 (Bottom)", 
    # UPDATED RED SCALE: Pale Pink -> Salmon -> Bright Red -> Dark Red -> Blackish Red
    colors = c("#FDEDEC", "#F1948A", "#E74C3C", "#B03A2E", "#641E16"), 
    title_col = "#C0392B"
  ), 
  
  "2" = list(
    name = "Middle Third", 
    filter_match = "Tercile 2 (Middle)", 
    # Pure Yellow Scale: Ivory -> Pastel Yellow -> Lemon -> Deep Yellow -> Amber
    # This avoids the "Brown/Burnt Orange" look entirely.
    colors = c("#FEF9E7", "#FFF176", "#FFEB3B", "#FBC02D", "#F57F17"), 
    title_col = "#F1C40F"
  ),
  
  "3" = list(
    name = "Top Third",    
    filter_match = "Tercile 3 (Top)",    
    # Green-Magma: Pale Green -> Green -> Emerald -> Dark Green -> Blackish
    colors = c("#EAFAF1", "#82E0AA", "#27AE60", "#145A32", "#0B3B24"), 
    title_col = "#27AE60"
  )
)

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  message(paste("Mapping:", config$name))
  
  current_layer <- plot_ready_data %>%
    filter(income_tercile == config$filter_match)
  
  p <- ggplot(current_layer) +
    
    # Render Map
    geom_sf(aes(fill = pct_of_puma), color = NA) + 
    
    # UPDATED SCALE: Uses 'gradientn' to handle the 5-color magma lists
    scale_fill_gradientn(
      colors = config$colors,
      name = "Pop Share", 
      labels = percent_format(accuracy = 1)
    ) +
    
    labs(
      title = paste0("Neighborhood Concentration: ", config$name),
      subtitle = "Each zone (PUMA) represents ~100k people. Brighter/Darker spots indicate high concentration.",
      caption = "Source: IPUMS USA 2023 ACS | Geography: 2020 Census PUMAs"
    ) +
    
    theme_void() + 
    theme(
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = config$title_col, margin = margin(b = 10)),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey50", margin = margin(b = 10)),
      legend.position = "right"
    )
  
  filename <- paste0("map_PUMA_Tercile_", i, ".png")
  ggsave(file.path(out_dir, filename), plot = p, width = 14, height = 10, bg = "white")
  message(paste("  -> Saved:", filename))
}

message("\n--- All High-Contrast PUMA maps generated. ---")