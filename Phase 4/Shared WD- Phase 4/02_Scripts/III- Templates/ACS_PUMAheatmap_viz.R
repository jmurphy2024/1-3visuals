# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_PUMAheatmap_viz.R
# Purpose: Maps income tercile concentration by PUMA using a manual download.
# Output:  03_output/visualizations_final/map_PUMA_Tercile_[1,2,3].png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(sf)

# --- 1. SETUP ---
# We need 'tigris' ONLY for the 'shift_geometry' function (to move AK/HI), 
# not for downloading.
if(!require(tigris)) { install.packages("tigris"); library(tigris) }

PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared PUMA Data not found. Please run Script 02_ACS_Race_PUMA_Prepare.R first.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. DIRECT DOWNLOAD OF MAP SHAPES ---
# We download the 2022 National PUMA file (2020 definitions) directly.
map_url  <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_puma20_500k.zip"
dest_zip <- here::here("01_data", "raw", "cb_2022_us_puma20_500k.zip")
shp_dir  <- here::here("01_data", "raw", "puma_shapefiles")

# A. Download (if not already there)
if(!file.exists(dest_zip)) {
  message("Downloading National PUMA Shapefile (approx 25MB)...")
  tryCatch({
    # Increased timeout to 300 seconds (5 mins) for slower connections
    options(timeout = 300) 
    download.file(map_url, dest_zip, mode = "wb")
  }, error = function(e) stop("Download failed. Check internet connection or Census website status."))
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
# Find the file ending in .shp
shp_file <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
us_pumas_raw <- st_read(shp_file, quiet = TRUE)

# D. Shift Geometry (Move AK/HI)
message("Shifting geometry for compact map...")
# This puts Hawaii and Alaska in the bottom corner so the map fits on one screen
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
# The shapefile uses 'GEOID20' (State+PUMA). Our data uses 'puma_geoid'.
map_data <- puma_data %>%
  group_by(puma_geoid, income_tercile) %>%
  summarise(count = sum(PERWT), .groups = "drop_last") %>%
  mutate(pct_of_puma = count / sum(count)) %>%
  ungroup() %>%
  select(puma_geoid, income_tercile, pct_of_puma)

# --- 4. MERGE DATA WITH SHAPES ---
message("Merging data with map shapes...")

# Join using GEOID20 (Census standard for 2020-based PUMAs)
plot_ready_data <- us_pumas %>%
  inner_join(map_data, by = c("GEOID20" = "puma_geoid"))

# --- 5. CONFIGURATION ---
tercile_config <- list(
  "1" = list(name = "Bottom Third", filter_match = "Tercile 1 (Bottom)", 
             low_col = "#FADBD8", high_col = "#641E16", title_col = "#C0392B"), 
  "2" = list(name = "Middle Third", filter_match = "Tercile 2 (Middle)", 
             low_col = "#FCF3CF", high_col = "#7D6608", title_col = "#F5B041"), 
  "3" = list(name = "Top Third",    filter_match = "Tercile 3 (Top)",    
             low_col = "#D5F5E3", high_col = "#0E6251", title_col = "#27AE60")
)

# --- 6. GENERATE MAPS ---
message("Generating PUMA Maps...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  message(paste("Mapping:", config$name))
  
  # Filter map data
  current_layer <- plot_ready_data %>%
    filter(income_tercile == config$filter_match)
  
  p <- ggplot(current_layer) +
    
    # Render the Map
    geom_sf(aes(fill = pct_of_puma), color = NA) + 
    
    scale_fill_gradient(
      low = config$low_col, 
      high = config$high_col, 
      name = "Pop Share", 
      labels = percent_format(accuracy = 1)
    ) +
    
    labs(
      title = paste0("Neighborhood Concentration: ", config$name),
      subtitle = "Each zone (PUMA) represents ~100k people. Darker areas have higher concentration.",
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

message("\n--- All PUMA maps generated successfully. ---")