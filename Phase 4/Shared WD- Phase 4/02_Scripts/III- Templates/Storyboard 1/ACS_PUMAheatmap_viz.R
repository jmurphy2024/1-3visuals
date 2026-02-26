# ==============================================================================
# SCRIPT: ACS_PUMA_National_Heatmap_Fixed.R
# Purpose: Maps Population Density with a National PUMA Shapefile.
# FIX: Extended timeout for slow Census servers.
# ==============================================================================

rm(list = ls()); gc()

# --- 0. EXTEND TIMEOUT & LOAD LIBS ---
# This gives the Census server 10 minutes instead of 60 seconds
options(timeout = 600)

libs <- c("dplyr", "readr", "here", "ggplot2", "sf", "tidyr", "tigris", "scales", "viridis")
if(any(!libs %in% installed.packages()[,1])) install.packages(libs[!libs %in% installed.packages()[,1]])
library(dplyr); library(readr); library(here); library(ggplot2); library(sf)
library(tidyr); library(tigris); library(scales); library(viridis)

# --- 1. SETUP & DATA LOADING ---
PREP_FILE    <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Master Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)
names(acs_data) <- toupper(names(acs_data))

# --- 2. AGGREGATE POPULATION BY PUMA ---
message("Summarizing population counts per PUMA...")

puma_counts <- acs_data %>%
  mutate(
    tercile_id = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom_Third",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle_Third",
      TRUE ~ "Top_Third"
    )
  ) %>%
  group_by(STATEFIP, PUMA, tercile_id) %>%
  summarise(pop_count = sum(PERWT), .groups = "drop") %>%
  pivot_wider(names_from = tercile_id, values_from = pop_count, values_fill = 0) %>%
  mutate(
    STATEFP  = sprintf("%02d", as.numeric(STATEFIP)),
    PUMACE10 = sprintf("%05d", as.numeric(PUMA))
  )

# --- 3. GEOGRAPHY SETUP (NATIONAL PUMA FILE) ---
message("Attempting high-speed National 2020 PUMA download...")

geo_dir <- here::here("01_data", "geography")
shp_zip <- file.path(geo_dir, "tl_2020_us_puma10.zip")
if(!dir.exists(geo_dir)) dir.create(geo_dir, recursive = TRUE)

# Download single national zip with extended timeout
if(!file.exists(shp_zip)) {
  url <- "https://www2.census.gov/geo/tiger/TIGER2020/PUMA/tl_2020_us_puma10.zip"
  tryCatch({
    download.file(url, destfile = shp_zip, mode = "wb")
    unzip(shp_zip, exdir = geo_dir)
  }, error = function(e) {
    stop("Download failed. The Census server timed out again. Try running the script again in a few minutes.")
  })
}

# Load and Shift
us_pumas <- sf::read_sf(file.path(geo_dir, "tl_2020_us_puma10.shp")) %>%
  filter(STATEFP10 != "72") %>% 
  tigris::shift_geometry()

map_data <- us_pumas %>%
  rename(STATEFP = STATEFP10) %>% 
  left_join(puma_counts, by = c("STATEFP", "PUMACE10"))

# --- 4. MAP GENERATION LOOP ---
terciles <- c("Bottom_Third", "Middle_Third", "Top_Third")
out_dir  <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

for (t_var in terciles) {
  message(paste("Rendering Map for:", t_var))
  
  max_val <- quantile(map_data[[t_var]], 0.99, na.rm = TRUE)
  
  p <- ggplot(map_data) +
    geom_sf(aes_string(fill = t_var), color = NA) +
    scale_fill_viridis_c(
      option = "rocket", direction = 1, name = "Population",
      limits = c(0, max_val), oob = scales::squish, na.value = "grey10", labels = scales::comma
    ) +
    labs(
      title = paste("Geography of the", gsub("_", " ", t_var)),
      subtitle = "Population density distribution by PUMA (Rocket Viridis Scale)",
      caption = "Source: ACS 2023 | Albers Projection",
      x = NULL, y = NULL
    ) +
    theme_void() +
    theme(
      plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50"),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40"),
      legend.position = "right"
    )
  
  ggsave(file.path(out_dir, paste0("Map_", t_var, "_Density.png")), p, width = 14, height = 9, bg = "white")
}

message("Success: All 3 Heatmaps saved.")