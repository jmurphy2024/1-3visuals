# ==============================================================================
## WD location:02_Scripts/III-Templates/ ACS
# Script: ACS_Calculate_Urban_Rural.R
# Purpose: Calculates Urban/Rural split using PLANAR GEOMETRY (100x Faster).
#          - Disables S2 (Spherical) math.
#          - Projects to EPSG:5070 (US Albers Equal Area).
#          - Fixed missing 'tidyr' package error.
# Output:  Printed Report
# ==============================================================================

rm(list = ls()); gc()
# ADDED 'tidyr' TO THIS LINE:
library(dplyr); library(readr); library(here); library(sf); library(tidyr)

# --- KEY PERFORMANCE TWEAK: DISABLE S2 GEOMETRY ---
# This forces R to use fast, flat 2D math instead of slow 3D spherical math.
sf::sf_use_s2(FALSE)

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared PUMA Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. LOAD & PROJECT PUMA MAP ---
message("Loading PUMA Geometry and projecting to flat map...")
shp_dir  <- here::here("01_data", "raw", "puma_shapefiles_2020")
shp_file <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
if(is.na(shp_file)) stop("PUMA Shapefile not found. Run Script 04 first.")

# Load AND Transform to EPSG:5070 (US Albers Equal Area)
pumas_sf <- st_read(shp_file, quiet = TRUE) %>%
  st_transform(5070) %>%
  st_make_valid()

# --- 3. LOAD & PROJECT URBAN AREAS ---
message("Loading Urban Areas and projecting to flat map...")
ua_dir <- here::here("01_data", "raw", "urban_areas_2020")

# Check if we need to download
if(!dir.exists(ua_dir)) {
  ua_url <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_ua20_500k.zip"
  ua_zip <- here::here("01_data", "raw", "cb_2020_us_ua20_500k.zip")
  if(!file.exists(ua_zip)) download.file(ua_url, ua_zip, mode = "wb")
  unzip(ua_zip, exdir = ua_dir)
}

ua_shp_file <- list.files(ua_dir, pattern = "\\.shp$", full.names = TRUE)[1]

# Load AND Transform to match PUMAs (EPSG:5070)
urban_sf <- st_read(ua_shp_file, quiet = TRUE) %>%
  st_transform(5070) %>%
  st_make_valid()

# --- 4. FAST INTERSECTION ---
message("Calculating Spatial Intersection (Planar Mode - much faster)...")

# Calculate Total PUMA Area (in square meters, because 5070 is metric)
pumas_sf$area_total <- st_area(pumas_sf)

# Simplification (Optional but safe): Remove details smaller than 10 meters
# This removes tiny zig-zags that slow down math without changing the result.
# We create 'pumas_simple' just for the math part to save RAM.
pumas_simple <- st_simplify(pumas_sf, dTolerance = 10, preserveTopology = TRUE) %>% select(GEOID20)
urban_simple <- st_simplify(urban_sf, dTolerance = 10, preserveTopology = TRUE)

# The Intersection
intersection <- st_intersection(pumas_simple, urban_simple)

# Calculate Urban Area
intersection$area_urban <- st_area(intersection)

# Sum up Urban Area per PUMA
puma_urban_stats <- intersection %>%
  st_drop_geometry() %>%
  group_by(GEOID20) %>%
  summarise(urban_area_sum = sum(area_urban))

# --- 5. CLASSIFY PUMAS ---
message("Classifying PUMAs...")

final_class <- pumas_sf %>%
  st_drop_geometry() %>%
  select(GEOID20, area_total) %>%
  left_join(puma_urban_stats, by = "GEOID20") %>%
  mutate(
    # replace_na() will now work because 'tidyr' is loaded
    urban_area_sum = replace_na(as.numeric(urban_area_sum), 0),
    area_total = as.numeric(area_total),
    
    pct_urban_land = urban_area_sum / area_total,
    
    # DEFINITIONS
    category = case_when(
      pct_urban_land >= 0.50 ~ "Urban/Suburban",
      pct_urban_land <= 0.05 ~ "Rural",
      TRUE                   ~ "Mixed/Exurban"
    )
  )

# --- 6. REPORT ---
message("\n--- FINAL REPORT: Location of the Bottom Third ---")

limit_1 <- main_cutoffs$main_cutoff1

stats <- acs_data %>%
  filter(REAL_INCOME < limit_1) %>%
  inner_join(final_class, by = c("puma_geoid" = "GEOID20")) %>%
  group_by(category) %>%
  summarise(pop_count = sum(PERWT)) %>%
  ungroup() %>%
  mutate(percent = pop_count / sum(pop_count) * 100)

urban_pct <- stats$percent[stats$category == "Urban/Suburban"]
rural_pct <- stats$percent[stats$category == "Rural"]
mixed_pct <- stats$percent[stats$category == "Mixed/Exurban"]

if(length(urban_pct) == 0) urban_pct <- 0
if(length(rural_pct) == 0) rural_pct <- 0
if(length(mixed_pct) == 0) mixed_pct <- 0

cat(sprintf("\nUrban/Suburban Share: %0.1f%%", urban_pct))
cat(sprintf("\nRural Share:          %0.1f%%", rural_pct))
cat(sprintf("\nMixed/Exurban Share:  %0.1f%%\n", mixed_pct))
message("--------------------------------------------------")