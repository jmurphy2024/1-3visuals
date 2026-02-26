# ==============================================================================
# SCRIPT 10: ANALYSIS (Metro vs. Non-Metro Split using PCTMETRO Logic)
# Script: ACS_metroshare_calculation.R
# Purpose: Calculates the share of the Bottom Third living in Metropolitan Areas.
#          1. Checks if 'PCTMETRO' variable exists in the data.
#          2. If missing, reconstructs it spatially using CBSA maps.
# Output:  Printed Report
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(sf); library(tidyr)
if(!require(tigris)) { install.packages("tigris"); library(tigris) }

# --- 1. SETUP ---
sf::sf_use_s2(FALSE) # Fast Math

PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared PUMA Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# Normalize Column Names (Upper case to match IPUMS)
names(acs_data) <- toupper(names(acs_data))

# --- 2. CHECK FOR PCTMETRO VARIABLE ---
if ("PCTMETRO" %in% names(acs_data)) {
  
  message(">>> SUCCESS: Found 'PCTMETRO' variable in dataset!")
  message("Using pre-calculated values...")
  
  # PCTMETRO in IPUMS is often 5 digits with 2 implied decimals (e.g. 10000 = 100.00%)
  # We check the max value to determine scaling.
  max_val <- max(acs_data$PCTMETRO, na.rm = TRUE)
  scale_factor <- if(max_val > 100) 100 else 1
  
  acs_with_metro <- acs_data %>%
    mutate(pct_metro_share = PCTMETRO / scale_factor / 100) # Convert to 0.0 - 1.0
  
} else {
  
  message(">>> NOTE: 'PCTMETRO' variable not found in dataset.")
  message(">>> RECONSTRUCTING IT SPATIALLY (Intersecting PUMAs with Metro Areas)...")
  
  # A. Load PUMAs
  shp_dir  <- here::here("01_data", "raw", "puma_shapefiles_2020")
  shp_file <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
  pumas_sf <- st_read(shp_file, quiet = TRUE) %>% st_transform(5070) %>% st_make_valid()
  
  # B. Download Metropolitan Areas (CBSAs)
  # We use 2022/2023 definitions to match the IPUMS PCTMETRO logic
  message("   -> Downloading Metropolitan Statistical Areas...")
  metros_sf <- tigris::core_based_statistical_areas(year = 2022, cb = TRUE, progress_bar = FALSE) %>%
    st_transform(5070) %>%
    st_make_valid() %>%
    # FILTER: IPUMS PCTMETRO only counts "Metropolitan", not "Micropolitan"
    filter(grepl("Metropolitan", NAMELSAD))
  
  # C. Calculate Intersection
  message("   -> Calculating PUMA overlap with Metro Areas...")
  pumas_sf$area_total <- st_area(pumas_sf)
  
  # Simplify for speed
  pumas_simple <- st_simplify(pumas_sf, dTolerance = 10, preserveTopology = TRUE) %>% select(GEOID20)
  metros_simple <- st_simplify(metros_sf, dTolerance = 10, preserveTopology = TRUE)
  
  # Intersection
  intersection <- st_intersection(pumas_simple, metros_simple)
  intersection$area_metro <- st_area(intersection)
  
  # D. Sum Metro Area per PUMA
  puma_metro_stats <- intersection %>%
    st_drop_geometry() %>%
    group_by(GEOID20) %>%
    summarise(metro_area_sum = sum(area_metro)) %>%
    right_join(st_drop_geometry(pumas_sf) %>% select(GEOID20, area_total), by = "GEOID20") %>%
    mutate(
      metro_area_sum = replace_na(as.numeric(metro_area_sum), 0),
      area_total = as.numeric(area_total),
      # Calculate Share (0.0 to 1.0)
      pct_metro_share = pmin(metro_area_sum / area_total, 1.0) 
    ) %>%
    select(GEOID20, pct_metro_share)
  
  # E. Join back to ACS Data
  acs_with_metro <- acs_data %>%
    left_join(puma_metro_stats, by = c("PUMA_GEOID" = "GEOID20")) %>%
    mutate(pct_metro_share = replace_na(pct_metro_share, 0))
}

# --- 3. CALCULATE BOTTOM THIRD SPLIT ---
message("\nCalculating population split for the Bottom Third...")

limit_1 <- main_cutoffs$main_cutoff1

stats <- acs_with_metro %>%
  filter(REAL_INCOME < limit_1) %>%
  summarise(
    total_pop  = sum(PERWT),
    metro_pop  = sum(PERWT * pct_metro_share),
    non_metro_pop = sum(PERWT * (1 - pct_metro_share))
  )

metro_pct     <- stats$metro_pop / stats$total_pop * 100
non_metro_pct <- stats$non_metro_pop / stats$total_pop * 100

# --- 4. REPORT ---
message("\n==================================================")
message("   METRO VS. NON-METRO ANALYSIS (Bottom Third)")
message("   (Based on IPUMS PCTMETRO Logic)")
message("==================================================")
cat(sprintf("Total Bottom Third Population: %s\n\n", format(round(stats$total_pop), big.mark=",")))
cat(sprintf("Living in METRO Areas:         %0.1f%%\n", metro_pct))
cat(sprintf("Living in NON-METRO Areas:     %0.1f%%\n", non_metro_pct))
message("--------------------------------------------------")
message("Metro = Metropolitan Statistical Area (Cities + Suburbs)")
message("Non-Metro = Rural + Micropolitan (Small Towns)")
message("==================================================")