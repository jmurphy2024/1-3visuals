# ==== 0. ABOUT ====
## WD location: 02_Scripts/I-Geo Areas Master File
## Script: I-D-iii_process_spatial_join.R
## Purpose: This script bridges the 2010 and 2020 census geographies. It takes the
##          2019 shapefiles (using 2010 boundaries), joins them with the definitive
##          2010-to-2020 crosswalk, and then performs a spatial join against the 2022
##          PUMA shapefile (2020 boundaries) to assign a predominant PUMA to every
##          granular 2010-era geography. This version now includes a separate
##          process for school districts which do not have official crosswalks.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-09-21
## Last Modified: 2025-09-22 (Added a spatial-only assignment function for school districts)
## Dependencies: sf, dplyr, here, purrr
## Input: Shapefiles from 'I-D-i', Crosswalk from 'I-D-ii'.
##        - '01_data/gis_shapefiles/*.rds'
##        - '01_data/processed/definitive_crosswalk_2010_to_2020.rds'
## Output: 01_data/processed/geography_to_puma_lookup_2020.rds (Lookup for 2020 geographies)
##         01_data/processed/geography_to_puma_lookup_2010.rds (Lookup for 2010 geographies)


# Load necessary libraries
if (!require(sf)) install.packages("sf")
if (!require(dplyr)) install.packages("dplyr")
if (!require(here)) install.packages("here")
if (!require(purrr)) install.packages("purrr")

library(sf)
library(dplyr)
library(here)
library(purrr)


# ==== 1. LOAD REQUIRED DATA FILES ====
message("Loading shapefiles and the definitive 2010-to-2020 crosswalk...")

# ===== 1.1. Define File Paths =====
shapefile_dir <- here("01_data", "gis_shapefiles")
processed_dir <- here("01_data", "processed")

# --- Shapefiles (2019 for 2010 boundaries, 2022 for 2020 PUMAs) ---
paths <- list(
  tracts_2019 = file.path(shapefile_dir, "tracts_2019_sf.rds"),
  cousub_2019 = file.path(shapefile_dir, "county_subdivisions_2019_sf.rds"),
  puma_2019 = file.path(shapefile_dir, "pumas_2019_sf.rds"),
  zcta_2019 = file.path(shapefile_dir, "zctas_2019_sf.rds"),
  elsd_2019 = file.path(shapefile_dir, "elementary_school_districts_2019_sf.rds"),
  scsd_2019 = file.path(shapefile_dir, "secondary_school_districts_2019_sf.rds"),
  unsd_2019 = file.path(shapefile_dir, "unified_school_districts_2019_sf.rds"),
  pumas_2022 = file.path(shapefile_dir, "pumas_2022_sf.rds"), # 2020-era PUMAs
  crosswalk = file.path(processed_dir, "definitive_crosswalk_2010_to_2020.rds")
)

# ===== 1.2. Load R Objects =====
load_data_object <- function(path, name) {
  if (!file.exists(path)) {
    stop(paste("Error:", name, "not found at", path, ". Please run previous scripts."))
  }
  readRDS(path)
}

# Load all data files into a named list
data_objects <- imap(paths, ~load_data_object(.x, .y))

message("All required data files loaded successfully.")


# ==== 2. HELPER FUNCTIONS FOR PUMA ASSIGNMENT ====

# ===== 2.1. Function for Geographies WITH a Crosswalk (Tracts, COUSUBs, ZCTAs) =====
harmonize_and_assign <- function(granular_sf_2010, crosswalk, pumas_sf_2020, geo_type) {
  message(paste("--- Starting harmonization (with crosswalk) for:", geo_type, "---"))
  
  granular_sf_2010_clean <- granular_sf_2010 %>%
    mutate(TL_GEO_ID = as.character(trimws(TL_GEO_ID)))
  
  crosswalk_filtered <- crosswalk %>%
    filter(GEOGRAPHY_TYPE == geo_type) %>%
    mutate(GEOID10 = as.character(trimws(GEOID10))) %>%
    select(GEOID10, GEOID20)
  
  granular_sf_with_xwalk <- granular_sf_2010_clean %>%
    left_join(crosswalk_filtered, by = c("TL_GEO_ID" = "GEOID10"))
  
  na_matches <- sum(is.na(granular_sf_with_xwalk$GEOID20))
  if (na_matches > 0) {
    message(paste("Warning:", na_matches, "of", nrow(granular_sf_with_xwalk), "records in", geo_type, "failed to find a match in the crosswalk."))
  }
  
  pumas_2020_id <- pumas_sf_2020 %>% select(PUMA_GEOID20 = TL_GEO_ID)
  
  message("... Performing spatial intersection...")
  intersection_sf <- st_intersection(st_make_valid(granular_sf_with_xwalk), st_make_valid(pumas_2020_id))
  
  intersection_sf$intersection_area <- st_area(intersection_sf)
  
  lookup_table <- st_drop_geometry(intersection_sf) %>%
    group_by(TL_GEO_ID) %>%
    slice_max(order_by = intersection_area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(GEOID10 = TL_GEO_ID, GEOID20, PUMA_GEOID20) %>%
    mutate(GEOGRAPHY_TYPE = geo_type)
  
  message(paste("--- Completed assignment for:", geo_type, "---"))
  return(lookup_table)
}

# ===== 2.2. Function for Geographies WITHOUT a Crosswalk (School Districts) =====
assign_puma_spatially_only <- function(granular_sf_2010, pumas_sf_2020, geo_type) {
  message(paste("--- Starting SPATIAL-ONLY assignment for:", geo_type, "---"))
  
  pumas_2020_id <- pumas_sf_2020 %>% select(PUMA_GEOID20 = TL_GEO_ID)
  
  message("... Performing direct spatial intersection...")
  intersection_sf <- st_intersection(st_make_valid(granular_sf_2010), st_make_valid(pumas_2020_id))
  
  intersection_sf$intersection_area <- st_area(intersection_sf)
  
  lookup_table <- st_drop_geometry(intersection_sf) %>%
    group_by(TL_GEO_ID) %>%
    slice_max(order_by = intersection_area, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(GEOID10 = TL_GEO_ID, PUMA_GEOID20) %>%
    # Since there's no crosswalk, we'll assume GEOID20 is the same as GEOID10 for consistency.
    # This is a safe assumption for school districts which are relatively stable.
    mutate(GEOID20 = GEOID10, GEOGRAPHY_TYPE = geo_type)
  
  message(paste("--- Completed assignment for:", geo_type, "---"))
  return(lookup_table)
}

# ==== 3. PROCESS ALL GEOGRAPHIES ====
message("Processing all 2010-era geographies to assign 2020 PUMAs.")

# --- Process geographies WITH crosswalks ---
tract_lookup <- harmonize_and_assign(data_objects$tracts_2019, data_objects$crosswalk, data_objects$pumas_2022, "TRACT")
cousub_lookup <- harmonize_and_assign(data_objects$cousub_2019, data_objects$crosswalk, data_objects$pumas_2022, "COUSUB")
zcta_lookup <- harmonize_and_assign(data_objects$zcta_2019, data_objects$crosswalk, data_objects$pumas_2022, "ZCTA")

# --- Process geographies WITHOUT crosswalks (School Districts) ---
elsd_lookup <- assign_puma_spatially_only(data_objects$elsd_2019, data_objects$pumas_2022, "ELSD")
scsd_lookup <- assign_puma_spatially_only(data_objects$scsd_2019, data_objects$pumas_2022, "SCSD")
unsd_lookup <- assign_puma_spatially_only(data_objects$unsd_2019, data_objects$pumas_2022, "UNSD")

# --- Process PUMAs (Tabular crosswalk only) ---
puma_2019_sf_clean <- data_objects$puma_2019 %>%
  st_drop_geometry() %>%
  mutate(TL_GEO_ID = as.character(trimws(TL_GEO_ID)))

crosswalk_df_puma_clean <- filter(data_objects$crosswalk, GEOGRAPHY_TYPE == "PUMA") %>%
  mutate(GEOID10 = as.character(trimws(GEOID10)))

puma_lookup <- puma_2019_sf_clean %>%
  left_join(crosswalk_df_puma_clean, by = c("TL_GEO_ID" = "GEOID10")) %>%
  select(GEOID10 = TL_GEO_ID, GEOID20) %>%
  mutate(
    PUMA_GEOID20 = GEOID20, # The 2020 PUMA is its own crosswalked ID
    GEOGRAPHY_TYPE = "PUMA"
  )

# ==== 4. CREATE AND SAVE FINAL LOOKUP TABLES ====
message("Creating and saving the final lookup tables.")

# ===== 4.1. Create Lookup for 2010 Geographies =====
geography_to_puma_lookup_2010 <- bind_rows(
  tract_lookup,
  cousub_lookup,
  zcta_lookup,
  elsd_lookup,
  scsd_lookup,
  unsd_lookup,
  puma_lookup
) %>% filter(!is.na(GEOID10) & !is.na(PUMA_GEOID20) & !is.na(GEOID20))

# ===== 4.2. Create Lookup for 2020 Geographies =====
master_crosswalk <- readRDS(here(processed_dir, "geographic_crosswalk_2010_to_2020.rds"))

# Add school districts to the master crosswalk conceptually, assuming their GEOIDs are stable
school_districts_stable <- geography_to_puma_lookup_2010 %>%
  filter(GEOGRAPHY_TYPE %in% c("ELSD", "SCSD", "UNSD")) %>%
  select(GEOID10, GEOID20, GEOGRAPHY_TYPE, AREALAND_PART = PUMA_GEOID20) %>% # Use PUMA as a dummy for AREALAND
  mutate(AREALAND_PART = 1) # Set area to 1 to ensure they are picked

master_crosswalk_extended <- bind_rows(master_crosswalk, school_districts_stable)

geography_to_puma_lookup_2010_join <- geography_to_puma_lookup_2010 %>%
  select(GEOID10, GEOGRAPHY_TYPE, PUMA_GEOID20)

geography_to_puma_lookup_2020 <- master_crosswalk_extended %>%
  left_join(geography_to_puma_lookup_2010_join, by = c("GEOID10", "GEOGRAPHY_TYPE")) %>%
  filter(!is.na(PUMA_GEOID20)) %>%
  group_by(GEOID20, GEOGRAPHY_TYPE) %>%
  # For non-school districts, pick the largest overlapping part. For school districts, it will just be one row.
  slice_max(order_by = AREALAND_PART, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(GEOID20, PUMA_GEOID20, GEOGRAPHY_TYPE) %>%
  distinct()

# ===== 4.3. Save Files =====
saveRDS(geography_to_puma_lookup_2010, here(processed_dir, "geography_to_puma_lookup_2010.rds"))
saveRDS(geography_to_puma_lookup_2020, here(processed_dir, "geography_to_puma_lookup_2020.rds"))

message("Successfully created and saved both 2010-based and 2020-based PUMA lookup tables.")

# ==== 5. EXPLORE AND VALIDATE ====
message("--- Summary of 2010-based Lookup Table ---")
print(head(geography_to_puma_lookup_2010))
print(geography_to_puma_lookup_2010 %>% count(GEOGRAPHY_TYPE))

message("--- Summary of 2020-based Lookup Table ---")
print(head(geography_to_puma_lookup_2020))
print(geography_to_puma_lookup_2020 %>% count(GEOGRAPHY_TYPE))

message("Script I-D-iii execution complete.")

