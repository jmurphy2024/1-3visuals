# ==============================================================================
# SCRIPT 16b: REPORT (Metro Share - All Groups + National Average)
# Script: 16b_ACS_Metro_Share_All_Groups.R
# Purpose: Calculates Metro Share for Bottom, Middle, Top, AND Total US Pop.
# Output:  Comparative Table
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(sf); library(tidyr)
sf::sf_use_s2(FALSE) # Planar math

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared PUMA Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)
names(acs_data) <- toupper(names(acs_data))

# --- 2. MAP PROCESSING ---
message("Loading Maps & Calculating Spatial Intersection...")

# Load PUMA Map
shp_dir  <- here::here("01_data", "raw", "puma_shapefiles_2020")
shp_file <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
pumas_sf <- st_read(shp_file, quiet = TRUE) %>% st_transform(5070) %>% st_make_valid()

# Load Metro Map (CBSA 2023)
cbsa_dir  <- here::here("01_data", "raw", "cbsa_shapefiles_2023")
if(!dir.exists(cbsa_dir)) stop("Metro Map missing. Run Script 11/14 to download.")

cbsa_file <- list.files(cbsa_dir, pattern = "\\.shp$", full.names = TRUE)[1]
metros_sf <- st_read(cbsa_file, quiet = TRUE) %>% 
  st_transform(5070) %>% 
  st_make_valid() %>%
  filter(LSAD == "M1") # Filter for Metro Only

# Calculate Overlap
pumas_sf$area_total <- st_area(pumas_sf)
pumas_simple  <- st_simplify(pumas_sf, dTolerance = 10, preserveTopology = TRUE) %>% select(GEOID20)
metros_simple <- st_simplify(metros_sf, dTolerance = 10, preserveTopology = TRUE)

intersection <- st_intersection(pumas_simple, metros_simple)
intersection$area_metro <- st_area(intersection)

# Sum Metro Area per PUMA
puma_metro_stats <- intersection %>%
  st_drop_geometry() %>%
  group_by(GEOID20) %>%
  summarise(metro_area_sum = sum(area_metro)) %>%
  right_join(st_drop_geometry(pumas_sf) %>% select(GEOID20, area_total), by = "GEOID20") %>%
  mutate(
    metro_area_sum = replace_na(as.numeric(metro_area_sum), 0),
    area_total = as.numeric(area_total),
    puma_metro_pct = pmin(metro_area_sum / area_total, 1.0)
  ) %>%
  select(GEOID20, puma_metro_pct)

# --- 3. DATA MERGE & TERCILE ASSIGNMENT ---
message("Classifying Population...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

acs_ready <- acs_data %>%
  left_join(puma_metro_stats, by = c("PUMA_GEOID" = "GEOID20")) %>%
  mutate(
    puma_metro_pct = replace_na(puma_metro_pct, 0),
    tercile = case_when(
      REAL_INCOME < limit_1 ~ "Bottom Third",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Middle Third",
      REAL_INCOME >= limit_2 ~ "Top Third",
      TRUE ~ "Unknown"
    )
  )

# --- 4. CALCULATE ALL STATS ---
message("Calculating Weighted Metro Shares...")

# A. Stats by Tercile
stats_tercile <- acs_ready %>%
  filter(tercile != "Unknown") %>%
  group_by(tercile) %>%
  summarise(
    total_pop = sum(PERWT),
    metro_pop = sum(PERWT * puma_metro_pct)
  ) %>%
  ungroup()

# B. Stats for Total Population
stats_total <- acs_ready %>%
  summarise(
    tercile   = "TOTAL US POP",
    total_pop = sum(PERWT),
    metro_pop = sum(PERWT * puma_metro_pct)
  )

# Combine
final_stats <- bind_rows(stats_tercile, stats_total) %>%
  mutate(pct_metro = metro_pop / total_pop * 100)

# --- 5. REPORT ---
message("\n=======================================================")
message("   METRO AREA SHARE (Complete Report)")
message("=======================================================")
cat(sprintf("%-15s | %-15s | %-12s\n", "Group", "Total Pop", "Metro Share"))
message("-------------------------------------------------------")

# Print in specific order
order_list <- c("Bottom Third", "Middle Third", "Top Third", "TOTAL US POP")

for(t in order_list) {
  row <- final_stats %>% filter(tercile == t)
  cat(sprintf("%-15s | %-15s | %0.1f%%\n", 
              t, 
              format(round(row$total_pop), big.mark=","), 
              row$pct_metro))
}
message("=======================================================")