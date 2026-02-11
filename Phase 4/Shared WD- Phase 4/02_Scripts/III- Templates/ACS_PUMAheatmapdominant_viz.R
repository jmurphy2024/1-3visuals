# ==============================================================================
# SCRIPT 7: VISUALIZE (All 3 on 1 Map - CLEAN)
# Script: 07_ACS_Map_Predominant_Tercile.R
# Purpose: Creates ONE map showing the "Winner" income group in each neighborhood.
# Update:  Removed CAPTION and LEGEND for a clean visual.
# Output:  03_output/visualizations_final/map_Predominant_Income_Tier.png
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
  message("Downloading National PUMA Shapefile...")
  tryCatch({ options(timeout = 300); download.file(map_url, dest_zip, mode = "wb") }, 
           error = function(e) stop("Download failed."))
}
if(!dir.exists(shp_dir)) unzip(dest_zip, exdir = shp_dir)

shp_file <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
us_pumas_raw <- st_read(shp_file, quiet = TRUE)

message("Shifting geometry...")
us_pumas <- us_pumas_raw %>% shift_geometry() %>% st_make_valid() 

# --- 3. CALCULATE DOMINANT GROUP ---
message("Calculating the 'Winner' for each PUMA...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

# A. Aggregate Counts per Group
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

# B. Find the Winner
dominant_data <- puma_stats %>%
  pivot_wider(names_from = income_tercile, values_from = count, values_fill = 0) %>%
  rowwise() %>%
  mutate(
    total = sum(`Bottom Third`, `Middle Third`, `Top Third`),
    winner = case_when(
      `Bottom Third` >= `Middle Third` & `Bottom Third` >= `Top Third` ~ "Bottom Third",
      `Middle Third` >= `Bottom Third` & `Middle Third` >= `Top Third` ~ "Middle Third",
      `Top Third` >= `Bottom Third` & `Top Third` >= `Middle Third` ~ "Top Third"
    )
  ) %>%
  ungroup() %>%
  select(puma_geoid, winner)

# --- 4. MERGE ---
message("Merging data with map...")
plot_ready_data <- us_pumas %>%
  inner_join(dominant_data, by = c("GEOID20" = "puma_geoid"))

# --- 5. PLOT (CLEAN: NO LEGEND/CAPTION) ---
message("Generating Dominant Class Map...")

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

p <- ggplot(plot_ready_data) +
  
  # Color by the "Winner"
  geom_sf(aes(fill = winner), color = NA) + 
  
  # Manual Colors
  scale_fill_manual(
    values = c(
      "Bottom Third" = "#C0392B",
      "Middle Third" = "#F5B041", 
      "Top Third"    = "#27AE60" 
    )
  ) +
  
  labs(
    title = "The Economic Geography of the US",
    subtitle = "Which income group is the largest population in each neighborhood?",
    x = NULL, y = NULL
    # Caption removed
  ) +
  
  theme_void() + 
  theme(
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5, color = "#2C3E50", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", margin = margin(b = 20)),
    
    # REMOVE LEGEND
    legend.position = "none" 
  )

# Save
filename <- "map_Predominant_Income_Tier.png"
ggsave(file.path(out_dir, filename), plot = p, width = 16, height = 10, bg = "white")
message(paste("  -> Saved:", filename))

# Display
print(p)

message("\n--- Clean Map generated. ---")