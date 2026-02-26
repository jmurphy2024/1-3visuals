# ==============================================================================
# SCRIPT 7: VISUALIZE (Combined Side-by-Side Map)
# Script: 07_ACS_Map_Combined_Terciles.R
# Purpose: Generates a single composite image containing all 3 income maps
#          side-by-side for easy comparison.
# Output:  03_output/visualizations_final/map_Combined_Terciles_Comparison.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); library(scales); library(sf)
# We use cowplot to stitch the maps together
if(!require(cowplot)) { install.packages("cowplot"); library(cowplot) }
if(!require(tigris)) { install.packages("tigris"); library(tigris) }

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared PUMA Data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. LOAD MAP SHAPES (Direct Download Check) ---
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

# --- 3. CALCULATE STATS ---
message("Calculating Stats...")
limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

puma_data <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "1", # Key for config matching
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "2",
      REAL_INCOME >= limit_2 ~ "3",
      TRUE ~ NA_character_
    )
  )

map_data <- puma_data %>%
  group_by(puma_geoid, income_tercile) %>%
  summarise(count = sum(PERWT), .groups = "drop_last") %>%
  mutate(pct_of_puma = count / sum(count)) %>%
  ungroup() %>%
  select(puma_geoid, income_tercile, pct_of_puma)

plot_ready_data <- us_pumas %>%
  inner_join(map_data, by = c("GEOID20" = "puma_geoid"))

# --- 4. DEFINE CONFIG (Your Custom Colors) ---
tercile_config <- list(
  "1" = list(name = "Bottom Third", colors = c("#FDEDEC", "#F1948A", "#E74C3C", "#B03A2E", "#641E16"), title_col = "#C0392B"), 
  "2" = list(name = "Middle Third", colors = c("#FEF9E7", "#FFF176", "#FFEB3B", "#FBC02D", "#F57F17"), title_col = "#F1C40F"), 
  "3" = list(name = "Top Third",    colors = c("#EAFAF1", "#82E0AA", "#27AE60", "#145A32", "#0B3B24"), title_col = "#27AE60")
)

# --- 5. GENERATE INDIVIDUAL PLOTS ---
message("Generating sub-maps...")
plot_list <- list()

for (i in 1:3) {
  config <- tercile_config[[as.character(i)]]
  
  current_layer <- plot_ready_data %>% filter(income_tercile == as.character(i))
  
  # We create a cleaner plot for the combined view (smaller titles)
  p <- ggplot(current_layer) +
    geom_sf(aes(fill = pct_of_puma), color = NA) + 
    scale_fill_gradientn(colors = config$colors, labels = percent_format(accuracy = 1)) +
    labs(title = config$name, fill = "% Share") +
    theme_void() + 
    theme(
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5, color = config$title_col),
      legend.position = "bottom",
      legend.key.width = unit(1.5, "cm"),
      legend.key.height = unit(0.3, "cm")
    )
  
  plot_list[[i]] <- p
}

# --- 6. STITCH TOGETHER ---
message("Stitching maps into composite...")

# Combine the 3 plots in 1 row
combined_plot <- plot_grid(
  plot_list[[1]], plot_list[[2]], plot_list[[3]], 
  ncol = 3, 
  align = "h"
)

# Add a Main Title
title_gg <- ggdraw() + 
  draw_label("Economic Segregation in the US: Comparison of Income Tiers", 
             fontface = 'bold', x = 0.5, hjust = 0.5, size = 20)

final_plot <- plot_grid(
  title_gg, combined_plot,
  ncol = 1,
  rel_heights = c(0.1, 1) # Allocate 10% height for title, 90% for maps
)

# --- 7. SAVE ---
out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
filename <- "map_Combined_Terciles_Comparison.png"

# We save a WIDE image (width=20) to fit 3 maps side-by-side
ggsave(file.path(out_dir, filename), plot = final_plot, width = 20, height = 8, bg = "white")

message(paste("  -> Saved Composite Map:", filename))