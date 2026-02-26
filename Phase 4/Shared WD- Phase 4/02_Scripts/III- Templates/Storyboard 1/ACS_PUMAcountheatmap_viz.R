# ==============================================================================
# SCRIPT: ACS_PUMA_Heatmap_Clean_White.R
# Purpose: Clean, high-impact density maps with simplified legend.
# Midpoints: Bottom=#C0392B, Middle=#F5B041, Top=#27AE60
# ==============================================================================

rm(list = ls()); gc()
options(timeout = 1200)

library(dplyr); library(readr); library(here); library(ggplot2); library(sf)
library(tidyr); library(tigris); library(scales); library(purrr); library(haven)

# --- 1. SETUP & DATA LOADING ---
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

acs_data <- readRDS(INPUT_DATA_FILE) %>% haven::zap_labels()
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- 2. AGGREGATE POPULATION ---
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
    PUMACE20 = sprintf("%05d", as.numeric(PUMA))
  )

# --- 3. GEOGRAPHY ASSEMBLY ---
raw_geo_dir <- here::here("01_data", "raw", "geography", "puma_2023")
state_fips  <- sprintf("%02d", c(1, 2, 4:6, 8:13, 15:42, 44:51, 53:56))

us_pumas <- map_df(state_fips, function(fips) {
  shp_path <- file.path(raw_geo_dir, paste0("tl_2023_", fips, "_puma20.shp"))
  if(file.exists(shp_path)) return(sf::read_sf(shp_path)) else return(NULL)
})

map_data <- us_pumas %>%
  rename(STATEFP = STATEFP20) %>%
  mutate(land_area_sqmi = as.numeric(ALAND20) * 0.0000003861) %>%
  left_join(puma_counts, by = c("STATEFP", "PUMACE20")) %>%
  tigris::shift_geometry()

# --- 4. DEFINE TERCIE CONFIGS ---
country_configs <- list(
  "Bottom_Third" = list(colors = c("#FDEDEC", "#E6B0AA", "#C0392B", "#922B21", "#4A0404")),
  "Middle_Third" = list(colors = c("#FEF5E7", "#FAD7A0", "#F5B041", "#D68910", "#5F370E")),
  "Top_Third"    = list(colors = c("#E9F7EF", "#A9DFBF", "#27AE60", "#1D8348", "#0B5345"))
)

# --- 5. MAP GENERATION LOOP ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

for (t_var in names(country_configs)) {
  message(paste("Rendering Clean Map for:", t_var))
  
  map_plot <- map_data %>%
    mutate(density = .data[[t_var]] / land_area_sqmi) %>%
    filter(density > 0)
  
  # Calculate breakpoints for the legend to make it "understandable"
  # We use log-breaks but label them intuitively
  min_d <- min(map_plot$density, na.rm = TRUE)
  max_d <- max(map_plot$density, na.rm = TRUE)
  
  p <- ggplot(map_plot) +
    geom_sf(aes(fill = density), color = "grey95", linewidth = 0.01) +
    
    scale_fill_gradientn(
      colors = country_configs[[t_var]]$colors,
      trans = "log10", 
      name = "Concentration",
      breaks = c(min_d, sqrt(min_d * max_d), max_d), # Dynamic min, mid, max
      labels = c("Sparse", "Moderate", "Extremely Dense"),
      na.value = "#FDFDFD"
    ) +
    
    labs(title = gsub("_", " ", t_var)) + # Only the Title remains
    
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(face = "bold", size = 26, hjust = 0.5, 
                                color = "#1A1A1A", margin = margin(t = 20, b = -10)),
      # Clean, understandable legend theme
      legend.position = "right",
      legend.title = element_text(face = "bold", size = 12, color = "#1A1A1A"),
      legend.text = element_text(size = 10, color = "grey30"),
      legend.key.height = unit(1.2, "cm"),
      legend.key.width = unit(0.4, "cm"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  ggsave(file.path(out_dir, paste0("Map_", t_var, "_Clean.png")), 
         p, width = 14, height = 9, bg = "white")
}

message("Success: All clean maps saved.")