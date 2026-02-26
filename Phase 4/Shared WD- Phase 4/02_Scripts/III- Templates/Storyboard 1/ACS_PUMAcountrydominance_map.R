# ==============================================================================
# SCRIPT: ACS_PUMA_Dominance_Map_Narrative.R
# Purpose: Consolidated map with a "Professional Narrative" balanced palette.
# Midpoints: Bottom=#C0392B, Middle=#E59866, Top=#45B39D
# ==============================================================================

rm(list = ls()); gc()
options(timeout = 1200)

library(dplyr); library(readr); library(here); library(ggplot2); library(sf)
library(tidyr); library(tigris); library(scales); library(purrr); library(haven)

# --- 1. SETUP & DATA LOADING ---
INPUT_DATA_FILE   <- here::here("01_data", "processed", "prepared_ACS_inclusive.rds")
MAIN_CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive.rds")

if(!file.exists(INPUT_DATA_FILE)) stop("Master Prepared Data not found.")

acs_data <- readRDS(INPUT_DATA_FILE) %>% haven::zap_labels()
main_cutoffs <- readRDS(MAIN_CUTOFFS_FILE)

# --- 2. AGGREGATE POPULATION ---
puma_counts <- acs_data %>%
  mutate(
    tercile_id = case_when(
      REAL_INCOME <= main_cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME <= main_cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
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

# --- 4. CALCULATE DOMINANCE ---
map_plot <- map_data %>%
  mutate(
    Dominant_Country = case_when(
      `Bottom Third` >= `Middle Third` & `Bottom Third` >= `Top Third` ~ "Bottom Third",
      `Middle Third` >= `Bottom Third` & `Middle Third` >= `Top Third` ~ "Middle Third",
      `Top Third` >= `Bottom Third` & `Top Third` >= `Middle Third` ~ "Top Third",
      TRUE ~ "Mixed"
    ),
    Dominant_Country = factor(Dominant_Country, 
                              levels = c("Bottom Third", "Middle Third", "Top Third"))
  )

# --- 5. VISUALIZATION: NARRATIVE BALANCED ---
p <- ggplot(map_plot) +
  # Using a subtle grey border reduces the "vibration" of colors touching
  geom_sf(aes(fill = Dominant_Country), color = "#F0F0F0", linewidth = 0.05) +
  
  scale_fill_manual(
    values = c(
      "Bottom Third" = "#D3212C", 
      "Middle Third" = "#FF8521", 
      "Top Third"    = "#00674F"  
    ),
    name = "Country Identity"
  ) +
  
  labs(title = "The Three Countries") +
  
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 26, hjust = 0.5, 
                              color = "#2C3E50", margin = margin(t = 20, b = 20)),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 12, color = "#2C3E50"),
    legend.text = element_text(size = 11, color = "grey30"),
    legend.key.height = unit(0.5, "cm"),
    legend.key.width = unit(2, "cm"),
    plot.margin = margin(10, 10, 10, 10)
  )

# --- 6. SAVE ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "Map_Three_Countries_Narrative_Final.png"), 
       p, width = 15, height = 10, dpi = 300, bg = "white")