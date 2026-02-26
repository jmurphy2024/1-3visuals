# ==============================================================================
# WD location: 02_Scripts/III-Templates/ACS
# Script: ACS_PUMAheatmap_viz.R
# Purpose: 1. Heatmap of Poverty Rate by PUMA.
#          2. Summary Table of Poverty by Income Third.
# Output:  map_PUMA_Poverty_Rate.png, table_Poverty_by_Third.png
# ==============================================================================

rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(ggplot2); 
library(scales); library(sf); library(grid); library(gridExtra); library(tigris)

# --- 1. SETUP ---
PREP_FILE    <- here::here("01_data", "processed", "IPUMS_Microdata", "prepared_ACS_Poverty_PUMA_us2023c.rds")
CUTOFFS_FILE <- here::here("01_data", "processed", "main_tercile_cutoffs_2023.rds")

if(!file.exists(PREP_FILE)) stop("Prepared Poverty PUMA data not found.")
acs_data     <- readRDS(PREP_FILE)
main_cutoffs <- readRDS(CUTOFFS_FILE)

# --- 2. LOAD & PREPARE MAP SHAPES ---
# Download/Load Shapefile (Same logic as before)
map_url  <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_puma20_500k.zip"
dest_zip <- here::here("01_data", "raw", "cb_2022_us_puma20_500k.zip")
shp_dir  <- here::here("01_data", "raw", "puma_shapefiles")

if(!file.exists(dest_zip)) {
  options(timeout = 300) 
  download.file(map_url, dest_zip, mode = "wb")
}
if(!dir.exists(shp_dir)) unzip(dest_zip, exdir = shp_dir)

shp_file <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE)[1]
us_pumas_raw <- st_read(shp_file, quiet = TRUE)

# Shift Geometry (AK/HI)
us_pumas <- us_pumas_raw %>% shift_geometry() %>% st_make_valid() 

# --- 3. DATA AGGREGATION ---
message("Aggregating Statistics...")

limit_1 <- main_cutoffs$main_cutoff1
limit_2 <- main_cutoffs$main_cutoff2

# Assign Terciles
acs_labeled <- acs_data %>%
  mutate(
    income_tercile = case_when(
      REAL_INCOME < limit_1 ~ "Bottom Third",
      REAL_INCOME >= limit_1 & REAL_INCOME < limit_2 ~ "Middle Third",
      REAL_INCOME >= limit_2 ~ "Top Third",
      TRUE ~ NA_character_
    )
  )

# A. Map Data (Poverty Rate per PUMA)
map_data <- acs_labeled %>%
  group_by(puma_geoid) %>%
  summarise(
    total_pop = sum(PERWT),
    poverty_pop = sum(PERWT[is_poor == 1]),
    poverty_rate = poverty_pop / total_pop,
    .groups = "drop"
  )

# B. Table Data (Poverty Rate per Income Third)
table_stats <- acs_labeled %>%
  group_by(income_tercile) %>%
  summarise(
    N = sum(PERWT),
    poverty_pop = sum(PERWT[is_poor == 1]),
    poverty_rate = poverty_pop / N,
    .groups = "drop"
  ) %>%
  # Add Total Row
  bind_rows(
    acs_labeled %>%
      summarise(
        income_tercile = "Total Population",
        N = sum(PERWT),
        poverty_pop = sum(PERWT[is_poor == 1]),
        poverty_rate = poverty_pop / N
      )
  ) %>%
  mutate(
    N_label = comma(N),
    rate_label = percent(poverty_rate, accuracy = 0.1)
  )

# --- 4. GENERATE HEATMAP ---
message("Generating Poverty Heatmap...")

# Join Data to Shapes
plot_ready_map <- us_pumas %>%
  inner_join(map_data, by = c("GEOID20" = "puma_geoid"))

out_dir <- here::here("03_output", "visualizations_final")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

p_map <- ggplot(plot_ready_map) +
  geom_sf(aes(fill = poverty_rate), color = NA) + 
  
  # Heatmap Gradient (Light Yellow -> Orange -> Dark Red)
  scale_fill_gradientn(
    colors = c("#FEF9E7", "#F4D03F", "#E67E22", "#C0392B", "#641E16"),
    values = scales::rescale(c(0, 0.05, 0.15, 0.30, 0.50)), # Custom breaks for better contrast
    name = "Poverty Rate",
    labels = percent_format(accuracy = 1)
  ) +
  
  labs(
    title = "U.S. Poverty Rate by Neighborhood (PUMA)",
    subtitle = "Percentage of population living below 100% of the federal poverty threshold",
    caption = "Source: IPUMS USA 2023 ACS | Geography: 2020 Census PUMAs"
  ) +
  
  theme_void() + 
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, color = "#2C3E50", margin = margin(b = 10)),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey50", margin = margin(b = 20)),
    legend.position = "right",
    legend.key.height = unit(1.5, "cm")
  )

ggsave(file.path(out_dir, "map_PUMA_Poverty_Rate.png"), plot = p_map, width = 14, height = 10, bg = "white")
message(" -> Saved Map: map_PUMA_Poverty_Rate.png")


# --- 5. GENERATE SUMMARY TABLE ---
message("Generating Summary Table...")

# Select Final Columns
table_final <- table_stats %>%
  select(income_tercile, N_label, rate_label) %>%
  rename(
    "Income Group" = income_tercile,
    "Total Population" = N_label,
    "Poverty Rate (<100% FPL)" = rate_label
  ) %>%
  # Sort Order
  arrange(factor(`Income Group`, levels = c("Total Population", "Bottom Third", "Middle Third", "Top Third")))

# GridExtra Formatting
rows_n <- nrow(table_final)
cols_n <- ncol(table_final)

adj_hjust <- matrix(0.5, nrow = rows_n, ncol = cols_n)
adj_hjust[, 1] <- 0 # Left align text
adj_x     <- matrix(0.5, nrow = rows_n, ncol = cols_n)
adj_x[, 1]     <- 0.05

adj_fontface <- matrix("plain", nrow = rows_n, ncol = cols_n)
adj_fontface[1, ] <- "bold" # Bold Total Row

table_grob <- tableGrob(
  table_final, rows = NULL, 
  theme = ttheme_minimal(
    base_family = "sans",
    core = list(fg_params = list(fontface = adj_fontface, hjust = as.vector(adj_hjust), x = as.vector(adj_x), fontsize = 10)),
    colhead = list(fg_params = list(fontsize = 11, fontface = "bold"), bg_params = list(fill = "#F2F2F2", col = "white"))
  )
)

# Notes
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 9, family = "sans"), hjust = 0, x = 0.02, y = 0.90),
  textGrob("1. Source: IPUMS ACS 2023.", gp = gpar(fontface = "italic", fontsize = 8.5, family = "sans"), hjust = 0, x = 0.02, y = 0.70),
  textGrob("2. Poverty is defined as family income <100% of the Census poverty threshold.", gp = gpar(fontface = "italic", fontsize = 8.5, family = "sans"), hjust = 0, x = 0.02, y = 0.50),
  textGrob("3. Income groups defined by national terciles.", gp = gpar(fontface = "italic", fontsize = 8.5, family = "sans"), hjust = 0, x = 0.02, y = 0.30)
)

final_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(1.5, "in")))

png(file.path(out_dir, "table_Poverty_by_Third.png"), width = 800, height = 400, res = 120)
grid.draw(final_layout)
dev.off()
message(" -> Saved Table: table_Poverty_by_Third.png")