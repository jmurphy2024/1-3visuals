# ==============================================================================
# SCRIPT: NCVS_Incident_Crime_Skyline.R
# Purpose: Generate 3-Country Skyline for Total Crime Volume (Incident Level)
# Logic: Dynamic RDA Load + 3-Way Join + Spatial RPP + Temporal Inflation + Pivot
# ==============================================================================
rm(list = ls()); gc()
library(dplyr); library(here); library(stringr); library(tidyr); library(ggplot2)

# 1. SOURCE MASTER LOGIC
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R")) 
set.seed(123) 

extract_code <- function(x) { as.numeric(str_extract(as.character(x), "\\d+")) } 

# 2. DATA ACQUISITION
HH_FILE   <- here::here("01_data", "raw", "NCVS", "ncvs_household_2023.rda")
PER_FILE  <- here::here("01_data", "raw", "NCVS", "ncvs_person_2023.rda")
INC_FILE  <- here::here("01_data", "raw", "NCVS", "ncvs_extract_2023.rda")

if (!file.exists(HH_FILE) | !file.exists(PER_FILE) | !file.exists(INC_FILE)) {
  stop("NCVS .rda files not found. Please verify the exact filenames.")
}

message("--- Loading NCVS RDA Files ---")
hh_obj_name  <- load(HH_FILE)
per_obj_name <- load(PER_FILE)
inc_obj_name <- load(INC_FILE)

ds2_raw <- get(hh_obj_name[1])
ds3_raw <- get(per_obj_name[1])
ds5_raw <- get(inc_obj_name[1])

rm(list = c(hh_obj_name, per_obj_name, inc_obj_name))
gc()

# 3. SPATIAL & TEMPORAL CONFIGURATION
region_rpp_lookup <- tibble(
  REGION_ID = c(1, 2, 3, 4),
  REG_RPP   = c(105.2, 92.8, 95.4, 104.1) 
)

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

# 4. DEDUPLICATION
ds2_unique <- ds2_raw %>% group_by(IDHH) %>% slice(1) %>% ungroup() 
ds3_unique <- ds3_raw %>% group_by(IDHH, IDPER) %>% slice(1) %>% ungroup() 

# 5. THE UNIFIED PIPELINE (INCIDENT LEVEL FOCUS)
prepared_data <- ds3_unique %>%
  left_join(ds2_unique %>% select(IDHH, V2026, V2127B), by = "IDHH") %>%
  left_join(ds5_raw %>% select(IDHH, IDPER, V4529), by = c("IDHH", "IDPER"), relationship = "one-to-many") %>% 
  mutate(
    income_code   = extract_code(V2026),
    PERWT         = as.numeric(as.character(WGTPERCY)),
    crime_code    = extract_code(V4529),
    MAPPED_REGION = as.numeric(V2127B), 
    ind_violent   = if_else(!is.na(crime_code) & crime_code >= 1 & crime_code <= 20, 1, 0), 
    ind_property  = if_else(!is.na(crime_code) & crime_code >= 31 & crime_code <= 59, 1, 0) 
  ) %>%
  group_by(IDHH, IDPER) %>%
  summarise(
    PERWT         = first(PERWT),
    income_code   = first(income_code),
    MAPPED_REGION = first(MAPPED_REGION),
    
    # SHIFT TO INCIDENCE: Use sum() instead of max() to count total crimes per person
    ind_violent   = sum(ind_violent, na.rm = TRUE), 
    ind_property  = sum(ind_property, na.rm = TRUE), 
    .groups       = "drop"
  ) %>%
  filter(!is.na(income_code) & income_code < 99 & PERWT > 0) %>% 
  mutate(
    raw_dollars = case_when(
      income_code == 1  ~ runif(n(), 0, 4999), 
      income_code == 2  ~ runif(n(), 5000, 9999),
      income_code == 3  ~ runif(n(), 10000, 14999),
      income_code == 4  ~ runif(n(), 15000, 24999),
      income_code == 5  ~ runif(n(), 25000, 34999),
      income_code == 6  ~ runif(n(), 35000, 49999),
      income_code == 7  ~ runif(n(), 50000, 74999),
      income_code == 17 ~ runif(n(), 75000, 250000), 
      TRUE              ~ runif(n(), 35000, 49999) 
    )
  ) %>%
  left_join(region_rpp_lookup, by = c("MAPPED_REGION" = "REGION_ID")) %>%
  mutate(
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)),
    Country = case_when(
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(REAL_INCOME), !is.na(Country))

# 6. VISUALIZATION: INCIDENT RATES PER 1,000
viz_data <- prepared_data %>%
  group_by(Country) %>%
  mutate(ventile = ntile(REAL_INCOME, 20)) %>% 
  group_by(Country, ventile) %>%
  summarise(
    # SHIFT TO INCIDENCE: Multiply the mean by 1,000 for standard per-capita reporting
    `Violent Crime`  = weighted.mean(ind_violent, w = PERWT, na.rm = TRUE) * 1000,
    `Property Crime` = weighted.mean(ind_property, w = PERWT, na.rm = TRUE) * 1000,
    .groups = "drop"
  ) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, ventile) %>%
  mutate(x_id = row_number()) %>%
  pivot_longer(cols = c(`Violent Crime`, `Property Crime`), names_to = "Crime_Type", values_to = "Rate")

# Create a small dataframe just for the end-of-line labels
label_data <- viz_data %>%
  group_by(Crime_Type) %>%
  filter(x_id == max(x_id))

income_breaks <- c(1, 10, 20, 30, 40, 50, 60)
income_labels <- c("$0", "$20,000", "$45,000", "$75,000", "$115,000", "$250,000", "$500,000+")
label_colors  <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")

# Build the custom Plot
p <- ggplot(viz_data, aes(x = x_id, y = Rate, color = Country)) +
  geom_line(aes(group = Crime_Type), linewidth = 4, alpha = 0.15, show.legend = FALSE) + 
  geom_line(aes(group = Crime_Type), linewidth = 1, linejoin = "round", lineend = "round") +
  
  geom_text(data = label_data, aes(label = Crime_Type, x = x_id + 0.5), 
            color = "black", hjust = 0, fontface = "bold", size = 3.5) +
  
  scale_color_manual(values = c("Bottom Third" = "#9B2226", "Middle Third" = "#E9C46A", "Top Third" = "#386641"), guide = "none") + 
  
  # SHIFT TO INCIDENCE: Swap out percentages for standard comma formatting
  scale_y_continuous(labels = scales::label_comma(), expand = c(0.05, 0.05)) + 
  scale_x_continuous(breaks = income_breaks, labels = income_labels, expand = expansion(mult = c(0.01, 0.18))) + 
  theme_minimal() + 
  theme(
    legend.position = "none", 
    panel.grid      = element_blank(),  
    axis.title.x    = element_text(face = "bold", size = 14, margin = margin(t = 15)), 
    axis.title.y    = element_text(face = "bold", size = 14, margin = margin(r = 15)), 
    axis.text.x     = element_text(color = label_colors, face = "bold", size = 10), 
    axis.text.y     = element_text(color = "black", size = 10), 
    axis.line.x     = element_line(color = "black", linewidth = 1.5), 
    axis.line.y     = element_line(color = "black", linewidth = 1.5), 
    plot.background = element_rect(fill = "white", color = NA) 
  ) +
  labs(
    x = "Household Income (Real Adjusted Dollars)", 
    y = "Total Crimes Experienced (per 1,000 Persons)",
    caption = "Note: This chart shows the total number of crimes reported per 1,000 people.  Unlike the prevalence chart, this version counts every incident, including 
               cases where the same person was victimized multiple times."
  ) +
  theme(
    plot.caption = element_text(hjust = 0, size = 9, color = "black", margin = margin(t = 15))
  )
# Auto-Save to Visualizations Final Folder
out_dir <- here::here("03_output", "visualizations_final")
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

ggsave(file.path(out_dir, "NCVS_2023_Incident_Crime_Skyline.png"), p, width = 10, height = 6, dpi = 300)
print(p)