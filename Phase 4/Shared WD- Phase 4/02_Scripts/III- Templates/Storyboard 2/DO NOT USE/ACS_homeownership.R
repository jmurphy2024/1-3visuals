# ==============================================================================
# SCRIPT: ACS_Homeownership_Skyline.R
# Purpose: Generate 3-Country Skyline for Homeownership Rates
# Logic: IPUMS ACS + Spatial RPP + 10-Decile Smoothing + Master Design
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(ggplot2); library(scales); library(tidyr)

# 1. SOURCE MASTER LOGIC
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R")) 
set.seed(123)

# 2. CONFIGURATION & EXTRACT
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2022a" # ACS 2022 1-Year Sample
# OWNERSHP: 1 = Owned/being bought, 2 = Rented
VARS_NEEDED <- c("PERWT", "HHWT", "OWNERSHP", "HHINCOME", "REGION","AGE")
TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_housing")
TARGET_FILE <- file.path(TARGET_DIR, "acs_homeownership.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS ACS Housing Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection  = "usa", 
    samples     = USER_SAMPLE,
    variables   = VARS_NEEDED,
    description = "Three Countries ACS Homeownership"
  )
  
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  raw_data <- readRDS(TARGET_FILE)
}

# 3. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
# Map standard Census Regions to RPP Values
region_rpp_lookup <- tibble(
  REGION_ID = c(11, 12, 21, 22, 31, 32, 33, 41, 42),
  REG_RPP   = c(105.2, 105.2, 92.8, 92.8, 95.4, 95.4, 95.4, 104.1, 104.1) 
)

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2022, base_year = 2023)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>%
  # Remove N/A or institutional group quarters (OWNERSHP == 0)
  filter(OWNERSHP %in% c(1, 2), HHINCOME < 9999999) %>%
  left_join(region_rpp_lookup, by = c("REGION" = "REGION_ID")) %>%
  
  mutate(
    # Recode Target: 1 if Homeowner, 0 if Renter
    is_homeowner = if_else(OWNERSHP == 1, 1, 0),
    
    # Calculate Real Income using standard project logic
    REAL_INCOME = (HHINCOME * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)),
    
    Country = case_when(
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(REAL_INCOME), PERWT > 0)

# 4. DECILE SUMMARIZATION (10 Points per Country)
# ------------------------------------------------------------------------------
viz_data <- prepared_data %>%
  group_by(Country) %>%
  mutate(decile = ntile(REAL_INCOME, 10)) %>%
  group_by(Country, decile) %>%
  summarise(
    # We use PERWT to represent the % of *people* living in an owned home
    val = weighted.mean(is_homeowner, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, decile) %>%
  mutate(x_id = row_number())

# 5. VISUALIZATION (Master Skyline Design)
# ------------------------------------------------------------------------------
income_breaks <- c(1, 5, 10, 15, 20, 25, 30) 
income_labels <- c("$0", "$20,000", "$45,000", "$75,000", "$115,000", "$250,000", "$500,000+")
label_colors <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")

p <- ggplot(viz_data, aes(x = x_id, y = val)) +
  geom_line(aes(color = Country, group = 1), linewidth = 4, alpha = 0.15, show.legend = FALSE) + 
  geom_line(aes(color = Country, group = 1), linewidth = 1, linejoin = "round", lineend = "round") +
  
  scale_color_manual(
    values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641"), 
    guide = "none"
  ) + 
  
  scale_y_continuous(
    name = "Homeownership Rate (%)",
    labels = label_percent(), 
    limits = c(0, 1),
    expand = c(0, 0)
  ) +
  scale_x_continuous(
    name = "Household Income (Real Adjusted Dollars)",
    breaks = income_breaks, 
    labels = income_labels, 
    expand = c(0.01, 0.01)
  ) + 
  coord_cartesian(clip = "off") + 
  
  theme_minimal(base_family = "sans") + 
  theme(
    panel.grid      = element_blank(),  
    axis.title.x    = element_text(face = "bold", size = 14, margin = margin(t = 15)), 
    axis.title.y    = element_text(face = "bold", size = 14, margin = margin(r = 15)), 
    axis.text.x     = element_text(color = label_colors, face = "bold", size = 10), 
    axis.text.y     = element_text(color = "black", size = 10),
    axis.line.x     = element_line(color = "black", linewidth = 1.5), 
    axis.line.y     = element_line(color = "black", linewidth = 1.5),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(t = 20, r = 10, b = 40, l = 10),
    plot.caption    = element_text(hjust = 0, size = 9, color = "grey30", margin = margin(t = 20))
  ) +
  labs(
    caption = stringr::str_wrap("Note: This chart displays the percentage of individuals living in an owner-occupied household. Income is adjusted for spatial price parity (RPP) and inflation. Data is grouped into 10 deciles per income cohort to smooth micro-level demographic variance.", width = 110)
  )

print(p)

# --- 6. EXPORT ---
out_dir <- here::here("03_output", "visualizations_final")
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
ggsave(file.path(out_dir, "ACS_Homeownership_Skyline.png"), plot = p, width = 10, height = 6.5, dpi = 300)

message("ACS Homeownership Skyline generated successfully.")