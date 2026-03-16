# ==============================================================================
# SCRIPT: UITemplate.R
# Purpose: Generate 3-Country Skyline for [INSERT TOPIC HERE] 
# Logic:   Urban Institute API + Census ACS API (Income), RPP Adjusted
# Engine:  educationdata & tidycensus APIs, data.table for high-speed linking
# ==============================================================================
rm(list = ls()); gc()

# Ensure you have tidycensus and purrr installed
library(educationdata); library(tidycensus); library(purrr); library(dplyr); library(here); library(scales); library(data.table); library(ggplot2); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# ==============================================================================
# 2. CONFIGURATION (USER INPUTS REQUIRED)
# ==============================================================================
# [TODO 1]: Set your Year and File Names
TARGET_YEAR   <- 2019 
TARGET_DIR    <- here::here("01_data", "raw", "Urban_Institute")
TARGET_FILE   <- file.path(TARGET_DIR, "ui_template_raw_v2.rds")

# [TODO 2]: Define Urban Institute API Parameters (See Urban Data Portal Documentation)
UI_SOURCE     <- "edfacts"      # e.g., "edfacts", "ccd", "crdc"
UI_TOPIC      <- "grad-rates"   # e.g., "grad-rates", "enrollment", "assessments"

# [TODO 3]: Define Census ACS Variable for the X-Axis (Default is Median HH Income)
CENSUS_VAR    <- "B19013_001"

# [TODO 4]: Define Plot Output Labels
Y_AXIS_LABEL  <- "[INSERT Y-AXIS LABEL HERE] (%)"
PLOT_CAPTION  <- "Note: School-level data (Urban Institute) is aggregated up to the district level and linked to School District Median Household Income (Census ACS). Data excludes suppressed or missing reports. Income is adjusted for inflation and spatial price parity. All boundaries derived from the Master ACS Baseline V2."
SAVE_NAME     <- "Urban_Inst_Template_Output_V2.png"
# ==============================================================================


# 3. ACQUISITION (Urban Institute API & Census API)
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering Education and Census APIs ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  # A. Fetch School-Level Data (Urban Institute)
  message(paste0("Fetching School Data (", UI_SOURCE, " / ", UI_TOPIC, ")..."))
  school_data <- get_education_data(
    level = "schools", 
    source = UI_SOURCE, 
    topic = UI_TOPIC, 
    filters = list(year = TARGET_YEAR)
  )
  
  # B. Fetch District Median Incomes (Census ACS via tidycensus)
  message("Fetching District Median Incomes by State (Census ACS)...")
  
  safe_get_acs <- purrr::possibly(get_acs, otherwise = NULL)
  state_list <- c(state.abb, "DC")
  
  inc_uni <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (unified)", variables = CENSUS_VAR, state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  inc_ele <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (elementary)", variables = CENSUS_VAR, state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  inc_sec <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (secondary)", variables = CENSUS_VAR, state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  
  income_data <- bind_rows(inc_uni, inc_ele, inc_sec)
  
  # Combine into a list to save locally
  raw_data <- list(schools = school_data, income = income_data)
  saveRDS(raw_data, TARGET_FILE)
  
} else {
  message("--- Loading existing API data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 4. CONSTRUCT LINKED DATASET (District-Level Aggregation)
# ------------------------------------------------------------------------------
message("Aggregating Schools to District Level...")
dt_schools <- as.data.table(raw_data$schools)
dt_income  <- as.data.table(raw_data$income)

# [TODO 5]: Keep only the needed anchor variables. 
# REPLACE 'cohort_num' and 'grad_rate_midpt' with your specific UI target variables.
dt_schools <- dt_schools[, .(year, leaid, fips, cohort_num, grad_rate_midpt)]

# [TODO 6]: Filter out Missing/Suppressed codes (usually -1, -2, -3 in Urban Inst data)
dt_schools_clean <- dt_schools[cohort_num >= 0 & grad_rate_midpt >= 0]

# [TODO 7]: Roll up schools to the District Level. 
# Adjust the math here (e.g., sum, weighted.mean) based on your specific metric.
dt_district_agg <- dt_schools_clean[, .(
  district_weight = sum(cohort_num, na.rm = TRUE),
  district_value  = weighted.mean(grad_rate_midpt / 100, w = cohort_num, na.rm = TRUE),
  fips            = first(fips) 
), by = .(leaid, year)]

# Clean the income data
dt_income <- dt_income[, .(leaid = GEOID, est_median_hh_inc = estimate)]

# Merge the District Data with the District Median Incomes
dt_linked <- merge(dt_district_agg, dt_income, by = "leaid", all.x = TRUE)

# 5. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

INFLATION_ADJ <- get_inflation_multiplier(data_year = TARGET_YEAR, base_year = 2023)

prepared_data <- as_tibble(dt_linked) %>%
  rename(STATEFIP = fips) %>% 
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    # Link the variables created in [TODO 7] to the Master Skyline engine
    PERWT            = district_weight,
    target_indicator = district_value,
    hh_income_clean  = if_else(est_median_hh_inc > 0, as.numeric(est_median_hh_inc), NA_real_),
    
    # Apply standard spatial/temporal adjustments to District Income
    REAL_INCOME = (hh_income_clean * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # Clean universe filter (Includes the >= 30000 floor to cut extreme rural outlier jitter)
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), REAL_INCOME >= 30000, !is.na(Country), !is.na(target_indicator))

# 6. VISUALIZATION (Single-Curve V2 Style)
# ------------------------------------------------------------------------------
message("Generating V2 Skyline Plot...")

viz_data <- prepared_data %>%
  group_by(Country) %>%
  mutate(ventile = ntile(REAL_INCOME, 20)) %>% 
  group_by(Country, ventile) %>%
  summarise(
    val = weighted.mean(target_indicator, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, ventile) %>%
  mutate(x_id = row_number())

income_breaks <- c(1, 10, 20, 30, 40, 50, 60)
income_labels <- c("$0", "$20,000", "$45,000", "$75,000", "$115,000", "$250,000", "$500,000+")
label_colors  <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")

p <- ggplot(viz_data, aes(x = x_id, y = val)) +
  geom_line(aes(color = Country, group = 1), linewidth = 4, alpha = 0.15) + 
  geom_line(aes(color = Country, group = 1), linewidth = 1.5, linejoin = "round", lineend = "round") +
  scale_color_manual(values = c(
    "Bottom Third" = "#9B2226", 
    "Middle Third" = "#E9C46A", 
    "Top Third"    = "#386641"
  )) +
  # Note: If your new metric is NOT a percentage, change scales::label_percent() to scales::label_number() below
  scale_y_continuous(labels = scales::label_percent(), expand = c(0.05, 0.05)) +
  scale_x_continuous(breaks = income_breaks, labels = income_labels, expand = c(0.01, 0.01)) +
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
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(t = 20, r = 10, b = 20, l = 10),
    plot.caption    = element_text(hjust = 0, size = 9, color = "grey30", margin = margin(t = 20), lineheight = 1.2)
  ) +
  labs(
    x = "District Median Household Income (Real Adjusted Dollars)", 
    y = Y_AXIS_LABEL,
    caption = stringr::str_wrap(PLOT_CAPTION, width = 125)
  )

print(p)

# Auto-save logic
out_path <- here::here("03_output", "visualizations_final", SAVE_NAME)
ggsave(out_path, p, width = 10, height = 6, dpi = 300)