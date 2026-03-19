# ==============================================================================
# SCRIPT: Urban_Inst_Grad_Rates_V2.R
# Purpose: Generate 3-Country Skyline for Graduation Rates 
# Logic:   Urban Institute (Grad Rates) + Census ACS (Income), RPP Adjusted
# Engine:  educationdata & tidycensus APIs, data.table for high-speed linking
# ==============================================================================
rm(list = ls()); gc()

# Ensure you have tidycensus, purrr, and gridExtra installed
library(educationdata); library(tidycensus); library(purrr); library(dplyr); library(here); library(scales); library(data.table); library(ggplot2); library(stringr); library(tidyr); library(gridExtra); library(grid)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
# FIX APPLIED: Removed II-D Income Normalization source to prevent File Not Found crashes.

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION 
# ------------------------------------------------------------------------------
TARGET_YEAR   <- 2019 # Using 2019 as a robust pre-COVID baseline year
TARGET_DIR    <- here::here("01_data", "raw", "Urban_Institute")
TARGET_FILE   <- file.path(TARGET_DIR, "ui_grad_acs_income_raw_v2.rds")

# 3. ACQUISITION (Urban Institute API & Census API)
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering Education and Census APIs ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  # A. Fetch School-Level Graduation Data (Urban Institute)
  message("Fetching School Graduation Rates...")
  school_data <- get_education_data(
    level = "schools", 
    source = "edfacts", 
    topic = "grad-rates", 
    filters = list(year = TARGET_YEAR)
  )
  
  # B. Fetch District Median Incomes (Census ACS via tidycensus)
  message("Fetching District Median Incomes by State (Census ACS)...")
  
  safe_get_acs <- purrr::possibly(get_acs, otherwise = NULL)
  state_list <- c(state.abb, "DC")
  
  inc_uni <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (unified)", variables = "B19013_001", state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  inc_ele <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (elementary)", variables = "B19013_001", state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  inc_sec <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (secondary)", variables = "B19013_001", state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  
  income_data <- bind_rows(inc_uni, inc_ele, inc_sec)
  
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

dt_schools_clean <- dt_schools[cohort_num >= 0 & grad_rate_midpt >= 0]

dt_district_grad <- dt_schools_clean[, .(
  district_cohort_size = sum(cohort_num, na.rm = TRUE),
  district_grad_rate   = weighted.mean(grad_rate_midpt / 100, w = cohort_num, na.rm = TRUE),
  fips                 = first(fips) 
), by = .(leaid, year)]

dt_income <- dt_income[, .(leaid = GEOID, est_median_hh_inc = estimate)]
dt_linked <- merge(dt_district_grad, dt_income, by = "leaid", all.x = TRUE)

# 5. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

# FIX APPLIED: Hardcoded inflation (2019 to 2023 base year) to avoid missing file dependencies
INFLATION_ADJ <- 304.702 / 255.657

prepared_data <- as_tibble(dt_linked) %>%
  rename(STATEFIP = fips) %>% 
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    PERWT            = district_cohort_size,
    target_indicator = district_grad_rate,
    hh_income_clean  = if_else(est_median_hh_inc > 0, as.numeric(est_median_hh_inc), NA_real_),
    
    REAL_INCOME = (hh_income_clean * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), !is.na(target_indicator))

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
    y = "High School Graduation Rate (%)",
    caption = stringr::str_wrap("Note: School-level graduation rates (Urban Institute EDFacts) are aggregated up to the district level and linked to School District Median Household Income (Census ACS 5-Year Estimates).", width = 125)
  )

dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)
out_path <- here::here("03_output", "visualizations_final", "Urban_Inst_Grad_Rates_V2.png")
ggsave(out_path, p, width = 10, height = 6, dpi = 300)

# ==============================================================================
# 7. SUMMARY TABLE GENERATION (Quartiles & Overall)
# ==============================================================================
message("Generating Summary Table PNG...")

overall_rates <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Grad_Rate = weighted.mean(target_indicator, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Quartile = "Overall")

quartile_rates <- prepared_data %>%
  group_by(Country) %>%
  mutate(quartile = ntile(REAL_INCOME, 4)) %>%
  group_by(Country, quartile) %>%
  summarise(
    Grad_Rate = weighted.mean(target_indicator, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Quartile = paste0("Q", quartile)) %>%
  select(-quartile)

summary_table <- bind_rows(overall_rates, quartile_rates) %>%
  mutate(Grad_Rate = scales::percent(Grad_Rate, accuracy = 0.1)) %>%
  pivot_wider(names_from = Country, values_from = Grad_Rate) %>%
  select(Quartile, `Bottom Third`, `Middle Third`, `Top Third`) %>%
  rename(`Income Quartile` = Quartile) %>%
  arrange(match(`Income Quartile`, c("Overall", "Q1", "Q2", "Q3", "Q4")))

table_grob <- tableGrob(
  summary_table, 
  rows = NULL,
  theme = ttheme_default(
    core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 1.1)),
    colhead = list(fg_params = list(cex = 1.2, fontface = "bold"))
  )
)

ggsave(
  filename = here::here("03_output", "visualizations_final", "Urban_Inst_Grad_Rates_Table.png"), 
  plot = table_grob, 
  width = 8, 
  height = 3.5, 
  bg = "white",
  dpi = 300
)

message("Summary table successfully generated and saved!")