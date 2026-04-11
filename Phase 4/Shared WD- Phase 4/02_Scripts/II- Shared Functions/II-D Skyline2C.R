# ==============================================================================
# SCRIPT: NCHS_infant_mortality_rate_2C.R
# Purpose: Generate 3-Country Skyline for Infant Mortality
# Logic:   CDC WONDER Linked Births (2017-2023) + Census ACS Income by County
# Logic Check: Explicitly removes negative income and N/A codes (HHINCOME >= 0)
# Engine:  Native wtd.quantile deciles, standardized dot + line ggplot
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit for heavy ggplot rendering
options(expressions = 500000)

library(tidycensus); library(purrr); library(dplyr); library(here); library(scales)
library(data.table); library(stringr); library(tidyr); library(ggplot2)
library(gridExtra); library(grid); library(Hmisc) 

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))

# ==============================================================================
# VISUAL THEME STANDARDIZATION
# ==============================================================================
apply_standard_theme <- function(p, cutoffs = NULL, include_cutoffs = TRUE) {
  pb <- ggplot_build(p)
  y_breaks <- pb$layout$panel_params[[1]]$y.major
  if(is.null(y_breaks)) y_breaks <- pb$layout$panel_params[[1]]$y$get_breaks()
  y_breaks <- y_breaks[!is.na(y_breaks)]
  top_y_val <- max(y_breaks)
  
  p_updated <- p +
    theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      text              = element_text(family = "serif", size = 11), 
      legend.position   = "bottom",
      legend.title      = element_blank(),
      legend.text       = element_text(family = "serif", size = 11),
      legend.margin     = margin(t = 20),
      panel.grid        = element_blank(), 
      axis.text.x       = element_blank(), 
      axis.text.y       = element_text(color = "black", size = 11, family = "serif"), 
      axis.line.x       = element_line(color = "black", linewidth = 0.75),
      axis.line.y       = element_line(color = "black", linewidth = 0.75), 
      axis.title.x      = element_blank(), 
      axis.title.y      = element_blank(),
      plot.title        = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 40), family = "serif"),
      plot.margin       = margin(t = 30, r = 20, b = 60, l = 20),
      plot.background   = element_rect(fill = "white", color = NA)
    )
  
  if (include_cutoffs && !is.null(cutoffs)) {
    cutoff_markers <- tibble::tibble(
      x_pos = c(10.5, 20.5),
      label = c(paste0("Cutoff:\n", scales::dollar(cutoffs$main_cutoff1, accuracy = 1)),
                paste0("Cutoff:\n", scales::dollar(cutoffs$main_cutoff2, accuracy = 1)))
    )
    p_updated <- p_updated +
      geom_segment(data = cutoff_markers, aes(x = x_pos, xend = x_pos, y = 0, yend = top_y_val), 
                   color = c("#9B2226", "#E9C46A"), linetype = "dashed", linewidth = 1, alpha = 0.8, 
                   inherit.aes = FALSE, show.legend = FALSE) +
      geom_text(data = cutoff_markers, aes(x = x_pos, y = 0, label = label), 
                color = c("#9B2226", "#E9C46A"), hjust = 1.15, vjust = -0.2, fontface = "bold", 
                size = 11 / .pt, family = "serif", inherit.aes = FALSE, show.legend = FALSE)
  }
  return(p_updated)
}

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & ACQUISITION
# ------------------------------------------------------------------------------
TARGET_YEAR <- 2022 
TARGET_DIR  <- here::here("01_data", "raw", "NCHS_CDC")
NCHS_FILE   <- here::here("Linked Birth  Infant Death Records, 2017-2023 Expanded.csv") 
TARGET_FILE <- file.path(TARGET_DIR, "nchs_acs_infant_mortality_raw_v2.rds")

if (!file.exists(TARGET_FILE)) {
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  nchs_data <- read.csv(NCHS_FILE, check.names = FALSE)
  acs_income <- purrr::map_dfr(c(state.abb, "DC"), ~get_acs(
    geography = "county", variables = "B19013_001", state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE
  ))
  saveRDS(list(nchs = nchs_data, acs = acs_income), TARGET_FILE)
} else {
  raw_data <- readRDS(TARGET_FILE)
}

# 3. DATA ENGINEERING: MERGING & NEGATIVE INCOME REMOVAL
# ------------------------------------------------------------------------------
message("Standardizing income and dropping negative values...")
dt_acs <- as.data.table(raw_data$acs)[, .(fips = GEOID, est_median_hh_inc = estimate)]

# CORE LOGIC: Set 9999999 to NA and filter for non-negative income
dt_acs[est_median_hh_inc == 9999999, est_median_hh_inc := NA_real_]
dt_acs <- dt_acs[est_median_hh_inc >= 0] 

dt_nchs <- as.data.table(raw_data$nchs)[!is.na(`County Code`) & `County Code` != "", .(
  fips   = sprintf("%05d", as.numeric(`County Code`)), 
  deaths = as.numeric(Deaths),
  births = as.numeric(Births)
)][!endsWith(fips, "999") & births > 0]

dt_nchs[, infant_mort_rate := deaths / births]
dt_linked <- merge(dt_nchs, dt_acs, by = "fips", all.x = FALSE) # Inner join drops counties with missing/removed income

# 4. NORMALIZATION & COMPOSITE BUILD
# ------------------------------------------------------------------------------
state_rpp_lookup <- tibble::tibble(STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56), STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6))
INFLATION_ADJ <- get_inflation_multiplier(data_year = TARGET_YEAR, base_year = 2023)

prepared_data <- as_tibble(dt_linked) %>%
  mutate(STATEFIP = as.numeric(substr(fips, 1, 2))) %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    PERWT            = births,
    target_indicator = infant_mort_rate,
    REAL_INCOME      = (as.numeric(est_median_hh_inc) * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    Country          = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(REAL_INCOME), REAL_INCOME >= 0) # Double-check non-negative real income

# 5. SMOOTHING & SUMMARY
# ------------------------------------------------------------------------------
prepared_data <- prepared_data %>%
  filter(target_indicator >= quantile(target_indicator, 0.01, na.rm = TRUE),
         target_indicator <= quantile(target_indicator, 0.99, na.rm = TRUE))

overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Births = sum(PERWT, na.rm = TRUE),
    `Infant Mortality (per 1,000)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Births) * 1000, 2),
    .groups = "drop"
  )

# ... (Quartile calculation and tableGrob generation same as previous version)

# 6. VISUALIZATION EXECUTION (Skyline 2C Format)
# ------------------------------------------------------------------------------
summary_plot_data <- prepared_data %>%
  group_by(Country) %>%
  mutate(decile = as.integer(cut(REAL_INCOME, 
                                 breaks = c(-Inf, wtd.quantile(REAL_INCOME, weights = PERWT, probs = seq(0.1, 0.9, by = 0.1), na.rm = TRUE), Inf),
                                 labels = 1:10, include.lowest = TRUE))) %>%
  group_by(Country, decile) %>%
  summarise(avg_val = weighted.mean(target_indicator, w = PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third")), x_id = row_number())

p_chart <- ggplot(summary_plot_data, aes(x = x_id, y = avg_val, color = Country)) +
  geom_line(aes(group = 1), linewidth = 3, alpha = 0.2) + 
  geom_line(aes(group = 1), linewidth = 1, linejoin = "round") +
  geom_point(size = 2) +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  labs(title = "NCHS Infant Mortality Rate (Negatives Removed)")

p_chart <- apply_standard_theme(p_chart, cutoffs)
ggsave(here::here("03_output", "visualizations_final", "NCHS_Infant_Mortality_TrueV2_Smoothed_Skyline_2C.png"), p_chart, width = 10, height = 7, dpi = 300, bg = "white")