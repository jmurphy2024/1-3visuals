# ==============================================================================
# SCRIPT: CPS_ASEC_NetWorth2.R
# Purpose: Generate 3-Country Skyline for Net Assets (Interest, Rent, Dividends)
# Logic:   V2 Aggregated Income (SERIAL + INCTOT), RPP Adjusted, Dynamic Borders
# Engine:  data.table for high-speed household aggregation
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales); library(data.table); library(tidyr); library(ggplot2); library(stringr)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION 
# ------------------------------------------------------------------------------
USER_SAMPLE   <- "cps2023_03s" 

# CHANGED: Replaced EMPSTAT with INCINT, INCRENT, INCDIVID
VARS_NEEDED   <- c("SERIAL", "ASECWT", "INCTOT", "STATEFIP", "AGE", "INCINT", "INCRENT", "INCDIVID")

TARGET_DIR    <- here::here("01_data", "raw", "IPUMS_Microdata", "cps_asec_v2")
# Renamed file to trigger fresh API download for the new variables
TARGET_FILE   <- file.path(TARGET_DIR, "cps_asec_raw_net_assets_inctot.rds")

# 3. ACQUISITION
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "cps", 
    samples = USER_SAMPLE, 
    variables = VARS_NEEDED,
    description = "Three Countries CPS-ASEC Extract (V2 INCTOT Aggregation) - Net Assets"
  )
  
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  message("--- Loading existing CPS-ASEC data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 4. CONSTRUCT TRUE HOUSEHOLD INCOME (data.table Engine)
# ------------------------------------------------------------------------------
message("Aggregating individual incomes by household...")
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

# Native aggregation of INCTOT (Filters out CPS missing/NIU codes like 99999999)
hh_aggregated <- dt_raw[
  INCTOT < 99999999, 
  .(AGGREGATED_HHINCOME = sum(INCTOT, na.rm = TRUE)), 
  by = SERIAL
]
hh_aggregated <- as_tibble(hh_aggregated)
message("Aggregation complete.")

# 5. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
state_rpp_lookup <- tibble::tibble(
  STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56),
  STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6)
)

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

prepared_data <- as_tibble(dt_raw) %>%
  left_join(hh_aggregated, by = "SERIAL") %>%
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    PERWT = ASECWT,
    
    # Clean Asset Variables: 999999999 = Not in Universe -> Convert to NA
    INCINT_CLEAN   = if_else(as.numeric(INCINT) == 999999999, NA_real_, as.numeric(INCINT)),
    INCRENT_CLEAN  = if_else(as.numeric(INCRENT) == 999999999, NA_real_, as.numeric(INCRENT)),
    INCDIVID_CLEAN = if_else(as.numeric(INCDIVID) == 999999999, NA_real_, as.numeric(INCDIVID)),
    
    # INDICATORS: Create Binary Flags
    ind_int  = if_else(!is.na(INCINT_CLEAN) & INCINT_CLEAN != 0, 1, 0),
    ind_rent = if_else(!is.na(INCRENT_CLEAN) & INCRENT_CLEAN != 0, 1, 0),
    ind_div  = if_else(!is.na(INCDIVID_CLEAN) & INCDIVID_CLEAN != 0, 1, 0),
    
    # Real Income based on INCTOT aggregation
    REAL_INCOME = (AGGREGATED_HHINCOME * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # Filter includes AGE >= 25 for asset analysis
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), AGE >= 25)

# 6. VISUALIZATION (Multi-Curve V2 Style)
# ------------------------------------------------------------------------------
message("Generating V2 Multi-Curve Skyline Plot...")

# A. Summarize all three variables into the V2 60-ventile structure
multi_viz_data <- prepared_data %>%
  group_by(Country) %>%
  mutate(ventile = ntile(REAL_INCOME, 20)) %>% 
  group_by(Country, ventile) %>%
  summarise(
    Interest  = weighted.mean(ind_int, w = PERWT, na.rm = TRUE),
    Rent      = weighted.mean(ind_rent, w = PERWT, na.rm = TRUE),
    Dividends = weighted.mean(ind_div, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, ventile) %>%
  mutate(x_id = row_number()) %>%
  pivot_longer(cols = c(Interest, Rent, Dividends), names_to = "Asset_Type", values_to = "Rate")

# B. Formatting Elements
income_breaks <- c(1, 10, 20, 30, 40, 50, 60)
income_labels <- c("$0", "$20,000", "$45,000", "$75,000", "$115,000", "$250,000", "$500,000+")
label_colors  <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")

# C. Generate Plot
p <- ggplot(multi_viz_data, aes(x = x_id, y = Rate, color = Asset_Type)) +
  geom_line(linewidth = 1.5, linejoin = "round", lineend = "round") +
  scale_color_manual(values = c(
    "Interest"  = "#386641", 
    "Dividends" = "#E9C46A", 
    "Rent"      = "#9B2226"  
  )) +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0.05, 0.05)) +
  scale_x_continuous(breaks = income_breaks, labels = income_labels, expand = c(0.01, 0.01)) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title    = element_blank(),
    legend.text     = element_text(face = "bold", size = 12),
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
    x = "Household Income (Real Adjusted Dollars)", 
    y = "Asset Ownership Rate (%)",
    caption = stringr::str_wrap("Note: The initial spike reflects the 'Zero-Income Paradox', where households reporting $0 or negative income often include business owners with paper losses or wealthy retirees living off accumulated assets. Universe restricted to adults 25+. Income is generated by natively aggregating personal income (INCTOT) within households. Data is adjusted for inflation and spatial price parity. All boundaries derived from the Master ACS Baseline V2.", width = 125)
  )

print(p)

# Auto-save logic
out_path <- here::here("03_output", "visualizations_final", "CPS_ASEC_net_assets_inctot_v2.png")
ggsave(out_path, p, width = 10, height = 6, dpi = 300)