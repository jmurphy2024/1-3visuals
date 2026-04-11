# ==============================================================================
# SCRIPT: ACS_incarceration_rates_2C.R
# Purpose: Generate 3-Country Skyline for the Adult Incarceration Rate
# Logic:   Native HHINCOME, Negatives RETAINED, Person-Weighted
# Visual:  Minimalist Skyline 2C (No grids, strict min/max y-axis, robust ntile)
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit for heavy ggplot rendering
options(expressions = 500000)

library(ipumsr); library(dplyr); library(here); library(ggplot2); library(tibble)
library(data.table); library(scales); library(stringr); library(tidyr)
library(gridExtra); library(grid); library(Hmisc)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))

# ==============================================================================
# VISUAL THEME STANDARDIZATION (MINIMALIST)
# ==============================================================================
apply_standard_theme <- function(p, r_margin = 30) {
  p_updated <- p +
    theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      legend.position   = "none", 
      panel.grid        = element_blank(), 
      axis.text.x       = element_blank(), 
      axis.text.y       = element_text(color = "black", size = 11, family = "serif", margin = margin(r = 5)), 
      axis.line.x       = element_line(color = "black", linewidth = 1.2), 
      axis.line.y       = element_line(color = "black", linewidth = 1.2), 
      axis.title        = element_blank(), 
      plot.title        = element_blank(), 
      plot.caption      = element_blank(), 
      plot.margin       = margin(t = 30, r = r_margin, b = 30, l = 20),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA)
    )
  return(p_updated)
}

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION & EXTRACT
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 
VARS_NEEDED <- c("SERIAL", "PERWT", "HHINCOME", "STATEFIP", "AGE", "GQ", "GQTYPE")
TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_incarceration_rate")
TARGET_FILE <- file.path(TARGET_DIR, "acs_incarceration_rate_aggregated.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", samples = USER_SAMPLE, variables = VARS_NEEDED,
    description = "Three Countries ACS Skyline - Incarceration Rate (Native HHINCOME)"
  )
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  message("--- Loading existing IPUMS ACS API data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 3. DATA ENGINEERING (Native HHINCOME, Retaining Negatives)
# ------------------------------------------------------------------------------
message("Cleaning data (Retaining negative household incomes)...")
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

dt_raw[, HHINCOME_clean := as.numeric(HHINCOME)]
# Properly filter out the Census 9999999 'N/A' code, but leave negatives intact
dt_raw[HHINCOME_clean == 9999999, HHINCOME_clean := NA_real_]
dt_filtered <- dt_raw[!is.na(HHINCOME_clean)]

INFLATION_ADJ <- get_inflation_multiplier(data_year = as.numeric(substring(USER_SAMPLE, 3, 6)), base_year = 2023)

# 4. NORMALIZATION & RECODING
# ------------------------------------------------------------------------------
message("Applying normalization and dynamic cutoffs...")

prepared_data <- as_tibble(dt_filtered) %>%
  mutate(
    PERWT      = as.numeric(PERWT),
    gq_num     = as.numeric(GQ),
    gqtype_num = as.numeric(GQTYPE),
    
    # INDICATOR LOGIC: 
    # GQ == 3 (Institutional Group Quarters)
    # GQTYPE == 1 or 10-19 (Correctional Facilities: Federal, State, and Local Jails)
    target_indicator = if_else(gq_num == 3 & (gqtype_num == 1 | (gqtype_num >= 10 & gqtype_num < 20)), 1, 0),
    
    # --- ONE-STEP REAL INCOME NORMALIZATION ---
    REAL_INCOME = HHINCOME_clean * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  # UNIVERSE FILTER: All adults ages 18+
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), as.numeric(AGE) >= 18)

# ==============================================================================
# 5. SUMMARY STATISTICS (TERCILES & QUARTILES WITHIN)
# ==============================================================================
message("\n=== INCARCERATION RATE SUMMARY (ADULTS 18+) ===")

overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Incarceration Rate (%)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Population) * 100, 2),
    .groups = "drop"
  )

prepared_data_q <- prepared_data %>%
  group_by(Country) %>%
  mutate(
    q25 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.25, na.rm = TRUE),
    q50 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.50, na.rm = TRUE),
    q75 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.75, na.rm = TRUE),
    Quartile = case_when(
      REAL_INCOME <= q25 ~ "Q1 (Bottom 25%)",
      REAL_INCOME > q25 & REAL_INCOME <= q50 ~ "Q2",
      REAL_INCOME > q50 & REAL_INCOME <= q75 ~ "Q3",
      TRUE ~ "Q4 (Top 25%)"
    )
  ) %>%
  ungroup()

quartile_stats <- prepared_data_q %>%
  group_by(Country, Quartile) %>%
  summarise(
    Subgroup = first(Quartile),
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Incarceration Rate (%)` = round((sum(target_indicator * PERWT, na.rm = TRUE) / Total_Population) * 100, 2),
    .groups = "drop"
  ) %>%
  select(-Quartile)

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(Total_Population = scales::comma(Total_Population))

print(as.data.frame(macro_summary))

# ==============================================================================
# 6. VISUALIZATION EXECUTION (Minimalist Format)
# ==============================================================================
message("\nGenerating Minimalist Skyline Plot...")
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

summary_plot_data <- prepared_data %>%
  group_by(Country) %>%
  mutate(decile = ntile(REAL_INCOME, 10)) %>%
  group_by(Country, decile) %>%
  summarise(avg_val = weighted.mean(target_indicator, w = PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third")), x_id = row_number())

p_chart <- ggplot(summary_plot_data, aes(x = x_id, y = avg_val, color = Country)) +
  geom_line(aes(group = 1), linewidth = 3, alpha = 0.2) + 
  geom_line(aes(group = 1), linewidth = 1.2, linejoin = "round") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1), 
    limits = c(0, NA), 
    breaks = function(x) c(0, max(x, na.rm = TRUE)), 
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.05))) +
  labs(x = NULL, y = NULL)

p_chart <- apply_standard_theme(p_chart, r_margin = 30)

# --- RSTUDIO DIRECT OUTPUTS ---
# Generate and Render Table
table_grob <- tableGrob(macro_summary, rows = NULL,
                        theme = ttheme_default(core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)),
                                               colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))))
grid.newpage()
grid.draw(table_grob)

# Render the Skyline plot in RStudio plot window
print(p_chart)

# Save Outputs
ggsave(here::here("03_output", "visualizations_final", "ACS_Incarceration_Rates_Summary_Table_2C.png"), table_grob, width = 7.5, height = 5, bg = "white", dpi = 300)
ggsave(here::here("03_output", "visualizations_final", "ACS_Incarceration_Rates_Skyline_2C.png"), p_chart, width = 10, height = 7, dpi = 300, bg = "white")

message("Processing & Visualizations Complete!")