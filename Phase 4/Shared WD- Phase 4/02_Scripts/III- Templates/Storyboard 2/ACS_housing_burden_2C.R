# ==============================================================================
# SCRIPT: ACS_housing_burden_2C.R
# Purpose: Generate 3-Country Skyline for Housing Cost Burden (Mortgage + Rent)
# Logic:   Native HHINCOME (No INCTOT Aggregation), Working-Age (18-64)
# Visual:  Minimalist Skyline 2C (No titles, no gridlines, strict min/max y-axis)
# Fix:     Uses ntile() for robust decile binning to avoid 'breaks are not unique' error
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
# Note: Since we are building the plots directly in this script, we don't strictly 
# need Skyline2C.R, but it is kept if you have dependencies on it.

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

# 2. CONFIGURATION & EXTRACT (2023 ACS)
# ------------------------------------------------------------------------------
USER_SAMPLE <- "us2023c" 
VARS_NEEDED <- c("SERIAL", "PERWT", "HHINCOME", "STATEFIP", "AGE", "OWNERSHP", "RENTGRS", "OWNCOST")

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", "acs_housing_burden")
TARGET_FILE <- file.path(TARGET_DIR, "acs_housing_burden_raw.rds")

if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS USA (ACS) API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  extract_def <- define_extract_micro(
    collection = "usa", samples = USER_SAMPLE, variables = VARS_NEEDED,
    description = "Three Countries ACS Housing Cost Burden (Working-Age, Native HHINCOME)"
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

# 3. DATA ENGINEERING (Native HHINCOME & Drop Negatives)
# ------------------------------------------------------------------------------
message("Cleaning data and dropping negative households...")
dt_raw <- as.data.table(raw_data)
setnames(dt_raw, toupper(names(dt_raw)))

dt_raw[, HHINCOME_clean := as.numeric(HHINCOME)]
dt_raw[HHINCOME_clean == 9999999, HHINCOME_clean := NA_real_]
dt_filtered <- dt_raw[HHINCOME_clean >= 0] # Explicit drop of negative incomes

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2023, base_year = 2023)

# 4. NORMALIZATION & BURDEN MATH
# ------------------------------------------------------------------------------
message("Applying normalization and dynamic cutoffs...")

prepared_data <- as_tibble(dt_filtered) %>%
  mutate(
    PERWT = as.numeric(PERWT),
    
    monthly_cost = case_when(
      as.numeric(OWNERSHP) == 1 & as.numeric(OWNCOST) > 0 & as.numeric(OWNCOST) < 99999 ~ as.numeric(OWNCOST),
      as.numeric(OWNERSHP) == 2 & as.numeric(RENTGRS) > 0 & as.numeric(RENTGRS) < 9999 ~ as.numeric(RENTGRS),
      TRUE ~ NA_real_
    ),
    
    annual_cost  = monthly_cost * 12,
    burden_ratio = case_when(
      !is.na(annual_cost) & HHINCOME_clean > 0 ~ annual_cost / HHINCOME_clean,
      !is.na(annual_cost) & HHINCOME_clean == 0 & annual_cost > 0 ~ 1.0, 
      TRUE ~ NA_real_
    ),
    
    # --- PILLAR LOGIC (THE 3 DEGREES OF HARDSHIP) ---
    `Cost Burdened (>30%)`     = if_else(!is.na(burden_ratio) & burden_ratio > 0.30, 1, 0),
    `Severely Burdened (>50%)` = if_else(!is.na(burden_ratio) & burden_ratio > 0.50, 1, 0),
    `Extremely Burdened (>70%)`= if_else(!is.na(burden_ratio) & burden_ratio > 0.70, 1, 0),
    
    Burden_Composite = `Cost Burdened (>30%)`,
    
    REAL_INCOME = HHINCOME_clean * INFLATION_ADJ * get_state_rpp_multiplier(as.numeric(STATEFIP)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country), 
         !is.na(burden_ratio), as.numeric(AGE) >= 18, as.numeric(AGE) <= 64)

# ==============================================================================
# 5. SUMMARY STATISTICS (TERCILES & QUARTILES WITHIN)
# ==============================================================================
message("\n=== HOUSING COST BURDEN SUMMARY (WORKING AGE 18-64) ===")

overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Cost Burdened (>30%)`      = round((sum(`Cost Burdened (>30%)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Severely Burdened (>50%)`  = round((sum(`Severely Burdened (>50%)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Extremely Burdened (>70%)` = round((sum(`Extremely Burdened (>70%)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
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
    `Cost Burdened (>30%)`      = round((sum(`Cost Burdened (>30%)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Severely Burdened (>50%)`  = round((sum(`Severely Burdened (>50%)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Extremely Burdened (>70%)` = round((sum(`Extremely Burdened (>70%)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  ) %>%
  select(-Quartile)

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(Total_Population = scales::comma(Total_Population))

print(as.data.frame(macro_summary))

dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

# Generate and Render Table
table_grob <- tableGrob(macro_summary, rows = NULL,
                        theme = ttheme_default(core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)),
                                               colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))))
grid.newpage()
grid.draw(table_grob)
ggsave(here::here("03_output", "visualizations_final", "ACS_Housing_Burden_Summary_Table_2C.png"), table_grob, width = 10, height = 5, bg = "white", dpi = 300)

# ==============================================================================
# 6. VISUALIZATION EXECUTION (Minimalist Format)
# ==============================================================================
message("\nGenerating Minimalist Skyline Plots...")

# --- PLOT 1: PILLARS CHART (MULTI-VARIABLE) ---
viz_data_multi <- prepared_data %>%
  group_by(Country) %>%
  mutate(decile = ntile(REAL_INCOME, 10)) %>% # THE FIX IS HERE
  group_by(Country, decile) %>%
  summarise(
    `Cost Burdened (>30%)`      = weighted.mean(`Cost Burdened (>30%)`, w = PERWT, na.rm = TRUE),
    `Severely Burdened (>50%)`  = weighted.mean(`Severely Burdened (>50%)`, w = PERWT, na.rm = TRUE),
    `Extremely Burdened (>70%)` = weighted.mean(`Extremely Burdened (>70%)`, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Cost Burdened (>30%)", "Severely Burdened (>50%)", "Extremely Burdened (>70%)"), 
               names_to = "Variable", values_to = "val") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Variable, Country, decile) %>%
  group_by(Variable) %>%
  mutate(x_id = row_number()) %>%
  ungroup()

p_pillars <- ggplot(viz_data_multi, aes(x = x_id, y = val, group = Variable)) +
  geom_line(aes(color = Country), linewidth = 3, alpha = 0.2) + 
  geom_line(aes(color = Country), linewidth = 1.2, linejoin = "round") +
  # Direct label at the end of the line
  geom_text(data = viz_data_multi %>% filter(x_id == 30), 
            aes(label = Variable), hjust = -0.1, size = 4, fontface = "bold", color = "black", family = "serif") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1), limits = c(0, NA), 
    breaks = function(x) c(0, max(x, na.rm = TRUE)), expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.35))) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL)

p_pillars <- apply_standard_theme(p_pillars, r_margin = 160) # Expanded right margin for text

# --- PLOT 2: INDEX CHART (SINGLE-VARIABLE COMPOSITE) ---
viz_data_single <- prepared_data %>%
  group_by(Country) %>%
  mutate(decile = ntile(REAL_INCOME, 10)) %>% # THE FIX IS HERE
  group_by(Country, decile) %>%
  summarise(avg_val = weighted.mean(Burden_Composite, w = PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third")), x_id = row_number())

p_index <- ggplot(viz_data_single, aes(x = x_id, y = avg_val, color = Country)) +
  geom_line(aes(group = 1), linewidth = 3, alpha = 0.2) + 
  geom_line(aes(group = 1), linewidth = 1.2, linejoin = "round") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 1), limits = c(0, 1), 
    breaks = function(x) c(0, max(x, na.rm = TRUE)), expand = expansion(mult = c(0, 0.0))
  ) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.05))) +
  labs(x = NULL, y = NULL)

p_index <- apply_standard_theme(p_index, r_margin = 30)

# Render Plots to RStudio
print(p_pillars)
print(p_index)

# Save Final Charts
ggsave(here::here("03_output", "visualizations_final", "ACS_housing_burden_working_age_pillars_2C.png"), p_pillars, width = 10, height = 7, dpi = 300, bg = "white")
ggsave(here::here("03_output", "visualizations_final", "ACS_housing_burden_working_age_composite_2C.png"), p_index, width = 10, height = 7, dpi = 300, bg = "white")

message("Processing & Visualizations Complete!")