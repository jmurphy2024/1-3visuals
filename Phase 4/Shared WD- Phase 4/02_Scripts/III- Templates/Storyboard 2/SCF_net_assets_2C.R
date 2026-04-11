# ==============================================================================
# SCRIPT: SCF_net_assets_2C.R
# Purpose: Generate 3-Country Skyline for Net Assets & Wealth Resilience
# Logic:   Fed Survey of Consumer Finances (SCF) -> Terciles -> Wealth Pillars
# Visual:  Minimalist Skyline 2C (No grids, 0-to-100% y-axis, robust ntile)
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit for heavy ggplot rendering
options(expressions = 500000)

library(dplyr); library(here); library(scales); library(data.table); 
library(stringr); library(tidyr); library(ggplot2); library(httr); library(haven)
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

# 2. CONFIGURATION & CACHE SETUP (2022 Federal Reserve SCF)
# ------------------------------------------------------------------------------
TARGET_DIR  <- here::here("01_data", "raw", "Federal_Reserve_SCF")
TARGET_FILE <- file.path(TARGET_DIR, "scf_summary_2022.rds")

# The Federal Reserve provides a public summary zip of all macro wealth variables
SCF_URL <- "https://www.federalreserve.gov/econres/files/scfp2022s.zip"

# 3. DATA ACQUISITION
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Downloading Federal Reserve Survey of Consumer Finances (2022) ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  temp_zip <- tempfile(fileext = ".zip")
  
  # Download the zip file
  res <- tryCatch({
    GET(SCF_URL, write_disk(temp_zip, overwrite = TRUE), timeout(120))
  }, error = function(e) {
    stop(paste("Failed to download SCF data:", e$message))
  })
  
  # Unzip and identify the specific file inside
  extracted_file <- unzip(temp_zip, list = TRUE)$Name[1]
  unzip(temp_zip, files = extracted_file, exdir = TARGET_DIR)
  
  full_path <- file.path(TARGET_DIR, extracted_file)
  
  # Dynamically read the file based on the format the Fed provided
  if (grepl("\\.dta$", extracted_file, ignore.case = TRUE)) {
    message(paste("-> Detected Stata format:", extracted_file, "- Reading via haven..."))
    raw_data <- haven::read_dta(full_path)
  } else if (grepl("\\.csv$", extracted_file, ignore.case = TRUE)) {
    message(paste("-> Detected CSV format:", extracted_file, "- Reading via data.table..."))
    raw_data <- data.table::fread(full_path)
  } else {
    stop("Unknown file format extracted from ZIP.")
  }
  
  saveRDS(raw_data, TARGET_FILE)
  
  # Clean up temp files
  unlink(temp_zip)
  unlink(full_path)
  message("--- SCF Acquisition Complete and Cached ---")
  
} else {
  message("--- Loading cached SCF data from local disk ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 4. NORMALIZATION & PILLAR LOGIC (PERSON-LEVEL)
# ------------------------------------------------------------------------------
message("Calculating Wealth and Asset Thresholds at the Person Level...")

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2022, base_year = 2023)

prepared_data <- as_tibble(raw_data) %>%
  rename_with(toupper, everything()) %>%  
  mutate(
    # --- CONVERT TO PERSON-LEVEL WEIGHTS ---
    # 1. Calculate the number of adults (MARRIED=1 means 2 adults, otherwise 1)
    Adults = if_else(MARRIED == 1, 2, 1),
    
    # 2. Total Family Size = Adults + KIDS
    PEU_SIZE = Adults + KIDS,
    
    # 3. Multiply the Household Weight by the Family Size to get Total People
    PERWT = WGT * PEU_SIZE,
    
    # Adjust Income to 2023 dollars
    REAL_INCOME = INCOME * INFLATION_ADJ,
    
    # 1. Positive Net Worth (Total Assets > Total Debt)
    `Positive Net Worth` = if_else(NETWORTH > 0, 1, 0),
    
    # 2. Liquid Savings Buffer (Checking, Savings, Money Market > $5,000)
    `Liquid Savings (>$5k)` = if_else((LIQ * INFLATION_ADJ) >= 5000, 1, 0),
    
    # 3. Homeownership (Has >$0 value in primary residence)
    `Homeowner` = if_else(HOUSES > 0, 1, 0),
    
    # Composite: Has all three foundations of baseline wealth
    Wealth_Resilience_Index = if_else(`Positive Net Worth` == 1 & 
                                        `Liquid Savings (>$5k)` == 1 & 
                                        `Homeowner` == 1, 1, 0),
    
    # Categorize into the Three Countries (SCF lacks state RPPs, so we use national cutoffs)
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(PERWT > 0, !is.na(Country), !is.na(REAL_INCOME))

# ==============================================================================
# 5. SUMMARY STATISTICS (PERSON-LEVEL: TERCILES & QUARTILES WITHIN)
# ==============================================================================
message("\n=== NET ASSETS & WEALTH RESILIENCE SUMMARY (PERSON LEVEL) ===")

overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile",
    Total_Population = sum(PERWT, na.rm = TRUE),
    `Wealth Resilience Index (%)` = round((sum(Wealth_Resilience_Index * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Median Net Worth ($)` = median(rep(NETWORTH * INFLATION_ADJ, times = pmax(1, round(PERWT / 100))), na.rm = TRUE),
    `Positive Net Worth (%)` = round((sum(`Positive Net Worth` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Liquid Savings >$5k (%)` = round((sum(`Liquid Savings (>$5k)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Homeowner (%)`          = round((sum(Homeowner * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
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
    `Wealth Resilience Index (%)` = round((sum(Wealth_Resilience_Index * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Median Net Worth ($)` = median(rep(NETWORTH * INFLATION_ADJ, times = pmax(1, round(PERWT / 100))), na.rm = TRUE),
    `Positive Net Worth (%)` = round((sum(`Positive Net Worth` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Liquid Savings >$5k (%)` = round((sum(`Liquid Savings (>$5k)` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Homeowner (%)`          = round((sum(Homeowner * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  ) %>%
  select(-Quartile)

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(
    Total_Population = scales::comma(Total_Population),
    `Median Net Worth ($)` = scales::dollar(round(`Median Net Worth ($)`, 0))
  )

print(as.data.frame(macro_summary))
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)
table_grob <- tableGrob(macro_summary, rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)), colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))))

# ==============================================================================
# 6. VISUALIZATION EXECUTION (Minimalist Format)
# ==============================================================================
message("\nGenerating Minimalist Skyline Plots...")

# --- PLOT 1: PILLARS CHART (MULTI-VARIABLE) ---
viz_data_multi <- prepared_data %>%
  group_by(Country) %>% mutate(decile = ntile(REAL_INCOME, 10)) %>% group_by(Country, decile) %>%
  summarise(
    `Positive Net Worth`    = weighted.mean(`Positive Net Worth`, w = PERWT, na.rm = TRUE),
    `Liquid Savings (>$5k)` = weighted.mean(`Liquid Savings (>$5k)`, w = PERWT, na.rm = TRUE),
    `Homeowner`             = weighted.mean(`Homeowner`, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Positive Net Worth", "Liquid Savings (>$5k)", "Homeowner"), names_to = "Variable", values_to = "val") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Variable, Country, decile) %>% group_by(Variable) %>% mutate(x_id = row_number()) %>% ungroup()

p_pillars <- ggplot(viz_data_multi, aes(x = x_id, y = val, group = Variable)) +
  geom_line(aes(color = Country), linewidth = 3, alpha = 0.2) + geom_line(aes(color = Country), linewidth = 1.2, linejoin = "round") +
  # Direct label at the end of the line
  geom_text(data = viz_data_multi %>% filter(x_id == 30), aes(label = Variable), hjust = -0.1, size = 4, fontface = "bold", color = "black", family = "serif") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1), breaks = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.35))) + coord_cartesian(clip = "off") + labs(x = NULL, y = NULL)

p_pillars <- apply_standard_theme(p_pillars, r_margin = 160) 

# --- PLOT 2: INDEX CHART (SINGLE-VARIABLE COMPOSITE) ---
viz_data_single <- prepared_data %>%
  group_by(Country) %>% mutate(decile = ntile(REAL_INCOME, 10)) %>% group_by(Country, decile) %>%
  summarise(avg_val = weighted.mean(Wealth_Resilience_Index, w = PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third")), x_id = row_number())

p_index <- ggplot(viz_data_single, aes(x = x_id, y = avg_val, color = Country)) +
  geom_line(aes(group = 1), linewidth = 3, alpha = 0.2) + geom_line(aes(group = 1), linewidth = 1.2, linejoin = "round") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, 1), breaks = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.05))) + labs(x = NULL, y = NULL)

p_index <- apply_standard_theme(p_index, r_margin = 30)

# --- RSTUDIO DIRECT OUTPUTS & SAVING ---
grid.newpage(); grid.draw(table_grob)
print(p_pillars)
print(p_index)

ggsave(here::here("03_output", "visualizations_final", "SCF_Net_Assets_Resilience_Summary_Table_2C.png"), table_grob, width = 12, height = 5, bg = "white", dpi = 300)
ggsave(here::here("03_output", "visualizations_final", "SCF_Net_Assets_Resilience_Pillars_2C.png"), p_pillars, width = 10, height = 7, dpi = 300, bg = "white")
ggsave(here::here("03_output", "visualizations_final", "SCF_Net_Assets_Resilience_Composite_2C.png"), p_index, width = 10, height = 7, dpi = 300, bg = "white")
message("Processing & Visualizations Complete!")