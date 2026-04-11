# ==============================================================================
# SCRIPT: GSS_rehabilitative_justice_2C.R
# Purpose: Generate 3-Country Skyline for Rehabilitative & Restorative Justice
# Definition: Maps the structural belief in criminal rehabilitation, treatment, 
#             and systemic proportionality over punitive/lethal measures.
# Visual: Minimalist Skyline 2C (No grids, strict min/max y-axis, robust ntile)
# ==============================================================================
rm(list = ls()); gc()
options(expressions = 500000)

library(haven); library(dplyr); library(here); library(tidyr); library(ggplot2); library(stringr)
library(gridExtra); library(grid); library(scales); library(Hmisc)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))

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
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found.")
cutoffs <- readRDS(cutoffs_path)
set.seed(123)

# 2. DATA ACQUISITION & PIPELINE
# ------------------------------------------------------------------------------
TARGET_FILE <- here::here("01_data", "raw", "GSS2024.sav")
if(!file.exists(TARGET_FILE)) stop("GSS .sav file not found.")
raw_data <- read_sav(TARGET_FILE)

INFLATION_ADJ <- get_inflation_multiplier(data_year = 2024, base_year = 2023)

message("Applying normalization and dynamic cutoffs...")
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  mutate(
    PERWT = as.numeric(WTSSNRPS),
    MAPPED_REGION = case_when(
      as.numeric(REGION) %in% c(1, 2) ~ 1,
      as.numeric(REGION) %in% c(3, 4) ~ 2,
      as.numeric(REGION) %in% c(5, 6, 7) ~ 3,
      as.numeric(REGION) %in% c(8, 9) ~ 4,
      TRUE ~ NA_real_
    ),
    
    # PILLAR LOGIC
    `Systemic Proportionality`   = if_else(!is.na(COURTS) & COURTS != 2, 1, 0, missing = 0), 
    `Restorative Sentencing`     = if_else(!is.na(CAPPUN) & CAPPUN == 2, 1, 0, missing = 0),
    `Treatment Support`          = if_else(!is.na(NATDRUG) & NATDRUG <= 2, 1, 0, missing = 0),
    Rehabilitative_Justice_Index = if_else(`Systemic Proportionality` == 1 & `Restorative Sentencing` == 1 & `Treatment Support` == 1, 1, 0),
    
    income_num = as.numeric(INCOME16)
  ) %>%
  filter(!is.na(income_num) & income_num > 0) %>%
  mutate(
    raw_dollars = case_when(
      income_num <= 10 ~ runif(n(), 0, 19999),      
      income_num <= 17 ~ runif(n(), 20000, 49999),   
      income_num <= 21 ~ runif(n(), 50000, 89999),   
      income_num <= 25 ~ runif(n(), 90000, 169999),  
      income_num == 26 ~ runif(n(), 170000, 500000), 
      TRUE             ~ runif(n(), 50000, 74999) 
    ),
    REAL_INCOME = raw_dollars * INFLATION_ADJ * get_regional_rpp_multiplier(MAPPED_REGION),
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(PERWT), PERWT > 0, !is.na(REAL_INCOME), !is.na(Country))

# 3. SUMMARY STATISTICS
# ------------------------------------------------------------------------------
message("\n=== REHABILITATIVE JUSTICE SUMMARY ===")
overall_tercile <- prepared_data %>%
  group_by(Country) %>%
  summarise(
    Subgroup = "Overall Tercile", Total_Population = sum(PERWT, na.rm = TRUE),
    `Systemic Proportionality (%)` = round((sum(`Systemic Proportionality` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Restorative Sentencing (%)`   = round((sum(`Restorative Sentencing` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Treatment Support (%)`        = round((sum(`Treatment Support` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Rehabilitative Index (%)`     = round((sum(Rehabilitative_Justice_Index * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  )

prepared_data_q <- prepared_data %>%
  group_by(Country) %>%
  mutate(
    q25 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.25, na.rm = TRUE),
    q50 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.50, na.rm = TRUE),
    q75 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.75, na.rm = TRUE),
    Quartile = case_when(REAL_INCOME <= q25 ~ "Q1", REAL_INCOME <= q50 ~ "Q2", REAL_INCOME <= q75 ~ "Q3", TRUE ~ "Q4")
  ) %>% ungroup()

quartile_stats <- prepared_data_q %>%
  group_by(Country, Quartile) %>%
  summarise(
    Subgroup = first(Quartile), Total_Population = sum(PERWT, na.rm = TRUE),
    `Systemic Proportionality (%)` = round((sum(`Systemic Proportionality` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Restorative Sentencing (%)`   = round((sum(`Restorative Sentencing` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Treatment Support (%)`        = round((sum(`Treatment Support` * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    `Rehabilitative Index (%)`     = round((sum(Rehabilitative_Justice_Index * PERWT, na.rm = TRUE) / Total_Population) * 100, 1),
    .groups = "drop"
  ) %>% select(-Quartile)

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  mutate(Total_Population = scales::comma(Total_Population))

print(as.data.frame(macro_summary))
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)
table_grob <- tableGrob(macro_summary, rows = NULL, theme = ttheme_default(core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)), colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))))

# 4. VISUALIZATION
# ------------------------------------------------------------------------------
message("\nGenerating Minimalist Skyline Plots...")

# Pillars Chart
viz_data_multi <- prepared_data %>%
  group_by(Country) %>% mutate(decile = ntile(REAL_INCOME, 10)) %>% group_by(Country, decile) %>%
  summarise(
    `Systemic Proportionality` = weighted.mean(`Systemic Proportionality`, w = PERWT, na.rm = TRUE),
    `Restorative Sentencing`   = weighted.mean(`Restorative Sentencing`, w = PERWT, na.rm = TRUE),
    `Treatment Support`        = weighted.mean(`Treatment Support`, w = PERWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Systemic Proportionality", "Restorative Sentencing", "Treatment Support"), names_to = "Variable", values_to = "val") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Variable, Country, decile) %>% group_by(Variable) %>% mutate(x_id = row_number()) %>% ungroup()

p_pillars <- ggplot(viz_data_multi, aes(x = x_id, y = val, group = Variable)) +
  geom_line(aes(color = Country), linewidth = 3, alpha = 0.2) + geom_line(aes(color = Country), linewidth = 1.2, linejoin = "round") +
  geom_text(data = viz_data_multi %>% filter(x_id == 30), aes(label = Variable), hjust = -0.1, size = 4, fontface = "bold", color = "black", family = "serif") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, NA), breaks = function(x) c(0, max(x, na.rm = TRUE)), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.35))) + coord_cartesian(clip = "off") + labs(x = NULL, y = NULL)
p_pillars <- apply_standard_theme(p_pillars, r_margin = 160) 

# Index Chart
viz_data_single <- prepared_data %>%
  group_by(Country) %>% mutate(decile = ntile(REAL_INCOME, 10)) %>% group_by(Country, decile) %>%
  summarise(avg_val = weighted.mean(Rehabilitative_Justice_Index, w = PERWT, na.rm = TRUE), .groups = "drop") %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third")), x_id = row_number())

p_index <- ggplot(viz_data_single, aes(x = x_id, y = avg_val, color = Country)) +
  geom_line(aes(group = 1), linewidth = 3, alpha = 0.2) + geom_line(aes(group = 1), linewidth = 1.2, linejoin = "round") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1), limits = c(0, NA), breaks = function(x) c(0, max(x, na.rm = TRUE)), expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.05))) + labs(x = NULL, y = NULL)
p_index <- apply_standard_theme(p_index, r_margin = 30)

# Render & Save
grid.newpage(); grid.draw(table_grob)
print(p_pillars)
print(p_index)

ggsave(here::here("03_output", "visualizations_final", "GSS_rehabilitative_justice_Summary_Table_2C.png"), table_grob, width = 10, height = 5, bg = "white", dpi = 300)
ggsave(here::here("03_output", "visualizations_final", "GSS_rehabilitative_justice_pillars_2C.png"), p_pillars, width = 10, height = 7, dpi = 300, bg = "white")
ggsave(here::here("03_output", "visualizations_final", "GSS_rehabilitative_justice_composite_2C.png"), p_index, width = 10, height = 7, dpi = 300, bg = "white")
message("Processing & Visualizations Complete!")