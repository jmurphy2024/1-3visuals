# ==============================================================================
# SCRIPT: II-D Income Normalization and Design
# Purpose: Shared Normalization, Temporal Adjustment, & Connected Visual Standards
# Updates: Fully dynamic to V2 Aggregated Cutoffs (No hardcoded 45k/115k)
# Colors: Bottom (#9B2226), Middle (#E9C46A), Top (#386641)
# ==============================================================================
library(dplyr); library(ggplot2); library(tidyr); library(here); library(tibble); library(scales)

# --- 1. CPI INFLATION ADJUSTMENT (TEMPORAL) ---
cpi_lookup <- tibble::tribble(
  ~YEAR, ~CPI_U,
  2000,  172.200,  
  2005,  195.300,  
  2010,  218.056,  
  2015,  237.017,  
  2018,  251.107,
  2019,  255.657,
  2020,  258.811,
  2021,  270.970,
  2022,  292.655,
  2023,  304.702,  
  2024,  313.359   
)

get_inflation_multiplier <- function(data_year, base_year = 2023) {
  base_cpi <- cpi_lookup$CPI_U[cpi_lookup$YEAR == base_year]
  data_cpi <- cpi_lookup$CPI_U[cpi_lookup$YEAR == data_year]
  
  if (length(base_cpi) == 0 | length(data_cpi) == 0) stop("Year not found in CPI lookup table.")
  return(base_cpi / data_cpi)
}

# --- 2. INCOME NORMALIZATION FUNCTION (SPATIAL + TEMPORAL) ---
apply_three_countries_logic <- function(data, income_col, state_col, adj_val = 1.0) {
  rpp_lookup <- readRDS(here::here("01_data", "processed", "state_rpp_lookup.rds"))
  
  # DYNAMIC: Load the V2 Cutoffs
  cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
  if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found.")
  cutoffs <- readRDS(cutoffs_path)
  
  data %>%
    mutate(
      income_raw  = as.numeric(get(income_col)),
      rpp_val     = rpp_lookup$STATE_RPP[match(as.numeric(get(state_col)), rpp_lookup$STATEFIP)],
      
      REAL_INCOME = (income_raw * adj_val) * (100 / coalesce(rpp_val, 100)),
      
      # DYNAMIC: Replaced hardcoded 45k/115k with dynamic variables
      Country = case_when(
        REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
        REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
        TRUE ~ "Top Third"
      )
    ) %>%
    filter(!is.na(Country), !is.na(REAL_INCOME))
}

# --- 3. THE DEFINITIVE ECONOMIC SKYLINE VISUAL ---
plot_economic_skyline <- function(data, indicator_var, weight_var, y_format = "percent", 
                                  y_axis_label = "Value (%)", plot_title = "Economic Skyline") {
  
  # DYNAMIC: Load V2 Cutoffs for Axis Formatting
  cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
  cutoffs <- if(file.exists(cutoffs_path)) readRDS(cutoffs_path) else list(main_cutoff1=45000, main_cutoff2=115000)
  
  # A. Data Summarization (10 Deciles per Country)
  viz_data <- data %>%
    group_by(Country) %>%
    mutate(decile = ntile(REAL_INCOME, 10)) %>% 
    group_by(Country, decile) %>%
    summarise(
      val = weighted.mean(get(indicator_var), w = get(weight_var), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
    arrange(Country, decile) %>%
    mutate(x_id = row_number())
  
  # B. Formatting Elements (Adjusted for 30-Point Scale)
  y_label_type <- if(y_format == "percent") scales::label_percent() else scales::label_dollar()
  
  # DYNAMIC: X-Axis labels automatically round to the nearest hundred dollars of your true cutoffs
  label_t1 <- scales::dollar(cutoffs$main_cutoff1, accuracy = 100)
  label_t2 <- scales::dollar(cutoffs$main_cutoff2, accuracy = 100)
  
  income_breaks <- c(1, 5, 10, 15, 20, 25, 30) 
  income_labels <- c("$0", "$20,000", label_t1, "$75,000", label_t2, "$250,000", "$500,000+")
  label_colors <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")
  
  # C. Create Continuous Visualization with Color-Matched Glow
  p <- ggplot(viz_data, aes(x = x_id, y = val)) +
    geom_line(aes(color = Country, group = 1), linewidth = 4, alpha = 0.15) + 
    geom_line(aes(color = Country, group = 1), linewidth = 1, linejoin = "round", lineend = "round") +
    scale_color_manual(values = c(
      "Bottom Third" = "#9B2226", 
      "Middle Third" = "#E9C46A", 
      "Top Third"    = "#386641"
    )) +
    
    # D. Clinical Axis and Theme Design
    scale_y_continuous(labels = y_label_type, expand = c(0.05, 0.05)) +
    scale_x_continuous(breaks = income_breaks, labels = income_labels, expand = c(0.01, 0.01)) +
    coord_cartesian(clip = "off") + 
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
    labs(x = "Household Income (Real Adjusted Dollars)", y = y_axis_label)
  
  # E. Auto-Save
  out_path <- here::here("03_output", "visualizations_final", paste0(gsub(" ", "_", plot_title), ".png"))
  ggsave(out_path, p, width = 10, height = 6, dpi = 300)
  
  return(p)
}

# --- DEFINITIVE MULTI-VARIABLE SKYLINE (Standardized V2) ---
plot_economic_skyline_2 <- function(data, indicator_vars, weight_var, y_format = "percent", 
                                    y_axis_label = "Value (%)", plot_title = "Multi-Variable Skyline") {
  
  # DYNAMIC: Load V2 Cutoffs for Axis Formatting
  cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
  cutoffs <- if(file.exists(cutoffs_path)) readRDS(cutoffs_path) else list(main_cutoff1=45000, main_cutoff2=115000)
  
  # A. Data Summarization (10 Deciles per Country per Variable)
  viz_data <- data %>%
    group_by(Country) %>%
    mutate(decile = ntile(REAL_INCOME, 10)) %>% 
    group_by(Country, decile) %>%
    summarise(
      across(all_of(indicator_vars), ~ weighted.mean(.x, w = get(weight_var), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = all_of(indicator_vars), names_to = "Variable", values_to = "val") %>%
    mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
    arrange(Variable, Country, decile) %>%
    group_by(Variable) %>%
    mutate(x_id = row_number()) %>%
    ungroup()
  
  # B. Formatting Elements (Identical to Definitive 1-Var Script)
  y_label_type <- if(y_format == "percent") scales::label_percent() else scales::label_dollar()
  label_t1 <- scales::dollar(cutoffs$main_cutoff1, accuracy = 100)
  label_t2 <- scales::dollar(cutoffs$main_cutoff2, accuracy = 100)
  
  income_breaks <- c(1, 5, 10, 15, 20, 25, 30) 
  income_labels <- c("$0", "$20,000", label_t1, "$75,000", label_t2, "$250,000", "$500,000+")
  label_colors <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")
  
  # C. Create Continuous Visualization with Color-Matched Glow
  p <- ggplot(viz_data, aes(x = x_id, y = val, group = Variable)) +
    # The Glow Layer: group = Variable ensures the line is continuous
    # color = Country ensures the segments change color at the cutoffs
    geom_line(aes(color = Country), linewidth = 4, alpha = 0.15) + 
    geom_line(aes(color = Country), linewidth = 1, linejoin = "round", lineend = "round") +
    
    # D. NCVS Style Direct Labels (Right-aligned)
    geom_text(data = viz_data %>% filter(x_id == 30), 
              aes(label = Variable), 
              hjust = -0.1, size = 3.5, fontface = "bold", color = "#386641") +
    
    scale_color_manual(values = c(
      "Bottom Third" = "#9B2226", 
      "Middle Third" = "#E9C46A", 
      "Top Third"    = "#386641"
    )) +
    
    # E. Clinical Axis and Theme Design
    scale_y_continuous(labels = y_label_type, expand = expansion(mult = c(0.05, 0.2))) +
    scale_x_continuous(breaks = income_breaks, labels = income_labels, expand = c(0.01, 0.01)) +
    coord_cartesian(clip = "off") + 
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
    labs(x = "Household Income (Real Adjusted Dollars)", y = y_axis_label)
  
  # F. Auto-Save
  out_path <- here::here("03_output", "visualizations_final", paste0(gsub(" ", "_", plot_title), ".png"))
  ggsave(out_path, p, width = 10, height = 6, dpi = 300)
  
  return(p)
}