# ==============================================================================
# SCRIPT: II-D Income Normalization 2.R
# Purpose: Master V2 Visualization Engine (Single & Multi-Variable)
# Features: Dynamic V2 Borders, Black Labels, Wide Margins, & Native Captions
# ==============================================================================
library(dplyr); library(ggplot2); library(tidyr); library(scales); library(here)

# --- 1. DEFINITIVE SINGLE-VARIABLE SKYLINE (Standardized V2) ---
# Use this for: Life Satisfaction, Equity Index, Single Crime Rates
plot_economic_skyline <- function(data, indicator_var, weight_var, y_format = "percent", 
                                  y_axis_label = "Value (%)", plot_title = "Economic Skyline",
                                  caption_text = NULL) {
  
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
  
  # B. Formatting Elements
  y_label_type <- if(y_format == "percent") scales::label_percent() else scales::label_dollar()
  label_t1 <- scales::dollar(cutoffs$main_cutoff1, accuracy = 100)
  label_t2 <- scales::dollar(cutoffs$main_cutoff2, accuracy = 100)
  
  income_breaks <- c(1, 5, 10, 15, 20, 25, 30) 
  income_labels <- c("$0", "$20,000", label_t1, "$75,000", label_t2, "$250,000", "$500,000+")
  label_colors <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")
  
  # C. Create Visualization
  p <- ggplot(viz_data, aes(x = x_id, y = val)) +
    geom_line(aes(color = Country, group = 1), linewidth = 4, alpha = 0.15) + 
    geom_line(aes(color = Country, group = 1), linewidth = 1.2, linejoin = "round") +
    
    scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
    
    # D. Clinical Axis and Theme Design
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
      plot.background = element_rect(fill = "white", color = NA),
      
      # Standardized Margins for Caption Support
      plot.margin     = margin(t = 20, r = 80, b = 60, l = 10),
      plot.caption    = element_text(hjust = 0, size = 9, color = "grey30", margin = margin(t = 20), lineheight = 1.1)
    ) +
    labs(x = "Household Income (Real Adjusted Dollars)", y = y_axis_label, caption = caption_text)
  
  # E. Auto-Save
  out_path <- here::here("03_output", "visualizations_final", paste0(gsub(" ", "_", plot_title), ".png"))
  ggsave(out_path, p, width = 10, height = 7, dpi = 300)
  
  return(p)
}

# --- 2. DEFINITIVE MULTI-VARIABLE SKYLINE (Standardized V2) ---
# Use this for: Health Ins (Public/Priv), Crime (Violent/Property), Equity Pillars
plot_economic_skyline_2 <- function(data, indicator_vars, weight_var, y_format = "percent", 
                                    y_axis_label = "Value (%)", plot_title = "Multi-Variable Skyline",
                                    caption_text = NULL) {
  
  cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
  cutoffs <- if(file.exists(cutoffs_path)) readRDS(cutoffs_path) else list(main_cutoff1=45000, main_cutoff2=115000)
  
  # A. Data Summarization (Long Format)
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
  
  y_label_type <- if(y_format == "percent") scales::label_percent() else scales::label_dollar()
  label_t1 <- scales::dollar(cutoffs$main_cutoff1, accuracy = 100)
  label_t2 <- scales::dollar(cutoffs$main_cutoff2, accuracy = 100)
  
  income_breaks <- c(1, 5, 10, 15, 20, 25, 30) 
  income_labels <- c("$0", "$20,000", label_t1, "$75,000", label_t2, "$250,000", "$500,000+")
  label_colors <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")
  
  # B. Create Visualization
  p <- ggplot(viz_data, aes(x = x_id, y = val, group = Variable)) +
    geom_line(aes(color = Country), linewidth = 4, alpha = 0.15) + 
    geom_line(aes(color = Country), linewidth = 1.2, linejoin = "round") +
    
    # Standardized Black Labels on Right Edge
    geom_text(data = viz_data %>% filter(x_id == 30), 
              aes(label = Variable), 
              hjust = -0.1, size = 3.5, fontface = "bold", color = "black") +
    
    scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
    
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
      plot.background = element_rect(fill = "white", color = NA),
      
      # Standardized Margins for Caption Support
      plot.margin     = margin(t = 20, r = 80, b = 60, l = 10),
      plot.caption    = element_text(hjust = 0, size = 9, color = "grey30", margin = margin(t = 20), lineheight = 1.1)
    ) +
    labs(x = "Household Income (Real Adjusted Dollars)", y = y_axis_label, caption = caption_text)
  
  # C. Auto-Save
  out_path <- here::here("03_output", "visualizations_final", paste0(gsub(" ", "_", plot_title), ".png"))
  ggsave(out_path, p, width = 10, height = 7, dpi = 300)
  
  return(p)
}