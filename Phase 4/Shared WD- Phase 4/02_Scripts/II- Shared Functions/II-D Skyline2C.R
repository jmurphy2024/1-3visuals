# ==============================================================================
# SCRIPT: II-D Skyline 2C.R
# Purpose: Master Visualization Engine for the Minimalist 2C Skyline Format
# Features: Strict min/max y-axis breaks, no gridlines, heavy axis lines,
#           no titles/captions, Serif fonts, and robust ntile logic.
# ==============================================================================
library(dplyr); library(ggplot2); library(tidyr); library(scales); library(here)

# --- 1. MINIMALIST 2C THEME WRAPPER ---
# Abstracted to ensure uniform visual formatting across all charts
apply_standard_theme_2C <- function(p, r_margin = 30) {
  p_updated <- p +
    theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      legend.position   = "none", 
      panel.grid        = element_blank(), 
      axis.text.x       = element_blank(), 
      
      # UPDATED: Added face = "bold" and ensured size is 15
      axis.text.y       = element_text(color = "black", size = 28, family = "serif", face = "bold", margin = margin(r = 5)), 
      
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

# --- 2. SINGLE INDICATOR SKYLINE (Minimalist 2C) ---
plot_economic_skyline_2C <- function(data, indicator_var, weight_var = "PERWT", y_format = "percent") {
  
  # 1. Aggregate Deciles natively
  viz_data_single <- data %>%
    filter(!is.na(Country), !is.na(.data[[weight_var]]), .data[[weight_var]] > 0) %>%
    group_by(Country) %>% 
    mutate(decile = ntile(REAL_INCOME, 10)) %>% 
    group_by(Country, decile) %>%
    summarise(
      avg_val = weighted.mean(.data[[indicator_var]], w = .data[[weight_var]], na.rm = TRUE), 
      .groups = "drop"
    ) %>%
    mutate(
      Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third")), 
      x_id = row_number()
    )
  
  y_label_type <- if(y_format == "percent") scales::label_percent(accuracy = 1) else scales::label_dollar()
  
  # 2. Build minimal structure
  p_index <- ggplot(viz_data_single, aes(x = x_id, y = avg_val, color = Country)) +
    geom_line(aes(group = 1), linewidth = 3, alpha = 0.2) + 
    geom_line(aes(group = 1), linewidth = 1.2, linejoin = "round") +
    scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
    scale_y_continuous(
      labels = y_label_type, 
      limits = c(0, NA), 
      breaks = function(x) c(0, max(x, na.rm = TRUE)), # Strict min/max calculation
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.05))) + 
    labs(x = NULL, y = NULL)
  
  # 3. Apply Theme
  p_index <- apply_standard_theme_2C(p_index, r_margin = 30)
  
  return(p_index)
}

# --- 3. MULTI-INDICATOR PILLARS SKYLINE (Minimalist 2C) ---
plot_economic_skyline_multi_2C <- function(data, indicator_vars, weight_var = "PERWT", y_format = "percent") {
  
  # 1. Aggregate Deciles natively
  viz_data_multi <- data %>%
    filter(!is.na(Country), !is.na(.data[[weight_var]]), .data[[weight_var]] > 0) %>%
    group_by(Country) %>% 
    mutate(decile = ntile(REAL_INCOME, 10)) %>% 
    group_by(Country, decile) %>%
    summarise(
      across(all_of(indicator_vars), ~ weighted.mean(.x, w = .data[[weight_var]], na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    pivot_longer(cols = all_of(indicator_vars), names_to = "Variable", values_to = "val") %>%
    mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
    arrange(Variable, Country, decile) %>% 
    group_by(Variable) %>% 
    mutate(x_id = row_number()) %>% 
    ungroup()
  
  y_label_type <- if(y_format == "percent") scales::label_percent(accuracy = 1) else scales::label_dollar()
  
  # 2. Build minimal structure
  p_pillars <- ggplot(viz_data_multi, aes(x = x_id, y = val, group = Variable)) +
    geom_line(aes(color = Country), linewidth = 3, alpha = 0.2) + 
    geom_line(aes(color = Country), linewidth = 1.2, linejoin = "round") +
    geom_text(
      data = viz_data_multi %>% filter(x_id == 30), 
      aes(label = Variable), 
      hjust = -0.1, size = 4, fontface = "bold", color = "black", family = "serif"
    ) +
    scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
    scale_y_continuous(
      labels = y_label_type, 
      limits = c(0, NA), 
      breaks = function(x) c(0, max(x, na.rm = TRUE)), # Strict min/max calculation
      expand = expansion(mult = c(0, 0.05))
    ) +
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.35))) + 
    coord_cartesian(clip = "off") + 
    labs(x = NULL, y = NULL)
  
  # 3. Apply Theme (wider right margin to accommodate end-of-line labels)
  p_pillars <- apply_standard_theme_2C(p_pillars, r_margin = 160)
  
  return(p_pillars)
}