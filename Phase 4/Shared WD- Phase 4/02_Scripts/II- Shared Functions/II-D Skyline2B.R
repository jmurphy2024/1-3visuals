# ==============================================================================
# SCRIPT: II-D Skyline2B.R
# Purpose: Master V2 Visualization Engine (Single & Multi-Variable)
# Features: Dynamic V2 Borders, True Min/Max Y-Axis (Hard 0 Floor), Cleaned Axes, 2B Suffix
# ==============================================================================
library(dplyr); library(ggplot2); library(tidyr); library(scales); library(here)

# --- 1. DEFINITIVE SINGLE-VARIABLE SKYLINE (Standardized V2) ---
plot_economic_skyline <- function(data, indicator_var, weight_var, y_format = "percent", 
                                  y_axis_label = "Value (%)", plot_title = "Economic Skyline",
                                  caption_text = NULL) {
  
  cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
  cutoffs <- if(file.exists(cutoffs_path)) readRDS(cutoffs_path) else list(main_cutoff1=45000, main_cutoff2=115000)
  
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
  
  y_label_type <- if(y_format == "percent") scales::label_percent() else scales::label_dollar()
  
  p <- ggplot(viz_data, aes(x = x_id, y = val)) +
    geom_line(aes(color = Country, group = 1), linewidth = 4, alpha = 0.15) + 
    geom_line(aes(color = Country, group = 1), linewidth = 1.2, linejoin = "round") +
    scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
    
    # THE FIX: Hard floor at 0, exact max value from the data (not the axis), keeps top padding
    scale_y_continuous(
      limits = c(0, NA), 
      breaks = c(0, max(viz_data$val, na.rm = TRUE)), 
      labels = y_label_type, 
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.05))) +
    coord_cartesian(clip = "off") + 
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid      = element_blank(), 
      axis.title.x    = element_blank(),
      axis.title.y    = element_blank(),
      axis.text.x     = element_blank(),
      axis.text.y     = element_text(color = "black", size = 10),
      axis.line.x     = element_line(color = "black", linewidth = 1), 
      axis.line.y     = element_line(color = "black", linewidth = 1),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(t = 20, r = 80, b = 60, l = 10),
      plot.caption    = element_blank()
    ) +
    labs(x = NULL, y = NULL, caption = NULL)
  
  # Automatically append "_2B" to the saved filename
  out_path <- here::here("03_output", "visualizations_final", paste0(gsub(" ", "_", plot_title), "_2B.png"))
  ggsave(out_path, p, width = 10, height = 7, dpi = 300)
  
  return(p)
}

# --- 2. DEFINITIVE MULTI-VARIABLE SKYLINE (Standardized V2) ---
plot_economic_skyline_2 <- function(data, indicator_vars, weight_var, y_format = "percent", 
                                    y_axis_label = "Value (%)", plot_title = "Multi-Variable Skyline",
                                    caption_text = NULL) {
  
  cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
  cutoffs <- if(file.exists(cutoffs_path)) readRDS(cutoffs_path) else list(main_cutoff1=45000, main_cutoff2=115000)
  
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
  
  p <- ggplot(viz_data, aes(x = x_id, y = val, group = Variable)) +
    geom_line(aes(color = Country), linewidth = 4, alpha = 0.15) + 
    geom_line(aes(color = Country), linewidth = 1.2, linejoin = "round") +
    geom_text(data = viz_data %>% filter(x_id == 30), 
              aes(label = Variable), 
              hjust = -0.1, size = 3.5, fontface = "bold", color = "black") +
    scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
    
    # THE FIX: Hard floor at 0, exact max value from the data (not the axis), keeps top padding
    scale_y_continuous(
      limits = c(0, NA), 
      breaks = c(0, max(viz_data$val, na.rm = TRUE)), 
      labels = y_label_type, 
      expand = expansion(mult = c(0, 0.2))
    ) +
    scale_x_continuous(breaks = NULL, expand = expansion(mult = c(0.01, 0.30))) +
    coord_cartesian(clip = "off") + 
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid      = element_blank(), 
      axis.title.x    = element_blank(),
      axis.title.y    = element_blank(),
      axis.text.x     = element_blank(),
      axis.text.y     = element_text(color = "black", size = 15),
      axis.line.x     = element_line(color = "black", linewidth = 1), 
      axis.line.y     = element_line(color = "black", linewidth = 1),
      plot.background = element_rect(fill = "white", color = NA),
      plot.margin     = margin(t = 20, r = 140, b = 60, l = 10),
      plot.caption    = element_blank()
    ) +
    labs(x = NULL, y = NULL, caption = NULL)
  
  # Automatically append "_2B" to the saved filename
  out_path <- here::here("03_output", "visualizations_final", paste0(gsub(" ", "_", plot_title), "_2B.png"))
  ggsave(out_path, p, width = 10, height = 7, dpi = 300)
  
  return(p)
}