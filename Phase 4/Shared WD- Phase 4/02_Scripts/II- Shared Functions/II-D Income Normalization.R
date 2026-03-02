# ==============================================================================
# SCRIPT: II-D Income Normalization and Design
# Purpose: Shared Normalization & Connected Visual Standards
# Colors: Bottom (#9B2226), Middle (#E9C46A), Top (#386641)
# ==============================================================================
library(dplyr); library(ggplot2); library(tidyr); library(here); library(tibble); library(scales)

# --- 1. INCOME NORMALIZATION FUNCTION ---
apply_three_countries_logic <- function(data, income_col, state_col, adj_val = 1.0) {
  rpp_lookup <- readRDS(here::here("01_data", "processed", "state_rpp_lookup.rds")) #
  
  data %>%
    mutate(
      income_raw  = as.numeric(get(income_col)),
      rpp_val     = rpp_lookup$STATE_RPP[match(as.numeric(get(state_col)), rpp_lookup$STATEFIP)],
      # Math remains untouched
      REAL_INCOME = (income_raw * adj_val) * (100 / coalesce(rpp_val, 100)),
      
      Country = case_when(
        REAL_INCOME <= 45000 ~ "Bottom Third",
        REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
        TRUE ~ "Top Third"
      )
    ) %>%
    filter(!is.na(Country), !is.na(REAL_INCOME)) #
}

# --- THE DEFINITIVE ECONOMIC SKYLINE VISUAL ---
plot_economic_skyline <- function(data, indicator_var, weight_var, y_format = "percent", 
                                  y_axis_label = "Value (%)", plot_title = "Economic Skyline") {
  
  # A. Data Summarization (20 Ventiles per Country)
  viz_data <- data %>%
    group_by(Country) %>%
    mutate(ventile = ntile(REAL_INCOME, 20)) %>% 
    group_by(Country, ventile) %>%
    summarise(
      val = weighted.mean(get(indicator_var), w = get(weight_var), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
    arrange(Country, ventile) %>%
    mutate(x_id = row_number())
  
  # B. Formatting Elements
  y_label_type <- if(y_format == "percent") scales::label_percent() else scales::label_dollar()
  income_breaks <- c(1, 10, 20, 30, 40, 50, 60)
  income_labels <- c("$0", "$20,000", "$45,000", "$75,000", "$115,000", "$250,000", "$500,000+")
  
  # Transition highlight colors
  label_colors <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")
  
  # C. Create Continuous Visualization with Color-Matched Glow
  p <- ggplot(viz_data, aes(x = x_id, y = val)) +
    # 1. The Glow - Updated to match the Country color
    geom_line(aes(color = Country, group = 1), linewidth = 4, alpha = 0.15) + 
    
    # 2. The Main Skyline - Group = 1 ensures the line is connected
    geom_line(aes(color = Country, group = 1), linewidth = 1, linejoin = "round", lineend = "round") +
    
    # Non-Negotiable Color Palette
    scale_color_manual(values = c(
      "Bottom Third" = "#9B2226", 
      "Middle Third" = "#E9C46A", 
      "Top Third"    = "#386641"
    )) +
    
    # D. Clinical Axis and Theme Design
    scale_y_continuous(labels = y_label_type, expand = c(0.05, 0.05)) +
    scale_x_continuous(breaks = income_breaks, labels = income_labels, expand = c(0.01, 0.01)) +
    theme_minimal() +
    theme(
      legend.position = "none",
      panel.grid      = element_blank(), # No gridlines
      axis.title.x    = element_text(face = "bold", size = 14, margin = margin(t = 15)),
      axis.title.y    = element_text(face = "bold", size = 14, margin = margin(r = 15)),
      axis.text.x     = element_text(color = label_colors, face = "bold", size = 10),
      axis.text.y     = element_text(color = "black", size = 10),
      axis.line.x     = element_line(color = "black", linewidth = 1.5), # Bold frame
      axis.line.y     = element_line(color = "black", linewidth = 1.5),
      plot.background = element_rect(fill = "white", color = NA)
    ) +
    labs(x = "Household Income (Real Adjusted Dollars)", y = y_axis_label)
  
  # E. Auto-Save
  out_path <- here::here("03_output", "visualizations_final", paste0(gsub(" ", "_", plot_title), ".png"))
  ggsave(out_path, p, width = 10, height = 6, dpi = 300)
  
  return(p)
}