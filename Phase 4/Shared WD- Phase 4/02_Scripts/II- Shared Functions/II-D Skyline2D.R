# ==============================================================================
# SCRIPT: II-D Skyline2D.R
# Purpose: Master Visualization Engine for the Three Countries Model
# Aesthetic: Minimalist 2D (Solid Axes, Min/Max % Y-Axis, No grids)
# Logic: True Smooth Trend Lines (Additive Glow & High-Smoothing LOESS)
# ==============================================================================

library(ggplot2)
library(dplyr)
library(rlang)
library(scales) # Added for percentage formatting

# ------------------------------------------------------------------------------
# FUNCTION 1: Single Indicator Trend Line 
# ------------------------------------------------------------------------------
plot_economic_skyline_2C <- function(data, indicator_var, weight_var) {
  
  # 1. Aggregate to 100 percentiles
  trend_data <- data %>%
    mutate(percentile = ntile(REAL_INCOME, 100)) %>%
    group_by(percentile) %>%
    summarise(
      x_val = mean(REAL_INCOME, na.rm = TRUE),
      y_actual = weighted.mean(!!sym(indicator_var), w = !!sym(weight_var), na.rm = TRUE),
      Country = first(Country), 
      .groups = "drop"
    ) %>%
    arrange(percentile)
  
  # 2. FIT THE MATH ON THE PERCENTILE RANK
  loess_model <- loess(y_actual ~ percentile, data = trend_data, span = 0.75)
  trend_data$y_trend <- predict(loess_model)
  
  # Calculate dynamic min/max breaks rounded to nearest 5% (0.05)
  min_val <- min(trend_data$y_trend, na.rm = TRUE)
  max_val <- max(trend_data$y_trend, na.rm = TRUE)
  custom_breaks <- unique(c(
    round(min_val * 20) / 20, 
    round(max_val * 20) / 20
  ))
  
  # 3. Apply Minimalist Design 
  p <- ggplot(trend_data, aes(x = x_val, y = y_trend, color = Country, group = 1)) +
    
    # Layer 1: Additive Glow 
    geom_line(linewidth = 2, alpha = 0.2, lineend = "round") +
    
    # Layer 2: Main Solid Trend Line
    geom_line(linewidth = 1.2, lineend = "round") +
    
    # Minimalist 2C Aesthetics & Colors
    scale_color_manual(values = c("Bottom Third" = "#9B2226", 
                                  "Middle Third" = "#E9C46A", 
                                  "Top Third"  = "#386641")) +
    
    # FORMAT Y-AXIS AS MIN/MAX PERCENTAGES
    scale_y_continuous(
      breaks = custom_breaks,
      labels = scales::label_percent(accuracy = 1)
    ) +
    
    theme_minimal() +
    theme(
      panel.grid      = element_blank(),
      axis.line       = element_line(color = "black", linewidth = 1.5),
      axis.ticks      = element_blank(), 
      axis.title      = element_blank(),
      axis.text.x     = element_blank(),
      axis.text.y     = element_text(color = "black", size = 28, family = "serif", face = "bold", margin = margin(r = 5)),
      plot.title      = element_blank(),
      plot.caption    = element_blank(),
      legend.position = "none"
    )
  
  return(p)
}

# ------------------------------------------------------------------------------
# FUNCTION 2: Multi-Indicator Trend Line (Pillars)
# ------------------------------------------------------------------------------
plot_economic_skyline_multi_2C <- function(data, indicator_vars, weight_var) {
  
  # 1. Base Aggregation into Percentiles
  base_data <- data %>%
    mutate(percentile = ntile(REAL_INCOME, 100))
  
  # 2. Iterate through each indicator to calculate high-smoothing LOESS trends
  trend_list <- lapply(indicator_vars, function(ind_var) {
    
    temp_agg <- base_data %>%
      group_by(percentile) %>%
      summarise(
        x_val = mean(REAL_INCOME, na.rm = TRUE),
        y_actual = weighted.mean(!!sym(ind_var), w = !!sym(weight_var), na.rm = TRUE),
        Country = first(Country),
        .groups = "drop"
      ) %>%
      arrange(percentile)
    
    # Fit the high-smoothing math for each indicator
    model <- loess(y_actual ~ percentile, data = temp_agg, span = 0.75)
    temp_agg$y_trend <- predict(model)
    temp_agg$Indicator <- ind_var
    
    return(temp_agg)
  })
  
  final_trend_data <- bind_rows(trend_list)
  
  # Calculate dynamic min/max breaks rounded to nearest 5% (0.05) across all lines
  min_val <- min(final_trend_data$y_trend, na.rm = TRUE)
  max_val <- max(final_trend_data$y_trend, na.rm = TRUE)
  custom_breaks <- unique(c(
    round(min_val * 20) / 20, 
    round(max_val * 20) / 20
  ))
  
  # 3. Apply the Minimalist 2D Design 
  p <- ggplot(final_trend_data, aes(x = x_val, y = y_trend, color = Country, group = Indicator)) +
    
    # Layer 1: Additive Glow
    geom_line(linewidth = 2, alpha = 0.2, lineend = "round") +
    
    # Layer 2: Main Solid Trend Line
    geom_line(linewidth = 1.2, lineend = "round") +
    
    # Minimalist 2C Aesthetics & Colors
    scale_color_manual(values = c("Bottom Third" = "#9B2226", 
                                  "Middle Third" = "#E9C46A", 
                                  "Top Third"  = "#386641")) +
    
    # FORMAT Y-AXIS AS MIN/MAX PERCENTAGES
    scale_y_continuous(
      breaks = custom_breaks,
      labels = scales::label_percent(accuracy = 1)
    ) +
    
    theme_minimal() +
    theme(
      panel.grid      = element_blank(),
      axis.line       = element_line(color = "black", linewidth = 1.5),
      axis.ticks      = element_blank(), 
      axis.title      = element_blank(),
      axis.text.x     = element_blank(),
      axis.text.y     = element_text(color = "black", size = 28, family = "serif", face = "bold", margin = margin(r = 5)),
      plot.title      = element_blank(),
      plot.caption    = element_blank(),
      legend.position = "none"
    )
  
  return(p)
}