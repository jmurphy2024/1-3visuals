# ==============================================================================
# SCRIPT: II-B Shared Visuals.R
# LOCATION: 02_Scripts/II- Shared Functions/
# PURPOSE:  Standardized visualization functions for the 1/3 Country Project.
# UPDATES:  Synced to Master Colors and 30-Point (10 Decile) X-Axis Standard.
# ==============================================================================

# ==== 1. LOAD REQUIRED LIBRARIES ====
library(ggplot2); library(dplyr); library(readr); library(purrr); library(stringr)
library(here); library(ggtext); library(glue); library(grid); library(gridExtra)
library(scales); library(ggnewscale); library(cowplot); library(tidyr)

# ==== 2. MAIN VISUALIZATION FUNCTION ====
create_bar_dot_line_plot <- function(
    summary_data, 
    y_var, 
    plot_title, 
    y_axis_label, 
    output_filename, 
    # MASTER PROJECT COLORS
    t1_color="#9B2226", 
    t2_color="#E9C46A", 
    t3_color="#386641",
    y_label_format = scales::dollar_format() 
) {
  
  # --- Validation ---
  if (!y_var %in% names(summary_data)) stop(paste("Error: Variable", y_var, "not found."))
  
  # --- Data Prep ---
  # Ensure strict ordering of groups 1-30 (10 deciles x 3 countries)
  plot_data <- summary_data %>%
    dplyr::mutate(
      income_tercile = factor(income_tercile, levels = c("Tercile 1 (Bottom)", "Tercile 2 (Middle)", "Tercile 3 (Top)")),
      decile = as.numeric(decile)
    ) %>%
    dplyr::arrange(income_tercile, decile) %>%
    # Create continuous X-axis 1 to 30
    dplyr::mutate(x_id = dplyr::row_number())
  
  # --- Design Constants ---
  # Shading Boundaries for 30-point scale (Divider after point 10 and 20)
  x_bound_1 <- 10.5
  x_bound_2 <- 20.5
  
  # --- Plot Construction ---
  p <- ggplot(plot_data, aes(x = x_id, y = .data[[y_var]])) +
    
    # 1. Background Shading (Vertical Bands for T1 & T3)
    annotate("rect", xmin = -Inf, xmax = x_bound_1, ymin = -Inf, ymax = Inf, fill = t1_color, alpha = 0.1) +
    annotate("rect", xmin = x_bound_2, xmax = Inf, ymin = -Inf, ymax = Inf, fill = t3_color, alpha = 0.1) +
    
    # 2. Vertical Dividers (Dashed Lines)
    geom_vline(xintercept = x_bound_1, linetype = "dashed", color = t2_color, alpha = 0.5, linewidth = 1) +
    geom_vline(xintercept = x_bound_2, linetype = "dashed", color = t3_color, alpha = 0.5, linewidth = 1) +
    
    # 3. Bars
    geom_col(aes(fill = income_tercile, color = income_tercile), alpha = 0.7, width = 0.85) +
    
    # 4. Trend Line (Connecting tops of bars)
    geom_line(group = 1, color = "black", linewidth = 0.8) +
    
    # 5. Points (Dots on top)
    geom_point(aes(color = income_tercile), size = 2.5) +
    
    # 6. Scales & Formatting
    scale_fill_manual(values = c(t1_color, t2_color, t3_color), name = "Economic Country") +
    scale_color_manual(values = c(t1_color, t2_color, t3_color), name = "Economic Country") +
    
    scale_y_continuous(labels = y_label_format, expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(breaks = NULL) + 
    
    labs(
      title = plot_title,
      x = "Population Deciles (10 per Country)",
      y = y_axis_label,
      caption = "Source: ACS 5-Year Estimates (Person-Weighted, Aggregated Income)"
    ) +
    
    # 7. Theme Standard
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5, margin = margin(b = 10)),
      axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
      axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 11, face = "bold")
    )
  
  # --- Save Output ---
  out_dir <- here::here("03_output", "visualizations_final")
  if(!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  
  ggsave(file.path(out_dir, output_filename), p, width = 12, height = 7, bg = "white", dpi = 300)
  message(paste("Saved plot to:", output_filename))
}

# ==== 3. SUMMARY TABLE FUNCTION ====
print_tercile_stats <- function(data_grouped, val_var, w_var = "PERWT", format_func = scales::dollar) {
  
  message("\n==========================================")
  message(paste("      SUMMARY STATISTICS:", val_var))
  message("==========================================\n")
  
  if (!val_var %in% names(data_grouped)) stop(paste("Error:", val_var, "not found."))
  
  # Table A: Main Terciles
  tercile_summary <- data_grouped %>%
    group_by(income_tercile) %>%
    summarise(
      Population   = sum(.data[[w_var]]),
      Avg_Value    = weighted.mean(.data[[val_var]], w = .data[[w_var]], na.rm = TRUE),
      Min_Value    = min(.data[[val_var]], na.rm = TRUE),
      Max_Value    = max(.data[[val_var]], na.rm = TRUE)
    ) %>%
    mutate(
      `Pop %`      = scales::percent(Population / sum(Population), accuracy = 0.1),
      Population   = scales::comma(Population),
      `Avg Value`  = format_func(Avg_Value),
      `Range`      = paste0(format_func(Min_Value), " - ", format_func(Max_Value))
    ) %>%
    select(income_tercile, Population, `Pop %`, `Avg Value`, `Range`)
  
  print(as.data.frame(tercile_summary))
}