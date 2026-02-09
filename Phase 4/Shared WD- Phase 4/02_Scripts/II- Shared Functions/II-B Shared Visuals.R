# ==============================================================================
# SCRIPT: II-B Shared Visuals.R
# LOCATION: 02_Scripts/II- Shared Functions/
# PURPOSE:  Standardized visualization functions for the 1/3 Country Project.
#           Contains logic for:
#             1. Bar + Dot + Line Charts (for Average Income/Cost)
#             2. Single Line Plots (for Prevalence Rates)
# UPDATES:  Added 'create_bar_dot_line_plot' and column name robustness.
# AUTHOR:   EPAG / Gemini
# ==============================================================================

# ==== 1. LOAD REQUIRED LIBRARIES ====
library(ggplot2); library(dplyr); library(readr); library(purrr); library(stringr)
library(here); library(ggtext); library(glue); library(grid); library(gridExtra)
library(scales); library(ggnewscale); library(cowplot); library(tidyr)


# ==== 2. INTERNAL HELPER FUNCTIONS ====

# --- A. Legend Extraction ---
get_legend <- function(plot) {
  plot_gtable <- tryCatch(ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot)), error = function(e) NULL)
  if (is.null(plot_gtable)) return(grid::nullGrob())
  legend_index <- which(sapply(plot_gtable$grobs, function(x) x$name) == "guide-box")
  if (length(legend_index) > 0) return(plot_gtable$grobs[[legend_index]])
  else return(grid::nullGrob())
}

# --- B. Null Coalescing Operator ---
`%||%` <- function(a, b) { if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a }

# --- C. X-Axis Data Preparation (For Line Plots) ---
.prepare_x_axis_data <- function(borders_data, fine_group_level, border_t1_t2, border_t2_t3) {
  
  # Robustness: Handle column name mismatch (MainTercile vs income_tercile_group)
  if("income_tercile_group" %in% names(borders_data) && !"MainTercile" %in% names(borders_data)) {
    borders_data <- borders_data %>% rename(MainTercile = income_tercile_group)
  }
  
  num_groups_per_tercile <- as.numeric(str_extract(fine_group_level, "\\d+"))
  total_groups <- num_groups_per_tercile * 3
  
  tercile_labels <- borders_data %>%
    filter(QuantileGroup == fine_group_level) %>%
    distinct(MainTercile) %>%
    pull(MainTercile)
  
  t1_label <- tercile_labels[str_detect(tercile_labels, regex("1|Bottom", ignore_case = TRUE))]
  t2_label <- tercile_labels[str_detect(tercile_labels, regex("2|Middle", ignore_case = TRUE))]
  t3_label <- tercile_labels[str_detect(tercile_labels, regex("3|Top", ignore_case = TRUE))]
  
  max_income_row <- borders_data %>%
    filter(QuantileGroup == fine_group_level, MainTercile == t3_label) %>%
    slice_max(order_by = CutoffValue, n = 1)
  
  approx_max_income <- (max_income_row$CutoffValue %||% (border_t2_t3 * 2)) * 1.2
  
  sub_group_borders_prepared <- borders_data %>%
    filter(QuantileGroup == fine_group_level) %>%
    arrange(MainTercile, CutoffValue) %>%
    group_by(MainTercile) %>%
    mutate(within_tercile_sub_group_num = row_number()) %>%
    ungroup() %>%
    mutate(
      quantile_position = case_when(
        MainTercile == t1_label ~ within_tercile_sub_group_num,
        MainTercile == t2_label ~ num_groups_per_tercile + within_tercile_sub_group_num,
        MainTercile == t3_label ~ (2 * num_groups_per_tercile) + within_tercile_sub_group_num,
        TRUE ~ NA_real_
      )
    ) %>%
    select(quantile_position, upper_income_bound = CutoffValue) %>%
    filter(!is.na(quantile_position)) %>%
    bind_rows(
      tibble(quantile_position = 0, upper_income_bound = 0),
      tibble(quantile_position = total_groups, upper_income_bound = approx_max_income)
    ) %>%
    distinct(quantile_position, .keep_all = TRUE) %>%
    arrange(quantile_position)
  
  get_quantile_pos <- function(inc_target, border_df) {
    lower <- border_df %>% filter(upper_income_bound < inc_target) %>% slice_max(upper_income_bound, n = 1)
    upper <- border_df %>% filter(upper_income_bound >= inc_target) %>% slice_min(upper_income_bound, n = 1)
    if (nrow(lower) == 0) return(0.5)
    if (nrow(upper) == 0) return(total_groups + 0.5)
    frac <- (inc_target - lower$upper_income_bound) / (upper$upper_income_bound - lower$upper_income_bound)
    return(lower$quantile_position + frac * (upper$quantile_position - lower$quantile_position))
  }
  
  critical_borders <- sort(unique(c(border_t1_t2, border_t2_t3)))
  desired_ticks <- c(10000, 25000, 50000, 100000, 175000, 250000, 500000)
  target_incomes <- sort(unique(c(critical_borders, desired_ticks)))
  
  income_label_positions <- map_dbl(target_incomes, ~ get_quantile_pos(., sub_group_borders_prepared))
  
  x_axis_info <- tibble(breaks = income_label_positions, income = target_incomes) %>%
    arrange(breaks) %>%
    mutate(
      is_border = income %in% critical_borders,
      break_diff = breaks - lag(breaks, default = -Inf)
    ) %>%
    filter(break_diff > (0.04 * total_groups) | is_border) %>%
    mutate(
      breaks = case_when(
        income == border_t1_t2 ~ num_groups_per_tercile + 0.5,
        income == border_t2_t3 ~ (2 * num_groups_per_tercile) + 0.5,
        TRUE ~ breaks
      ),
      labels_raw = dollar(income, accuracy = 1)
    ) %>%
    distinct(breaks, .keep_all = TRUE)
  
  return(list(x_axis_info = x_axis_info, total_groups = total_groups, num_groups_per_tercile = num_groups_per_tercile))
}

# --- D. Base Plot Builder (Background Shading) ---
.build_base_plot <- function(x_axis_data, plot_title, t1_color, t2_color, t3_color, 
                             background_alpha, vline_alpha, base_font, border_t1_t2, border_t2_t3) {
  
  num_groups_per_tercile <- x_axis_data$num_groups_per_tercile
  total_groups <- x_axis_data$total_groups
  x_axis_info <- x_axis_data$x_axis_info
  x_axis_info_no_borders <- x_axis_info %>% filter(!is_border)
  
  ggplot() +
    annotate("rect", xmin = 0.5, xmax = num_groups_per_tercile + 0.5, ymin = -Inf, ymax = Inf, fill = t1_color, alpha = background_alpha) +
    annotate("rect", xmin = num_groups_per_tercile + 0.5, xmax = (2 * num_groups_per_tercile) + 0.5, ymin = -Inf, ymax = Inf, fill = t2_color, alpha = background_alpha) +
    annotate("rect", xmin = (2 * num_groups_per_tercile) + 0.5, xmax = total_groups + 0.5, ymin = -Inf, ymax = Inf, fill = t3_color, alpha = background_alpha) +
    geom_vline(data = x_axis_info_no_borders, aes(xintercept = breaks), color = "grey85", linetype = "dotted", linewidth = 0.6) +
    geom_vline(xintercept = num_groups_per_tercile + 0.5, linetype = "dashed", color = t2_color, linewidth = 1, alpha = vline_alpha) +
    geom_vline(xintercept = (2 * num_groups_per_tercile) + 0.5, linetype = "dashed", color = t3_color, linewidth = 1, alpha = vline_alpha) +
    annotate("text", x = num_groups_per_tercile + 0.5, y = -Inf, label = scales::dollar(border_t1_t2), 
             vjust = 2, color = t2_color, fontface = "bold", size = 3.5, family = base_font) +
    annotate("text", x = (2 * num_groups_per_tercile) + 0.5, y = -Inf, label = scales::dollar(border_t2_t3), 
             vjust = 2, color = t3_color, fontface = "bold", size = 3.5, family = base_font) +
    scale_x_continuous(limits = c(0.5, total_groups + 0.5), expand = c(0.01, 0.01), 
                       breaks = x_axis_info_no_borders$breaks, labels = x_axis_info_no_borders$labels_raw) +
    labs(title = plot_title, x = NULL) +
    theme_minimal(base_family = base_font) +
    theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(), plot.margin = margin(t = 10, r = 10, b = 25, l = 10)) +
    coord_cartesian(clip = "off")
}

# --- E. Final Layout & Saving (With 3-Color Bottom Labels) ---
.arrange_and_save_plot <- function(ggplot_obj, output_filename, ...) {
  args <- list(...)
  t1_color <- args$t1_color %||% "#C0392B"
  t2_color <- args$t2_color %||% "#F5B041"
  t3_color <- args$t3_color %||% "#27AE60"
  base_font <- args$base_font %||% "sans"
  
  plot_legend <- get_legend(ggplot_obj)
  g_no_legend <- ggplot_obj + theme(legend.position = "none")
  
  # Standard 1/3 Country Labels
  label_grob_t1 <- textGrob("Bottom Third Country", gp = gpar(fontsize = 12, col = t1_color, fontfamily = base_font, fontface = "bold"))
  label_grob_t2 <- textGrob("Middle Third Country", gp = gpar(fontsize = 12, col = t2_color, fontfamily = base_font, fontface = "bold"))
  label_grob_t3 <- textGrob("Top Third Country", gp = gpar(fontsize = 12, col = t3_color, fontfamily = base_font, fontface = "bold"))
  
  # X-Axis Title (General)
  x_title_grob <- textGrob("Population Distribution by Household Income", gp = gpar(fontsize = 11, fontfamily = base_font, fontface = "bold"))
  
  # Assemble Bottom Row
  bottom_row <- cowplot::plot_grid(label_grob_t1, label_grob_t2, label_grob_t3, nrow = 1)
  
  # Determine Layout based on legend presence
  plot_list <- list(g_no_legend, bottom_row, x_title_grob)
  heights <- c(1, 0.06, 0.04)
  
  if (!inherits(plot_legend, "nullGrob")) { 
    plot_list <- append(plot_list, list(plot_legend))
    heights <- c(heights, 0.1) 
  }
  
  final_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1, rel_heights = heights)
  
  # Ensure directory exists
  output_path <- here::here("03_output", "visualizations_final", output_filename)
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  
  cowplot::save_plot(output_path, final_plot, base_width = 12, base_height = 8, bg = "white")
  message(paste("Saved plot to:", output_path))
}


# ==============================================================================
# ==== 3. EXPORTED VISUALIZATION FUNCTIONS ====
# ==============================================================================

# --- A. SINGLE LINE PLOT (For Prevalence Rates) ---
create_single_line_plot <- function(summary_data, y_var, ...) {
  args <- list(...)
  
  # Default Colors
  t1_col <- args$t1_color %||% "#C0392B"
  t2_col <- args$t2_color %||% "#F5B041"
  t3_col <- args$t3_color %||% "#27AE60"
  
  # Parameters
  fine_group_level <- args$fine_group_level %||% "Groups_20"
  num_groups_per_tercile <- as.numeric(str_extract(fine_group_level, "\\d+"))
  
  border_t1_t2 <- args$border_t1_t2
  border_t2_t3 <- args$border_t2_t3
  
  # Prepare Plot Data
  plot_data <- summary_data %>%
    mutate(
      tercile_num = as.numeric(str_extract(income_tercile, "\\d")),
      group_num = as.numeric(str_extract(fine_income_group, "\\d+$")),
      quantile_position = (tercile_num - 1) * num_groups_per_tercile + group_num
    )
  
  # Load & Prep X-Axis Borders
  borders_data <- readr::read_csv(here("01_data", "processed", "within_tercile_quantile_borders_2023.csv"), show_col_types = FALSE)
  x_axis_data <- .prepare_x_axis_data(borders_data, fine_group_level, border_t1_t2, border_t2_t3)
  
  # Build Base
  base_plot <- .build_base_plot(x_axis_data, args$plot_title, t1_col, t2_col, t3_col, 
                                args$background_alpha %||% 0.2, args$vline_alpha %||% 0.25, 
                                args$base_font %||% "sans", border_t1_t2, border_t2_t3)
  
  # Add Line Logic
  final_plot <- base_plot +
    geom_smooth(data = plot_data, aes(x = quantile_position, y = .data[[y_var]]), 
                method = "loess", span = 0.75, se = FALSE, color = "black") +
    scale_y_continuous(name = args$y_axis_label, labels = label_percent(accuracy = 1))
  
  .arrange_and_save_plot(final_plot, args$output_filename, t1_color=t1_col, t2_color=t2_col, t3_color=t3_col, base_font="sans")
}


# --- B. BAR + DOT + LINE PLOT (For Average Income) ---
create_bar_dot_line_plot <- function(summary_data, y_var, plot_title, y_axis_label, output_filename, 
                                     fine_group_level="Groups_20", 
                                     t1_color="#C0392B", t2_color="#F5B041", t3_color="#27AE60", ...) {
  
  # Ensure data is sorted for correct plotting
  plot_data <- summary_data %>%
    arrange(income_tercile, fine_income_group) %>%
    mutate(group_id = row_number()) # Numeric 1-20 ID for X-axis
  
  # Calculate approximate tercile boundaries for shading
  # (e.g., if 20 groups total, split at 6.66 and 13.33)
  total_groups <- max(plot_data$group_id)
  x_bound_1 <- total_groups * (1/3) + 0.5
  x_bound_2 <- total_groups * (2/3) + 0.5
  
  p <- ggplot(plot_data, aes(x = group_id, y = .data[[y_var]])) +
    
    # 1. Background Shading (Light Overlay)
    annotate("rect", xmin = -Inf, xmax = x_bound_1, ymin = -Inf, ymax = Inf, fill = t1_color, alpha = 0.1) +
    annotate("rect", xmin = x_bound_1, xmax = x_bound_2, ymin = -Inf, ymax = Inf, fill = t2_color, alpha = 0.1) +
    annotate("rect", xmin = x_bound_2, xmax = Inf, ymin = -Inf, ymax = Inf, fill = t3_color, alpha = 0.1) +
    
    # 2. Vertical Dividers
    geom_vline(xintercept = x_bound_1, linetype = "dashed", color = t2_color, alpha = 0.5) +
    geom_vline(xintercept = x_bound_2, linetype = "dashed", color = t3_color, alpha = 0.5) +
    
    # 3. Bars
    geom_col(aes(fill = income_tercile, color = income_tercile), alpha = 0.6, width = 0.85) +
    
    # 4. Trend Line (Connecting tops of bars)
    geom_line(group = 1, color = "black", linewidth = 0.5) +
    
    # 5. Points (Dots on top)
    geom_point(aes(color = income_tercile), size = 1.5) +
    
    # 6. Scales & Labels
    scale_fill_manual(values = c(t1_color, t2_color, t3_color)) +
    scale_color_manual(values = c(t1_color, t2_color, t3_color)) +
    scale_y_continuous(labels = label_dollar(), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(breaks = NULL) + # Clean look: no X-axis numbers
    
    labs(title = plot_title, y = y_axis_label, x = NULL) +
    
    theme_minimal(base_family = "sans") +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18, face = "bold", margin = margin(b = 15)),
      axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  # 7. Save
  .arrange_and_save_plot(p, output_filename, t1_color=t1_color, t2_color=t2_color, t3_color=t3_color)
}