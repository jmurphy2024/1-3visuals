## WD location: 02_Scripts/II- Shared Functions
## Script: II-B Shared Visuals.r
## Purpose: Defines standardized visualizations for the 1/3 Country Project.
## Updates: Updated for 2023 5-Year Sample and ADJUST normalization.
## Author: Janica Murphy, Max Goshert, EPAG / Gemini
## Last Modified: 2026-02-06

# ==== 1. LOAD REQUIRED LIBRARIES ====
library(ggplot2); library(dplyr); library(readr); library(purrr); library(stringr)
library(here); library(ggtext); library(glue); library(grid); library(gridExtra)
library(scales); library(ggnewscale); library(cowplot); library(tidyr)

# ==== 2. INTERNAL HELPER FUNCTIONS ====
get_legend <- function(plot) {
  plot_gtable <- tryCatch(ggplot2::ggplot_gtable(ggplot2::ggplot_build(plot)), error = function(e) NULL)
  if (is.null(plot_gtable)) return(grid::nullGrob())
  legend_index <- which(sapply(plot_gtable$grobs, function(x) x$name) == "guide-box")
  if (length(legend_index) > 0) return(plot_gtable$grobs[[legend_index]])
  else return(grid::nullGrob())
}

`%||%` <- function(a, b) { if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a }

.prepare_x_axis_data <- function(borders_data, fine_group_level, border_t1_t2, border_t2_t3) {
  # CHANGE: Point to 2023 5-Year border file
  borders_path <- here("01_data", "processed", "within_tercile_quantile_borders_2023.csv")
  
  if(file.exists(borders_path)) {
    borders_data <- readr::read_csv(borders_path, show_col_types = FALSE)
  } else {
    stop("Error: 2023 Adjusted borders not found. Run II-C Border Setup first.")
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

.arrange_and_save_plot <- function(ggplot_obj, output_filename, ...) {
  args <- list(...); t1_color <- args$t1_color; t2_color <- args$t2_color; t3_color <- args$t3_color
  base_font <- args$base_font; plot_legend <- get_legend(ggplot_obj)
  g_no_legend <- ggplot_obj + theme(legend.position = "none")
  
  label_grob_t1 <- textGrob("Bottom Third Country", gp = gpar(fontsize = 10, col = t1_color, fontfamily = base_font, fontface = "bold"))
  label_grob_t2 <- textGrob("Middle Third Country", gp = gpar(fontsize = 10, col = t2_color, fontfamily = base_font, fontface = "bold"))
  label_grob_t3 <- textGrob("Top Third Country", gp = gpar(fontsize = 10, col = t3_color, fontfamily = base_font, fontface = "bold"))
  x_title_grob <- textGrob("Population Distribution by Household Income", gp = gpar(fontsize = 11, fontfamily = base_font, fontface = "bold"))
  
  bottom_row <- cowplot::plot_grid(label_grob_t1, label_grob_t2, label_grob_t3, nrow = 1)
  plot_list <- list(g_no_legend, bottom_row, x_title_grob)
  heights <- c(1, 0.06, 0.04)
  if (!inherits(plot_legend, "nullGrob")) { plot_list <- append(plot_list, list(plot_legend)); heights <- c(heights, 0.1) }
  
  final_plot <- cowplot::plot_grid(plotlist = plot_list, ncol = 1, rel_heights = heights)
  output_path <- here::here("03_output", "visualizations_final", output_filename)
  cowplot::save_plot(output_path, final_plot, base_width = 10, base_height = 7, bg = "white")
}

# ==== 3. EXPORTED VISUALIZATION FUNCTIONS ====
create_single_line_plot <- function(summary_data, y_var, ...) {
  args <- list(...)
  
  # CHANGE: Point to 2023 5-Year cutoff file
  main_cutoffs <- readRDS(here("01_data", "processed", "main_tercile_cutoffs_2023.rds"))
  
  # CHANGE: Use consistent naming from updated II-C ($main_cutoff1/2)
  border_t1_t2 <- args$border_t1_t2 %||% main_cutoffs$main_cutoff1
  border_t2_t3 <- args$border_t2_t3 %||% main_cutoffs$main_cutoff2
  
  fine_group_level <- args$fine_group_level %||% "Groups_20"
  num_groups_per_tercile <- as.numeric(str_extract(fine_group_level, "\\d+"))
  
  plot_data <- summary_data %>%
    mutate(tercile_num = as.numeric(str_extract(income_tercile, "\\d")),
           group_num = as.numeric(str_extract(fine_income_group, "\\d+$")),
           quantile_position = (tercile_num - 1) * num_groups_per_tercile + group_num)
  
  # CHANGE: Point to 2023 border CSV
  borders_data <- readr::read_csv(here("01_data", "processed", "within_tercile_quantile_borders_2023.csv"), show_col_types = FALSE)
  x_axis_data <- .prepare_x_axis_data(borders_data, fine_group_level, border_t1_t2, border_t2_t3)
  
  base_plot <- .build_base_plot(x_axis_data, args$plot_title, args$t1_color %||% "#C0392B", args$t2_color %||% "#F5B041", 
                                args$t3_color %||% "#27AE60", args$background_alpha %||% 0.2, args$vline_alpha %||% 0.25, 
                                args$base_font %||% "sans", border_t1_t2, border_t2_t3)
  
  final_plot <- base_plot +
    geom_smooth(data = plot_data, aes(x = quantile_position, y = .data[[y_var]]), method = "loess", span = 0.75, se = FALSE, color = "black") +
    scale_y_continuous(name = args$y_axis_label, labels = label_percent(accuracy = 1))
  
  .arrange_and_save_plot(final_plot, args$output_filename, t1_color = "#C0392B", t2_color = "#F5B041", t3_color = "#27AE60", base_font = "sans")
}