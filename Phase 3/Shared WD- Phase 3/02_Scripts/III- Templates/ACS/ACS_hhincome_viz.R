# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates/ACS
## Script: ACS_hhincome_viz.R
## Purpose: Standardized shaded area plot with vertical median intercept and staggered X-axis.
## Last Modified: 2026-01-23

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable)

# Load project shared functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP PATHS & DATA ====
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"

# Project Shared Values
USER_IPUMS_SAMPLE_ID  <- "us2023b" 
USER_INDICATOR_NAME   <- "avg_hh_income"
USER_WEIGHT_VARIABLE  <- "HHWT"
US_MEDIAN_INCOME      <- 78538 # 2019-2023 ACS National Median
CLR_T1 <- "#C0392B"; CLR_T2 <- "#F5B041"; CLR_T3 <- "#27AE60"

OUTPUT_PLOT_NAME  <- paste0("plot_", USER_INDICATOR_NAME, ".png")
OUTPUT_TABLE_NAME <- paste0("table_", USER_INDICATOR_NAME, ".png")

# Load prepared data
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

prepared_data <- readRDS(PREPARED_DATA_FILE)
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign groups
data_with_groups <- assign_income_groups(prepared_data, borders_df, "HHINCOME", "Groups_20", 
                                         main_cutoffs$main_cutoff1, main_cutoffs$main_cutoff2) %>% 
  filter(!is.na(income_tercile))

# ==== 2. PREPARE X-AXIS MAPPING & MEDIAN INTERCEPT ====
# Replicating library logic to find the representative population index for $78,538
x_axis_base <- .prepare_x_axis_data(borders_df, "Groups_20", main_cutoffs$main_cutoff1, main_cutoffs$main_cutoff2)
median_pos  <- x_axis_base$x_axis_info$breaks[which.min(abs(x_axis_base$x_axis_info$income - US_MEDIAN_INCOME))]

# Staggered labels to prevent horizontal overlap
staggered_labels <- x_axis_base$x_axis_info %>%
  filter(income != 100000) %>% 
  bind_rows(tibble(income = US_MEDIAN_INCOME, labels_raw = scales::dollar(US_MEDIAN_INCOME), is_border = FALSE, breaks = median_pos)) %>%
  distinct(income, .keep_all = TRUE) %>%
  arrange(breaks) %>%
  mutate(labels_staggered = ifelse(row_number() %% 2 == 0, paste0("\n", labels_raw), labels_raw))

# ==== 3. GENERATE TREND PLOT ====
summary_stats <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(avg_income = weighted.mean(HHINCOME, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE), .groups = "drop")

# Use standardized function to build base, but use a dummy filename to satisfy internal save
final_plot <- create_single_line_plot(
  summary_data       = summary_stats,
  y_var              = "avg_income",
  plot_title         = "Average Household Income Across the 'Three Countries'",
  y_axis_label       = "Average Household Income",
  y_axis_format      = "dollar",
  output_filename    = "dummy_save.png", 
  fine_group_level   = "Groups_20",
  border_t1_t2       = main_cutoffs$main_cutoff1,
  border_t2_t3       = main_cutoffs$main_cutoff2
)

# Apply shaded area and vertical median benchmark
final_plot <- final_plot + 
  geom_area(fill = "grey30", alpha = 0.1) + 
  geom_vline(xintercept = median_pos, linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text", x = median_pos + 1, y = US_MEDIAN_INCOME + 5000, 
           label = paste0("U.S. Median: ", scales::dollar(US_MEDIAN_INCOME)), 
           color = "red", fontface = "bold", hjust = 0) +
  scale_x_continuous(breaks = staggered_labels$breaks, labels = staggered_labels$labels_staggered, expand = c(0.01, 0.01)) +
  coord_cartesian(clip = "off")

# ==== 4. EXPORT SUMMARY TABLE ====
# Replicating the transportation integrated table layout
table_data_final <- data_with_groups %>%
  group_by(income_tercile) %>%
  summarise(n = n(), "Avg Income" = weighted.mean(HHINCOME, w = .data[[USER_WEIGHT_VARIABLE]]), .groups = "drop") %>%
  mutate(Country = case_when(income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
                             income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
                             income_tercile == "Tercile 3 (Top)"    ~ "Top Country")) %>%
  select(Country, n, "Avg Income") %>%
  mutate(across(where(is.numeric) & !n, ~scales::dollar(.)), n = scales::comma(n))

colnames(table_data_final) <- c("bold('Country')", "bold(italic('n'))", "bold('Avg HH Income')")

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(base_family = "serif",
                         core = list(fg_params = list(hjust = 0.5, x = 0.5, fontsize = 10)),
                         colhead = list(fg_params = list(fontsize = 11, parse = TRUE), bg_params = list(fill = "#F2F2F2", col = "white"))
  )
)

notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.90),
  textGrob(paste0("1. Source: 2023 IPUMS ACS 5-Year Sample."), gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.75),
  textGrob(paste0("2. Benchmark: Red vertical line indicates national median (", scales::dollar(US_MEDIAN_INCOME), ")."), gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.60)
)

final_table_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(7, "lines")))

# SAVE TO PNG USING DIRECT PATHS
grid.newpage(); grid.draw(final_plot)
grid.newpage(); grid.draw(final_table_layout)

ggsave(here::here("03_output", PLOT_SUBFOLDER, OUTPUT_PLOT_NAME), final_plot, width = 11, height = 7)

png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 550, res = 120)
grid.draw(final_table_layout); dev.off()

message("SUCCESS: Visuals exported to verified 03_output folders.")