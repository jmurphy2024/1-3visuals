# ==== 0. ABOUT ====
## Script: ACS_bottomelectricity_viz.R
## Purpose: Generate a granular table with income ranges and a prevalence plot for Bottom Country.
## Author: Janica Murphy, Gemini / User
## Last Modified: 2026-01-27


# ==== 0. ABOUT ====
## Script: Electricity_BottomCountry_viz.R
## Purpose: Generate a granular table and trend plot using maximum income as the primary identifier.
## Author: Gemini / User
## Last Modified: 2026-01-27

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP PATHS & DATA ====
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"

USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_INDICATOR_NAME   <- "Electricity_Insecurity_Bottom"
USER_WEIGHT_VARIABLE  <- "HHWT" 

OUTPUT_PLOT_NAME  <- "plot_electricity_bottom_maxinc.png"
OUTPUT_TABLE_NAME <- "table_electricity_bottom_maxinc.png"

# Load prepared data
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_ACS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))

prepared_data <- readRDS(PREPARED_DATA_FILE)
main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign and filter for Bottom Country (Tercile 1)
data_bottom <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "HHINCOME",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
) %>% 
  filter(income_tercile == "Tercile 1 (Bottom)")

# ==== 2. DATA SUMMARIZATION ====
group_stats <- data_bottom %>%
  group_by(fine_income_group) %>%
  summarise(
    n = n(),
    max_inc = max(HHINCOME, na.rm = TRUE),
    at_risk = weighted.mean(ind_at_risk, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    no_elec = weighted.mean(ind_no_electricity, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  )

# Format table data to use Max Income instead of Range
table_data_final <- group_stats %>%
  mutate("Max Income" = scales::dollar(max_inc)) %>%
  select(`Max Income`, n, no_elec, at_risk) %>%
  mutate(across(c(no_elec, at_risk), ~sprintf("%.2f%%", . * 100)),
         n = scales::comma(n))

colnames(table_data_final) <- c("bold('Max Income')", "bold(italic('n'))", "bold('No Electricity')", "bold('At-Risk (Burden)')")

# ==== 3. GENERATE TREND PLOT ====

# Title is bolded, text is black, and line uses #C0392B
final_plot <- ggplot(group_stats, aes(x = max_inc, y = at_risk, group = 1)) +
  geom_line(color = "#C0392B", size = 0.5) + 
  geom_point(color = "#C0392B", size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  # Using n.breaks to keep horizontal labels from overlapping
  scale_x_continuous(labels = scales::dollar_format(), n.breaks = 10) +
  labs(
    title = "Energy Burden",
    x = "Maximum Household Income of Bottom Country Groups", 
    y = "Prevalence (%)"
  ) +
  theme_minimal(base_family = "sans serif") +
  theme(
    # Bolding, centering the title, and setting all text to black
    plot.title = element_text(fontface = "bold", color = "black", size = 14, hjust = 0.5),
    axis.title = element_text(color = "black"),
    axis.text  = element_text(color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.grid.minor = element_blank()
  )

final_plot_no_elec <- ggplot(group_stats, aes(x = max_inc, y = no_elec, group = 1)) +
  geom_line(color = "#C0392B", size = 0.5) + 
  geom_point(color = "#C0392B", size = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) + # Higher accuracy for smaller prevalence
  scale_x_continuous(labels = scales::dollar_format(), n.breaks = 10) +
  labs(
    title = "Extreme Low Usage (No Electricity Proxy)",
    x = "Maximum Household Income of Bottom Country Groups", 
    y = "Prevalence (%)"
  ) +
  theme_minimal(base_family = "sans serif") +
  theme(
    # Bolding, centering the title, and setting all text to black
    plot.title = element_text(face = "bold", color = "black", size = 14, hjust = 0.5),
    axis.title = element_text(color = "black"),
    axis.text  = element_text(color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5), 
    panel.grid.minor = element_blank()
  )

# Display and Export
grid.newpage(); grid.draw(final_plot_no_elec)
ggsave(here::here("03_output", PLOT_SUBFOLDER, "plot_no_electricity_bottom.png"), 
       plot = final_plot_no_elec, width = 8, height = 5, dpi = 300)
# ==== 4. EXPORT TABLE WITH UPDATED NOTES ====
rows_n <- nrow(table_data_final); cols_n <- ncol(table_data_final)
adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)

table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(
    base_family = "serif",
    core = list(fg_params = list(fontface = "plain", hjust = adj_hjust, x = adj_x, fontsize = 8.5)),
    colhead = list(fg_params = list(fontsize = 9, parse = TRUE), bg_params = list(fill = "#F2F2F2", col = "white"))
  )
)

notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.90),
  
  textGrob("1. Source: IPUMS ACS 2023 1-Year Sample, weighted by Household Weight (HHWT).", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), hjust = 0, x = 0.05, y = 0.75),
  
  textGrob("2. Interpretation: Max Income represents the upper dollar limit for each of the 20 Bottom Country groups.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), hjust = 0, x = 0.05, y = 0.60),
  
  textGrob("3. Variables: 'No Electricity' is cost < $100/yr; 'At-Risk' is energy burden > 10% of gross household income.", 
           gp = gpar(fontface = "italic", fontsize = 7.5, family = "serif"), hjust = 0, x = 0.05, y = 0.45)
)

final_table_layout <- grid.arrange(table_grob, notes_vbox, heights = unit.c(unit(1, "null"), unit(6, "lines")))

# SAVE FILES
ggsave(here::here("03_output", PLOT_SUBFOLDER, OUTPUT_PLOT_NAME), plot = final_plot, width = 8, height = 5, dpi = 300)
png(here::here("03_output", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 850, res = 120)
grid.draw(final_table_layout)
dev.off()

message("SUCCESS: Table updated to Max Income and exported.")