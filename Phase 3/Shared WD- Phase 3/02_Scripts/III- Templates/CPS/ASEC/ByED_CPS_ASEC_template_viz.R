## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: ASEC_Stability_Master_viz.R
## Purpose: Final Master Script with Age 25+ Filter and NCVS Formatting.
##          Generates: (1) Faceted Plot with Notes, (2) Hierarchical Summary Table.
## Author: Janica Murphy, Maxwell Goshert EPAG/ Gemini

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here)
library(readr); library(tidyr); library(gtable)

source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. DATA INGESTION & AGE FILTER ====
# Load the 146,133 person-record anchored file
prepared_data_raw <- readRDS(here::here("01_data", "processed", "IPUMS_Microdata", "prepared_stability_mosaic_2023.rds"))

# Applying the Age 25+ Filter to ensure education completion and labor force prime
prepared_data <- prepared_data_raw %>% 
  filter(as.numeric(AGE) >= 25)

main_cutoffs  <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df    <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

data_with_groups <- assign_income_groups(
  data_to_process = prepared_data, borders_df = borders_df, income_var_name = "HHINCOME",
  detail_level = "Groups_20", main_cutoff1 = main_cutoffs$main_cutoff1, main_cutoff2 = main_cutoffs$main_cutoff2
)

# ==== 2. DATA PREP FOR SMOOTHING ====
summary_plotting <- data_with_groups %>%
  group_by(edu_group, income_tercile, fine_income_group) %>%
  summarise(
    "Full-Year Work"       = weighted.mean(ind_full_year, w = ASECWT, na.rm = TRUE),
    "Public Retirement/SS" = weighted.mean(ind_public_retirement, w = ASECWT, na.rm = TRUE),
    "Private Asset Income" = weighted.mean(ind_private_assets, w = ASECWT, na.rm = TRUE),
    "Homeownership"        = weighted.mean(ind_homeowner, w = ASECWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c("Full-Year Work", "Public Retirement/SS", "Private Asset Income", "Homeownership"),
               names_to = "Indicator", values_to = "Prevalence")

summary_plotting <- summary_plotting %>% mutate(group_idx = as.numeric(as.factor(fine_income_group)))

# Anchored Breaks Logic
ncvs_breaks  <- c(1, 10, 20, 30, 40, 50, 60)
ncvs_labels  <- c("$10,000", "$25,000", "$64,400", "$100,000", "$130,000", "$250,000", "$500,000")
label_colors <- case_when(ncvs_labels == "$64,400" ~ "#E69F00", ncvs_labels == "$130,000" ~ "#009E73", TRUE ~ "black")
label_faces   <- ifelse(ncvs_labels %in% c("$64,400", "$130,000"), "bold", "plain")

# ==== 3. GENERATE FACETED TREND PLOT (SANS-SERIF TITLE & LABELS) ====
final_plot <- ggplot(summary_plotting, aes(x = group_idx, y = Prevalence, color = Indicator, group = Indicator)) +
  annotate("rect", xmin = 1, xmax = 20, ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.03) +
  annotate("rect", xmin = 20, xmax = 40, ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.03) +
  annotate("rect", xmin = 40, xmax = 60, ymin = -Inf, ymax = Inf, fill = "green", alpha = 0.03) +
  geom_vline(xintercept = 20, color = "#E69F00", linetype = "dashed", linewidth = 0.5) + 
  geom_vline(xintercept = 40, color = "#009E73", linetype = "dashed", linewidth = 0.5) + 
  geom_smooth(method = "loess", span = 0.3, se = FALSE, linewidth = 0.5) +
  facet_wrap(~edu_group) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1), breaks = seq(0, 1, 0.25)) +
  scale_x_continuous(breaks = ncvs_breaks, labels = ncvs_labels) +
  scale_color_manual(values = c("#D95F02", "#1B9E77", "#7570B3", "#E7298A")) + 
  labs(title = "Economic Stability Portfolios by Education",
       x = "Population Distribution by Household Income", 
       y = "Weighted Prevalence (%)") +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 20)),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 8, color = label_colors, face = label_faces),
    axis.title.x = element_text(margin = margin(t = 10))
  )

# ==== 4. GENERATE NCVS-STYLE NOTES (SERIF FONT) ====
notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), hjust = 0, x = 0.05, y = 0.95),
  textGrob("1. 2023 IPUMS CPS Annual Social and Economic (ASEC) Supplement.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.82),
  textGrob("2. Percentages represent weighted population prevalence for adults aged 25+.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.69),
  textGrob("3. Full-year work: Employed 50-52 weeks. Public: Realized INCRETIR/INCSS. Private: Dividends, Interest, Rent.", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.56),
  textGrob("4. Education Divisions: HS or Lower (EDUC <= 073), Bachelors (EDUC == 111), Masters & Up (EDUC >= 123).", gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.43)
)

# ==== 5. GENERATE HIERARCHICAL NCVS SUMMARY TABLE (SANS-SERIF DATA) ====
edu_country_summary <- data_with_groups %>%
  group_by(income_tercile, edu_group) %>%
  summarise(
    n = n(),
    "Full-Year Work"       = weighted.mean(ind_full_year, w = ASECWT, na.rm = TRUE),
    "Public Retirement/SS" = weighted.mean(ind_public_retirement, w = ASECWT, na.rm = TRUE),
    "Private Asset Income" = weighted.mean(ind_private_assets, w = ASECWT, na.rm = TRUE),
    "Homeownership"        = weighted.mean(ind_homeowner, w = ASECWT, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  ))

# Hierarchical restructuring
final_table_list <- list()
countries <- c("Bottom Country", "Middle Country", "Top Country")

for(cty in countries) {
  header_row <- data.frame(Variable = cty, n = "", "Full-Year Work" = "", "Public Retirement/SS" = "", 
                           "Private Asset Income" = "", "Homeownership" = "", stringsAsFactors = FALSE, check.names = FALSE)
  sub_rows <- edu_country_summary %>% filter(Country == cty) %>%
    mutate(across(where(is.numeric) & !n, ~sprintf("%.1f%%", . * 100))) %>%
    mutate(n = scales::comma(n)) %>%
    select(Variable = edu_group, n, everything(), -income_tercile, -Country) %>%
    mutate(Variable = paste0("   ", Variable))
  final_table_list[[cty]] <- bind_rows(header_row, sub_rows)
}
hierarchical_data <- bind_rows(final_table_list)

colnames(hierarchical_data) <- c("bold('Variable')", "bold(italic('n'))", "bold('Full-Year Work')", 
                                 "bold('Public Retirement/SS')", "bold('Private Asset Income')", "bold('Homeownership')")
rows_n <- nrow(hierarchical_data); is_header <- !grepl("^   ", hierarchical_data[[1]])
adj_hjust <- matrix(rep(c(0, 0.5, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)
adj_x     <- matrix(rep(c(0.02, 0.5, 0.5, 0.5, 0.5, 0.5), each = rows_n), nrow = rows_n)

table_grob <- tableGrob(hierarchical_data, rows = NULL, 
                        theme = ttheme_minimal(base_family = "sans", 
                                               core = list(fg_params = list(hjust = adj_hjust, x = adj_x, fontsize = 9.5, fontface = ifelse(is_header, "bold", "plain"))),
                                               colhead = list(fg_params = list(fontsize = 10.5, parse = TRUE), bg_params = list(fill = "#F2F2F2", col = "white"))))
# Create the specific Notes section for the Table
# This section uses Serif font to distinguish methodology from data
table_notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.90),
  textGrob("1. The summary tables do not show simple mathematical averages; they display weighted population prevalence,", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.75),
  textGrob("   which represents the percentage of the estimated national population that possesses a specific stability anchor.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.62),
  textGrob("2. Weighted prevalence is calculated as the sum of ASECWT for individuals with an anchor divided by the total sum of ASECWT for the cohort segment.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.49),
  textGrob("3. Universe: Adults aged 25+ within specified Income Geography and Educational Attainment groups.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.36)
)

# Create the specific Notes section for the Table
# This section uses Serif font to distinguish methodology from data
table_notes_vbox <- grobTree(
  textGrob("Notes:", gp = gpar(fontface = "bold", fontsize = 8.5, family = "serif"), 
           hjust = 0, x = 0.05, y = 0.90),
  textGrob("1. Universe: Adults aged 25+ within specified Income Geography and Educational Attainment groups.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.75),
  textGrob("2. The summary show the percentage of the estimated national population that possesses a specific stability anchor.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.62),
  textGrob("3. Weighted prevalence is calculated as the sum of ASECWT for individuals with an anchor divided by the total sum of ASECWT for the cohort segment.", 
           gp = gpar(fontface = "italic", fontsize = 8, family = "serif"), hjust = 0, x = 0.05, y = 0.49)
)

# ==== 6. EXPORTS ====
dir.create(here::here("03_output", "PNGs"), showWarnings = FALSE, recursive = TRUE)

# Output 1: Plot + Notes (Existing logic)
plot_layout <- grid.arrange(final_plot, notes_vbox, heights = unit.c(unit(1, "null"), unit(6, "lines")))
png(here::here("03_output", "PNGs", "plot_asec_stability_faceted_AGE25.png"), width = 14, height = 8, units = "in", res = 300)
grid.draw(plot_layout); dev.off()

# Updated Output 2: Hierarchical Table + Table-Specific Notes
# We combine the table grob and the notes grob into a single layout for export
table_final_layout <- grid.arrange(
  table_grob, 
  table_notes_vbox, 
  heights = unit.c(unit(1, "null"), unit(7, "lines")) # Adjusted lines for multiline note
)

png(here::here("03_output", "PNGs", "table_asec_stability_hierarchical_AGE25.png"), width = 12, height = 8, units = "in", res = 300)
grid.draw(table_final_layout)
dev.off()

message("SUCCESS: Separate Faceted Plot and Hierarchical Table with Notes (Age 25+) exported.")