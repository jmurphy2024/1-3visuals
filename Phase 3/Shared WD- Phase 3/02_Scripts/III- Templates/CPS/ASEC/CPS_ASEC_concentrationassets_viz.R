# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(ggplot2); library(grid); library(gridExtra); library(here); library(readr); library(tidyr); library(gtable)

# Load custom plotting and group assignment functions
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ==== 1. SETUP PATHS & DATA ====
# We identify the existing folders you created
PLOT_SUBFOLDER  <- "plots"
TABLE_SUBFOLDER <- "tables"

USER_INDICATOR_NAME  <- "Net Assets by Income"
USER_ASEC_SAMPLE_ID  <- "cps2023_03s"
USER_WEIGHT_VARIABLE <- "ASECWT" 

OUTPUT_PLOT_NAME  <- "plot_netassets_share.png"
OUTPUT_TABLE_NAME <- "table_netassets_share.png"

# LOAD PREPARED DATA
PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", 
                                 paste0("prepared_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))

prepared_data    <- readRDS(PREPARED_DATA_FILE)
main_cutoffs     <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
borders_df       <- readr::read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)

# Assign households to income groups
data_with_groups <- assign_income_groups(
  data_to_process = prepared_data,
  borders_df      = borders_df,
  income_var_name = "HHINCOME",
  detail_level    = "Groups_20",
  main_cutoff1    = main_cutoffs$main_cutoff1,
  main_cutoff2    = main_cutoffs$main_cutoff2
) %>% filter(!is.na(income_tercile))

# ==== 2. DATA SUMMARIZATION (INCOME SHARE LOGIC) ====

# We use the share variables created in your updated Prepare script
summary_plotting <- data_with_groups %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(
    "Interest"  = weighted.mean(share_int,  w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    "Rent"      = weighted.mean(share_rent, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    "Dividends" = weighted.mean(share_div,  w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
    .groups = "drop"
  )

# ==== 3. GENERATE TREND PLOT (THE PATH FIX) ====

# IMPORTANT: To stop nesting, pass ONLY the filename. 
# The create_multi_line_plot function internally handles the directory path.
final_plot <- create_multi_line_plot(
  summary_data     = summary_plotting,
  y_vars           = c("Interest", "Rent", "Dividends"),
  y_labels         = c("Interest Share", "Rental Share", "Dividend Share"),
  plot_title       = "Average Asset Share of Total Income",
  y_axis_label     = "Weighted Mean Share (%)",
  y_axis_format    = "percent",
  
  # FIX: Pass only the name, not the full path
  output_filename  = OUTPUT_PLOT_NAME, 
  
  fine_group_level = "Groups_20",
  border_t1_t2     = main_cutoffs$main_cutoff1,
  border_t2_t3     = main_cutoffs$main_cutoff2
)

# ==== 4. EXPORT SUMMARY TABLE ====

# Create the table summary data
summarize_assets <- function(df) {
  summarise(df,
            n = n(),
            "Interest Share"  = weighted.mean(share_int,  w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
            "Rent Share"      = weighted.mean(share_rent, w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE),
            "Dividend Share"  = weighted.mean(share_div,  w = .data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE))
}

total_pop <- data_with_groups %>% 
  mutate(Country = "bold('CPS ASEC 2023 Universe')") %>% 
  group_by(Country) %>% summarize_assets()

country_summary <- data_with_groups %>% 
  group_by(income_tercile) %>% summarize_assets() %>%
  mutate(Country = case_when(
    income_tercile == "Tercile 1 (Bottom)" ~ "Bottom Country",
    income_tercile == "Tercile 2 (Middle)" ~ "Middle Country",
    income_tercile == "Tercile 3 (Top)"    ~ "Top Country"
  )) %>% select(Country, n, everything(), -income_tercile)

table_data_final <- bind_rows(total_pop, country_summary) %>%
  mutate(across(where(is.numeric) & !n, ~sprintf("%.2f%%", . * 100))) %>%
  mutate(n = scales::comma(n))

# Table Drawing Logic
table_grob <- tableGrob(
  table_data_final, rows = NULL, 
  theme = ttheme_minimal(
    base_family = "serif",
    core = list(fg_params = list(fontsize = 9.5))
  )
)

# Export the table using a clean path
png(here::here("03_output", "visualizations_final", TABLE_SUBFOLDER, OUTPUT_TABLE_NAME), width = 900, height = 550, res = 120)
grid.draw(table_grob)
dev.off()

message("SUCCESS: Net Assets Share visuals exported to visualizations_final.")