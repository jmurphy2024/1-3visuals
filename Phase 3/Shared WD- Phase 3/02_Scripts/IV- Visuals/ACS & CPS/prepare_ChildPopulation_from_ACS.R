# ==== 0. ABOUT ====
## WD location: 02_Scripts/IV-Visuals/ACS & CPS
## Script: prepare_ChildPopulation_from_ACS.R
## Purpose: Prepares ACS data to calculate the weighted number of children (<18)
##          by fine-grained income group for the dual-axis visualization.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Dependencies: dplyr, here, readr, stringr
## Input: Raw RDS file for ACS (us2023a).
## Output: 01_data/processed/summary_acs_child_population.rds

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(stringr)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))

# ==== 1. PARAMETERS ====
USER_IPUMS_SAMPLE_ID  <- "us2023a"
USER_WEIGHT_VARIABLE <- "PERWT"
USER_FINE_GROUP_LEVEL     <- "Groups_20"

# ==== 2. DATA PREPARATION ====
RAW_DATA_FILE <- here::here("01_data", "raw", "IPUMS_Microdata", paste0("usa_", USER_IPUMS_SAMPLE_ID), "raw_data.rds")
raw_data <- readRDS(RAW_DATA_FILE)

cleaned_data <- raw_data %>% mutate(HHINCOME = if_else(HHINCOME == 9999999, NA_real_, as.numeric(HHINCOME)))

main_cutoffs <- readRDS(here::here("01_data", "processed", "main_tercile_cutoffs.rds"))
within_tercile_borders <- read_csv(here::here("01_data", "processed", "within_tercile_quantile_borders.csv"), show_col_types = FALSE)
data_with_groups <- assign_income_groups(cleaned_data, within_tercile_borders, "HHINCOME", USER_FINE_GROUP_LEVEL, main_cutoffs$main_cutoff1, main_cutoffs$main_cutoff2)

summary_child_population <- data_with_groups %>%
  filter(!is.na(fine_income_group) & AGE < 18) %>%
  group_by(income_tercile, fine_income_group) %>%
  summarise(n_children_weighted = sum(.data[[USER_WEIGHT_VARIABLE]], na.rm = TRUE), .groups = "drop")

saveRDS(summary_child_population, here::here("01_data", "processed", "summary_acs_child_population.rds"))
message("ACS child population summary saved.")
