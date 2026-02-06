# ==== 0. ABOUT ====
## WD location: 02_Scripts/I-Geo Areas Master File
## Script: I-E_harmonize_nhgis_income_data.R
## Purpose: This script creates the final harmonized geographic income database.
##          It standardizes geographic identifiers, joins granular NHGIS geographies
##          with the 2020 PUMA lookup table, then imputes missing, zero, or
##          top-coded median household incomes using the corrected 7-digit PUMA GEOIDs.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-08-19
## Last Modified: 2025-09-22 (Corrected PUMA lookup join key and standardized NHGIS GEOIDs)
## Dependencies: dplyr, readr, here, purrr, tidyr, stringr
## Input: 01_data/processed/geographic_income_database_nhgis_summary.rds
##        01_data/processed/puma_income_bracket_median_lookup.rds
##        01_data/processed/geography_to_puma_lookup_2020.rds
## Output: 01_data/processed/geographic_income_database_harmonized.rds (R data file)
##         01_data/processed/geographic_income_database_harmonized.csv (CSV file)


# ==== 0. SETUP & ENVIRONMENT PREPARATION ====

# ===== 0.1. Install and Load Required Packages =====
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(here)) install.packages("here")
if (!require(purrr)) install.packages("purrr")
if (!require(tidyr)) install.packages("tidyr")
if (!require(stringr)) install.packages("stringr")

library(dplyr)
library(readr)
library(here)
library(purrr)
library(tidyr)
library(stringr)


# ==== 1. LOAD REQUIRED DATA ====
message("Loading required data files...")

# ===== 1.1. Define File Paths =====
processed_dir <- here("01_data", "processed")
nhgis_summary_path <- file.path(processed_dir, "geographic_income_database_nhgis_summary.rds")
puma_median_lookup_path <- file.path(processed_dir, "puma_income_bracket_median_lookup.rds")
geo_to_puma_lookup_20_path <- file.path(processed_dir, "geography_to_puma_lookup_2020.rds")

# ===== 1.2. Load R Objects =====
load_data_object <- function(path, name) {
  if (!file.exists(path)) {
    stop(paste("Error:", name, "not found at", path, ". Please run previous scripts."))
  }
  readRDS(path)
}

nhgis_summary_df <- load_data_object(nhgis_summary_path, "NHGIS Summary Data")
puma_median_lookup_df <- load_data_object(puma_median_lookup_path, "PUMA Median Lookup Table")
geo_to_puma_lookup_20_df <- load_data_object(geo_to_puma_lookup_20_path, "2020 Geography-to-PUMA Lookup")

message("All required data files loaded successfully.")


# ==== 2. PUMA ASSIGNMENT FOR GRANULAR GEOGRAPHIES ====
message("Assigning PUMAs to granular geographies needing potential imputation...")

# ===== 2.1. Standardize All Geographic Identifiers for Joining =====
# **CORRECTION**: Standardize the NHGIS TL_GEO_ID by removing the prefix to match the TIGER/Line GEOIDs.
# Standardize the geography level names to match the PUMA lookup file.
nhgis_standardized <- nhgis_summary_df %>%
  mutate(
    JOIN_GEO_ID = str_sub(TL_GEO_ID, -11), # Extract the 11-digit FIPS code
    JOIN_GEO_TYPE = case_when(
      geography_level == "Census Tract" ~ "TRACT",
      geography_level == "County Subdivision" ~ "COUSUB",
      geography_level == "Elementary School District" ~ "ELSD",
      geography_level == "Secondary School District" ~ "SCSD",
      geography_level == "Unified School District" ~ "UNSD",
      geography_level == "PUMA" ~ "PUMA",
      geography_level == "Urban Area" ~ "UA",
      geography_level == "ZCTA" ~ "ZCTA",
      TRUE ~ geography_level
    )
  )

# ===== 2.2. Isolate and Join Only Necessary Geographies =====
geos_to_join <- c("TRACT", "COUSUB", "ELSD", "SCSD", "UNSD", "PUMA", "ZCTA")
nhgis_to_join <- nhgis_standardized %>% filter(JOIN_GEO_TYPE %in% geos_to_join)
nhgis_skipped <- nhgis_standardized %>% filter(!JOIN_GEO_TYPE %in% geos_to_join)

nhgis_joined <- nhgis_to_join %>%
  left_join(geo_to_puma_lookup_20_df, by = c("JOIN_GEO_ID" = "GEOID20", "JOIN_GEO_TYPE" = "GEOGRAPHY_TYPE")) %>%
  mutate(PUMA_GEOID20 = if_else(geography_level == "PUMA", JOIN_GEO_ID, PUMA_GEOID20))

# ===== 2.3. Recombine and Finalize PUMA Assignment =====
nhgis_skipped$PUMA_GEOID20 <- NA_character_
nhgis_with_pumas_df <- bind_rows(nhgis_joined, nhgis_skipped)
message("PUMA assignment complete.")


# ==== 3. HARMONIZE AND IMPUTE MEDIAN HOUSEHOLD INCOME ====

# ===== 3.1. Define B19001 Column Names and Brackets =====
b19001_bracket_cols <- paste0("ASQOE", sprintf("%03d", 2:17))
total_hh_col <- "ASQOE001"
income_brackets_16_nhgis <- c(
  "Less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999",
  "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
  "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999",
  "$50,000 to $59,999", "$60,000 to $74,999", "$75,000 to $99,999",
  "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $199,999",
  "$200,000 or more"
)
bracket_map <- setNames(income_brackets_16_nhgis, b19001_bracket_cols)

# ===== 3.2. Find Median Bracket and Prepare for Imputation =====
message("Identifying records for imputation and calculating median brackets...")
nhgis_prepped_for_imputation <- nhgis_with_pumas_df %>%
  mutate(
    imputation_source = case_when(
      is.na(median_household_income) ~ "Missing",
      median_household_income <= 0 ~ "Zero or Negative",
      median_household_income > 250000 ~ "Top-coded",
      TRUE ~ "Original"
    ),
    imputation_flag = if_else(imputation_source != "Original", TRUE, FALSE)
  )

median_bracket_lookup <- nhgis_prepped_for_imputation %>%
  filter(imputation_source %in% c("Missing", "Zero or Negative")) %>%
  select(TL_GEO_ID, !!total_hh_col, all_of(b19001_bracket_cols)) %>%
  pivot_longer(cols = all_of(b19001_bracket_cols), names_to = "bracket_col", values_to = "count") %>%
  mutate(Income_Bracket_ID = bracket_map[bracket_col], count = as.numeric(count)) %>%
  filter(!is.na(count) & count > 0) %>%
  mutate(Income_Bracket_ID = factor(Income_Bracket_ID, levels = income_brackets_16_nhgis)) %>%
  arrange(TL_GEO_ID, Income_Bracket_ID) %>%
  group_by(TL_GEO_ID) %>%
  mutate(cumulative_pct = cumsum(count) / first(!!sym(total_hh_col))) %>%
  filter(cumulative_pct >= 0.5) %>%
  slice_min(order_by = cumulative_pct, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(TL_GEO_ID, Median_Income_Bracket_Calculated = Income_Bracket_ID)

# ===== 3.3. Join Median Bracket Info and Impute Incomes =====
message("Imputing problematic median incomes...")
nhgis_harmonized_df <- nhgis_prepped_for_imputation %>%
  left_join(median_bracket_lookup, by = "TL_GEO_ID") %>%
  mutate(
    Income_Bracket_For_Lookup = case_when(
      imputation_source == "Top-coded" ~ "$250,000 or more",
      TRUE ~ as.character(Median_Income_Bracket_Calculated)
    )
  ) %>%
  # **CORRECTED**: Join using the corrected 'PUMA_GEOID' column from the lookup table
  left_join(
    puma_median_lookup_df,
    by = c("PUMA_GEOID20" = "PUMA_GEOID", "Income_Bracket_For_Lookup" = "Income_Bracket_ID")
  ) %>%
  mutate(
    median_household_income_final = if_else(
      imputation_flag,
      PUMA_Bracket_Median_HHINCOME,
      median_household_income
    )
  )

# ==== 4. FINALIZE AND SAVE THE DATABASE ====
message("Finalizing and saving the harmonized database...")

# ===== 4.1. Select and Rename Final Columns =====
final_database <- nhgis_harmonized_df %>%
  select(
    GISJOIN, TL_GEO_ID, YEAR, STATE, COUNTY, PUMA_GEOID20, geography_level, NAME,
    median_household_income_original = median_household_income,
    median_household_income_final,
    imputation_flag, imputation_source,
    median_household_income_moe,
    all_of(paste0("ASQOE", sprintf("%03d", 1:17))),
    all_of(paste0("ASQOM", sprintf("%03d", 1:17)))
  )

# ===== 4.2. Save Data Files =====
saveRDS(final_database, here("01_data", "processed", "geographic_income_database_harmonized.rds"))
write.csv(final_database, here("01_data", "processed", "geographic_income_database_harmonized.csv"), row.names = FALSE)
message("Final harmonized database files saved.")


# ==== 5. VALIDATION REPORT AND SPOT CHECKS ====
message("\n\n==========================================================")
message("====      FINAL VALIDATION & IMPUTATION REPORT      ====")
message("==========================================================")

total_records <- nrow(final_database)
imputed_records <- sum(final_database$imputation_flag, na.rm = TRUE)
puma_assigned_records <- sum(!is.na(final_database$PUMA_GEOID20))
final_na_count <- sum(is.na(final_database$median_household_income_final))

message(paste("\nTotal Records Processed:", format(total_records, big.mark = ",")))
message(paste("PUMAs Assigned (Granular Geographies Only):", format(puma_assigned_records, big.mark = ",")))
message(paste("Total Records Flagged for Imputation:", format(imputed_records, big.mark = ",")))

imputed_summary <- final_database %>%
  filter(imputation_flag) %>%
  count(imputation_source) %>%
  rename(Reason = imputation_source, Count = n)

message("\n--- Breakdown of Imputed Records by Reason ---")
print(as.data.frame(imputed_summary))

message("\n--- Spot Check 1: Validating a 'Missing' Income Imputation ---")
missing_example <- nhgis_harmonized_df %>% filter(imputation_source == "Missing", !is.na(PUMA_GEOID20)) %>% slice(1)

if (nrow(missing_example) > 0) {
  tl_geoid_check1 <- missing_example$TL_GEO_ID
  puma_check1 <- missing_example$PUMA_GEOID20
  bracket_check1 <- missing_example$Income_Bracket_For_Lookup
  imputed_value1 <- missing_example$median_household_income_final
  
  lookup_value1 <- puma_median_lookup_df %>%
    filter(PUMA_GEOID == puma_check1, Income_Bracket_ID == bracket_check1) %>%
    pull(PUMA_Bracket_Median_HHINCOME)
  
  message(paste("  - Example TL_GEO_ID:", tl_geoid_check1))
  message(paste("  - Original Value:", "NA (Missing)"))
  message(paste("  - Assigned PUMA:", puma_check1))
  message(paste("  - Calculated Median Bracket:", bracket_check1))
  message(paste("  - Value from PUMA Lookup Table:", scales::dollar(lookup_value1, accuracy = 1)))
  message(paste("  - Final Imputed Value:", scales::dollar(imputed_value1, accuracy = 1)))
  
  if (!is.na(imputed_value1) && !is.na(lookup_value1) && round(imputed_value1) == round(lookup_value1)) {
    message("  - ✅ PASS: Imputed value matches the PUMA lookup table value.")
  } else {
    message("  - ❌ FAIL: Imputed value does NOT match the lookup table value.")
  }
} else {
  message("  - No records with 'Missing' income and an assigned PUMA were found to perform a spot check.")
}

message("\n--- Spot Check 2: Validating a 'Top-coded' Income Imputation ---")
topcoded_example <- nhgis_harmonized_df %>% filter(imputation_source == "Top-coded", !is.na(PUMA_GEOID20)) %>% slice(1)

if (nrow(topcoded_example) > 0) {
  tl_geoid_check2 <- topcoded_example$TL_GEO_ID
  original_value2 <- topcoded_example$median_household_income
  puma_check2 <- topcoded_example$PUMA_GEOID20
  bracket_check2 <- topcoded_example$Income_Bracket_For_Lookup
  imputed_value2 <- topcoded_example$median_household_income_final
  
  lookup_value2 <- puma_median_lookup_df %>%
    filter(PUMA_GEOID == puma_check2, Income_Bracket_ID == bracket_check2) %>%
    pull(PUMA_Bracket_Median_HHINCOME)
  
  message(paste("  - Example TL_GEO_ID:", tl_geoid_check2))
  message(paste("  - Original Value:", scales::dollar(original_value2, accuracy = 1)))
  message(paste("  - Assigned PUMA:", puma_check2))
  message(paste("  - Bracket Used for Lookup:", bracket_check2))
  message(paste("  - Value from PUMA Lookup Table:", scales::dollar(lookup_value2, accuracy = 1)))
  message(paste("  - Final Imputed Value:", scales::dollar(imputed_value2, accuracy = 1)))
  
  if (!is.na(imputed_value2) && !is.na(lookup_value2) && round(imputed_value2) == round(lookup_value2)) {
    message("  - ✅ PASS: Imputed value matches the PUMA lookup table value for the top bracket.")
  } else {
    message("  - ❌ FAIL: Imputed value does NOT match the lookup table value.")
  }
} else {
  message("  - No records with 'Top-coded' income and an assigned PUMA were found to perform a spot check.")
}

message(paste("\nTotal Records with NA Final Income:", format(final_na_count, big.mark = ",")))
if (final_na_count > 0) {
  message("Note: Remaining NAs are records that needed imputation but could not be assigned a PUMA.")
  
  final_na_summary <- final_database %>%
    filter(is.na(median_household_income_final)) %>%
    count(geography_level, imputation_source, sort = TRUE)
  
  message("\n--- Breakdown of Records with Final NA Income ---")
  print(as.data.frame(final_na_summary))
}

message("\n==========================================================")
message("Script I-E execution complete.")

