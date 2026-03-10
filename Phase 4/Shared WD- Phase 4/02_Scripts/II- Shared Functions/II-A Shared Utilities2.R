# ==============================================================================
# SCRIPT: II-A Shared Utilities2.R
# Purpose: Helper functions for assigning income groups and inflation adjustments.
# Updated: Aligned to V2 Aggregated Logic & Added Inflation Utility
# ==============================================================================

library(dplyr); library(rlang); library(stringr); library(readr); library(here); library(ipumsr)

#' Assign Income Groups Based on Person-Weighted Borders
#' @param data_to_process The dataframe containing the population (must have REAL_INCOME).
#' @param main_cutoff1 The $ threshold for the Bottom Third (from V2 RDS).
#' @param main_cutoff2 The $ threshold for the Middle Third (from V2 RDS).
#' @param income_var_name Default "REAL_INCOME".
assign_income_groups <- function(data_to_process, main_cutoff1, main_cutoff2, income_var_name = "REAL_INCOME") {
  
  # --- Input Validation ---
  if (!is.data.frame(data_to_process)) stop("Error: 'data_to_process' must be a data frame.")
  if (!income_var_name %in% names(data_to_process)) stop(paste("Error: Variable '", income_var_name, "' not found."))
  
  income_sym <- rlang::sym(income_var_name)
  
  message("Step 1: Assigning Main Terciles (Three Countries)...")
  data_with_groups <- data_to_process %>%
    dplyr::mutate(
      income_tercile = dplyr::case_when(
        !!income_sym <= main_cutoff1 ~ "Tercile 1 (Bottom)",
        !!income_sym <= main_cutoff2 ~ "Tercile 2 (Middle)",
        TRUE ~ "Tercile 3 (Top)"
      )
    )
  
  message("Step 2: Applying 10-Decile Smoothing per Country...")
  final_data <- data_with_groups %>%
    dplyr::group_by(income_tercile) %>%
    # Dynamically slice each country into 10 equal demographic buckets
    dplyr::mutate(decile = dplyr::ntile(!!income_sym, 10)) %>%
    dplyr::ungroup()
  
  return(final_data)
}

#' --- Inflation Adjustment Utility ---
#' Standard CPI-U-RS Based Multipliers (Baseline 2023)
#' @param data_year The year the data was collected.
#' @param base_year The target year for dollar value (default 2023).
get_inflation_multiplier <- function(data_year, base_year = 2023) {
  # Multipliers based on official BLS/Census inflation indices
  multipliers <- list(
    "2024" = 0.970, # Deflate 2024 to 2023 dollars
    "2023" = 1.000, # Baseline
    "2022" = 1.041,
    "2021" = 1.124,
    "2020" = 1.177,
    "2019" = 1.192
  )
  
  val <- multipliers[[as.character(data_year)]]
  
  # Default to no adjustment (1.0) if year is not in the predefined list
  if(is.null(val)) {
    warning(paste("Inflation data for year", data_year, "not found. Using 1.0 multiplier."))
    return(1.0)
  }
  
  return(val)
}