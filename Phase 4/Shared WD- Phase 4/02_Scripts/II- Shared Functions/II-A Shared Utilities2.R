# ==============================================================================
# SCRIPT: II-A Shared Utilities.R
# Purpose: Helper functions for assigning income groups.
# Updated: Aligned to V2 Aggregated Logic & 10-Decile Per Country Standard
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