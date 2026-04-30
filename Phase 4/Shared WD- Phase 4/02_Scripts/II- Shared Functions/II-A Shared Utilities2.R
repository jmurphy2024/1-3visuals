# ==============================================================================
# SCRIPT: II-A Shared Utilities2.R
# Purpose: Helper functions for assigning income groups, CPI, and RPP adjustments.
# Updated: Centralized both Spatial (State/Regional), Temporal adjustments, and Summary Stats.
# ==============================================================================

library(dplyr); library(rlang); library(stringr); library(readr); library(here); library(ipumsr)

#' --- 1. Income Class Assignment Utility ---
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

#' --- 2. Inflation Adjustment Utility (Temporal) ---
#' Standard CPI-U-RS Based Multipliers (Baseline 2023)
#' @param data_year The year the data was collected.
#' @param base_year The target year for dollar value (default 2023).
get_inflation_multiplier <- function(data_year, base_year = 2023) {
  # Multipliers based on official BLS/Census inflation indices
  multipliers <- list(
    "2024" = 0.970, # Deflate 2024 to 2023 dollars
    "2023" = 1.000, # Baseline
    "2022" = 1.041, # Inflate 2022 to 2023 dollars
    "2021" = 1.124, 
    "2020" = 1.177,
    "2019" = 1.192,
    "2018" = 1.214, # <-- ADDED 2018
    "2017" = 1.243, # <-- ADDED 2017
    "2016" = 1.270  # <-- ADDED 2016
  )
  
  # Fetch data year
  data_val <- multipliers[[as.character(data_year)]]
  if (is.null(data_val)) stop(paste("CPI data not available for data_year:", data_year))
  
  # Fetch base year
  base_val <- multipliers[[as.character(base_year)]]
  if (is.null(base_val)) stop(paste("CPI data not available for base_year:", base_year))
  
  # Calculate exact ratio (Allows you to change your base year anytime!)
  return(data_val / base_val)
}

#' --- 3. Regional Price Parity (RPP) Utilities (Spatial) ---
#' These functions return the spatial adjustment multiplier for a given geography.
#' Formula is (100 / RPP). Baseline national average is 100.

#' 3A. REGIONAL RPP (For use with GSS / Region-Level Data)
#' @param region_id Numeric region code (1=Northeast, 2=Midwest, 3=South, 4=West)
get_regional_rpp_multiplier <- function(region_id) {
  dplyr::case_match(
    region_id,
    1 ~ 100 / 105.2, # Northeast
    2 ~ 100 / 92.8,  # Midwest
    3 ~ 100 / 95.4,  # South
    4 ~ 100 / 104.1, # West
    .default = 1.0   # Default to national average if missing/unknown
  )
}

#' 3B. STATE RPP (For use with ACS / State-Level FIPS Data)
#' @param state_fips Numeric STATEFIP code from Census/IPUMS
get_state_rpp_multiplier <- function(state_fips) {
  # Based on the 2023 official Bureau of Economic Analysis (BEA) RPP values
  dplyr::case_match(
    state_fips,
    1  ~ 100 / 89.9,  # Alabama
    2  ~ 100 / 101.7, # Alaska
    4  ~ 100 / 101.1, # Arizona
    5  ~ 100 / 86.5,  # Arkansas
    6  ~ 100 / 112.6, # California
    8  ~ 100 / 101.4, # Colorado
    9  ~ 100 / 103.7, # Connecticut
    10 ~ 100 / 101.9, # Delaware
    11 ~ 100 / 112.1, # Dist. of Columbia
    12 ~ 100 / 103.5, # Florida
    13 ~ 100 / 96.7,  # Georgia
    15 ~ 100 / 108.6, # Hawaii
    16 ~ 100 / 91.8,  # Idaho
    17 ~ 100 / 98.9,  # Illinois
    18 ~ 100 / 90.9,  # Indiana
    19 ~ 100 / 89.9,  # Iowa
    20 ~ 100 / 91.1,  # Kansas
    21 ~ 100 / 88.6,  # Kentucky
    22 ~ 100 / 90.8,  # Louisiana
    23 ~ 100 / 97.5,  # Maine
    24 ~ 100 / 105.7, # Maryland
    25 ~ 100 / 108.2, # Massachusetts
    26 ~ 100 / 92.4,  # Michigan
    27 ~ 100 / 98.3,  # Minnesota
    28 ~ 100 / 87.4,  # Mississippi
    29 ~ 100 / 90.1,  # Missouri
    30 ~ 100 / 92.5,  # Montana
    31 ~ 100 / 89.5,  # Nebraska
    32 ~ 100 / 98.4,  # Nevada
    33 ~ 100 / 105.1, # New Hampshire
    34 ~ 100 / 108.9, # New Jersey
    35 ~ 100 / 91.2,  # New Mexico
    36 ~ 100 / 108.4, # New York
    37 ~ 100 / 94.8,  # North Carolina
    38 ~ 100 / 89.2,  # North Dakota
    39 ~ 100 / 91.5,  # Ohio
    40 ~ 100 / 89.5,  # Oklahoma
    41 ~ 100 / 103.2, # Oregon
    42 ~ 100 / 96.9,  # Pennsylvania
    44 ~ 100 / 101.2, # Rhode Island
    45 ~ 100 / 92.5,  # South Carolina
    46 ~ 100 / 89.5,  # South Dakota
    47 ~ 100 / 91.1,  # Tennessee
    48 ~ 100 / 97.5,  # Texas
    49 ~ 100 / 96.1,  # Utah
    50 ~ 100 / 100.4, # Vermont
    51 ~ 100 / 101.5, # Virginia
    53 ~ 100 / 109.0, # Washington
    54 ~ 100 / 87.9,  # West Virginia
    55 ~ 100 / 92.6,  # Wisconsin
    56 ~ 100 / 92.6,  # Wyoming
    .default = 1.0    # Default to national average if missing/unknown
  )
}

#' --- 4. Summary Statistics Utility ---
#' Calculates True Weighted Min, Median, Average, and Max.
#' Returns BOTH the Overall Country (Tercile) summary AND the 4 internal Income Quartiles.
#' @param df The prepared dataframe containing your target variable and weights.
#' @param target_var The name of the column you are summarizing (as a string).
#' @param weight_var The name of the weight column (default is "PERWT").
#' @param income_var The income variable used to create quartiles (default "REAL_INCOME").
#' @param target_country Optional: "Bottom Third", "Middle Third", or "Top Third" to isolate one.
get_country_summary <- function(df, target_var, weight_var = "PERWT", income_var = "REAL_INCOME", target_country = NULL) {
  
  # Input validation
  if (!target_var %in% names(df)) stop(paste("Error: Variable", target_var, "not found."))
  if (!weight_var %in% names(df)) stop(paste("Error: Weight variable", weight_var, "not found."))
  if (!"Country" %in% names(df)) stop("Error: 'Country' column not found.")
  if (!income_var %in% names(df)) stop(paste("Error: Income variable", income_var, "not found."))
  
  # Filter to a specific country if requested
  if (!is.null(target_country)) {
    df <- df %>% dplyr::filter(Country == target_country)
  }
  
  # Clean base data
  df_clean <- df %>%
    dplyr::filter(!is.na(.data[[target_var]]), !is.na(.data[[weight_var]]), .data[[weight_var]] > 0)
  
  # ==========================================
  # PART 1: OVERALL TERCILE (COUNTRY) SUMMARY
  # ==========================================
  summary_overall <- df_clean %>%
    dplyr::arrange(Country, .data[[target_var]]) %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(
      cum_weight_target = cumsum(.data[[weight_var]]),
      tot_weight_target = sum(.data[[weight_var]])
    ) %>%
    dplyr::summarise(
      Income_Quartile  = "Overall",
      Total_Population = sum(.data[[weight_var]], na.rm = TRUE),
      Minimum          = min(.data[[target_var]], na.rm = TRUE),
      Weighted_Median  = .data[[target_var]][which(cum_weight_target >= tot_weight_target / 2)[1]],
      Weighted_Average = weighted.mean(.data[[target_var]], w = .data[[weight_var]], na.rm = TRUE),
      Maximum          = max(.data[[target_var]], na.rm = TRUE),
      .groups          = "drop"
    )
  
  # ==========================================
  # PART 2: INTERNAL QUARTILE SUMMARY
  # ==========================================
  summary_quartiles <- df_clean %>%
    # Sort by Income to group poorest to richest
    dplyr::arrange(Country, .data[[income_var]]) %>%
    dplyr::group_by(Country) %>%
    dplyr::mutate(
      cum_w = cumsum(.data[[weight_var]]),
      tot_w = sum(.data[[weight_var]]),
      Income_Quartile = dplyr::case_when(
        cum_w <= 0.25 * tot_w ~ "Q1 (Lowest 25%)",
        cum_w <= 0.50 * tot_w ~ "Q2 (Second 25%)",
        cum_w <= 0.75 * tot_w ~ "Q3 (Third 25%)",
        TRUE                  ~ "Q4 (Highest 25%)"
      )
    ) %>%
    dplyr::ungroup() %>%
    # Re-sort by the TARGET variable to correctly calculate the cumulative median
    dplyr::arrange(Country, Income_Quartile, .data[[target_var]]) %>%
    dplyr::group_by(Country, Income_Quartile) %>%
    dplyr::mutate(
      cum_weight_target = cumsum(.data[[weight_var]]),
      tot_weight_target = sum(.data[[weight_var]])
    ) %>%
    dplyr::summarise(
      Total_Population = sum(.data[[weight_var]], na.rm = TRUE),
      Minimum          = min(.data[[target_var]], na.rm = TRUE),
      Weighted_Median  = .data[[target_var]][which(cum_weight_target >= tot_weight_target / 2)[1]],
      Weighted_Average = weighted.mean(.data[[target_var]], w = .data[[weight_var]], na.rm = TRUE),
      Maximum          = max(.data[[target_var]], na.rm = TRUE),
      .groups          = "drop"
    )
  
  # ==========================================
  # PART 3: COMBINE AND FORMAT
  # ==========================================
  final_summary <- dplyr::bind_rows(summary_overall, summary_quartiles) %>%
    dplyr::mutate(
      # Force the sort order so "Overall" is pinned to the top of each block
      Income_Quartile = factor(Income_Quartile, levels = c(
        "Overall", "Q1 (Lowest 25%)", "Q2 (Second 25%)", "Q3 (Third 25%)", "Q4 (Highest 25%)"
      ))
    ) %>%
    dplyr::arrange(Country, Income_Quartile)
  
  return(final_summary)
}