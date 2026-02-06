## WD location: 02_Scripts/II- Shared Functions
## Script: II-A Shared Utilities.r
## Updates: Optimized for 2023 5-Year REAL_INCOME (Using ADJUST normalization).

library(dplyr); library(rlang); library(stringr); library(readr); library(here); library(ipumsr)

#' Assign Income Groups Based on 5-Year Adjusted Borders
#' @param income_var_name Default is "REAL_INCOME". For 5-year data, this must 
#' be pre-processed using the ADJUST variable.
assign_income_groups <- function(data_to_process, borders_df, income_var_name = "REAL_INCOME", detail_level, main_cutoff1, main_cutoff2) {
  
  # --- Input Validation ---
  if (!is.data.frame(data_to_process)) stop("Error: 'data_to_process' must be a data frame.")
  if (!is.data.frame(borders_df)) stop("Error: 'borders_df' must be a data frame.")
  
  # Ensure the income variable exists in the dataframe
  if (!income_var_name %in% names(data_to_process)) {
    stop(paste("Error: Adjusted income variable '", income_var_name, "' not found."))
  }
  
  income_sym <- rlang::sym(income_var_name)
  
  # --- 1. Assign Main Income Tercile ---
  # UNIVERSE CHECK: This logic preserves NAs for population-based prevalence denominators.
  data_with_groups <- data_to_process %>%
    dplyr::mutate(
      income_tercile_num = dplyr::case_when(
        is.na(!!income_sym) ~ NA_integer_,
        !!income_sym <= main_cutoff1 ~ 1L,
        !!income_sym > main_cutoff1 & !!income_sym <= main_cutoff2 ~ 2L,
        !!income_sym > main_cutoff2 ~ 3L,
        TRUE ~ NA_integer_
      ),
      income_tercile = factor(
        income_tercile_num,
        levels = 1:3,
        labels = c("Tercile 1 (Bottom)", "Tercile 2 (Middle)", "Tercile 3 (Top)")
      )
    )
  
  # --- 2. Assign Fine Income Group (e.g., Groups_20) ---
  borders_filtered <- borders_df %>%
    dplyr::filter(QuantileGroup == detail_level) %>%
    dplyr::mutate(income_tercile_num = dplyr::case_when(
      stringr::str_detect(MainTercile, "1|Bottom") ~ 1L,
      stringr::str_detect(MainTercile, "2|Middle") ~ 2L,
      stringr::str_detect(MainTercile, "3|Top") ~ 3L,
      TRUE ~ NA_integer_
    )) %>%
    dplyr::select(income_tercile_num, CutoffValue) %>%
    dplyr::arrange(income_tercile_num, CutoffValue)
  
  data_with_groups <- data_with_groups %>%
    dplyr::group_by(income_tercile_num) %>%
    dplyr::mutate(
      fine_group_num = if_else(
        !is.na(income_tercile_num),
        {
          current_tercile <- income_tercile_num[1]
          tercile_cutoffs <- borders_filtered %>%
            dplyr::filter(income_tercile_num == current_tercile) %>%
            dplyr::pull(CutoffValue)
          
          if (length(tercile_cutoffs) > 0) {
            findInterval(!!income_sym, tercile_cutoffs) + 1L
          } else {
            NA_integer_
          }
        },
        NA_integer_
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      fine_income_group = if_else(
        !is.na(fine_group_num) & !is.na(income_tercile_num),
        paste0("T", income_tercile_num, "-G", stringr::str_pad(fine_group_num, 2, pad = "0")),
        NA_character_
      )
    ) %>%
    dplyr::select(-fine_group_num, -income_tercile_num)
  
  return(data_with_groups)
}

message("II-A Utilities updated for 2023 5-Year (ADJUST) flow.")