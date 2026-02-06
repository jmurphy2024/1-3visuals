## WD location: 02_Scripts/II- Shared Functions
## Script: II-A Shared Utilities.r
## Purpose: Contains functions for assigning income groups to microdata
##          and generating human-readable codebooks from IPUMS DDI files.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-09-24
## Last Modified: 2025-09-26 (Completed function refactoring from Phase 1 script)
## Dependencies: dplyr, rlang, stringr, readr, here, ipumsr
## Input: A user-provided data frame; foundational border files from II-C.
## Output: A modified data frame or a text codebook file.

# ==== 1. START OF FUNCTION DEFINITIONS ====

#' Assign Income Groups Based on Pre-calculated Borders
#'
#' Adds `income_tercile` and `fine_income_group` columns to a data frame. This is a core
#' function for operationalizing the 1/3 Country framework on microdata.
#'
#' @param data_to_process A data frame containing the income variable.
#' @param borders_df A data frame of within-tercile income borders from `II-C_Border_Setup.R`.
#' @param income_var_name The name of the household income variable in `data_to_process`.
#' @param detail_level The `QuantileGroup` label from `borders_df` (e.g., "Groups_20" for ventiles).
#' @param main_cutoff1 The income border between the Bottom and Middle Thirds.
#' @param main_cutoff2 The income border between the Middle and Top Thirds.
#'
#' @return The input data frame with two additional factor columns.
#' @export
assign_income_groups <- function(data_to_process, borders_df, income_var_name, detail_level, main_cutoff1, main_cutoff2) {
  
  # --- Input Validation ---
  if (!is.data.frame(data_to_process)) stop("Error: 'data_to_process' must be a data frame.")
  if (!is.data.frame(borders_df)) stop("Error: 'borders_df' must be a data frame.")
  if (!income_var_name %in% names(data_to_process)) stop(paste("Error: Income variable '", income_var_name, "' not found."))
  if (!detail_level %in% unique(borders_df$QuantileGroup)) stop(paste("Error: 'detail_level' '", detail_level, "' not found in borders_df."))
  if (main_cutoff1 >= main_cutoff2) stop("Error: 'main_cutoff1' must be less than 'main_cutoff2'.")
  
  income_sym <- rlang::sym(income_var_name)
  
  # --- 1. Assign Main Income Tercile ---
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
  
  # --- 2. Prepare Within-Tercile Borders for Assignment ---
  borders_filtered <- borders_df %>%
    dplyr::filter(QuantileGroup == detail_level) %>%
    dplyr::mutate(income_tercile_num = dplyr::case_when(
      stringr::str_detect(MainTercile, "1|Bottom") ~ 1L,
      stringr::str_detect(MainTercile, "2|Middle") ~ 2L,
      stringr::str_detect(MainTercile, "3|Top") ~ 3L,
      TRUE ~ NA_integer_
    )) %>%
    dplyr::filter(!is.na(income_tercile_num)) %>%
    dplyr::select(income_tercile_num, CutoffValue) %>%
    dplyr::arrange(income_tercile_num, CutoffValue)
  
  # --- 3. Assign Fine Income Group using findInterval ---
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


#' Generate a Human-Readable Text Codebook from an IPUMS DDI File
#'
#' Reads an IPUMS DDI file and extracts metadata for a specified list of
#' variables, writing the output to a text file for easy review.
#'
#' @param ddi_path Character string. The full path to the .xml DDI file.
#' @param vars_list Character vector. A list of variable names to include.
#' @param output_txt_path Character string. The full path for the output .txt file.
#'
#' @return Invisibly returns TRUE if successful, FALSE otherwise.
#' @export
generate_codebook_from_ddi <- function(ddi_path, vars_list, output_txt_path) {
  if (!file.exists(ddi_path)) {
    warning(paste("DDI file not found at:", ddi_path))
    return(invisible(FALSE))
  }
  
  tryCatch({
    ddi_obj <- ipumsr::read_ipums_ddi(ddi_path)
    var_info_all <- ipumsr::ipums_var_info(ddi_obj)
    vars_in_ddi <- intersect(vars_list, var_info_all$var_name)
    
    if(length(vars_in_ddi) == 0){
      warning("None of the requested variables found in the DDI. Skipping codebook.")
      return(invisible(FALSE))
    }
    
    sink(output_txt_path)
    cat(paste("IPUMS Data Extract Codebook\n"))
    cat(paste("Source DDI:", basename(ddi_path), "\nGenerated on:", Sys.time(), "\n"))
    cat("========================================\n\n")
    
    for (var in vars_in_ddi) {
      var_info_single <- var_info_all %>% dplyr::filter(var_name == var)
      cat(paste0("## Variable: ", var, " ##\n"))
      cat(paste("Label:", var_info_single$var_label %||% "N/A", "\n"))
      cat(paste("Description:", var_info_single$var_desc %||% "N/A", "\n"))
      
      val_labels_df <- ipumsr::ipums_val_labels(ddi_obj, var = !!var)
      if (nrow(val_labels_df) > 0) {
        cat("Codes and Labels:\n")
        formatted_labels <- paste0("  ", format(val_labels_df$val), ": ", val_labels_df$lbl)
        cat(paste(formatted_labels, collapse = "\n"))
        cat("\n")
      } else {
        cat("Codes and Labels: (Continuous or string variable)\n")
      }
      cat("----------------------------------------\n\n")
    }
    sink()
    message(paste("... Codebook successfully written to:", output_txt_path))
    return(invisible(TRUE))
    
  }, error = function(e) {
    if(sink.number() > 0) { sink() }
    warning(paste("Error generating codebook:", e$message))
    return(invisible(FALSE))
  })
}


# Helper function for default values
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
}


# ==== 2. END OF FUNCTION DEFINITIONS ====
message("Shared utility functions ('II-A Shared Utilities.r') loaded.")