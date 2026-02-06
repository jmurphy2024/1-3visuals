# ==== 0. ABOUT ====
## WD location: 02_Scripts/II- Shared Functions
## Script: II-C_Border_Setup.R
## Purpose: Calculates and saves the definitive income borders for the 1/3 Country Project.
##          This script uses a minimal IPUMS USA (ACS) microdata sample as the single
##          source of truth for income terciles and the finer-grained quantiles
##          within them. The output files are foundational for all subsequent analyses.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-09-24
## Last Modified: 2025-09-25 (Added stringr library to fix validation report error)
## Dependencies: ipumsr, dplyr, readr, srvyr, survey, rlang, tibble, purrr, here, stringr
## Input: IPUMS USA ACS microdata (via API).
## Output: 01_data/processed/main_tercile_cutoffs.rds
##         01_data/processed/within_tercile_quantile_borders.csv


# Clear environment at the start of the session/script
rm(list = ls())
gc() # Explicitly call garbage collector


# ==== 1. PROJECT SETUP ====
# Purpose: Configure the R session environment.

# ===== 1.1. Load Core Packages =====
# Load packages essential for setup and definition stages.
library(ipumsr)
library(dplyr)
library(readr)
library(srvyr)
library(survey)
library(rlang)
library(tibble)
library(purrr)
library(here)
library(stringr) # Added to ensure str_extract() is available for the validation report

print("Section 1.1: Core packages loaded.")


# ===== 1.2. Define Global Parameters & File Paths =====
# Define parameters and paths consistently used across the project scripts using here().

# ====== 1.2.1. IPUMS Sample Details ======
# Define the specific IPUMS sample ID required for the analysis.
ACS_SAMPLE_ID <- "us2023a" # Primary source for income borders

print(paste("Target ACS Sample for Borders:", ACS_SAMPLE_ID))


# ====== 1.2.2. Project File Paths & Output Files ======
# Define paths relative to the project root using here::here()
processed_data_dir <- here::here("01_data", "processed")
ipums_download_dir <- here::here("01_data", "raw", "ipums_downloads") # For storing raw extracts

# Ensure directories exist
dir.create(processed_data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(ipums_download_dir, showWarnings = FALSE, recursive = TRUE)

# --- Key Output File Names (Saved to processed_data_dir) ---
income_borders_file <- file.path(processed_data_dir, "within_tercile_quantile_borders.csv")
main_cutoffs_file   <- file.path(processed_data_dir, "main_tercile_cutoffs.rds")

print(paste("Project Root (identified by here package):", here::here()))
print(paste("Income borders (within tercile) will be saved to:", income_borders_file))
print(paste("Main tercile cutoffs will be saved to:", main_cutoffs_file))


# ====== 1.2.3. Core Variable Names Checklist ======
# Define standard names for key variables used in this script.
HH_INCOME_VAR <- "HHINCOME"
HH_WEIGHT_VAR <- "HHWT"
CLUSTER_VAR   <- "CLUSTER" # Verify Name for your ACS sample
STRATA_VAR    <- "STRATA"  # Verify Name for your ACS sample


# ====== 1.2.4. Analysis Parameters ======
NUM_TERCILES <- 3

print("Section 1.2: Global parameters and file paths defined using here().")


# ==== 2. DEFINE AND ACQUIRE BORDER DATA ====
# Purpose: Calculate income thresholds for main terciles and finer ranges
#          using minimal primary data (ACS), then save these borders for later use.

# ===== 2.1. Define Minimal Extract for Income Borders =====
# Variables needed ONLY to define income borders from the primary ACS sample.
border_vars <- c(
  HH_INCOME_VAR,
  HH_WEIGHT_VAR,
  CLUSTER_VAR,
  STRATA_VAR
)
border_vars <- unique(border_vars) # Ensure no duplicates

print(paste("Defining minimal extract for sample:", ACS_SAMPLE_ID))
minimal_extract_def <- define_extract_micro(
  description = "1/3 Country Project - Minimal data for income borders",
  collection = "usa",
  samples = ACS_SAMPLE_ID,
  variables = border_vars
)


# ===== 2.2. Verify API Key and Submit Extract =====
if (Sys.getenv("IPUMS_API_KEY") == "") {
  stop(
    "IPUMS API key not found.\n",
    "Please set your IPUMS API key using the New User Setup Protocol before proceeding.\n",
    "Script execution halted because the API key is missing."
  )
} else {
  print("Section 2.2: IPUMS API key found successfully via environment variable.")
}

print("Submitting minimal extract request...")
submitted_minimal_extract <- tryCatch({
  submit_extract(minimal_extract_def)
}, error = function(e) {
  stop("Error submitting IPUMS extract request: ", e$message)
})
extract_number_borders <- submitted_minimal_extract$number
print(paste("Minimal extract submitted. Extract Number:", extract_number_borders))


# ===== 2.3. Wait for and Download Extract =====
print(paste("Waiting for minimal extract", extract_number_borders, "to be ready..."))
ready_minimal_extract <- tryCatch({
  wait_for_extract(submitted_minimal_extract)
}, error = function(e) {
  stop("Error waiting for IPUMS extract: ", e$message)
})
print("Minimal extract is ready.")

# Create a dataset-specific subdirectory for the download
download_subdir_name_borders <- paste0("ACS_Border_Extract_", extract_number_borders, "_", Sys.Date())
download_dir_specific_borders <- file.path(ipums_download_dir, download_subdir_name_borders)
dir.create(download_dir_specific_borders, showWarnings = FALSE, recursive = TRUE)

print(paste("Downloading minimal extract to:", download_dir_specific_borders))
minimal_files <- tryCatch({
  download_extract(ready_minimal_extract, download_dir = download_dir_specific_borders, overwrite = TRUE)
}, error = function(e) {
  stop("Error downloading IPUMS extract: ", e$message)
})


# ===== 2.4. Load and Clean Downloaded Data =====
print("Attempting to load IPUMS data using the DDI file...")

# Find the .xml file path within the vector of downloaded file paths.
ddi_file_path_borders <- minimal_files[grep("\\.xml$", minimal_files)]

if (length(ddi_file_path_borders) == 0 || !file.exists(ddi_file_path_borders)) {
  stop(paste("FATAL ERROR: Could not find downloaded DDI (.xml) file in directory:", download_dir_specific_borders))
}
print(paste("... DDI file path identified:", ddi_file_path_borders))

ipums_data <- tryCatch({
  read_ipums_micro(ddi_file_path_borders, verbose = FALSE)
}, error = function(e) {
  stop("Error reading IPUMS data with read_ipums_micro: ", e$message)
})
print(paste("Successfully loaded IPUMS data. Dimensions:", paste(dim(ipums_data), collapse = " x ")))

# --- Clean HHINCOME Variable ---
if (!HH_INCOME_VAR %in% names(ipums_data)) { stop(paste("FATAL ERROR: Required income variable", HH_INCOME_VAR, "not found.")) }
income_codes_to_na_borders <- c(9999999, 9999998) # Common IPUMS missing codes
ipums_data <- ipums_data %>%
  mutate(
    !!HH_INCOME_VAR := as.numeric(!!sym(HH_INCOME_VAR)),
    !!HH_INCOME_VAR := if_else(!!sym(HH_INCOME_VAR) %in% income_codes_to_na_borders | !!sym(HH_INCOME_VAR) <= 0, NA_real_, !!sym(HH_INCOME_VAR))
  )
print(paste("... HHINCOME NAs after cleaning (including <=0):", sum(is.na(ipums_data[[HH_INCOME_VAR]]))))


# ==== 3. CALCULATE INCOME BORDERS ====

# ===== 3.1. Create Survey Design Object =====
required_design_vars <- c(CLUSTER_VAR, STRATA_VAR, HH_WEIGHT_VAR, HH_INCOME_VAR)
if (!all(required_design_vars %in% names(ipums_data))) {
  stop("FATAL ERROR: Missing required variables for survey design: ", paste(setdiff(required_design_vars, names(ipums_data)), collapse=", "))
}

print("Creating survey design object...")
ipums_data_design <- ipums_data %>%
  mutate(across(all_of(c(CLUSTER_VAR, STRATA_VAR, HH_WEIGHT_VAR)), as.numeric)) %>%
  filter(!is.na(!!sym(HH_WEIGHT_VAR)) & !!sym(HH_WEIGHT_VAR) > 0 & !is.na(!!sym(HH_INCOME_VAR)))

if(nrow(ipums_data_design) == 0) { stop("FATAL ERROR: No valid observations for survey design after filtering.")}

survey_design <- svydesign(
  ids = as.formula(paste0("~", CLUSTER_VAR)),
  strata = as.formula(paste0("~", STRATA_VAR)),
  weights = as.formula(paste0("~", HH_WEIGHT_VAR)),
  data = ipums_data_design,
  nest = TRUE
)
print("Survey design object created.")


# ===== 3.2. Calculate and Save Main Tercile Borders =====
print("Calculating main tercile borders using survey::svyquantile...")
hh_income_sym <- rlang::sym(HH_INCOME_VAR)
main_tercile_probs <- c(1/3, 2/3)

quantile_result_svy <- survey::svyquantile(
  x = stats::as.formula(paste0("~", rlang::as_string(hh_income_sym))),
  design = survey_design,
  quantiles = main_tercile_probs,
  na.rm = TRUE,
  ci = FALSE
)
quantile_values <- quantile_result_svy[[1]][1,]

if (!is.null(quantile_values) && is.numeric(quantile_values) && length(quantile_values) == 2) {
  main_tercile_cutoffs <- list(
    main_cutoff1 = quantile_values[1],
    main_cutoff2 = quantile_values[2]
  )
  saveRDS(main_tercile_cutoffs, file = main_cutoffs_file)
  print(paste("Main tercile cutoffs saved to:", main_cutoffs_file))
} else {
  stop("Main tercile quantile values could not be correctly extracted.")
}


# ===== 3.3. Calculate and Save Within-Tercile Borders =====

# ------ 3.3.1. Helper function to calculate borders within a group ------
calculate_within_borders <- function(tercile_design, income_sym, tercile_label) {
  if (is.null(tercile_design) || !inherits(tercile_design, "survey.design") || nrow(tercile_design$variables) == 0) {
    warning(paste("Invalid/empty design for", tercile_label, ". Skipping."))
    return(NULL)
  }
  
  quantile_defs <- list(
    "Groups_3" = list(probs = c(1/3, 2/3)),
    "Groups_4" = list(probs = c(1/4, 2/4, 3/4)),
    "Groups_5" = list(probs = c(1/5, 2/5, 3/5, 4/5)),
    "Groups_10" = list(probs = seq(0.1, 0.9, by = 0.1)),
    "Groups_20" = list(probs = seq(0.05, 0.95, by = 0.05))
  )
  
  print(paste("--- Calculating borders for main tercile:", tercile_label, "---"))
  
  purrr::map_dfr(quantile_defs, .id = "QuantileGroup", function(q_info) {
    q_values_raw <- tryCatch({
      survey::svyquantile(
        x = stats::as.formula(paste0("~", rlang::as_string(income_sym))),
        design = tercile_design,
        quantiles = q_info$probs,
        na.rm = TRUE,
        ci = FALSE
      )
    }, error = function(e) { NULL })
    
    if (!is.null(q_values_raw)) {
      tibble::tibble(
        QuantileProbability = q_info$probs,
        CutoffValue = q_values_raw[[1]][1,]
      )
    } else {
      tibble()
    }
  })
}

# ------ 3.3.2. Create tercile subsets and calculate borders ------
print("Creating tercile subsets and calculating within-tercile borders...")
survey_design_with_terciles <- survey_design %>%
  as_survey() %>%
  mutate(
    income_tercile_group = case_when(
      !!hh_income_sym <= main_tercile_cutoffs$main_cutoff1 ~ "Tercile 1 (Bottom)",
      !!hh_income_sym <= main_tercile_cutoffs$main_cutoff2 ~ "Tercile 2 (Middle)",
      TRUE ~ "Tercile 3 (Top)"
    )
  )

all_borders_df <- survey_design_with_terciles %>%
  group_by(income_tercile_group) %>%
  summarise(
    borders = list(calculate_within_borders(cur_svy(), hh_income_sym, first(income_tercile_group)))
  ) %>%
  rename(MainTercile = income_tercile_group) %>%
  tidyr::unnest(borders)

# ------ 3.3.3. Export the combined border data ------
readr::write_csv(all_borders_df, income_borders_file)
print(paste("Within-tercile borders exported to:", income_borders_file))


# ==== 4. VALIDATION REPORT ====
message("\n\n=======================================================")
message("====      INCOME BORDER VALIDATION REPORT      ====")
message("=======================================================")

# ===== 4.1. Display Main Tercile Borders =====
if (exists("main_tercile_cutoffs")) {
  message("\n--- Main Tercile Cutoffs (Weighted) ---")
  message(paste("End of Bottom Third (T1):", scales::dollar(main_tercile_cutoffs$main_cutoff1, accuracy = 1)))
  message(paste("End of Middle Third (T2):", scales::dollar(main_tercile_cutoffs$main_cutoff2, accuracy = 1)))
  message("---------------------------------------")
} else {
  message("\n--- Main Tercile Cutoffs: Not available for display. ---")
}

# ===== 4.2. Display Within-Tercile Borders =====
if (file.exists(income_borders_file)) {
  message("\n--- Within-Tercile Quantile Borders (Weighted) ---")
  
  # Read the saved file for validation
  border_results_for_display <- readr::read_csv(income_borders_file, show_col_types = FALSE)
  
  # Format for better console output
  border_results_formatted <- border_results_for_display %>%
    mutate(
      Quantile = paste0(str_extract(QuantileGroup, "Groups_\\d+"), " - P", QuantileProbability * 100),
      CutoffValue_Formatted = scales::dollar(CutoffValue, accuracy = 1)
    ) %>%
    select(MainTercile, Quantile, CutoffValue_Formatted) %>%
    tidyr::pivot_wider(names_from = MainTercile, values_from = CutoffValue_Formatted)
  
  print(as.data.frame(border_results_formatted))
  message("--------------------------------------------------")
  
} else {
  message("\n--- Within-Tercile Borders: Output file not found for display. ---")
}


# ==== 5. CLEAN UP MEMORY ====
print("Cleaning up large objects...")
objects_to_remove <- c(
  "ipums_data", "ipums_data_design", "survey_design", "survey_design_with_terciles",
  "ready_minimal_extract", "minimal_files", "submitted_minimal_extract"
)

print("Memory cleanup complete.")

message("\n=======================================================")
message("Script II-C execution complete.")

