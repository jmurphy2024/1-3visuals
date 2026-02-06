## WD location: 02_Scripts/III-Data Prep Templates/Urban Institute Education Data
## Script: UEID_absence_suspensions_acquire_data.R
## Purpose: Downloads all necessary raw datasets (enrollment, suspensions,
##          chronic absenteeism, directory, and NHGIS geo data) from the
##          Urban Institute Education Data portal for a specified year and
##          validates the downloads.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-01
## Last Modified: 2025-10-01 (Moved environment clearing to the start of the script)
## Dependencies: educationdata, dplyr, here, httr, purrr
## Input: Urban Institute & NHGIS data via API.
## Output: Five separate raw RDS files in `01_data/raw/Urban Institute Education Data/[YEAR]/`

# ==== 0. SETUP ====
# ===== 0.1. Clear Environment =====
rm(list = ls())
gc()

# ===== 0.2. Load Libraries =====
library(educationdata)
library(dplyr)
library(here)
library(httr)
library(purrr)

message("Setup complete. Environment cleared and libraries loaded.")


# ==== 1. USER CONFIGURATION ====
USER_YEAR <- 2017

# Define a dedicated directory for this indicator's raw files
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "Urban Institute Education Data", USER_YEAR)
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

message(paste("Data will be downloaded for the year:", USER_YEAR))
message(paste("Raw output files will be saved to:", OUTPUT_RAW_DIR))


# ==== 2. DATA ACQUISITION ====
# ===== 2.1. Simplified Helper Function for Direct API Downloads =====
get_ui_data <- function(level, source, topic, subtopic = NULL, filters = list(year = USER_YEAR), ...) {
  message(sprintf("-> Downloading data for: %s (source: %s, level: %s)", topic, source, level))
  
  res <- tryCatch({
    httr::with_config(httr::timeout(300), {
      get_education_data(level = level,
                         source = source,
                         topic = topic,
                         subtopic = subtopic,
                         filters = filters
      )
    })
  }, error = function(e) e)
  
  if (inherits(res, "error")) {
    message(paste("\n--- API DOWNLOAD FAILED for topic:", topic, "---\nError:", res$message))
    return(NULL)
  }
  
  message(paste("   ...Success. Downloaded", nrow(res), "rows."))
  return(res)
}


# ===== 2.2. Perform All Data Pulls =====
message("Starting data acquisition from Urban Institute API...")

datasets_to_download <- list(
  enrollment = list(level = "schools", source = "ccd", topic = "enrollment"),
  suspensions = list(level = "schools", source = "crdc", topic = "suspensions-days", subtopic = c("disability", "sex")),
  absence = list(level = "schools", source = "crdc", topic = "chronic-absenteeism", subtopic = c("race", "sex")),
  directory = list(level = "school-districts", source = "ccd", topic = "directory"),
  nhgis_geo = list(level = "schools", source = "nhgis", topic = "census-2010")
)

raw_datasets <- purrr::map(datasets_to_download, ~do.call(get_ui_data, .x))


# ==== 3. SAVE RAW DATASETS ====
message("\nSaving individual raw datasets...")

purrr::iwalk(raw_datasets, ~{
  if (!is.null(.x) && is.data.frame(.x)) {
    file_name <- paste0("raw_uied_", .y, "_", USER_YEAR, ".rds")
    output_path <- file.path(OUTPUT_RAW_DIR, file_name)
    saveRDS(.x, file = output_path)
    message(paste("   ...Saved:", file_name))
  } else {
    message(paste("   ...Skipping save for '", .y, "' as download failed or returned empty."))
  }
})

message("...File saving process complete.")


# ==== 4. VALIDATION REPORT ====
message("\n\n=======================================================")
message("====      DOWNLOAD VALIDATION REPORT           ====")
message("=======================================================")

validation_results <- purrr::map(names(datasets_to_download), ~{
  data_obj <- raw_datasets[[.x]]
  is_valid <- !is.null(data_obj) && is.data.frame(data_obj) && nrow(data_obj) > 0
  
  status_message <- if (is_valid) {
    paste0("✅ SUCCESS (", format(nrow(data_obj), big.mark = ","), " rows)")
  } else {
    "❌ FAILED"
  }
  
  list(
    dataset = .x,
    status = status_message,
    is_valid = is_valid
  )
})

purrr::walk(validation_results, ~message(sprintf("%-15s: %s", .x$dataset, .x$status)))

failures <- purrr::keep(validation_results, ~!.x$is_valid)

if (length(failures) > 0) {
  failed_names <- purrr::map_chr(failures, "dataset")
  stop(paste("\nHalting script. The following datasets failed to download correctly:", paste(failed_names, collapse = ", ")), call. = FALSE)
} else {
  message("\nAll datasets downloaded and validated successfully.")
}
message("=======================================================\n")
message("Data acquisition script complete.")