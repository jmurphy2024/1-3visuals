## WD location: 02_Scripts/IV- Variable Visual Scripts/Urban Institute Education Data/Gifted_Fiber_Certified
## Script: UIED_gfc_acquire.R
## Purpose: Downloads specified datasets for Teacher Certification, Internet,
##          and Advanced Coursework from the Urban Institute Education Data portal.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-10-02
## Last Modified: 2025-10-02 (Shortened indicator name to prevent long path errors)

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(educationdata); library(dplyr); library(here); library(httr); library(purrr)

# ================================================================= #
# ==== 1. SCRIPT CONFIGURATION (Generated from User Inputs) ====
# ================================================================= #

USER_YEAR <- 2017
## MODIFICATION: Shortened name to prevent Windows path length errors.
USER_INDICATOR_NAME <- "gfc" 

datasets_to_download <- list(
  directory = list(level = "school-districts", source = "ccd", topic = "directory"),
  nhgis_geo = list(level = "schools", source = "nhgis", topic = "census-2010"),
  certified_teachers = list(level = "schools", source = "crdc", topic = "teachers-staff", subtopic = NULL),
  internet_access = list(level = "schools", source = "crdc", topic = "internet-access", subtopic = NULL, filters = list(year = 2020)),
  advanced_coursework = list(level = "schools", source = "crdc", topic = "ap-ib-enrollment", subtopic = c("race", "sex")),
  enrollment_crdc = list(level = "schools", source = "crdc", topic = "enrollment", subtopic = c("race", "sex"))
)

# ================================================================= #
# ==== 2. GENERIC LOGIC (No changes needed below this line) ====
# ================================================================= #

OUTPUT_RAW_DIR <- here::here("01_data", "raw", "Urban Institute Education Data", USER_INDICATOR_NAME, USER_YEAR)
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

get_ui_data <- function(level, source, topic, subtopic = NULL, filters = list(year = USER_YEAR), ...) {
  year_to_download <- filters$year %||% USER_YEAR
  message(sprintf("-> Downloading data for: %s (source: %s, level: %s, year: %d)", topic, source, level, year_to_download))
  
  res <- tryCatch({
    httr::with_config(httr::timeout(300), {
      get_education_data(level = level, source = source, topic = topic, subtopic = subtopic, filters = filters)
    })
  }, error = function(e) e)
  
  if (inherits(res, "error")) { message(paste("\n--- DOWNLOAD FAILED for topic:", topic, "---\nError:", res$message)); return(NULL) }
  message(paste("   ...Success. Downloaded", nrow(res), "rows.")); return(res)
}

raw_datasets <- purrr::map(datasets_to_download, ~do.call(get_ui_data, .x))

purrr::iwalk(raw_datasets, ~{
  if (!is.null(.x) && is.data.frame(.x)) {
    file_year <- datasets_to_download[[.y]]$filters$year %||% USER_YEAR
    file_name <- paste0("raw_uied_", .y, "_", file_year, ".rds")
    saveRDS(.x, file = file.path(OUTPUT_RAW_DIR, file_name))
    message(paste("   ...Saved:", file_name))
  }
})

message("\nData acquisition script complete.")