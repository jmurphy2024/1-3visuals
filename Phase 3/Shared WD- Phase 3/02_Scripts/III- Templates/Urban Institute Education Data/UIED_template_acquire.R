# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: UIED_template_acquire.R
## Purpose: Downloads all 4 Lifelong Learning indicators at the national level. Utilizes bulk CSV downloads for maximum speed.
## Author: Janica Murphy/ Gemini
## Date Created: 2026-01-23
## Dependencies: educationdata, httr, dplyr, here, readr


# ==== 0. SETUP ====
rm(list = ls()); gc()
library(educationdata); library(dplyr); library(here); library(purrr)

# ================================================================= #
# ==== 1. USER CONFIGURATION ====
# ================================================================= #
# IPEDS Total Enrollment (Race/Sex) is available for 2021
# IPEDS Adult Learners (Age/Sex) is available through 2020 in the API
# EdFacts District data is most stable for 2018/2019

DOWNLOAD_TASKS <- list(
  "K12_Graduation"   = list(level = "school-districts",  src = "edfacts", topic = "grad-rates", 
                            yr = 2018, opt = list()),
  
  "Total_Enrollment" = list(level = "college-university", src = "ipeds",   topic = "fall-enrollment", 
                            yr = 2021, opt = list(subtopic = list("race", "sex"))),
  
  # FIX: Switched to 2020 to match API availability for age subtopic
  "Adult_Learners"   = list(level = "college-university", src = "ipeds",   topic = "fall-enrollment", 
                            yr = 2020, opt = list(subtopic = list("age", "sex"))),
  
  "CTE_Completion"   = list(level = "school-districts",  src = "edfacts", topic = "grad-rates", 
                            yr = 2018, opt = list())
)

RAW_HUB <- here::here("01_data", "raw", "Urban_Education")
dir.create(RAW_HUB, recursive = TRUE, showWarnings = FALSE)

# ================================================================= #
# ==== 2. EXECUTE NATIONAL BULK DOWNLOADS ====
# ================================================================= #
message("--- Finalizing National Data Acquisition ---")

iwalk(DOWNLOAD_TASKS, ~{
  dest_dir <- file.path(RAW_HUB, .y)
  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)
  
  message(paste("\nDownloading full national file for:", .y, "| Year:", .x$yr))
  
  tryCatch({
    # Using 'csv = TRUE' for high-speed national bulk downloads
    raw_data <- get_education_data(
      level    = .x$level, 
      source   = .x$src, 
      topic    = .x$topic,
      subtopic = .x$opt$subtopic, 
      filters  = list(year = .x$yr),
      add_labels = TRUE, 
      csv      = TRUE
    )
    
    if (!is.null(raw_data) && nrow(raw_data) > 0) {
      saveRDS(raw_data, file.path(dest_dir, "raw_data.rds"))
      message(paste("✅ SUCCESS: Saved", nrow(raw_data), "rows to", .y))
    }
  }, error = function(e) {
    message(paste("❌ Failed for", .y, ":", e$message))
  })
})

message("\n--- All indicators are now localized. Proceed to Master Prepare. ---")
