# ==== 0. ABOUT ====
## WD location: 02_Scripts/I-Geo Areas Master File
## Script: I-D-i_acquire_shapefiles.R
## Purpose: Downloads TIGER/Line shapefiles for 2010-era (2019 vintage) and
##          2020-era (2022/2023 vintages) census geographies. This version
##          downloads cartographic boundaries (cb=TRUE) for most areas to improve
##          efficiency, but uses full-resolution files (cb=FALSE) for PUMAs.
##          It now includes elementary, secondary, and unified school districts
##          and generates a robust success/failure report upon completion.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-09-03
## Last Modified: 2025-09-21 (Added school districts, switched to cb=TRUE for non-PUMAs, and added validation report)
## Dependencies: sf, tigris, purrr, dplyr, here
## Input: None (downloads data directly from Census Bureau)
## Output: .rds files for each successfully downloaded geography, saved to
##         `01_data/gis_shapefiles/`. Also prints a detailed summary to the console.

# ==== 0. SETUP & ENVIRONMENT PREPARATION ====

# ===== 0.1. Install and Load Required Packages =====
if (!require(sf)) install.packages("sf")
if (!require(tigris)) install.packages("tigris")
if (!require(purrr)) install.packages("purrr")
if (!require(dplyr)) install.packages("dplyr")
if (!require(here)) install.packages("here")

library(sf)
library(tigris)
library(purrr)
library(dplyr)
library(here)

# ===== 0.2. Configure tigris Options =====
# Cache downloaded files to avoid re-downloading during the same session.
options(tigris_use_cache = TRUE)
# Force refresh to download fresh files and avoid using a potentially corrupted cache from a previous run.
options(tigris_refresh = TRUE)


# ==== 1. DEFINE PARAMETERS AND HELPER FUNCTIONS ====

# ===== 1.1. Define Global Parameters =====
# Set the target Coordinate Reference System (CRS) for all shapefiles.
target_crs <- "EPSG:5070" # US National Atlas Equal Area projection

# Get a list of all state FIPS codes to iterate over, excluding territories.
states_to_download <- states(year = 2023)$STATEFP
states_to_download <- states_to_download[!states_to_download %in% c("60", "66", "69", "78")] # Exclude territories

# Define and create the output directory for shapefiles.
output_dir <- here("01_data", "gis_shapefiles")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# ===== 1.2. Initialize Download Log =====
# This list will store the status of every download attempt.
download_log <- list()

# ===== 1.3. Helper Function to Download and Prepare Geographies =====
download_and_prep_state_file <- function(tigris_function, year, states_list, geo_name, cb_flag, type_arg = NULL) {
  download_type <- if (cb_flag) "Cartographic Boundary" else "Full Resolution"
  # Create a more descriptive name if a 'type' is specified (e.g., for school districts)
  full_geo_name <- if (!is.null(type_arg)) paste(type_arg, "school districts") else geo_name
  
  message(paste("   -> Downloading", full_geo_name, "for", year, "by state (", download_type, ")..."))
  
  # Use purrr::map to loop over each state FIPS code and download the data.
  results <- purrr::map(states_list, function(st) {
    log_entry_base <- list(year = year, geography = full_geo_name, state = st, type = "State-by-State")
    
    sf_data <- tryCatch({
      # Call the appropriate tigris function, passing the 'type' argument only if it's provided.
      if (is.null(type_arg)) {
        tigris_function(state = st, year = year, cb = cb_flag, progress_bar = TRUE)
      } else {
        tigris_function(state = st, year = year, cb = cb_flag, type = type_arg, progress_bar = TRUE)
      }
    }, error = function(e) {
      # On failure, log the error and return NULL.
      download_log <<- append(download_log, list(c(log_entry_base, status = "FAILED", error_message = e$message)))
      return(NULL)
    })
    
    # On success, log the success and return the downloaded data.
    if (!is.null(sf_data)) {
      download_log <<- append(download_log, list(c(log_entry_base, status = "SUCCESS", error_message = NA)))
    }
    return(sf_data)
  })
  
  # Combine all successfully downloaded state data frames into one national file.
  national_sf <- compact(results) %>% bind_rows()
  
  if (nrow(national_sf) == 0) {
    warning(paste("No data was successfully downloaded for", full_geo_name, "in", year, "."))
    return(NULL)
  }
  
  # Transform the combined shapefile to the target CRS.
  return(st_transform(national_sf, target_crs))
}

# ===== 1.4. Helper Functions for Robust GEOID Renaming =====
# These functions find common GEOID variants and rename them to a standard `TL_GEO_ID`.
rename_geoid_10 <- function(sf_obj) {
  if (is.null(sf_obj)) return(NULL)
  if ("GEOID10" %in% names(sf_obj)) {
    rename(sf_obj, TL_GEO_ID = GEOID10)
  } else if ("ZCTA5CE10" %in% names(sf_obj)) {
    rename(sf_obj, TL_GEO_ID = ZCTA5CE10)
  } else if ("GEOID" %in% names(sf_obj)) {
    rename(sf_obj, TL_GEO_ID = GEOID)
  } else {
    warning("Could not find a standard 2010-era GEOID column.")
    sf_obj
  }
}

rename_geoid_20 <- function(sf_obj) {
  if (is.null(sf_obj)) return(NULL)
  if ("GEOID20" %in% names(sf_obj)) {
    rename(sf_obj, TL_GEO_ID = GEOID20)
  } else if ("ZCTA5CE20" %in% names(sf_obj)) {
    rename(sf_obj, TL_GEO_ID = ZCTA5CE20)
  } else if ("GEOID" %in% names(sf_obj)) {
    rename(sf_obj, TL_GEO_ID = GEOID)
  } else {
    warning("Could not find a standard 2020-era GEOID column.")
    sf_obj
  }
}


# ==== 2. DOWNLOAD 2010-ERA BOUNDARY FILES (USING 2019 VINTAGE) ====
message("\n==== Downloading 2010-era boundary files (using year = 2019) ====")
year_2010_era <- 2019

# --- Download Cartographic Boundaries (cb = TRUE) ---
geographies_cb_true_2010 <- list(
  tracts = tracts,
  county_subdivisions = county_subdivisions
)
walk(names(geographies_cb_true_2010), function(geo_name) {
  sf_obj <- download_and_prep_state_file(geographies_cb_true_2010[[geo_name]], year_2010_era, states_to_download, geo_name, cb_flag = TRUE)
  if (!is.null(sf_obj) && nrow(sf_obj) > 0) {
    sf_obj_renamed <- rename_geoid_10(sf_obj)
    saveRDS(sf_obj_renamed, here(output_dir, paste0(geo_name, "_", year_2010_era, "_sf.rds")))
    rm(sf_obj, sf_obj_renamed); gc()
  }
})

# --- Download Full Resolution Boundaries (cb = FALSE) ---
sf_obj_puma10 <- download_and_prep_state_file(pumas, year_2010_era, states_to_download, "pumas", cb_flag = FALSE)
if (!is.null(sf_obj_puma10) && nrow(sf_obj_puma10) > 0) {
  sf_obj_renamed <- rename_geoid_10(sf_obj_puma10)
  saveRDS(sf_obj_renamed, here(output_dir, paste0("pumas_", year_2010_era, "_sf.rds")))
  rm(sf_obj_puma10, sf_obj_renamed); gc()
}

# --- Download School Districts (cb = TRUE) ---
school_district_types <- c("elementary", "secondary", "unified")
walk(school_district_types, function(sd_type) {
  geo_name <- paste0(sd_type, "_school_districts")
  sf_obj <- download_and_prep_state_file(school_districts, year_2010_era, states_to_download, geo_name, cb_flag = TRUE, type_arg = sd_type)
  if (!is.null(sf_obj) && nrow(sf_obj) > 0) {
    sf_obj_renamed <- rename_geoid_10(sf_obj)
    saveRDS(sf_obj_renamed, here(output_dir, paste0(geo_name, "_", year_2010_era, "_sf.rds")))
    rm(sf_obj, sf_obj_renamed); gc()
  }
})


# ==== 3. DOWNLOAD 2020-ERA BOUNDARY FILES (USING 2022/2023 VINTAGES) ====
message("\n==== Downloading 2020-era boundary files (using year = 2023, PUMAs = 2022) ====")
year_2020_era_general <- 2023
year_2020_era_pumas <- 2022 # Corrected year for 2020-era PUMAs

# --- Download Cartographic Boundaries (cb = TRUE) ---
geographies_cb_true_2020 <- list(
  tracts = tracts,
  county_subdivisions = county_subdivisions
)
walk(names(geographies_cb_true_2020), function(geo_name) {
  sf_obj <- download_and_prep_state_file(geographies_cb_true_2020[[geo_name]], year_2020_era_general, states_to_download, geo_name, cb_flag = TRUE)
  if (!is.null(sf_obj) && nrow(sf_obj) > 0) {
    sf_obj_renamed <- rename_geoid_20(sf_obj)
    saveRDS(sf_obj_renamed, here(output_dir, paste0(geo_name, "_", year_2020_era_general, "_sf.rds")))
    rm(sf_obj, sf_obj_renamed); gc()
  }
})

# --- Download Full Resolution Boundaries (cb = FALSE) ---
sf_obj_puma20 <- download_and_prep_state_file(pumas, year_2020_era_pumas, states_to_download, "pumas", cb_flag = FALSE)
if (!is.null(sf_obj_puma20) && nrow(sf_obj_puma20) > 0) {
  sf_obj_renamed <- rename_geoid_20(sf_obj_puma20)
  saveRDS(sf_obj_renamed, here(output_dir, paste0("pumas_", year_2020_era_pumas, "_sf.rds")))
  rm(sf_obj_puma20, sf_obj_renamed); gc()
}

# --- Download School Districts (cb = TRUE) ---
walk(school_district_types, function(sd_type) {
  geo_name <- paste0(sd_type, "_school_districts")
  sf_obj <- download_and_prep_state_file(school_districts, year_2020_era_general, states_to_download, geo_name, cb_flag = TRUE, type_arg = sd_type)
  if (!is.null(sf_obj) && nrow(sf_obj) > 0) {
    sf_obj_renamed <- rename_geoid_20(sf_obj)
    saveRDS(sf_obj_renamed, here(output_dir, paste0(geo_name, "_", year_2020_era_general, "_sf.rds")))
    rm(sf_obj, sf_obj_renamed); gc()
  }
})


# ==== 4. GENERATE AND DISPLAY DEBUGGER REPORT ====
message("\n\n=======================================================")
message("====      DOWNLOAD DEBUGGER & SUMMARY REPORT     ====")
message("=======================================================")

if (length(download_log) == 0) {
  message("\nNo download attempts were logged. Please check the script's loops.")
} else {
  
  log_df <- bind_rows(lapply(download_log, as.data.frame.list))
  
  # --- Failure Report ---
  failures <- log_df %>% filter(status == "FAILED")
  
  if (nrow(failures) > 0) {
    message(paste("\n🔴 Total Failures:", nrow(failures)))
    message("-------------------------------------------------------")
    
    failure_summary <- failures %>%
      group_by(year, geography) %>%
      summarise(
        failed_states = n(),
        example_error = first(error_message),
        .groups = 'drop'
      )
    
    print(as.data.frame(failure_summary), row.names = FALSE)
    
  } else {
    message("\n✅ All downloads were successful!")
  }
  
  # --- Success Report ---
  successes <- log_df %>% filter(status == "SUCCESS")
  
  if (nrow(successes) > 0) {
    message(paste("\n\n🟢 Total Successes:", nrow(successes)))
    message("-------------------------------------------------------")
    
    success_summary <- successes %>%
      group_by(year, geography) %>%
      summarise(successful_states = n(), .groups = 'drop') %>%
      arrange(year, geography)
    
    print(as.data.frame(success_summary), row.names = FALSE)
  }
}

message("\n=======================================================")
message("Script I-D-i execution complete.")