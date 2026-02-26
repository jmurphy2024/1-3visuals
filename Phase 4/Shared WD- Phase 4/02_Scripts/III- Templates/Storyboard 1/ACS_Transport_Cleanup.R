# ==============================================================================
# Script: 01_fetch_clean_transport_data.R
# Purpose: Programmatically fetch ACS Transport data via IPUMS API, clean it,
#          and export for Tableau.
# Author: Senior Data Engineer (Gemini)
# ==============================================================================

# 1. SETUP & LIBRARIES ----------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("ipumsr")) install.packages("ipumsr")

library(tidyverse)
library(ipumsr)

# Ensure API Key is loaded
# Note: Store your key in .Renviron as IPUMS_API_KEY="your_key_here"
# Restart R after editing .Renviron for it to take effect.
api_key <- Sys.getenv("IPUMS_API_KEY")

if (api_key == "") {
  stop("API Key not found. Please add IPUMS_API_KEY to your .Renviron file.")
}

set_ipums_api_key(api_key, save = TRUE, overwrite = TRUE)

# 2. DEFINE THE EXTRACT ---------------------------------------------------
# We are requesting the ACS 2024 1-Year Sample ("us2024a")
# Variables: FTOTINC (Income), TRANWORK (Transport Mode), TRANTIME (Commute Time)

message("Defining IPUMS Extract...")

extract_def <- define_extract_usa(
  description = "Tableau Transport Data (Automated via API)",
  samples = "us2024a", 
  variables = list(
    "FTOTINC",  # Total Family Income
    "TRANWORK", # Means of transportation to work
    "TRANTIME"  # Travel time to work
  )
)

# 3. SUBMIT & WAIT --------------------------------------------------------
message("Submitting extract to IPUMS servers...")
submitted_extract <- submit_extract(extract_def)

message(paste("Extract submitted. ID:", submitted_extract$number))
message("Waiting for processing (this may take a few minutes)...")

# This pauses the script until IPUMS finishes building the file
downloadable_extract <- wait_for_extract(submitted_extract)

# 4. DOWNLOAD DATA --------------------------------------------------------
message("Extract ready! Downloading files...")

# Create data directory if it doesn't exist
if (!dir.exists("01_data")) dir.create("01_data")

# Download .xml (DDI) and .dat.gz (Data)
files <- download_extract(
  downloadable_extract,
  download_dir = "01_data",
  overwrite = TRUE
)

# 5. LOAD & AUTO-LABEL ----------------------------------------------------
message("Loading and labeling data...")

# read_ipums_micro() automatically reads the DDI file to apply labels.
# We keep variables as 'labelled' class first to inspect codes if needed,
# then convert to factors/numeric.

raw_data <- read_ipums_micro(files)

# 6. CLEANING & TRANSFORMATION --------------------------------------------
message("Cleaning data for Tableau...")

clean_data <- raw_data %>%
  # A. Convert Labelled Categories to Text (Factors)
  # This handles TRANWORK automatically (10 -> "Auto, truck, or van", etc.)
  mutate(
    # Convert TRANWORK to human-readable text labels
    TRANWORK_LABEL = as_factor(TRANWORK),
    
    # B. Clean FTOTINC (Total Family Income)
    # 9999999 = N/A -> Convert to NA
    # 0000000 = No Income -> Keep as 0 (it's valid data)
    # -000001 = Net Loss -> Keep (it's valid data)
    FTOTINC_CLEAN = if_else(as.numeric(FTOTINC) == 9999999, NA_real_, as.numeric(FTOTINC)),
    
    # C. Clean TRANTIME (Travel Time)
    # 000 = N/A (Not in universe/Did not work) -> Convert to NA
    TRANTIME_CLEAN = if_else(as.numeric(TRANTIME) == 0, NA_real_, as.numeric(TRANTIME))
  ) %>%
  
  # D. Final Selection & Formatting
  select(
    SAMPLE,              # Keep Sample ID for reference
    SERIAL,              # Household Serial (for distinct counts if needed)
    FTOTINC = FTOTINC_CLEAN,
    TRANWORK = TRANWORK_LABEL,
    TRANTIME = TRANTIME_CLEAN,
    PERWT                # Person Weight (CRITICAL for Tableau aggregations)
  ) %>%
  
  # Filter out rows where crucial dimensions are NA (optional, based on your needs)
  filter(!is.na(TRANWORK))

# 7. EXPORT ---------------------------------------------------------------
output_file <- "transport_tableau_data.csv"
message(paste("Exporting clean data to", output_file, "..."))

write_csv(clean_data, output_file)

message("SUCCESS: Script completed. Data is ready for Tableau.")


# ==============================================================================
# BONUS: How to find Sample IDs
# ==============================================================================
# If "us2024a" returns an error (because it's not released yet), 
# run this code in your console to see all available ACS samples:
#
# all_samples <- get_sample_info("usa")
# View(all_samples)
# 
# Look for the 'name' column (e.g., 'us2023a', 'us2022a').
# ==============================================================================
