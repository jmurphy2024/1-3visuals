# ==== 0. ABOUT ====
## WD location: 02_Scripts/I-Geo Areas Master File
## Script: I-C_calculate_puma_median_income.R
## Purpose: Calculates weighted median household income for each PUMA-level
##          income bracket using the corrected 7-digit PUMA GEOID from the microdata.
## Author: Max Goshert, EPAG / Gemini
## Date Created: 2025-08-18
## Last Modified: 2025-09-22 (Updated to use 7-digit PUMA_GEOID)
## Dependencies: dplyr, readr, here, purrr
## Input: 01_data/processed/ipums_acs_2019_2023_microdata.rds
## Output: 01_data/processed/puma_income_bracket_median_lookup.rds

# Load necessary libraries
if (!require(dplyr)) install.packages("dplyr")
if (!require(readr)) install.packages("readr")
if (!require(here)) install.packages("here")
if (!require(purrr)) install.packages("purrr")

library(dplyr)
library(readr)
library(here)
library(purrr)

# ==== 1. LOAD IPUMS ACS MICRODATA ====
input_microdata_path <- here("01_data", "processed", "ipums_acs_2019_2023_microdata.rds")

if (!file.exists(input_microdata_path)) {
  stop(paste("Error: Microdata file not found at", input_microdata_path,
             ". Please ensure Task I-B has been completed successfully."))
}

message(paste("Loading IPUMS ACS microdata from:", input_microdata_path))
ipums_microdata <- readRDS(input_microdata_path)
message("Microdata loaded successfully.")

# ==== 2. DEFINE STANDARD INCOME BRACKETS (NHGIS B19001) ====
income_breaks <- c(0, 9999, 14999, 19999, 24999, 29999, 34999, 39999, 44999,
                   49999, 59999, 74999, 99999, 124999, 149999, 199999,
                   249999, Inf)

income_labels <- c(
  "Less than $10,000", "$10,000 to $14,999", "$15,000 to $19,999",
  "$20,000 to $24,999", "$25,000 to $29,999", "$30,000 to $34,999",
  "$35,000 to $39,999", "$40,000 to $44,999", "$45,000 to $49,999",
  "$50,000 to $59,999", "$60,000 to $74,999", "$75,000 to $99,999",
  "$100,000 to $124,999", "$125,000 to $149,999", "$150,000 to $199,999",
  "$200,000 to $249,999", "$250,000 or more"
)

weighted_median <- function(x, w) {
  if (length(x) == 0 || sum(w) == 0) return(NA)
  df <- data.frame(x = x, w = w) %>% arrange(x)
  df$w_cumsum <- cumsum(df$w)
  median_weight <- sum(df$w) / 2
  df$x[which(df$w_cumsum >= median_weight)[1]]
}

# ==== 3. ASSIGN BRACKETS AND CALCULATE MEDIANS ====
message("Assigning households to income brackets...")
microdata_with_brackets <- ipums_microdata %>%
  mutate(
    Income_Bracket_ID = cut(
      HHINCOME,
      breaks = income_breaks,
      labels = income_labels,
      right = FALSE,
      include.lowest = TRUE
    )
  )

message("Calculating weighted median income for each PUMA-bracket group...")
# **CORRECTED** to group by the full 7-digit PUMA_GEOID
puma_bracket_medians <- microdata_with_brackets %>%
  filter(!is.na(Income_Bracket_ID)) %>%
  group_by(PUMA_GEOID, Income_Bracket_ID) %>%
  summarise(
    Calculated_Median_Income = weighted_median(HHINCOME, HHWT),
    .groups = 'drop'
  )

# ==== 4. CREATE AND SAVE LOOKUP TABLE ====
puma_bracket_lookup_table <- puma_bracket_medians %>%
  rename(PUMA_Bracket_Median_HHINCOME = Calculated_Median_Income)

output_dir <- here("01_data", "processed")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
rds_output_path <- here(output_dir, "puma_income_bracket_median_lookup.rds")

message(paste("Saving RDS file to:", rds_output_path))
saveRDS(puma_bracket_lookup_table, rds_output_path)
message("RDS file saved.")

# ==== 5. VALIDATION ====
message("--- First 10 rows of the PUMA-Bracket Median Lookup Table ---")
print(head(puma_bracket_lookup_table, 10))
message(paste("Number of unique PUMAs with calculated medians:", n_distinct(puma_bracket_lookup_table$PUMA_GEOID)))
message("Script I-C execution complete.")
