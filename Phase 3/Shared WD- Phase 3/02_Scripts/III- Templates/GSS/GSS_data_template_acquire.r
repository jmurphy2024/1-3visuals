# ==== 0. ABOUT ====
## WD location: [Specify your preferred working directory location, e.g., 02_Scripts/III-Data Prep Templates/GSS]
## Script: GSS_data_template_acquire.R
## Purpose: A standardized template for loading local GSS data (.sav format) for a new indicator.
##          This script loads the specified data, generates a basic text codebook,
##          and saves the relevant data as a raw RDS file.
## Author: Max Goshert, Janica Murphy, EPAG / Gemini 
## Date Created: 2025-10-28
## Last Modified: 2025-10-28 ## Changed variable names to lowercase
## Dependencies: haven, dplyr, here, stringr
## Input: User-defined path to GSS .sav file and variable list.
## Output: A raw RDS data file containing selected variables and a basic text codebook
##         in `01_data/raw/GSS_Data/`.

# ==== 0. SETUP ====
# This section loads necessary libraries.
rm(list = ls()); gc()
# install.packages(c("haven", "dplyr", "here", "stringr")) # Uncomment to install if needed
library(haven); library(dplyr); library(here); library(stringr)

# ================================================================= #
# ==== 1. USER INPUTS: CONFIGURE YOUR DATA LOADING ====
# ================================================================= #

# --- 1.1. Define GSS Data File Details ---
# Provide the relative path to your local GSS .sav file using here::here().
# This assumes your R script/project is located appropriately relative to the data file.
USER_GSS_SAV_FILE_PATH <- here::here("02_Scripts", "III- Templates", "GSS", "GSS2024.sav")
USER_GSS_YEAR_ID     <- "GSS2024" # A short identifier for the GSS data year/version
USER_INDICATOR_NAME  <- "Happiness" # Updated name for the analysis

# --- 1.2. Define All GSS Variables Needed for Your Analysis ---
# Refer to the GSS 2024 Codebook PDF for variable names. USE LOWERCASE.
# Include necessary identifiers, weights, survey design vars, demographics, and indicator-specific vars.
USER_VARIABLES_NEEDED <- c(
  # --- Identifier ---
  "id", # Assuming there's an id variable, adjust if needed
  
  # --- Weighting and Survey Design (Important for correct analysis!) ---
  "wtssnrps", # Post-stratification weights
  "vstrat", "vpsu",      # Stratum and PSU for variance estimation
  
  # --- Core Demographics (Examples) ---
  "age",          # Respondent's Age
  "sex",          # Respondent's Sex
  "race",         # Respondent's Race (summary variable)
  "hispanic",     # Hispanic Origin
  "educ",         # Respondent's Education (years)
  "degree",       # Respondent's Degree
  "marital",      # Marital Status
  
  # --- Indicator-specific variables (Example: Happiness) ---
  "income16",     # Total family income last year (detailed categories)
  "happy"         # General happiness
  # Add other variables relevant to your specific analysis here
)

# --- 1.3. Basic Variable Descriptions (Manual entry based on PDF codebook) ---
# Add descriptions for the variables listed above. USE LOWERCASE for variable names.
# Format: "variable_name" = "Description from PDF"
VARIABLE_DESCRIPTIONS <- list(
  "id" = "Unique Respondent Identifier (Assumed)",
  "wtssnrps" = "Weight variable adjusted for post-stratification and non-response (2004-2024)",
  "vstrat" = "Variance Stratum variable for complex survey design",
  "vpsu" = "Variance Primary Sampling Unit variable for complex survey design",
  "age" = "Respondent's Age in years",
  "sex" = "Respondent's Sex (coded by interviewer or derived)",
  "race" = "Respondent's Race (White, Black, Other - summary variable)",
  "hispanic" = "Are you Spanish, Hispanic, or Latino/Latina? IF YES: Which group are you from?",
  "educ" = "Highest Year of School Completed",
  "degree" = "Respondent's Highest Earned Degree",
  "marital" = "Are you currently married, widowed, divorced, separated, or never married?",
  "income16" = "In which of these groups did your total family income, from all sources, fall last year? That is, before taxes.",
  "happy" = "Taken all together, how would you say things are these days--would you say that you are very happy, pretty happy, or not too happy?"
  # Add descriptions matching your chosen USER_VARIABLES_NEEDED
)


# ================================================================= #
# ==== 2. GENERIC LOGIC (No changes needed below this line) ====
# ================================================================= #

# --- 2.1. Define Output Directory ---
OUTPUT_RAW_DIR <- here::here("01_data", "raw", "GSS_Data", USER_GSS_YEAR_ID)
dir.create(OUTPUT_RAW_DIR, showWarnings = FALSE, recursive = TRUE)

# --- 2.2. Acquire Data ---
# Load data from the specified local SPSS (.sav) file
if (file.exists(USER_GSS_SAV_FILE_PATH)) {
  message(paste("--- Loading GSS data from:", USER_GSS_SAV_FILE_PATH, "---"))
  # Use haven::read_sav to read the SPSS file
  # Note: haven typically preserves original case, but we select using lowercase later
  raw_data_full <- haven::read_sav(USER_GSS_SAV_FILE_PATH)
  # Standardize column names to lowercase immediately after loading
  names(raw_data_full) <- tolower(names(raw_data_full))
  message("--- GSS .sav file loaded successfully and names converted to lowercase. ---")
  
  # Select only the variables needed for the analysis (already defined in lowercase)
  # Ensure all requested variables actually exist in the loaded data
  available_vars <- names(raw_data_full)
  needed_vars_exist <- USER_VARIABLES_NEEDED %in% available_vars
  if (!all(needed_vars_exist)) {
    missing_vars <- USER_VARIABLES_NEEDED[!needed_vars_exist]
    warning("The following requested lowercase variables were NOT found in the .sav file: ",
            paste(missing_vars, collapse = ", "),
            "\nThey will be excluded.")
    USER_VARIABLES_NEEDED <- USER_VARIABLES_NEEDED[needed_vars_exist]
  }
  
  # Use dplyr::select with all_of() to handle potential missing vars gracefully
  # Since names(raw_data_full) are now lowercase, this should work
  raw_data <- raw_data_full %>%
    select(all_of(USER_VARIABLES_NEEDED))
  message(paste("--- Selected", length(USER_VARIABLES_NEEDED), "variables for analysis. ---"))
  
} else {
  stop("FATAL ERROR: GSS .sav file not found at the specified path: ", USER_GSS_SAV_FILE_PATH, ". Cannot proceed. Check the path and ensure your R session is in the correct project root directory.")
}

# --- 2.3. Generate Basic Codebook and Save Raw Data as RDS ---
if (exists("raw_data") && nrow(raw_data) > 0) {
  
  # Generate and save a basic, human-readable codebook in the raw data directory
  codebook_path <- file.path(OUTPUT_RAW_DIR, paste0("codebook_", USER_INDICATOR_NAME, ".txt"))
  
  # Create codebook content
  codebook_lines <- c(paste("Basic Codebook for GSS Indicator:", USER_INDICATOR_NAME),
                      paste("Data Source:", basename(USER_GSS_SAV_FILE_PATH)),
                      paste("Date Generated:", Sys.Date()),
                      "---",
                      "Selected Variables (lowercase):",
                      "---")
  
  # Iterate using lowercase names
  for (var in USER_VARIABLES_NEEDED) {
    desc <- VARIABLE_DESCRIPTIONS[[var]] # Retrieve description using lowercase key
    if (is.null(desc)) {
      desc <- "No description provided in script."
    }
    codebook_lines <- c(codebook_lines, paste0(var, ": ", desc)) # Use lowercase var name
  }
  
  # Write the codebook file
  writeLines(codebook_lines, con = codebook_path)
  message(paste("Basic codebook saved to:", codebook_path))
  
  # Save the selected raw data as an RDS file for much faster loading in the next script
  rds_output_path <- file.path(OUTPUT_RAW_DIR, paste0("raw_data_", USER_INDICATOR_NAME, ".rds"))
  saveRDS(raw_data, file = rds_output_path)
  message(paste("\nSelected raw data saved successfully as RDS to:", rds_output_path))
  message(paste("Output directory:", OUTPUT_RAW_DIR))
  
} else {
  stop("FATAL ERROR: Failed to load or select data from the .sav file. Cannot proceed.")
}

message("\n--- Data acquisition script complete. ---")