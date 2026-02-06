# ==== 0. ABOUT ====
## WD location: 02_Scripts/III-Data Prep Templates
## Script: UIED_template_prepare.R
## Purpose: Calculates Population-Based Prevalence for 4 UIED indicators. Processes mixed-year data from EdFacts and IPEDS.
## Author: Janica Murphy / Gemini
## Last Modified: 2026-01-26


# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(stringr); library(purrr)

# ================================================================= #
# ==== 1. CONFIGURATION ====
# ================================================================= #
USER_INCOME_VAR_NAME <- "HHINCOME"

INDICATOR_SPECS <- list(
  "K12_Graduation"   = list(type = "rate",      subgroup = "All Students"),
  "Total_Enrollment" = list(type = "count",     subgroup = "Total"),
  "Adult_Learners"   = list(type = "age_ratio", subgroup = "Total"),
  "CTE_Completion"   = list(type = "rate",      subgroup = "CTE Concentrators")
)

# Shared Pathing
INCOME_DB_FILE <- here::here("01_data", "processed", "geographic_income_database_harmonized.rds")
RAW_HUB        <- here::here("01_data", "raw", "Urban_Education")
UIED_HUB       <- here::here("01_data", "processed", "Urban_Education")
dir.create(UIED_HUB, recursive = TRUE, showWarnings = FALSE)

# ================================================================= #
# ==== 2. ROBUST INCOME LOAD ====
# ================================================================= #
income_db_raw <- readRDS(INCOME_DB_FILE) %>% rename_with(str_trim)

# Find any column containing "income"
found_col <- names(income_db_raw)[grepl("income", names(income_db_raw), ignore.case = TRUE)]

if (length(found_col) == 0) stop("FATAL: No income column detected.")

income_db <- income_db_raw %>%
  mutate(TL_GEO_ID = str_trim(as.character(TL_GEO_ID)),
         !!USER_INCOME_VAR_NAME := as.numeric(!!sym(found_col[1]))) %>% 
  select(TL_GEO_ID, all_of(USER_INCOME_VAR_NAME))

# ================================================================= #
# ==== 3. ROBUST PREVALENCE CALCULATION (JOIN FIX) ====
# ================================================================= #
message("--- Starting Robust Data Processing ---")

iwalk(INDICATOR_SPECS, function(spec, name) {
  raw_file <- file.path(RAW_HUB, name, "raw_data.rds")
  if (!file.exists(raw_file)) return(message("❌ Missing folder for: ", name))
  
  raw_data_raw <- readRDS(raw_file) %>% rename_with(str_trim)
  
  # --- 3.1. DYNAMIC ID DETECTION ---
  # Higher Ed uses 'unitid'; K-12 uses 'leaid'
  found_id_col <- intersect(c("leaid", "unitid"), names(raw_data_raw))
  if (length(found_id_col) == 0) return(message("⚠️ No ID found for ", name))
  
  # --- 3.2. SKELETON & STANDARDIZATION ---
  raw_data <- raw_data_raw %>%
    mutate(
      id_var = str_trim(as.character(.data[[found_id_col[1]]])),
      enrollment      = if("enrollment" %in% names(.)) as.numeric(enrollment) else NA_real_,
      grad_rate_midpt = if("grad_rate_midpt" %in% names(.)) as.numeric(grad_rate_midpt) else NA_real_,
      age_category    = if("age_category" %in% names(.)) as.character(age_category) else NA_character_
    )
  
  # --- 3.3. CALCULATION ---
  prepared_df <- raw_data %>%
    filter(if ("subgroup" %in% names(.)) subgroup == spec$subgroup else TRUE) %>%
    group_by(id_var) %>%
    summarise(
      indicator_raw = case_when(
        spec$type == "rate"      ~ mean(grad_rate_midpt / 100, na.rm = TRUE),
        spec$type == "count"     ~ sum(enrollment, na.rm = TRUE),
        spec$type == "age_ratio" ~ {
          adult_n = sum(enrollment[str_detect(age_category, "25-29|30-34|35-39|40-49|50-64|65 and over")], na.rm = TRUE)
          total_n = sum(enrollment, na.rm = TRUE)
          if_else(total_n > 0, adult_n / total_n, NA_real_)
        },
        TRUE ~ NA_real_
      ),
      PERWT = if_else(all(is.na(enrollment)), 1, sum(enrollment, na.rm = TRUE)),
      .groups = "drop"
    )
  
  # --- 3.4. THE JOIN CHECK ---
  # If the join returns 0 rows, it's because 'id_var' isn't in 'income_db'
  final_df <- prepared_df %>%
    inner_join(income_db, by = c("id_var" = "TL_GEO_ID"))
  
  if (nrow(final_df) == 0) {
    message("⚠️ ZERO MATCHES for ", name, ". Check if income_db supports ID type: ", found_id_col[1])
  }
  
  final_df <- final_df %>%
    mutate(indicator_to_plot = if_else(indicator_raw > 1 & spec$type != "count", 1, indicator_raw)) %>%
    filter(!is.na(indicator_to_plot))
  
  saveRDS(final_df, file.path(UIED_HUB, paste0("prepared_", name, ".rds")))
  message("✅ SUCCESS: Prepared ", name, " (", nrow(final_df), " records)")
})