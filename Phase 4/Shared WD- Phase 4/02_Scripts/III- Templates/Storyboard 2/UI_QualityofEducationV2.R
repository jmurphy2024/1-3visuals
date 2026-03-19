# ==============================================================================
# SCRIPT: UI_Resource_Ratios_V2.R
# Purpose: Generate 3-Curve Skyline for School Resource Ratios
# Logic:   CRDC Staffing + CCD Finance converted to Quality Thresholds
# Engine:  educationdata, tidycensus, plot_economic_skyline_2, API Fault Tolerance
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit to prevent node stack overflow when loading heavy plots
options(expressions = 500000)

library(educationdata); library(tidycensus); library(purrr); library(dplyr); library(here); library(scales); library(data.table); library(stringr); library(tidyr); library(gridExtra); library(grid)

# 1. SOURCE MASTER LOGIC & CUTOFFS (V2)
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2.R"))

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION 
# ------------------------------------------------------------------------------
TARGET_YEAR   <- 2017 
TARGET_DIR    <- here::here("01_data", "raw", "Urban_Institute")
# FIX: Renamed the target file so it forces a fresh API download to grab the finance data
TARGET_FILE   <- file.path(TARGET_DIR, "ui_resource_ratios_with_finance_raw_v2.rds") 
CENSUS_VAR    <- "B19013_001"

# 3. ACQUISITION (Multi-Endpoint Urban Institute API & Census)
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering Multi-Endpoint Education APIs ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  # Base Enrollment
  enroll <- tryCatch({
    get_education_data(level = "schools", source = "crdc", topic = "enrollment", subtopic = c("race", "sex"), filters = list(year = TARGET_YEAR, race = 99, sex = 99))
  }, error = function(e) stop("CRDC Enrollment API failed. Cannot calculate rates."))
  
  # Staff Data (Teachers and Counselors)
  staff <- tryCatch({
    get_education_data(level = "schools", source = "crdc", topic = "teachers-staff", filters = list(year = TARGET_YEAR))
  }, error = function(e) {
    message("\n[!] CAUGHT API BUG: Skipping staff data to protect the script.\n")
    data.frame(ncessch = character(), teachers_certified_fte = numeric(), teachers_uncertified_fte = numeric(), counselors_fte = numeric())
  })
  
  # District Finance Data (Per-Student Funding)
  finance <- tryCatch({
    get_education_data(level = "school-districts", source = "ccd", topic = "finance", filters = list(year = TARGET_YEAR))
  }, error = function(e) {
    message("\n[!] CAUGHT API BUG: Skipping finance data to protect the script.\n")
    data.frame(leaid = character(), rev_total = numeric())
  })
  
  # NEW: School Directory Data for Title I Status
  directory <- tryCatch({
    get_education_data(level = "schools", source = "ccd", topic = "directory", filters = list(year = TARGET_YEAR))
  }, error = function(e) {
    message("\n[!] CAUGHT API BUG: Skipping directory data.\n")
    data.frame(ncessch = character(), title_i_status = numeric())
  })
  
  message("Fetching District Median Incomes (Census ACS)...")
  safe_get_acs <- purrr::possibly(get_acs, otherwise = NULL)
  state_list <- c(state.abb, "DC")
  inc_uni <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (unified)", variables = CENSUS_VAR, state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  inc_ele <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (elementary)", variables = CENSUS_VAR, state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  inc_sec <- purrr::map_dfr(state_list, ~safe_get_acs(geography = "school district (secondary)", variables = CENSUS_VAR, state = .x, year = TARGET_YEAR, survey = "acs5", quiet = TRUE))
  income_data <- bind_rows(inc_uni, inc_ele, inc_sec)
  
  raw_data <- list(
    enroll = enroll, staff = staff, finance = finance, directory = directory, income = income_data
  )
  saveRDS(raw_data, TARGET_FILE)
} else {
  message("--- Loading existing Master API data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 4. DATA ENGINEERING & DUMMY VARIABLE CREATION (data.table)
# ------------------------------------------------------------------------------
message("Merging databases and calculating resource thresholds...")

# Safe unpacking of Enrollment
dt_enroll  <- as.data.table(raw_data$enroll)[enrollment_crdc >= 0, .(enrollment_crdc = sum(enrollment_crdc, na.rm=TRUE), leaid = leaid[1], fips = fips[1]), by = ncessch]

# Unpack Staffing
dt_staff <- as.data.table(raw_data$staff)[, .(ncessch, teachers_certified_fte, teachers_uncertified_fte, counselors_fte)]

# Unpack District Finance
dt_finance <- as.data.table(raw_data$finance)[rev_total >= 0, .(leaid, rev_total)]

# NEW: Unpack Directory for Title I Status
dt_dir <- as.data.table(raw_data$directory)[, .(ncessch, title_i_status)]

# Merge school-level tables
dt_schools <- Reduce(function(x, y) merge(x, y, by = "ncessch", all.x = TRUE), 
                     list(dt_enroll, dt_staff, dt_dir))

clean_num <- function(x) ifelse(is.na(x) | x < 0, 0, as.numeric(x))

dt_schools_clean <- dt_schools %>%
  mutate(
    enrollment       = clean_num(enrollment_crdc),
    total_teachers   = clean_num(teachers_certified_fte) + clean_num(teachers_uncertified_fte),
    total_counselors = clean_num(counselors_fte),
    
    # Meaningful Dummy Variables for Structural Quality
    opt_teacher   = if_else(total_teachers > 0 & (enrollment / total_teachers) <= 15, 1, 0),
    opt_counselor = if_else(total_counselors > 0 & (enrollment / total_counselors) <= 250, 1, 0),
    
    # NEW: Title I Dummy (1 = Targeted Assistance, 2 = Schoolwide, 3 = Eligible no program)
    is_title_i = if_else(title_i_status %in% c(1, 2, 3), 1, 0)
    
  ) %>% filter(enrollment > 0)

# Roll up to the District Level using Weighted Averages
dt_district_agg <- dt_schools_clean[, .(
  district_enrollment = sum(enrollment, na.rm = TRUE),
  fips                = first(fips),
  
  `Optimal Teacher Caseload`   = weighted.mean(opt_teacher, w = enrollment, na.rm = TRUE),
  `Optimal Counselor Caseload` = weighted.mean(opt_counselor, w = enrollment, na.rm = TRUE),
  
  # NEW: Count schools to aggregate Title I percentages later
  title_1_schools = sum(is_title_i, na.rm = TRUE),
  total_schools   = .N
  
), by = .(leaid)]

dt_income <- as.data.table(raw_data$income)[, .(leaid = GEOID, est_median_hh_inc = estimate)]

# Merge District Data (Aggregations, Income, and Finance)
dt_linked <- merge(dt_district_agg, dt_income, by = "leaid", all.x = TRUE)
dt_linked <- merge(dt_linked, dt_finance, by = "leaid", all.x = TRUE)

# 5. NORMALIZATION & COMPOSITE BUILD
# ------------------------------------------------------------------------------
state_rpp_lookup <- tibble::tibble(STATEFIP = c(1, 2, 4, 5, 6, 8, 9, 10, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56), STATE_RPP = c(89.9, 101.7, 101.1, 86.5, 112.6, 101.4, 103.7, 101.9, 103.5, 96.7, 108.6, 91.8, 98.9, 90.9, 89.9, 91.1, 88.6, 90.8, 97.5, 105.7, 108.2, 92.4, 98.3, 87.4, 90.1, 92.5, 89.5, 98.4, 105.1, 108.9, 91.2, 108.4, 94.8, 89.2, 91.5, 89.5, 103.2, 96.9, 101.2, 92.5, 89.5, 91.1, 97.5, 96.1, 100.4, 101.5, 109.0, 87.9, 92.6, 92.6))

INFLATION_ADJ <- 304.702 / 245.120

prepared_data <- as_tibble(dt_linked) %>%
  rename(STATEFIP = fips) %>% 
  left_join(state_rpp_lookup, by = "STATEFIP") %>%
  mutate(
    PERWT = district_enrollment,
    REAL_INCOME = (if_else(est_median_hh_inc > 0, as.numeric(est_median_hh_inc), NA_real_) * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    
    # Calculate Real Funding Per Student (Adjusted for Inflation and Spatial RPP)
    REAL_FUNDING_PER_STUDENT = ((rev_total / PERWT) * INFLATION_ADJ) * (100 / coalesce(STATE_RPP, 100)),
    
    # New Dummy Variable: Optimal Per-Student Funding (>= $15,000)
    `Optimal Per-Student Funding` = if_else(!is.na(REAL_FUNDING_PER_STUDENT) & REAL_FUNDING_PER_STUDENT >= 15000, 1, 0),
    
    income_tercile = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Tercile 1 (Bottom)",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Tercile 2 (Middle)",
      TRUE ~ "Tercile 3 (Top)"
    ),
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(PERWT > 0, REAL_INCOME >= 30000, !is.na(income_tercile))

# 6. AUTOMATED PLOTTING ENGINE (Using II-D Skyline2)
# ------------------------------------------------------------------------------
message("Generating Skyline Plot...")

dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

# NEW: Calculate the exact % of Title I schools in the bottom tercile dynamically
bottom_third <- prepared_data %>% filter(Country == "Bottom Third")
pct_title_1 <- sum(bottom_third$title_1_schools, na.rm = TRUE) / sum(bottom_third$total_schools, na.rm = TRUE)
pct_title_1_str <- scales::percent(pct_title_1, accuracy = 0.1)

ui_caption <- paste0(
  "Source: Urban Institute (CRDC Staffing, CCD Finance) & Census ACS 5-Year Estimates.\n",
  "• Optimal Teacher Caseload: % of students in districts with < 15:1 student-to-teacher ratio.\n",
  "• Optimal Counselor Caseload: % of students in districts meeting the ASCA < 250:1 standard.\n",
  "• Optimal Per-Student Funding: % of students in districts with > $15,000 per pupil (adjusted for inflation & RPP).\n", 
  "• Note: ", pct_title_1_str, " of schools in the lowest-income tier are Title I eligible, which provides federal grants that boost funding."
)

p_chart <- plot_economic_skyline_2(
  data           = prepared_data, 
  indicator_vars = c("Optimal Teacher Caseload", "Optimal Counselor Caseload", "Optimal Per-Student Funding"), 
  weight_var     = "PERWT", 
  y_axis_label   = "Students with Optimal Resources (%)",
  plot_title     = "School District Resource Ratios",
  caption_text   = ui_caption
)

print(p_chart)
ggsave(here::here("03_output", "visualizations_final", "UI_Resource_Ratios_V2.png"), p_chart, width = 12, height = 7, dpi = 300)

# ==============================================================================
# 7. SUMMARY TABLE GENERATION (Quartiles & Overall)
# ==============================================================================
message("Generating Summary Tables...")

generate_summary_table <- function(data, target_var, title_name) {
  
  # A. Ensure we filter out NAs for the specific target variable before aggregating
  data_clean <- data %>% filter(!is.na(.data[[target_var]]))
  
  overall_rates <- data_clean %>%
    group_by(Country) %>%
    summarise(Rate = weighted.mean(.data[[target_var]], w = PERWT, na.rm = TRUE), .groups = "drop") %>%
    mutate(Quartile = "Overall")
  
  quartile_rates <- data_clean %>%
    group_by(Country) %>%
    mutate(quartile = ntile(REAL_INCOME, 4)) %>%
    group_by(Country, quartile) %>%
    summarise(Rate = weighted.mean(.data[[target_var]], w = PERWT, na.rm = TRUE), .groups = "drop") %>%
    mutate(Quartile = paste0("Q", quartile)) %>%
    select(-quartile)
  
  summary_table <- bind_rows(overall_rates, quartile_rates) %>%
    mutate(Rate = scales::percent(Rate, accuracy = 0.1)) %>%
    pivot_wider(names_from = Country, values_from = Rate) %>%
    select(Quartile, `Bottom Third`, `Middle Third`, `Top Third`) %>%
    rename(`Income Quartile` = Quartile) %>%
    arrange(match(`Income Quartile`, c("Overall", "Q1", "Q2", "Q3", "Q4")))
  
  table_grob <- tableGrob(
    summary_table, rows = NULL,
    theme = ttheme_default(
      core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 1.1)),
      colhead = list(fg_params = list(cex = 1.2, fontface = "bold"))
    )
  )
  
  title_grob <- textGrob(paste(title_name, "Rates"), gp = gpar(fontsize = 14, fontface = "bold"))
  padding <- unit(5, "mm")
  table_with_title <- gtable::gtable_add_rows(table_grob, heights = grobHeight(title_grob) + padding, pos = 0)
  table_with_title <- gtable::gtable_add_grob(table_with_title, title_grob, 1, 1, 1, ncol(table_with_title))
  
  out_path <- here::here("03_output", "visualizations_final", paste0("UI_Resource_Ratios_", gsub(" |-", "_", title_name), "_V2_Table.png"))
  ggsave(filename = out_path, plot = table_with_title, width = 8, height = 4, bg = "white", dpi = 300)
}

# Loop to generate tables for all 3 metrics
generate_summary_table(prepared_data, "Optimal Teacher Caseload", "Optimal Teacher Caseload")
generate_summary_table(prepared_data, "Optimal Counselor Caseload", "Optimal Counselor Caseload")
generate_summary_table(prepared_data, "Optimal Per-Student Funding", "Optimal Per-Student Funding")

message("All visualizations and summary tables successfully generated and saved!")