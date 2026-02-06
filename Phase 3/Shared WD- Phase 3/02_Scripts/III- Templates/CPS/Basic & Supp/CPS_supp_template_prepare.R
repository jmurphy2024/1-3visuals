# ================================================================= #
# ==== 0. ABOUT ====
# ================================================================= #
## WD location: 02_Scripts/III-Data Prep Templates/CPS/ASEC
## Script: 02_prepare_income_wealth.R
## Purpose: Creates the "Big 4" Quality of Life Composite Variables.
## Author: 1/3 Country Project Assistant
## Date Created: 2026-01-08

# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(stringr); library(purrr)

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #
USER_INDICATOR_NAME      <- "Quality_of_Life_Composite"
USER_ASEC_SAMPLE_ID      <- "cps2023_03s"
USER_FOOD_SAMPLE_ID      <- "cps2023_12s"
USER_CIVIC_SAMPLE_ID     <- "cps2023_09s"

WGT_ASEC  <- "ASECWT"
WGT_FOOD  <- "FSSUPPWTH"
WGT_CIVIC <- "VLSUPPWT"

# Income Bin Mapping for Imputation
FAMINC_RANGES <- list(
  `100`=c(0,4999),   `210`=c(5000,7499),   `300`=c(7500,9999),   `430`=c(10000,12499),
  `470`=c(12500,14999), `500`=c(15000,19999), `600`=c(20000,24999), `710`=c(25000,29999),
  `720`=c(30000,34999), `730`=c(35000,39999), `740`=c(40000,49999), `810`=c(50000,74999),
  `820`=c(50000,59999), `830`=c(60000,74999),
  `840`=c(75000,99999), `841`=c(75000,99999), `842`=c(100000,149999), `843`=c(150000,Inf)
)
USER_RANDOM_SEED <- 20260108

# ================================================================= #
# ==== 2. GENERIC LOGIC ====
# ================================================================= #
set.seed(USER_RANDOM_SEED)

BASE_RAW_DIR <- here::here("01_data", "raw", "IPUMS_Microdata")
PROCESSED_DIR <- here::here("01_data", "processed", "IPUMS_Microdata")
dir.create(PROCESSED_DIR, showWarnings = FALSE, recursive = TRUE)
PROCESSED_DATA_FILE <- file.path(PROCESSED_DIR, paste0("prepared_CPS_", USER_INDICATOR_NAME, "_", USER_ASEC_SAMPLE_ID, ".rds"))

impute_income <- function(supp_data, donor_data) {
  donor_clean <- donor_data %>%
    filter(!is.na(HHINCOME) & HHINCOME < 99999999 & ASECWT > 0) %>%
    select(HHINCOME, ASECWT)
  imputation_pools <- map(FAMINC_RANGES, function(range) {
    donor_clean %>% filter(HHINCOME >= range[1] & HHINCOME < range[2])
  })
  imputed_vals <- map_dbl(supp_data$FAMINC, function(fam_code) {
    code_str <- as.character(fam_code)
    if (is.na(fam_code) || is.null(imputation_pools[[code_str]]) || nrow(imputation_pools[[code_str]]) == 0) return(NA_real_)
    pool <- imputation_pools[[code_str]]
    sample(pool$HHINCOME, size = 1, prob = pool$ASECWT)
  })
  return(imputed_vals)
}

# ================================================================= #
# ==== 3. PROCESS FILES ====
# ================================================================= #

# --- 3.1. Process ASEC (Balance & Health) ---
message("Processing ASEC Data...")
path_asec <- file.path(BASE_RAW_DIR, paste0("cps_ASEC_Income_Base_", USER_ASEC_SAMPLE_ID), "raw_ASEC_Income_Base.rds")
if(!file.exists(path_asec)) stop("ASEC file missing.")
raw_asec <- readRDS(path_asec)

df_asec <- raw_asec %>%
  mutate(
    HHINCOME = if_else(HHINCOME == 99999999, NA_real_, as.numeric(HHINCOME)),
    
    # 1. "Time Wealth" (Work Balance)
    # Works 1-45 hours (Active but not overwhelmed)
    val_flag_time_wealth = if_else(
      (AHRSWORKT >= 1 & AHRSWORKT <= 45), 1, 0, missing = 0
    ),
    
    # 2. "The Thriving Worker" (Health + Balance)
    # Works 35-45 hours AND Health is Excellent/VG
    val_flag_thriving_worker = if_else(
      (AHRSWORKT >= 35 & AHRSWORKT <= 45) & (HEALTH %in% c(1, 2)), 1, 0, missing = 0
    ),
    
    COMMON_WEIGHT = ASECWT
  ) %>%
  select(COMMON_WEIGHT, HHINCOME, val_flag_time_wealth, val_flag_thriving_worker)


# --- 3.2. Process Food Security (Basic Needs) ---
message("Processing Food Security Data...")
path_food <- file.path(BASE_RAW_DIR, paste0("cps_Food_Security_Supp_", USER_FOOD_SAMPLE_ID), "raw_Food_Security_Supp.rds")

if(file.exists(path_food)) {
  raw_food <- readRDS(path_food)
  raw_food$HHINCOME_IMP <- impute_income(raw_food, raw_asec)
  
  df_food <- raw_food %>%
    filter(!is.na(HHINCOME_IMP)) %>%
    mutate(
      HHINCOME = HHINCOME_IMP,
      
      # 3. "Secure Foundation" (Food Security)
      # 1=High Food Security. (Composite of 18 questions about anxiety/quality/quantity)
      val_flag_food_secure = case_when(FSSTATUS == 1 ~ 1, FSSTATUS %in% c(2,3) ~ 0, TRUE ~ NA_real_),
      
      COMMON_WEIGHT = FSSUPPWTH
    ) %>%
    select(COMMON_WEIGHT, HHINCOME, val_flag_food_secure)
} else { df_food <- NULL }


# --- 3.3. Process Civic Engagement (Social Bonds) ---
message("Processing Civic Engagement Data...")
path_civic <- file.path(BASE_RAW_DIR, paste0("cps_Civic_Engagement_Supp_", USER_CIVIC_SAMPLE_ID), "raw_Civic_Engagement_Supp.rds")

if(file.exists(path_civic)) {
  raw_civic <- readRDS(path_civic)
  raw_civic$HHINCOME_IMP <- impute_income(raw_civic, raw_asec)
  
  df_civic <- raw_civic %>%
    filter(!is.na(HHINCOME_IMP)) %>%
    mutate(
      HHINCOME = HHINCOME_IMP,
      
      # 4. "The Reciprocal Neighbor" (Active Bonds)
      # Talk (1-4) AND Favors (1-4)
      val_flag_reciprocal_neighbor = if_else(
        VLNEIGH %in% c(1,2,3,4) & VLHELPN %in% c(1,2,3,4), 1, 0, missing = 0
      ),
      
      COMMON_WEIGHT = VLSUPPWT 
    ) %>%
    select(COMMON_WEIGHT, HHINCOME, val_flag_reciprocal_neighbor)
} else { df_civic <- NULL }

# ================================================================= #
# ==== 4. COMBINE AND SAVE ====
# ================================================================= #
message("Combining datasets...")
final_prepared_data <- bind_rows(df_asec, df_food, df_civic) %>%
  mutate(
    Income_Tier = case_when(
      HHINCOME <= 64400 ~ "Bottom Third",
      HHINCOME > 64400 & HHINCOME <= 130000 ~ "Middle Third",
      HHINCOME > 130000 ~ "Top Third",
      TRUE ~ NA_character_
    ),
    Income_Tier = factor(Income_Tier, levels = c("Bottom Third", "Middle Third", "Top Third"))
  ) %>%
  filter(!is.na(Income_Tier))

saveRDS(final_prepared_data, file = PROCESSED_DATA_FILE)
message(paste("Data preparation complete. Saved to:", PROCESSED_DATA_FILE))