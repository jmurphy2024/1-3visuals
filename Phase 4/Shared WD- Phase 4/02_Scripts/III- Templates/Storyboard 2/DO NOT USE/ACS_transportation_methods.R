# ==============================================================================
# SCRIPT: ACS_Private_Transport_LineChart.R
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(scales); library(ggplot2)

# 1. SOURCE MASTER LOGIC
# Points to your renamed II-D Income Normalization and Design script
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R"))

# 2. CONFIGURATION 
USER_SAMPLE   <- "acs_2023_5yr" 
VARS_NEEDED   <- c("PERWT", "HHINCOME", "STATEFIP", "ADJUST", "AGE", "TRANWORK")
TARGET_DIR    <- here::here("01_data", "raw", "IPUMS_Microdata", USER_SAMPLE)
TARGET_FILE   <- file.path(TARGET_DIR, "raw_data.rds")

# 3. ACQUISITION (API Recovery)
# Automatically handles the download if the file is missing from your ASU Dropbox
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS API for 2023 5-Year Sample ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection = "usa", 
    samples = "us2023c", # Official IPUMS code for 2023 5-year
    variables = VARS_NEEDED,
    description = "Three Countries ACS 2023 5-Year Master - Transport"
  )
  
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  raw_data <- readRDS(TARGET_FILE)
}

# 4. COMPLEX RECODING
prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  # UNIVERSE: Workers 16+ who actually commute (TRANWORK != 0)
  filter(AGE >= 16 & TRANWORK != 0) %>% 
  mutate(
    # Clean top-coded household income
    HHINCOME = if_else(HHINCOME >= 9999999, NA_real_, as.numeric(HHINCOME)),
    # INDICATOR: Private Transport (10 = car, 20 = motorcycle, 50 = bicycle)
    target_indicator = if_else(TRANWORK %in% c(10, 20, 50), 1, 0),
    # 5-Year Inflation Adjustment factor
    ADJUST_FACTOR = as.numeric(ADJUST)
  )

# 5. EXECUTION & VISUALIZATION
# Normalizes dollars using RPP and the multi-year ADJUST factor
final_data <- prepared_data %>%
  apply_three_countries_logic("HHINCOME", "STATEFIP", adj_val = prepared_data$ADJUST_FACTOR)

# --- NEW LINE CHART CODE ---
# Summarize the weighted data by AGE (or swap AGE for an income group variable)
chart_data <- final_data %>%
  group_by(AGE) %>%
  summarize(
    # Calculates the weighted percentage of commuters using private transport/bikes
    Commute_Rate = sum(target_indicator * PERWT, na.rm = TRUE) / sum(PERWT, na.rm = TRUE)
  )

# Generate the Line Chart
ggplot(chart_data, aes(x = AGE, y = Commute_Rate)) +
  geom_line(color = "#2c3e50", linewidth = 1.2) +      # The line itself
  geom_point(color = "#e74c3c", size = 2) +            # The points on the line
  scale_y_continuous(labels = percent_format()) +      # Formats Y-axis as percentages
  labs(
    title = "Private & Bike Commute Rate by Age",
    x = "Age",
    y = "Commute Rate (%)",
    caption = "Source: IPUMS ACS 2023 5-Year Sample"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )

