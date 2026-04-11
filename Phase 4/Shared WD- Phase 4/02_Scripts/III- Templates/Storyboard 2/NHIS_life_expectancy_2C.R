# ==============================================================================
# SCRIPT: NHIS_LifeExpectancy_Skyline_2C.R
# Purpose: Generate 3-Country Skyline for Adult Life Expectancy (Age 25+)
# Logic: IPUMS NHIS Mortality Linkage + Jittered Income + Demographic Life Tables
# Visual: Minimalist Skyline (No titles, no gridlines, no dots, strict min/max y-axis)
# ==============================================================================
rm(list = ls()); gc()

# Increase expression limit for heavy ggplot rendering
options(expressions = 500000)

library(ipumsr); library(dplyr); library(here); library(stringr); library(tidyr); library(ggplot2)
library(gridExtra); library(grid); library(scales)

set.seed(123)

# 1. SOURCE MASTER LOGIC & CUTOFFS
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities2.R"))
source(here::here("02_Scripts", "II- Shared Functions", "II-D Skyline2C.R"))

# ==============================================================================
# VISUAL THEME STANDARDIZATION (MINIMALIST)
# ==============================================================================
apply_standard_theme <- function(p) {
  p_updated <- p +
    theme_minimal(base_size = 11, base_family = "serif") +
    theme(
      legend.position   = "none", 
      panel.grid        = element_blank(), 
      axis.text.x       = element_blank(), 
      axis.text.y       = element_text(color = "black", size = 11, family = "serif", margin = margin(r = 5)), 
      axis.line.x       = element_line(color = "black", linewidth = 1.2), 
      axis.line.y       = element_line(color = "black", linewidth = 1.2), 
      axis.title        = element_blank(), 
      plot.title        = element_blank(), 
      plot.caption      = element_blank(), 
      plot.margin       = margin(t = 30, r = 30, b = 30, l = 20),
      plot.background   = element_rect(fill = "white", color = NA),
      panel.background  = element_rect(fill = "white", color = NA)
    )
  return(p_updated)
}

cutoffs_path <- here::here("01_data", "processed", "main_tercile_cutoffs_person_inclusive2.rds")
if(!file.exists(cutoffs_path)) stop("V2 Cutoffs not found. Run II-C Border Setup V2 first.")
cutoffs <- readRDS(cutoffs_path)

# 2. CONFIGURATION (2018 Mortality Linkage)
# ------------------------------------------------------------------------------
USER_SAMPLE <- "ih2018" 

VARS_NEEDED <- c("MORTWTSA", "MORTSTAT", "MORTELIG", "INCFAM97ON2", "REGION", "AGE")

TARGET_DIR  <- here::here("01_data", "raw", "IPUMS_Microdata", USER_SAMPLE)
TARGET_FILE <- file.path(TARGET_DIR, "mortality_data.rds")

# 3. ACQUISITION (IPUMS API Recovery)
# ------------------------------------------------------------------------------
if (!file.exists(TARGET_FILE)) {
  message("--- Triggering IPUMS NHIS Mortality API Extract ---")
  dir.create(TARGET_DIR, recursive = TRUE, showWarnings = FALSE)
  
  extract_def <- define_extract_micro(
    collection  = "nhis", 
    samples     = USER_SAMPLE,
    variables   = VARS_NEEDED,
    description = "Three Countries Adult Life Expectancy"
  )
  
  submitted <- submit_extract(extract_def)
  downloadable <- wait_for_extract(submitted)
  path <- download_extract(downloadable, download_dir = TARGET_DIR)
  
  ddi <- read_ipums_ddi(path[grep("\\.xml$", path)])
  raw_data <- read_ipums_micro(ddi, verbose = FALSE)
  saveRDS(raw_data, TARGET_FILE)
} else {
  message("--- Loading existing NHIS 2018 Mortality data ---")
  raw_data <- readRDS(TARGET_FILE)
}

# 4. NORMALIZATION & PILLAR LOGIC
# ------------------------------------------------------------------------------
message("Applying normalization and dynamic cutoffs...")

region_rpp_lookup <- tibble(REGION_ID = c(1, 2, 3, 4), REG_RPP = c(105.2, 92.8, 95.4, 104.1))
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2018, base_year = 2023)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  filter(MORTELIG == 1) %>% 
  left_join(region_rpp_lookup, by = c("REGION" = "REGION_ID")) %>% 
  mutate(
    PERWT = as.numeric(MORTWTSA),
    income_code = as.numeric(INCFAM97ON2),
    
    # Use runif() to "jitter" incomes within their exact survey brackets
    raw_dollars = case_when(
      income_code == 10 ~ runif(n(), 0, 4999),
      income_code == 11 ~ runif(n(), 5000, 9999),
      income_code == 12 ~ runif(n(), 10000, 14999),
      income_code == 13 ~ runif(n(), 15000, 19999),
      income_code == 14 ~ runif(n(), 20000, 24999),
      income_code == 15 ~ runif(n(), 25000, 34999),
      income_code == 16 ~ runif(n(), 35000, 44999),
      income_code == 17 ~ runif(n(), 45000, 54999),
      income_code == 18 ~ runif(n(), 55000, 64999),
      income_code == 19 ~ runif(n(), 65000, 74999),
      income_code == 20 ~ runif(n(), 75000, 99999),
      income_code >= 21 & income_code <= 32 ~ runif(n(), 100000, 250000),
      TRUE ~ NA_real_
    ),
    
    REAL_INCOME = (raw_dollars * INFLATION_ADJ) * (100 / coalesce(REG_RPP, 100)),
    
    Country = case_when(
      REAL_INCOME <= cutoffs$main_cutoff1 ~ "Bottom Third",
      REAL_INCOME > cutoffs$main_cutoff1 & REAL_INCOME <= cutoffs$main_cutoff2 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(REAL_INCOME), PERWT > 0)

# ==============================================================================
# 5. DEMOGRAPHIC SUMMARIZATION (TERCILES & QUARTILES WITHIN)
# ==============================================================================
if(!require(Hmisc)) install.packages("Hmisc", dependencies = TRUE)
library(Hmisc)

# Helper function: Calculate life expectancy from a demographic life table
calc_life_expectancy <- function(df) {
  lt <- df %>%
    group_by(age_group) %>%
    summarise(
      deaths = sum(if_else(MORTSTAT == 1, 1, 0) * PERWT, na.rm = TRUE),
      pop    = sum(PERWT, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(age_group) %>%
    mutate(
      m_x = deaths / pop,
      n_years = if_else(row_number() == n(), Inf, 5),
      m_x = if_else(is.infinite(n_years) & (m_x == 0 | is.nan(m_x)), 0.15, m_x),
      m_x = coalesce(m_x, 0)
    )
  
  lt <- lt %>% mutate(
    q_x = if_else(is.infinite(n_years), 1, (n_years * m_x) / (1 + (n_years/2) * m_x)),
    q_x = pmin(q_x, 1)
  )
  
  l_x <- numeric(nrow(lt))
  l_x[1] <- 100000
  if(nrow(lt) > 1) { for(i in 2:nrow(lt)) { l_x[i] <- l_x[i-1] * (1 - lt$q_x[i-1]) } }
  lt$l_x <- l_x
  
  lt <- lt %>% mutate(
    d_x = l_x * q_x,
    L_x = if_else(is.infinite(n_years), l_x / m_x, n_years * (l_x - 0.5 * d_x))
  )
  lt$L_x[is.na(lt$L_x)] <- 0 
  lt$T_x <- rev(cumsum(rev(lt$L_x)))
  lt$e_x <- lt$T_x / lt$l_x
  
  return(lt$e_x[1] + 25) 
}

message("\n=== LIFE EXPECTANCY SUMMARY (AGE 25+) ===")

prepared_data_q <- prepared_data %>%
  filter(AGE >= 25) %>% 
  mutate(age_group = cut(AGE, breaks = c(seq(25, 85, by = 5), Inf), right = FALSE)) %>%
  group_by(Country) %>%
  mutate(
    q25 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.25, na.rm = TRUE),
    q50 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.50, na.rm = TRUE),
    q75 = Hmisc::wtd.quantile(REAL_INCOME, weights = PERWT, probs = 0.75, na.rm = TRUE),
    Quartile = case_when(
      REAL_INCOME <= q25 ~ "Q1 (Bottom 25%)",
      REAL_INCOME > q25 & REAL_INCOME <= q50 ~ "Q2",
      REAL_INCOME > q50 & REAL_INCOME <= q75 ~ "Q3",
      TRUE ~ "Q4 (Top 25%)"
    )
  ) %>%
  ungroup()

overall_tercile <- prepared_data_q %>%
  group_by(Country) %>%
  group_modify(~ tibble(
    Total_Population = sum(.x$PERWT, na.rm = TRUE),
    `Life Expectancy (Years)` = round(calc_life_expectancy(.x), 1)
  )) %>%
  mutate(Subgroup = "Overall Tercile") %>%
  ungroup()

quartile_stats <- prepared_data_q %>%
  group_by(Country, Quartile) %>%
  group_modify(~ tibble(
    Total_Population = sum(.x$PERWT, na.rm = TRUE),
    `Life Expectancy (Years)` = round(calc_life_expectancy(.x), 1)
  )) %>%
  rename(Subgroup = Quartile) %>%
  ungroup()

macro_summary <- bind_rows(overall_tercile, quartile_stats) %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, Subgroup != "Overall Tercile", Subgroup) %>%
  select(Country, Subgroup, Total_Population, `Life Expectancy (Years)`) %>%
  mutate(Total_Population = scales::comma(Total_Population))

print(as.data.frame(macro_summary))

# ==============================================================================
# 6. VISUALIZATION EXECUTION (Minimalist Format)
# ==============================================================================
message("\nGenerating Minimalist Skyline Plot...")
dir.create(here::here("03_output", "visualizations_final"), recursive = TRUE, showWarnings = FALSE)

viz_data <- prepared_data %>%
  filter(AGE >= 25) %>% 
  mutate(age_group = cut(AGE, breaks = c(seq(25, 85, by = 5), Inf), right = FALSE)) %>%
  group_by(Country) %>% 
  mutate(decile = ntile(REAL_INCOME, 10)) %>% 
  group_by(Country, decile) %>% 
  group_modify(~ tibble(Life_Expectancy = calc_life_expectancy(.x))) %>%
  ungroup() %>%
  mutate(Country = factor(Country, levels = c("Bottom Third", "Middle Third", "Top Third"))) %>%
  arrange(Country, decile) %>% 
  mutate(x_id = row_number())

p_chart <- ggplot(viz_data, aes(x = x_id, y = Life_Expectancy)) +
  geom_line(aes(color = Country, group = 1), linewidth = 4, alpha = 0.15) + 
  geom_line(aes(color = Country, group = 1), linewidth = 1.2, linejoin = "round") +
  scale_color_manual(values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641")) +
  scale_y_continuous(
    labels = scales::label_number(accuracy = 0.1), 
    limits = c(0, NA), 
    breaks = function(x) c(0, max(x, na.rm = TRUE)), # Force breaks to only be 0 and the actual max value
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(x = NULL, y = NULL)

p_chart <- apply_standard_theme(p_chart)

# --- RSTUDIO DIRECT OUTPUTS ---
# Show summary table in RStudio plot window
table_grob <- tableGrob(macro_summary, rows = NULL,
                        theme = ttheme_default(core = list(bg_params = list(fill = c("white", "#f9f9f9")), fg_params = list(cex = 0.9)),
                                               colhead = list(fg_params = list(cex = 1.0, fontface = "bold"))))
grid.newpage()
grid.draw(table_grob)

# Render the Skyline plot in RStudio plot window
print(p_chart)

ggsave(here::here("03_output", "visualizations_final", "NHIS_LifeExpectancy_Summary_Table_2C.png"), table_grob, width = 7.5, height = 5, bg = "white", dpi = 300)
ggsave(here::here("03_output", "visualizations_final", "NHIS_2018_LifeExpectancy_Skyline_2C.png"), p_chart, width = 10, height = 7, dpi = 300, bg = "white")

message("Processing & Visualizations Complete!")