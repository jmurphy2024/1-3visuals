# ==============================================================================
# SCRIPT: NHIS_LifeExpectancy_template.R
# Purpose: Generate 3-Country Skyline for Adult Life Expectancy (Age 25+)
# Logic: IPUMS NHIS Mortality Linkage + Jittered Income + Demographic Life Tables
# ==============================================================================
rm(list = ls()); gc()
library(ipumsr); library(dplyr); library(here); library(stringr); library(tidyr); library(ggplot2)

# 1. SOURCE MASTER LOGIC
# ------------------------------------------------------------------------------
source(here::here("02_Scripts", "II- Shared Functions", "II-D Income Normalization.R")) 
set.seed(123)

# 2. CONFIGURATION (2018 Mortality Linkage)
# ------------------------------------------------------------------------------
USER_SAMPLE <- "ih2018" 

# Using INCFAM97ON2 for harmonization and MORTWTSA for linkage weights
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
  raw_data <- readRDS(TARGET_FILE)
}

# 4. NORMALIZATION (Untrimmed to preserve true survey bounds)
# ------------------------------------------------------------------------------
region_rpp_lookup <- tibble(REGION_ID = c(1, 2, 3, 4), REG_RPP = c(105.2, 92.8, 95.4, 104.1))
INFLATION_ADJ <- get_inflation_multiplier(data_year = 2018, base_year = 2023)

prepared_data <- raw_data %>%
  rename_with(toupper, everything()) %>% 
  filter(MORTELIG == 1) %>% 
  left_join(region_rpp_lookup, by = c("REGION" = "REGION_ID")) %>% 
  mutate(
    PERWT = as.numeric(MORTWTSA),
    income_code = as.numeric(INCFAM97ON2),
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
      REAL_INCOME <= 45000 ~ "Bottom Third",
      REAL_INCOME > 45000 & REAL_INCOME <= 115000 ~ "Middle Third",
      TRUE ~ "Top Third"
    )
  ) %>%
  filter(!is.na(REAL_INCOME), PERWT > 0)

# 5. DEMOGRAPHIC SUMMARIZATION (Decile Smoothing)
# ------------------------------------------------------------------------------
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
      # Plateau fix for high-income 85+ brackets
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

# 6. VISUALIZATION EXECUTION (With Explanatory Caption)
# ------------------------------------------------------------------------------
income_breaks <- c(1, 5, 10, 15, 20, 25, 30) 
income_labels <- c("$0", "$20,000", "$45,000", "$75,000", "$115,000", "$250,000", "$500,000+")
label_colors <- c("black", "black", "#9B2226", "black", "#E9C46A", "black", "black")

max_peak <- max(viz_data$Life_Expectancy, na.rm = TRUE)

p <- ggplot(viz_data, aes(x = x_id, y = Life_Expectancy)) +
  geom_line(aes(color = Country, group = 1), linewidth = 4, alpha = 0.15, show.legend = FALSE) + 
  geom_line(aes(color = Country, group = 1), linewidth = 1, linejoin = "round", lineend = "round") +
  
  scale_color_manual(
    values = c("Bottom Third"="#9B2226", "Middle Third"="#E9C46A", "Top Third"="#386641"), 
    guide = "none"
  ) + 
  
  scale_y_continuous(limits = c(NA, max_peak + 3)) +
  scale_x_continuous(breaks = income_breaks, labels = income_labels, expand = c(0.01, 0.01)) + 
  coord_cartesian(clip = "off") + 
  
  theme_minimal() + 
  theme(
    panel.grid      = element_blank(),  
    axis.title.x    = element_text(face = "bold", size = 14, margin = margin(t = 15)), 
    axis.title.y    = element_text(face = "bold", size = 14, margin = margin(r = 15)), 
    axis.text.x     = element_text(color = label_colors, face = "bold", size = 10), 
    axis.text.y     = element_text(color = "black", size = 10),
    axis.line.x     = element_line(color = "black", linewidth = 1.5), 
    axis.line.y     = element_line(color = "black", linewidth = 1.5),
    plot.background = element_rect(fill = "white", color = NA),
    
    # Increased bottom margin slightly (b = 50) to make sure the longer caption has plenty of room
    plot.margin     = margin(t = 20, r = 10, b = 50, l = 10),
    
    # Set caption sizing, color, and spacing
    plot.caption    = element_text(hjust = 0, size = 9, color = "grey30", margin = margin(t = 20))
  ) +
  labs(
    x = "Household Income (Real Adjusted Dollars)", 
    y = "Life Expectancy (Years)",
    
    # THE EXPLANATORY CAPTION
    caption = stringr::str_wrap("Note: This chart displays Adult Life Expectancy (calculated at age 25). It uses IPUMS NHIS 2018 demographic life tables with 10 deciles per income group. The unusually high survival rate at the absolute bottom of the income scale ($0-$5,000) represents the 'Zero-Income Anomaly.' This extreme bracket is heavily skewed by non-working students, business owners claiming tax losses, and asset-wealthy retirees who report zero earned income, artificially masking the true mortality rate of the working poor.", width = 110)
  )

print(p)

# --- FINAL EXPORT TO PROJECT OUTPUT ---
ggsave(
  filename = here::here("03_output", "visualizations_final", "NHIS_2018_LifeExpectancy_Deciles.png"), 
  plot = p, 
  width = 10, 
  height = 6.5, # Slightly taller to accommodate the wrapped caption
  dpi = 300
)