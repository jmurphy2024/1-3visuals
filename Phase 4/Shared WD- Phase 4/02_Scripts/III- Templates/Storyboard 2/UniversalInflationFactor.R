# ==============================================================================
# FUNCTION: get_universal_inflation_factor
# Purpose: Calculates the temporal adjustment factor for non-ACS databases.
# Logic:   Uses CPI-U-RS to anchor source dollars to Master Base Year dollars.
# ==============================================================================

library(blscrapeR)
library(dplyr)

get_universal_inflation_factor <- function(source_year, base_year = 2023) {
  # 1. Fetch Consumer Price Index (CPI) data from BLS
  # Note: For production, cache this dataframe to avoid excessive API calls.
  df_cpi <- blscrapeR::get_bls_cpis() 
  
  # 2. Extract annual averages for the years in question
  # We use 'value' which represents the index level
  cpi_base   <- df_cpi %>% filter(year == base_year) %>% pull(value) %>% mean()
  cpi_source <- df_cpi %>% filter(year == source_year) %>% pull(value) %>% mean()
  
  if(is.na(cpi_base) || is.na(cpi_source)) {
    stop(paste("CPI data missing for years:", source_year, "or", base_year))
  }
  
  # 3. Calculate the factor
  # Factor > 1 if source_year is older than base_year (inflation)
  inflation_factor <- cpi_base / cpi_source
  
  return(inflation_factor)
}

