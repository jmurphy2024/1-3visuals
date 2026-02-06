# ==== 0. SETUP ====
rm(list = ls()); gc()
library(dplyr); library(readr); library(here); library(rlang); library(ggplot2); library(grid); library(gridExtra); library(survival); library(broom)
source(here::here("02_Scripts", "II- Shared Functions", "II-A Shared Utilities.r"))
source(here::here("02_Scripts", "II- Shared Functions", "II-B Shared Visuals.r"))

# ================================================================= #
# ==== 1. USER INPUTS ====
# ================================================================= #
USER_IPUMS_SAMPLE_ID  <- "ih2014"
USER_INDICATOR_NAME   <- "Life_Expectancy"
USER_FINE_GROUP_LEVEL <- "Groups_20"

PLOT_TITLE    <- "Life Expectancy by Income Percentile"
Y_AXIS_LABEL  <- "Estimated Life Expectancy (Years)"

# ================================================================= #
# ==== 2. VIZ LOGIC ====
# ================================================================= #

PREPARED_DATA_FILE <- here::here("01_data", "processed", "IPUMS_Microdata", paste0("prepared_NHIS_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".rds"))
OUTPUT_PLOT_FILENAME <- paste0("plot_", USER_INDICATOR_NAME, "_", USER_IPUMS_SAMPLE_ID, ".png")

if (!file.exists(PREPARED_DATA_FILE)) { stop("Prepared file not found.") }
prepared_data <- readRDS(PREPARED_DATA_FILE)

# --- 2.3. ASSIGN INCOME GROUPS (LOCAL METHOD) ---
# We calculate groups locally because NHIS income codes (10, 20...) 
# are not compatible with ACS dollar cutoffs.
message("Assigning income groups locally...")

data_with_groups <- prepared_data %>%
  filter(!is.na(HHINCOME)) %>%
  mutate(
    # 1. Force data into 20 evenly sized ranks (5% groups)
    fine_income_group_rank = ntile(HHINCOME, 20),
    
    # 2. Create labels
    fine_income_group = factor(fine_income_group_rank, levels = 1:20, labels = paste0(seq(5, 100, 5), "%")),
    
    # 3. Create broad terciles for coloring
    income_tercile = case_when(
      fine_income_group_rank <= 7  ~ "Tercile 1 (Bottom)",
      fine_income_group_rank <= 14 ~ "Tercile 2 (Middle)",
      TRUE                         ~ "Tercile 3 (Top)"
    )
  )

# --- 2.4. CUSTOM SURVIVAL CALCULATION ---
message("Running Survival Analysis per Income Group...")

calculate_life_expectancy <- function(df) {
  # Handle "Zero Time" Error (Start >= End)
  df <- df %>% mutate(age_at_event = ifelse(age_at_event <= age_entry, age_entry + 0.1, age_at_event))
  
  # Fit Survival Curve: Surv(Start Age, End Age, Event)
  # We use tryCatch to skip groups that are too small or fail to converge
  fit <- tryCatch(
    survfit(Surv(age_entry, age_at_event, status_flag) ~ 1, weights = MORTWT, data = df),
    error = function(e) return(NULL)
  )
  
  if (is.null(fit)) return(NA)
  
  # Extract Restricted Mean Survival Time (RMST) at age 90
  tryCatch(
    summary(fit, rmean = 90)$table["rmean"],
    error = function(e) return(NA)
  )
}

summary_stats <- data_with_groups %>%
  group_by(fine_income_group) %>%
  summarise(
    income_tercile = first(income_tercile), 
    indicator_value = calculate_life_expectancy(cur_data()),
    .groups = "drop"
  ) %>%
  filter(!is.na(indicator_value))

# --- 2.5. GENERATE PLOT ---
# Custom ggplot logic for this specific indicator
final_viz_output <- ggplot(summary_stats, aes(x = fine_income_group, y = indicator_value, group = 1)) +
  geom_line(color = "#2c3e50", linewidth = 1) +
  geom_point(aes(color = income_tercile), size = 3) +
  geom_smooth(method = "loess", se = FALSE, color = "grey50", linetype = "dashed", alpha = 0.5) +
  scale_color_manual(values = c("Tercile 1 (Bottom)" = "#E74C3C", 
                                "Tercile 2 (Middle)" = "#F1C40F", 
                                "Tercile 3 (Top)" = "#2ECC71")) +
  labs(
    title = PLOT_TITLE,
    subtitle = "Restricted Mean Survival Time (RMST, Age 90 cap)",
    y = Y_AXIS_LABEL,
    x = "Income Percentile (Ranked Family Income)",
    color = "Income Group"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  )

# Save
ggsave(filename = file.path(here::here("03_output", "visualizations_final"), OUTPUT_PLOT_FILENAME), 
       plot = final_viz_output, width = 8, height = 6)

message("Visualization complete.")
grid.newpage(); grid.draw(final_viz_output)