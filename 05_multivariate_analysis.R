# 05_multivariate_analysis.R
# Day 4: Multivariate Analysis
# NHIS 2021

# Load packages
library(tidyverse)
library(psych)

# Read data
nhis <- read_csv("NHIS _Data_2021.csv", show_col_types = FALSE)

# Clean and prepare analysis variables
nhis_mv <- nhis %>%
  mutate(
    # Recode missing values based on codebook
    AGEP_A = ifelse(AGEP_A %in% c(97, 98, 99), NA, AGEP_A),
    WEIGHTLBTC_A = ifelse(WEIGHTLBTC_A %in% c(996, 997, 998, 999), NA, WEIGHTLBTC_A),
    HEIGHTTC_A = ifelse(HEIGHTTC_A %in% c(96, 97, 98, 99), NA, HEIGHTTC_A),
    SEX_A = case_when(
      SEX_A == 1 ~ "Male",
      SEX_A == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    PHSTAT_A = case_when(
      PHSTAT_A == 1 ~ "Excellent",
      PHSTAT_A == 2 ~ "Very good",
      PHSTAT_A == 3 ~ "Good",
      PHSTAT_A == 4 ~ "Fair",
      PHSTAT_A == 5 ~ "Poor",
      TRUE ~ NA_character_
    ),
    EDUCP_A = case_when(
      EDUCP_A %in% 0:3 ~ "Less than High School",
      EDUCP_A == 4 ~ "High School Graduate",
      EDUCP_A %in% 5:7 ~ "Some College",
      EDUCP_A %in% 8:10 ~ "College Graduate or better",
      TRUE ~ NA_character_
    ),
    SEX_A = factor(SEX_A, levels = c("Male", "Female")),
    PHSTAT_A = factor(PHSTAT_A, levels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
    EDUCP_A = factor(
      EDUCP_A,
      levels = c("Less than High School", "High School Graduate",
                 "Some College", "College Graduate or better")
    )
  )

# -------------------------------
# Task 1A: Enhanced scatter plot
# HEIGHTTC_A vs WEIGHTLBTC_A
# Color by SEX_A and facet by PHSTAT_A
# -------------------------------
scatter_phstat <- nhis_mv %>%
  filter(
    !is.na(HEIGHTTC_A),
    !is.na(WEIGHTLBTC_A),
    !is.na(SEX_A),
    !is.na(PHSTAT_A)
  ) %>%
  ggplot(aes(x = HEIGHTTC_A, y = WEIGHTLBTC_A, color = SEX_A)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ PHSTAT_A) +
  labs(
    title = "Weight vs. Height by Sex, Faceted by General Health Status",
    x = "Height (inches)",
    y = "Weight (pounds)",
    color = "Sex"
  ) +
  theme_minimal()

print(scatter_phstat)

# Optional alternative: facet by education instead of health status
scatter_educp <- nhis_mv %>%
  filter(
    !is.na(HEIGHTTC_A),
    !is.na(WEIGHTLBTC_A),
    !is.na(SEX_A),
    !is.na(EDUCP_A)
  ) %>%
  ggplot(aes(x = HEIGHTTC_A, y = WEIGHTLBTC_A, color = SEX_A)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ EDUCP_A) +
  labs(
    title = "Weight vs. Height by Sex, Faceted by Education Level",
    x = "Height (inches)",
    y = "Weight (pounds)",
    color = "Sex"
  ) +
  theme_minimal()

print(scatter_educp)

# Save plots
ggsave("scatter_height_weight_by_sex_phstat.png", scatter_phstat,
       width = 11, height = 7, dpi = 300)
ggsave("scatter_height_weight_by_sex_educp.png", scatter_educp,
       width = 11, height = 7, dpi = 300)

# -------------------------------
# Task 1B: Correlation scatter plot matrix
# -------------------------------
corr_data <- nhis_mv %>%
  select(AGEP_A, WEIGHTLBTC_A, HEIGHTTC_A) %>%
  drop_na()

png("pairs_panels_age_weight_height.png", width = 1200, height = 1200, res = 150)
pairs.panels(
  corr_data,
  method = "pearson",
  hist.col = "lightblue",
  density = TRUE,
  ellipses = TRUE,
  lm = TRUE,
  main = "Correlation Scatter Plot Matrix: Age, Weight, and Height"
)
dev.off()
