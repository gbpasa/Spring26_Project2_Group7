

library(tidyverse)

# Import raw dataset
nhis <- read_csv("data/NHIS _Data_2021.csv")

# Select only required variables
nhis_selected <- nhis %>%
  select(
    AGEP_A,
    WEIGHTLBTC_A,
    HEIGHTTC_A,
    SEX_A,
    HISPALLP_A,
    EDUCP_A,
    PHSTAT_A,
    LSATIS4R_A
  )

# Recode missing/nonresponse values as NA
nhis_clean <- nhis_selected %>%
  mutate(
    AGEP_A = ifelse(AGEP_A %in% 97:99, NA, AGEP_A),
    WEIGHTLBTC_A = ifelse(WEIGHTLBTC_A %in% 996:999, NA, WEIGHTLBTC_A),
    HEIGHTTC_A = ifelse(HEIGHTTC_A %in% 96:99, NA, HEIGHTTC_A),
    SEX_A = ifelse(SEX_A %in% c(7, 9), NA, SEX_A),
    HISPALLP_A = ifelse(HISPALLP_A %in% 97:99, NA, HISPALLP_A),
    EDUCP_A = ifelse(EDUCP_A %in% 97:99, NA, EDUCP_A),
    PHSTAT_A = ifelse(PHSTAT_A %in% 7:9, NA, PHSTAT_A),
    LSATIS4R_A = ifelse(LSATIS4R_A %in% 7:9, NA, LSATIS4R_A)
  ) %>%
  drop_na()

# Check cleaned dataset
str(nhis_clean)
summary(nhis_clean)
head(nhis_clean)

# Recode EDUCP_A
nhis_clean <- nhis_clean %>%
  mutate(
    EDUCP_A = case_when(
      EDUCP_A %in% 0:3 ~ 1,
      EDUCP_A == 4 ~ 2,
      EDUCP_A %in% 5:7 ~ 3,
      EDUCP_A %in% 8:10 ~ 4
    ),
    EDUCP_A = factor(
      EDUCP_A,
      levels = c(1, 2, 3, 4),
      labels = c(
        "Less than High School",
        "High School Graduate",
        "Some College Education",
        "College Graduate or Better"
      )
    )
  )

# Save cleaned dataset
write_csv(nhis_clean, "data/nhis_clean.csv")
