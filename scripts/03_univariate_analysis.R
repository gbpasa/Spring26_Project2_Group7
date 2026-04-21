# Univariate & Bivariate Analysis & Visualization

library(tidyverse)
library(summarytools)

## Quantitative Variables

### Display summary statistics of the quantitative variables: AGEP_A, WEIGHTLBTC_A, and HEIGHTTC_A
lapply(nhis_clean[c("AGEP_A", "WEIGHTLBTC_A", "HEIGHTTC_A")], descr)

summary(nhis_clean$AGEP_A)
mean(nhis_clean$AGEP_A)
median(nhis_clean$AGEP_A)
sd(nhis_clean$AGEP_A)

summary(nhis_clean$WEIGHTLBTC_A)
mean(nhis_clean$WEIGHTLBTC_A)
median(nhis_clean$WEIGHTLBTC_A)
sd(nhis_clean$WEIGHTLBTC_A)

summary(nhis_clean$HEIGHTTC_A)
mean(nhis_clean$HEIGHTTC_A)
median(nhis_clean$HEIGHTTC_A)
sd(nhis_clean$HEIGHTTC_A)


### Create base R and ggplot2 histograms of AGEP_A
hist(
  nhis_clean$AGEP_A,
  main = "Distribution of Age Across Participants",
  col = "lightgreen",
  xlab = "Age in Years",
  ylab = "Count"
)

ggplot(nhis_clean, aes(x = AGEP_A)) +
  geom_histogram(binwidth = 2,
                 fill = "green4",
                 color = "black") +
  ggtitle("Distribution of Age Across Participants") +
  xlab("Age in Years") +
  ylab("Count") +
  theme_classic()

### Create base R and ggplot2 boxplots of AGEP_A
boxplot(nhis_clean$AGEP_A,
        main = "Distribution of Age Across Participants",
        col = "yellow",
        ylab = "Age in Years")

ggplot(nhis_clean, aes(y = AGEP_A)) +
  stat_boxplot(geom = 'errorbar', width = 0.5) +
  geom_boxplot(fill = "gold") +
  scale_x_discrete() +
  labs(title = "Distribution of Age Across Participants", y = "Age in Years") +
  theme_minimal()


### Create base r and ggplot2 histograms of WEIGHTLBTC_A
hist(
  nhis_clean$WEIGHTLBTC_A,
  main = "Distribution of Weight Across Participants",
  col = "orangered",
  xlab = "Weight in Pounds",
  ylab = "Count"
)

ggplot(nhis_clean, aes(x = WEIGHTLBTC_A)) +
  geom_histogram(
    binwidth = 5,
    fill = "orangered3",
    color = "black"
  ) +
  ggtitle("Distribution of Weight Across Participants") +
  xlab("Weight in Pounds") +
  ylab("Count") +
  theme_classic()

### Create base R and ggplot2 boxplots of WEIGHTLBTC_A
boxplot(
  nhis_clean$WEIGHTLBTC_A,
  main = "Distribution of Weight Across Participants",
  col = "rosybrown",
  ylab = "Weight in Pounds"
)

ggplot(nhis_clean, aes(y = WEIGHTLBTC_A)) +
  stat_boxplot(geom = 'errorbar', width = 0.5) +
  geom_boxplot(fill = "lightpink4") +
  scale_x_discrete() +
  labs(title = "Distribution of Weight Across Participants", y = "Weight in Pounds") +
  theme_minimal()

### Create base r and ggplot2 histograms of HEIGHTTC_A
hist(
  nhis_clean$HEIGHTTC_A,
  main = "Distribution of Height Across Participants",
  col = "dodgerblue",
  xlab = "Height in Inches",
  ylab = "Count"
)

ggplot(nhis_clean, aes(x = HEIGHTTC_A)) +
  geom_histogram(
    binwidth = 1,
    fill = "dodgerblue4",
    color = "black"
  ) +
  ggtitle("Distribution of Height Across Participants") +
  xlab("Height in Inches") +
  ylab("Count") +
  theme_classic()

### Create base R and ggplot2 boxplots of HEIGHTTC_A
boxplot(
  nhis_clean$HEIGHTTC_A,
  main = "Distribution of Height Across Participants",
  col = "mediumslateblue",
  ylab = "Height in Inches"
)

ggplot(nhis_clean, aes(y = HEIGHTTC_A)) +
  stat_boxplot(geom = 'errorbar', width = 0.5) +
  geom_boxplot(fill = "slateblue4") +
  scale_x_discrete() +
  labs(title = "Distribution of Weight Across Participants", y = "Height in Inches") +
  theme_minimal()


## Qualitative Variables

### Display frequency tables for qualitative variables: SEX_A, HISPALLP_A, EDUCP_A, PHSTAT_A, and LSATIS4R_A
lapply(nhis_clean[c("SEX_A", "HISPALLP_A", "EDUCP_A", "PHSTAT_A", "LSATIS4R_A")], table)


### Display base R and ggplot 2 bar chart of SEX_A
barplot(
  table(nhis_clean$SEX_A),
  names.arg = factor(c("Male", "Female")),
  xlab = "Sex",
  ylab = "Count",
  col = topo.colors(2),
  main = "Bar Chart of Participants' Sex",
  border = "black"
)

ggplot(nhis_clean, aes(x = factor(
  SEX_A,
  levels = c(1, 2),
  labels = c("Male", "Female")
))) +
  geom_bar(fill = topo.colors(2), color = "black") +
  ggtitle("Bar Chart of Participants' Sex") +
  xlab("Sex") +
  ylab("Count") +
  theme_classic()


### Display base R and ggplot 2 bar chart of HISPALLP_A
barplot(
  table(nhis_clean$HISPALLP_A),
  names.arg = factor(
    c(
      "Hispanic",
      "NH white only",
      "NH Black/AfricanAmerican only",
      "NH Asian only",
      "NH AIAN",
      "NH AIAN andany other group",
      "Other single and multiple races"
    )
  ),
  cex.names = .55,
  xlab = "Race/ Ethnicity",
  ylab = "Count",
  col = rainbow(7),
  main = "Bar Chart of Participants' Race/ Ethnicity",
  border = "black"
)


ggplot(nhis_clean, aes(x = factor(
  HISPALLP_A,
  levels = c(1, 2, 3, 4, 5, 6, 7),
  labels = c(
    "Hispanic",
    "NH white only",
    "NH Black/African
            American only",
    "NH Asian only",
    "NH AIAN",
    "NH AIAN and
             any other group",
    "Other single and
             multiple races"
  )
))) +
  geom_bar(fill = rainbow(7), color = "black") +
  ggtitle("Bar Chart of Participants' Race/ Ethnicity") +
  xlab("Race/Ethnicity") +
  ylab("Count") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 8))

### Display base R and ggplot 2 bar chart of EDUCP_A
barplot(
  table(nhis_clean$EDUCP_A),
  col = heat.colors(4),
  main = "Bar Chart of Participants' Educational Level",
  xlab = "Educational Level",
  ylab = "Count"
)


ggplot(nhis_clean, aes(x =EDUCP_A)) +
  geom_bar(fill = heat.colors(4), color = "black") +
  ggtitle("Bar Chart of Participants' Educational Level") +
  xlab("Educational Level") +
  ylab("Count") +
  theme_classic() +
  theme(axis.text.x = element_text(size = 6))

### Display base R and ggplot 2 bar chart of PHSTAT_A
barplot(
  table(nhis_clean$PHSTAT_A),
  names.arg = factor(c(
    "Excellent",
    "Very good",
    "Good", 
    "Fair", 
    "Poor"
  )),
  col = terrain.colors(5),
  main = "Bar Chart of Participants' General Health Status",
  xlab = "General Health Status",
  ylab = "Count",
  border = "black"
)

ggplot(nhis_clean, aes(x = factor(
  PHSTAT_A,
  levels = c(1, 2, 3, 4, 5),
  labels = c("Excellent", "Very good", "Good", "Fair", "Poor")
))) +
  geom_bar(fill = terrain.colors(5), color = "black") +
  ggtitle("Bar Chart of Participants' General Health Status") +
  xlab("General Health Status") +
  ylab("Count") +
  theme_classic()

### Display base R and ggplot 2 bar chart of LSATIS4R_A

barplot(
  table(nhis_clean$LSATIS4R_A),
  names.arg = factor(
    c(
      "Very Satisfied",
      "Satisfied",
      "Dissatisfied",
      "Very Dissatisfied"
    )
  ),
  xlab = "Quality of Life ",
  ylab = "Count",
  col = cm.colors(4),
  main = "Bar Chart of Participants' Life Satisfaction",
  border = "black"
)

ggplot(nhis_clean, aes(x = factor(
  LSATIS4R_A,
  levels = c(1, 2, 3, 4),
  labels = c(
    "Very Satisfied",
    "Satisfied",
    "Dissatisfied",
    "Very Dissatisfied"
  )
))) +
  geom_bar(fill = cm.colors(4), color = "black") +
  ggtitle("Bar Chart of Participants' Life Satisfaction") +
  xlab("Quality of Life ") +
  ylab("Count") +
  theme_classic()

