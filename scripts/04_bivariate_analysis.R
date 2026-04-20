# Bivariate Analysis

### Create side-by-side boxplots (using both plot() and ggplot2) to visualize the distribution of AGEP_A across different levels of SEX_A and EDUCP_A
boxplot(
  AGEP_A ~ factor(
    SEX_A,
    levels = c(1, 2),
    labels = c("Male", "Female")
  ) * EDUCP_A,
  main = "Distribution of Age by Sex and Education",
  xlab = "Education Level by Sex",
  ylab = "Age",
  col = c("mediumseagreen", "mediumslateblue"),
  las = 1,
  cex.axis = .5
)

ggplot(nhis_clean, aes(x = EDUCP_A, y = AGEP_A, fill = SEX_A)) +
  geom_boxplot() +
  ggtitle("Distribution of Age by Sex and Education") +
  xlab("Education Level") +
  ylab("Age") +
  scale_fill_manual(values = c("mediumseagreen", "mediumslateblue")) +
  theme_minimal()

### Create a clustered bar chart (using ggplot2) to show the relationship between PHSTAT_A (General Health) and LSATIS4R_A (Life Satisfaction)
ggplot(nhis_clean, aes(
  x = factor(
    PHSTAT_A,
    levels = c(1, 2, 3, 4, 5),
    labels = c(
      "Excellent", 
      "Very good", 
      "Good",
      "Fair",
      "Poor"
    )
  ),
  fill = factor(
    LSATIS4R_A,
    levels = c(1, 2, 3, 4),
    labels = c(
      "Very Satisfied",
      "Satisfied",
      "Dissatisfied",
      "Very Dissatisfied"
    )
  )
)) +
  geom_bar(position = "dodge") +
  labs(title = "Clustered Bar Chart of Life Satisfaction by General Health Status",
       x = "General Health Status",
       y = "Count",
       fill = "Quality of Life") +
  theme_minimal()

### Create a scatter plot (using both plot() and ggplot2) of HEIGHTTC_A vs. WEIGHTLBTC_A

plot(
  HEIGHTTC_A,
  WEIGHTLBTC_A,
  pch = 19,
  col = "cyan4",
  main = "Scatterplot of Height vs. Weight",
  xlab = "Height in Inches",
  ylab = "Weight in Pounds"
) 

ggplot(nhis_clean, aes(x= HEIGHTTC_A, y=WEIGHTLBTC_A))+
  geom_point(color="cyan4")+
  ggtitle("Scatterplot of Height vs. Weight")+
  xlab("Height in Inches")+
  ylab("Weight in Pounds")

### Calculate the correlation coefficient
cor(HEIGHTTC_A, WEIGHTLBTC_A)
