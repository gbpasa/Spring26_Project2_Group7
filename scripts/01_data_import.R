install.packages("tidyverse")
library(tidyverse)

nhis <- read_csv("data/NHIS _Data_2021.csv")

str(nhis)
summary(nhis)
head(nhis)
