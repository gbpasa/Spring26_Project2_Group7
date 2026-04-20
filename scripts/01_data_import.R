install.packages("tidyverse")
library(tidyverse)

install.packages("summarytools")
library(summarytools)

install.packages("ggplot2")
library(ggplot2)

nhis <- read_csv("data/NHIS _Data_2021.csv")

str(nhis)
summary(nhis)
head(nhis)
