install.packages("tidyverse")

# Load necessary Libraries
library(tidyverse)

# Import the NHIS 2021 Dataset
nhis <- read_csv("data/NHIS _Data_2021.csv")

#Initial exploration of data
str(nhis)
summary(nhis)
head(nhis)



