install.packages("tidyverse")

# Load necessary Libraries
library(tidyverse)

<<<<<<< HEAD
# Import the NHIS 2021 Dataset
=======
install.packages("summarytools")
library(summarytools)

install.packages("ggplot2")
library(ggplot2)

>>>>>>> 990ab7bc49bc64c2835f052a48c9130fe9f68b78
nhis <- read_csv("data/NHIS _Data_2021.csv")

#Initial exploration of data
str(nhis)
summary(nhis)
head(nhis)



