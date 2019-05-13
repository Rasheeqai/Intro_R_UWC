#Practice script

#loading the tidyverse package
library(tidyverse)

#CHAPTER 2
#adding the chickweight data
chicks <- as_tibble(ChickWeight)

#showing the first 6 rows in the data, remember to put datasets in the brackets instead of the other way around
head(chicks)
tail(chicks, n = 2)

#displaying the column names
colnames(chicks)

#calculating the mean, median, min, max.
summary(chicks)

#CHAPTER 3

#how many weights occur across all diets and time? 
chicks %>%
  summarise(length = n())

#or

length(chicks$weight)

#creating your own data 
dat1 <- c(23, 45, 23, 66, 13)

#calculating the mean
mean(dat1) 

#calculating the mean of only the weight column
chicks %>%
  summarise(mean_wt = mean(weight))








