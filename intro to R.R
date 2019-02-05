
#Day1
#Simple stats
#Rasheeqa
#Date


a+b # Here i am adding a to b

#1. What are the values after each statement in the following?
  
  mass <- 48              # mass? 48
mass <- mass * 2.0      # mass? 
age  <- 126             # age? 126
age  <- age - 17       # age?
mass_index <- mass/age  # mass_index? 

#2. Use R to calculate some simple mathematical expressions entered.

# Assign the value of 40 to x
# Assign the value of 23 to y
# Make z the value of x-y
# Display z in the console
x <- 40
y <- 23
z <- (x-y)
z
mass_index <- mass/age

#Day_1.R
#Calculate a monthly climatology per site
#Author: Rasheeqa Isaacs
#Date: 29 January 2019

library(tidyverse)
library(lubridate)


read_csv("data/data/SACTN_data.csv")

temp <- read.csv("data/data/SACTN_data.csv")

temp2 <- temp %>%
  dplyr::mutate(month = month(date)) %>% 
  dplyr::group_by(site, month) %>% 
  dplyr::summarise(temp = mean(temp, na.rm = TRUE)) %>% 
  ungroup()

  
  