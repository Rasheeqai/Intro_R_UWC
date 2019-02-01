#Day4
#Tidy data 2.0
#01 Feb 2019
#Rasheeqa Isaacs

#load libraries
library(tidyverse)
library(lubridate)

# Load the data from RData file
load("data/SACTNmonthly_v4.0.RData")
SACTN <- SACTNmonthly_v4.0 # Copy the data as a dataframe [assigning new name]
rm(SACTNmonthly_v4.0)  # Remove the original data
 
SACTN %>% 
  filter(site == "Amanzimtoti") #extract from site column and replace with different site

SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1) #straight line symbol means "or".
                                                                        #lubridate allows you to use dates, month and year if its not in the format
                                                                        # we use the numbers because of the combined dates in one column
#filter site "pollock beach" select month in the date column which is the last month ofthe year or select month in the date column but only the first month

SACTN %>% 
  arrange(depth, temp) #arrange columns from the lowest value to the highest value

SACTN %>% 
  arrange(desc(temp)) #arrange columns from highest to the lowest value

SACTN %>% 
  filter(site == "Humewood", year(date) == 1990) #filter date column to the year "1990" of the site "Humewood"

SACTN %>% 
  filter(site == "Humewood", year(date) == 1992) #filter date column to the year "1992" of the site "Humewood"

# Select columns individually by name
try1 <- SACTN %>% 
  select(site, src, date, temp)

# Select all columns between site and temp like a sequence
try2 <- SACTN %>% #new data frame
  select(site:temp) #from site to temp

# Select all columns except date and depth
try3 <- SACTN %>% 
  select(-date, -depth) #remove date and depth column

# Select all columns except those within a given sequence
# Note that the '-' goes outside of a new set of brackets
# that are wrapped around the sequence of columns to remove
try4 <- SACTN %>% 
  select(-(date:depth)) # ":" exlude from date to depth

try_5 <- SACTN %>% #select data set #pipe function "of then"
  mutate(kelvin = temp + 273.15) #fix error by removing one bracket #kelvin is the column we are mutating [creating new column] adding each temp value by 273.15 and name it kelvin [mutate]

SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), # summarise function require a name for a column
          sd_temp = sd(temp, na.rm = TRUE),       # na.rm- NA value affect dataset that means to remove NA values.
          min_temp = min(temp, na.rm = TRUE),
          max_temp = max(temp, na.rm = TRUE))


