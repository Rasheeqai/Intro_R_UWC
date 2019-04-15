#Exercise: Yearly Egg Breadths
#Rasheeqa Isaacs
#7 Feb 2019

#load libraries
library(tidyverse)
library(ggpubr)
library(scales)
library(lubridate)

Yearly_Egg_Breadths <- read.csv("data/YearlyEggBreadths.csv") #loading data

library(readr)
YearlyEggBreadths <- read_delim("data/YearlyEggBreadths.csv", 
                                ";", escape_double = FALSE, col_types = cols(StdDevMinBreadth1 = col_number()), 
                                trim_ws = TRUE)
View(YearlyEggBreadths)  #Yearly egg breadths changed from comma into semicolons then copy link into source

#character column occurs when there are commas and full stops in a column
#make sure we always put in the year, day and month in the data
#can plot both lines on one graph

#***do not need just for practice
YearlyEggBreadths1 <- YearlyEggBreadths %>%  #showing which dataframe is being used
  select(Months, AveragesMinBreadth1, AveragesMinBreadth2) %>%   #selecting the month, AveragesMinBreadth1 and AveragesMinBreadth2 columns 
  group_by(Months) %>%  #grouping by month
  na.omit()  #removing NA values

#creating a clean dataframe remove all unwanted rows and columns
new_egg <- YearlyEggBreadths %>%
  slice(1:12) %>%
  mutate(months = c(1:12))  #add in a month column

#creating a plot

YearlyEggBreadths1 <- ggplot(new_egg, aes(x = months)) +    #parent line using ggplot 
  geom_line(aes(y = AveragesMinBreadth1)) +
  geom_line(aes(y = AveragesMinBreadth2), colour = "orange") +
  labs(x = "Months", y = "Egg Breath (mm)") +
  geom_errorbar(aes(ymax = AveragesMinBreadth1 + as.numeric(StdDevMinBreadth1)), 
                    ymin = AveragesMinBreadth1 - as.numeric(StdDevMinBreadth1)) 
  geom_errorbar(aes(ymax = AveragesMinBreadth2 + as.numeric(StdDevMinBreadth2)),
                    ymin = AveragesMinBreadth2 - as.numeric(StdDevMinBreadth2)) 
  scale_x_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "July", "Aug", "Sep", "Oct", "Nov", "Dec"),
  scale_y_continuous(breaks = c(10, 20, 30, 40, 50, 60, 70), labels = c("10", "20", "30", "40", "50", "60", "70")))
               



                
  




  
 
  