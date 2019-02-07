#Exercise: Yearly Egg Breadths
#Rasheeqa Isaacs
#7 Feb 2019

#load libraries
library(tidyverse)
library(ggpubr)
library(scales)

Yearly_Egg_Breadths <- read.csv("data/YearlyEggBreadths.csv") #loading data

library(readr)
YearlyEggBreadths <- read_delim("data/YearlyEggBreadths.csv", 
                                ";", escape_double = FALSE, trim_ws = TRUE)
View(YearlyEggBreadths)  #Yearly egg breadths changed from comma into semicolons then copy link into source

YearlyEggBreadths1 <- YearlyEggBreadths %>%
  select(Months, AveragesMinBreadth1, AveragesMinBreadth2) %>%
  group_by(Months) %>% 
  na.omit()

YearlyEggBreadths1 <- ggplot(YearlyEggBreadths1, aes(x = Months, y = AveragesMinBreadth1)) +
  geom_point()




  
 
  