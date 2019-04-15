#Exercise 2
#Question 1
#Rasheeqa Isaacs
#5 Feb 2019

#load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)
library(scales)

#loading data
load("data/SACTNmonthly_v4.0.RData") #original data
SACTNmonthly <- SACTNmonthly_v4.0  #selecting certain columns
rm(SACTNmonthly_v4.0) #removing original data

SACTN_annual <- SACTNmonthly %>%
  filter(src == "KZNSB") %>%  #extract the source "KZNSB" 
  separate(col = date, into = c("year", "month", "day"), sep = "-") %>% #separating the column date into individual columns labelled "year", "month", "day"
  group_by(site, year) %>%   #grouping by site and year 
  summarise(mean_temp = mean(temp)) %>%  #using the summarise function calculating the mean tempereaure of the "KZNSB" source
  na.omit() #removing the NA values 
  
  ggplot(data = SACTN_annual, aes(x = year, y = mean_temp)) +  #parent line 
  geom_line(aes(group = site), colour = "coral2") + #creating line graph grouping sites and adding the colour "coral2"
  facet_wrap(~site, ncol = 5) + #facetting graphs by dividing or displaying them into columns of 5
  scale_x_discrete(breaks = c("1980", "2000"),
                   labels = c("1980", "2000")) + #setting the scale for the x axis using the concaternate function using the values 1980, 2000
  scale_y_continuous(breaks = c(20, 22, 24),
                     labels = c("20", "22", "24")) + #setting the scale for y axis using the concaternate function using values 20, 22,24
  labs(x = "Year", y = "Temperature (Celsius)") +  #adding labels to the x and y axis
  ggtitle("KZNSB: Series of annual means")  #adding title to the graph
    
  
#Exploring data
  
  head(SACTNmonthly, n = 10) #show first 10 rows 
  tail(SACTNmonthly, n = 12)#show last 12 rows
  
  glimpse(SACTNmonthly)  #shows the structure of the data and data that is contained in each variable 
  
  SACTNmonthly_select <- SACTNmonthly %>%
    select(date, temp) %>%  #selected only rows date and temperature
    slice(20:35)  #selected rows 20 to 35
                                                                                                                              







  

  
  
  
  
  
  
  
  
  