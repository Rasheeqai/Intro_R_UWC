#Exercise 2
#Question 1
#Rasheeqa Isaacs
#5 Feb 2019

#load libraries
library(tidyverse)
library(lubridate)
load("data/SACTNmonthly_v4.0.RData")
SACTNmonthly <- SACTNmonthly_v4.0
rm(SACTNmonthly_v4.0)

SACTNmonthly %>% #choose dataframe
  filter(site) %>% #select sites
  select(-depth, -type) %>%  # Remove the depth and type columns
  mutate(year = year(date),# Create year column
  site = paste(date, sep = "/ ") %>% # Create individual site column
  group_by(site, year) %>% # Group by individual sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), # Calculate mean temperature
            sd_temp = sd(temp, na.rm = TRUE))) # Calculate standard deviation
 
  ggplot(aes(x = year, y = mean_temp)) + # Begin ggplot, switch from '%>%' to '+'
  geom_line(col = "deepskyblue2", size = 0.3) + # Create lines within ribbon
  facet_wrap(~site) + # Facet by individual sites
  scale_x_continuous(breaks = seq(1980, 2000)) + # Control x axis ticks
  labs(x = "Year", y = "Temperature (°C)") + # Change labels
  theme_bw() # Setting theme
  
  
  
  
  
  
  
  
  