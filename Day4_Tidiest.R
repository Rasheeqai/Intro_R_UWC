#Day 4
#Chapter 12
# 01 Feb 2019
#Final Chapter

#load libraries
library(tidyverse)

# load the data from a .RData file
load("data/SACTNmonthly_v4.0.RData")
SACTN <- SACTNmonthly_v4.0  # Copy the data as a dataframe with a shorter name
rm(SACTNmonthly_v4.0)  # Remove the original loaded in dataset, keep data to a minimum and clean


# Calculate the mean temperature by depth
SACTN_depth_mean <- SACTN %>% 
  group_by(depth) %>%   #grouping  depth together
  summarise(mean_temp = mean(temp, na.rm = TRUE),  #calculating the mean temperature
            count = n())
SACTN_depth_mean


ggplot(data = SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE) +
  labs(x = "depth (m)", y = "mean_temp (Celsius)") + 
  ggtitle("Mean Temperature in relation to Depth")


SACTN_30_years <- SACTN %>% 
  group_by(site, src) %>%
  filter(n() > 360)       #only selecting data greater than 30 years
 
selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood") #concaternate creat sites by selecting it

SACTN %>% #select data set
  filter(site %in% selected_sites) %>% #want to filter 4 sites in selected sites #%in%: in 
  group_by(site, src) %>%     #group sites together and sources together
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE))

SACTN %>% 
  filter(site == "Port Nolloth") # selected only one site


# [A.A]
# Neat script
# Good commenting
# Should try different things, make small changes to the code and see what its outcome is
# Try new things

