#Mapping with google
#Afternoon session
#Rasheeqa Isaacs
#31 Jan 2019

# Load libraries
library(tidyverse)
library(ggmap)

# Load data
load("data/cape_point_sites.RData")

#getting map of the satellite for longitude and latitude variables
cape_point <- get_map(location = c(lon = 18.36519, lat = -34.2352581),
                      zoom = 10, maptype = 'satellite')
# load("data/cape_point.RData")

#Add labels to the point on the map
cp_1 <- ggmap(cape_point) +
  geom_point(data = cape_point_sites, aes(x = lon+0.002, y = lat-0.007), 
             colour = "red", size =  2.5) +
  labs(x = "", y = "")
cp_1

cp_2 <- cp_1 +
  geom_text(data = cape_point_sites, # Choose dataframe
            aes(lon+0.002, lat-0.007, label = site), # Set aesthetics
            hjust = 1.15, vjust = 0.5, # Adjust vertical and horizontal
            size = 3, colour = "white") # Adjust appearance
cp_2

# [A.A]
# Neat script
# Script runs completely
# Sufficient comments
# Always good to play around with the functions in R and try different things other than the thigs taught in class

