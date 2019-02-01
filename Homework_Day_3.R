#Homework_Day3
#Rasheeqa Isaacs
#31 Jan 2019

#Homework:
#Convert the laminaria to R.Data
#Write a 5 line paragraph of two packages ggsn and scales
#Read and Understand all work

library(tidyverse)
lam <- read_csv("data/laminaria.csv") #import the laminaria data as a csv. file
save(lam,file = "data/laminaria.RData") #then save it as a R.Data file 

??ggsn

??scales

#GGSN is a package that allows the north symbol and scale bar to be present on maps created in R using "ggplot" or "ggmap". The north symbols and scale bars in kilometers is added to maps in geographic and metric coordinates which is created with "ggplot" or "ggmap".
#Scales is a package used to add elements to a graph using "ggplot" such as adding colour to the outline of a country or adding colour by filling the country. This package is capable of adding text to a map as well as being able to move the text on the map to a desired position. It also produces an overview map of a continent or world map.




