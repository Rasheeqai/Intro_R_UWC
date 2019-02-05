#Mapping with ggplot 
#Day3
#Rasheeqa Isaacs
#31 Jan 2019

#load libraries
library(tidyverse)
library(ggpubr)

#These files are R extended format. But were excel format. 
#Load each RData file.
#Do not use read csv. when its RData format, just use LOAD.
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
load("data/MUR_low_res.RData")

#sst- sea surface temperature
#changed the name
sst <- MUR_low_res

# The colour pallette we will use for ocean temperature
cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc",
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a")

ChickWeight <- datasets::ChickWeight

ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point()

#South African coast dataset
#to follow a path using geom_path
#refer our variables to long and lat.
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_point() +
  labs(x = "longtde", y = "lat") +
  ggtitle("South African Coast")

#Include filling the path around South Africa with a line.
#geom polygon created in a way it automatically create a border around South Africa
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "red", fill = "blue", aes(group = group)) #The Land Mask

#BORDERS: Adding borders to provinces
#Used to diff. datasets. 

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) #The Province borders

#excluding coordinates.
#xlim: adding max and min value the same for ylim
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) + 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) #Force lon/lat extent

#aes specify what we put on our x and y axis.
#geom_raster: new data set "sst".
#Bins are range of temp. eg. (26,28) 
#(group = group) its a function and name of the column
#
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#Always lon on "x" and lat on "y"
#geom_polygon give outline of the coast
#sa_privince new coordinates
#scale_fill_manual- create label for scale
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0)

#
final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins), 
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4)) # Fine tune position of legend
final_map


lam <- read_csv("data/laminaria.csv")
save(laminaria, file= "data/laminaria.RData")


# [A.A]
# Scipt runs fully
# Self made comments will be benificial when understnading/studying the code for the exam
# A lot of these comments seems coppied and pasted
# EXplore and explain what each line does in future scripts
