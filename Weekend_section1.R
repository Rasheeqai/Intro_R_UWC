#Weekend Homework Section 1
#Rasheeqa Isaacs
#1 Feb 2019

# Section 1: 
# Make use of the rast_feb and rast_aug dataset:
# Explore the dataset (Hint* head, tail, glimpse etc) - Make use of google for more functions on exploring a dataset
# Create a map by making use of the lat and long variables
# Create a colour pallete using the link in the document and make use this colour pallete on the map
# Add complete labels and titles to the map
# Add the name of the oceans (Atlanic and indian ocean) on the map, increase the size of the labels
# The map should include the north arrow and scale bar
# Bonus marks for insetting (having a smaller map inside another map)
# Get creative, try new things.


#rast_feb make a graphs

#load libraries
library(tidyverse)
library(ggpubr)
library(ggsn)
load("data/rast_aug.RData")
load("data/rast_feb.RData")
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/africa_map.RData")

head(rast_aug, n=50) #showing the first 50 rows 
tail(rast_aug, n=35) #showing the last 35 rows

head(rast_feb, n=20) #showing the first 20 rows
tail(rast_feb, n=12) #showing the last 12 rows

glimpse(rast_aug) #shows the structure of the data and data that is contained in each variable 
glimpse(rast_feb)

rast_aug_select <- rast_aug %>% # Tell R which dataframe we are using
  select(season, temp) %>%  #select only season and temperature columns
  slice(45:65) #only choosing between rows 45 to 65 

rast_aug_max <- rast_aug %>%
  filter(lon == max(lon))  #filter the maximum longitude 

rast_feb_min <- rast_feb %>%
  filter(lat == min(lat)) #filter the minimum latitude



# The colour pallette we will use for ocean temperature
colr <- c("#5BD9E0", "#4FBEC4", "#44A4AA", "#398B90", "#2E7277", "#235A5E", "#194447")

colr1 <- c("#48B4B6", "#40A3A5", "#399394", "#328384", "#2B7374", "#246464", "#1E5555", "#174746")

#aes specify what we put on our x and y axis.
#geom_raster: new data set "rast_aug".
#Bins are range of temp. eg. (26,28) 
#(group = group) its a function and name of the column

rast_aug_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = rast_aug, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "grey28", fill = "orchid1", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = colr) + #the colour pallette
  coord_equal(xlim = c(14.5, 35), ylim = c(-36, -22), expand = 0) +
  scale_x_continuous(position = "top")

rast_aug_map1 <- rast_aug_map +
  annotate("text", label = "Atlantic\nOcean", #Adding the ocean labels
           x = 16, y = -32.0,  #locating it on the map using map coordinates
           size = 3.5, 
           angle = 30,    #setting the angle of the words
           colour = "maroon1") +   #changing the clour of the ocean name
  annotate("text", label = "Indian\nOcean", 
           x = 32, y = -32.5, 
           size = 4.0, 
           angle = 330, 
           colour = "chocolate1")

rast_aug_map2 <- rast_aug_map1 +
  scalebar(x.min = 26, x.max = 30, y.min = -34, y.max = -35, # Setting the location of bar
           dist = 150, height = 0.4, st.dist = 0.3, st.size = 2, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 17, x.max = 22, y.min = -22, y.max = -25, # Set location of symbol
        scale = 1.3, symbol = 5)

africa_map
rast_aug_map3 <- rast_aug_map2 +
  annotation_custom(grob = ggplotGrob(africa_map),  #insetting the africa_map onto the south african map
                  xmin = 26, xmax = 30,
                  ymin = -28, ymax = -24)

rast_aug_map_final <- rast_aug_map3 +
  labs(x = "longitude", y = "latitude") + #adding/changing the x and y axis lables
  ggtitle("Rast_Aug_Map")       #adding a title to the map
 
#mapping rast_feb data  
rast_feb_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = rast_feb, aes(fill = bins)) + # The ocean temperatures
  geom_polygon(colour = "grey80", fill = "chocolate", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = colr1) + #the colour pallette
  coord_equal(xlim = c(14.5, 35), ylim = c(-36, -22), expand = 0) +
  scale_x_continuous(position = "top")

rast_feb_map1 <- rast_feb_map +
  annotate("text", label = "Atlantic\nOcean", #Adding the ocean labels
           x = 16, y = -32.0,  #locating it on the map using map coordinates
           size = 3.5, 
           angle = 30,    #setting the angle of the words
           colour = "darkseagreen3") +   #changing the clour of the ocean name
  annotate("text", label = "Indian\nOcean", 
           x = 32, y = -32.5, 
           size = 4.0, 
           angle = 330, 
           colour = "cyan3")

rast_feb_map2 <- rast_feb_map1 +
  scalebar(x.min = 26, x.max = 30, y.min = -34, y.max = -35, # Setting the location of bar
           dist = 150, height = 0.4, st.dist = 0.3, st.size = 2, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance
  north(x.min = 16, x.max = 20, y.min = -34, y.max = -35, # Set location of symbol
        scale = 1.3, symbol = 5)

africa_map
rast_feb_map3 <- rast_feb_map2 +
  annotation_custom(grob = ggplotGrob(africa_map),  #insetting the africa_map onto the south african map
                    xmin = 20, xmax = 25,
                    ymin = -28, ymax = -32)

rast_feb_map_final <- rast_feb_map3 +
  labs(x = "longitude", y = "latitude") + #adding/changing the x and y axis lables
  ggtitle("Rast_Feb_Map")       #adding a title to the map




























