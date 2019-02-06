#Exercise 2
#Question 2
#Rasheeqa Isaacs
#5 Feb 2019

#load libraries
library(tidyverse)
library(ggpubr)
library(scales)
lam <- read.csv("data/laminaria.csv") #loading laminaria data

lam_1 <- lam %>%
  filter(region == "FB") #extracting only the "FB" region


  
lam_10 <-ggplot(data = lam_1, aes(x = blade_length, y = blade_weight)) + #parent line 
  scale_color_brewer(palette = "Accent") +#
  geom_point(aes (colour = site)) +  #plotting point and colour by site
  geom_line(aes (colour = site)) + #plotting the line graph and colour by site
  facet_wrap(~site, ncol=3) +  #facetting by dividing them into columns of 3
  scale_x_discrete(breaks = c("100", "125", "150", "175")) + #setting the scale for the x axis using the concaternate function using the values 100, 125, 150, 175
  scale_y_continuous(breaks = c(0,1,2,3)) +  #setting the scale for y axis using the concaternate function using values 0, 1, 2, 3
  labs(x = "Blade length (cm)", y = "Blade Mass (kg)") +  #adding labels to the x and y axis
  ggtitle("A crazy graph of some data for False Bay sites")
  

lam_2 <- ggplot(data = lam_1, aes(x = blade_length, y = blade_weight, colour = site)) + #parent line 
  geom_point(aes (colour = site)) +  #plotting point and colour by site
  geom_line(aes (colour = site)) + #plotting the line graph and colour by site
  facet_wrap(~site, ncol=3) +  #facetting by dividing them into columns of 3
  scale_x_discrete(breaks = c("100", "125", "150", "175")) + #setting the scale for the x axis using the concaternate function using the values 100, 125, 150, 175
  scale_y_continuous(breaks = c(0,1,2,3)) +  #setting the scale for y axis using the concaternate function using values 0, 1, 2, 3
  labs(x = "Blade length (cm)", y = "Blade Mass (kg)") +  #adding labels to the x and y axis
  ggtitle("A crazy graph of some data for False Bay sites")  #add title to the graph
 
#Roman Rock data showed because i did not add a specific colour

ggarrange(lam_10, lam_2)
          
          
  
head(lam, n = 7) #show first 7 rows 
tail(lam, n = 8)#show last 8 rows

glimpse(lam)  #shows the structure of the data and data that is contained in each variable 

lam_select <- lam %>%
  select(stipe_length, stipe_mass) %>%  #selecting columns stipe length and stipe mass 
  slice(2:12)  #selecting rows between 2 and 12
              
  
  
  
  

