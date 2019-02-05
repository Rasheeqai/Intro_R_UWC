#Weekend Homework Section 3: 
  # Make use of the SACTN_day1 data:
  # Here create a graph showing temperature variation between sites
  # Select all the temperatures recorded at the site Port Nolloth during August or September.
  # Select all the monthly temperatures recorded in Port Nolloth during the year 1994
  # Calculate the average temperature by depth
  # Work through the tidyverse section within the document. Show what you have done by creating comments/ notes throughout the script
  # tidy.tider. tidest

#loading libraries
library(tidyverse)
library(lubridate)

#load data
SACTN_day1 <- read.csv("data/SACTN_day_1.csv")

ggplot(data = SACTN_day1, aes(x = date, y = temp)) +       #specify dataset 
  geom_line(aes(colour = site, group = paste0(site, src))) +  #edit function must use aes. Use colour to make each site diff. colour. group = paste0 to include more than one variable it by site and source. 
  labs(x = "Date", y = "Temperature (°C)", colour = "site") +  #add labels
  ggtitle("Temperature Variation between various sites") +
  theme_classic()


Port_Nolloth <- SACTN_day1 %>%  
  filter(site == "Port Nolloth", month(date) == 8 | month(date) == 9) #extracting Port Nolloth data of months August and September

Port_Nolloth_1994 <- SACTN_day1 %>%
  filter(site == "Port Nolloth", year(date) == 1994) #Extracting data of Port Nolloth of the year 1994


SACTN_day1 %>%                   #***avrg depth
  group_by(site) %>%
  na.omit %>%   #remove the NA values
  summarise(avrg_SACTN = mean(temp))  


#work through tidy functions
#load data
#UTR- underwater temperature recorder
load("data/SACTN_mangled.RData")

ggplot(data = SACTN1, aes(x = date, y = temp)) +       #specify dataset 
  geom_line(aes(colour = site, group = paste0(site, src))) +  #edit function must use aes. Use colour to make each site diff. colour. group = paste0 to include more than one variable it by site and source. 
  labs(x = "", y = "Temperature (°C)", colour = "site") + #to add labels
  theme_cleveland() #add a theme


#Use the gather function to group sources "src" together in one column
SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")  #grouping data/sources together4







#Changing SACTN3- 2 columns into one column "depth" labelled SACTN3_tidy

SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val)   #spreading the data


#SACTN4a is used to seperate or split the one column into two columns
#it seperated the site and src into two columns
#use the concaternate (c) function to seperate more than one name or source
SACTN4a_tidy <- SACTN4a %>% 
  separate(col = index, into = c("site", "src"), sep = "/ ")


#combine data into one column [e.g.date]. sep- used to seperate
SACTN4b_tidy <- SACTN4b %>% 
  unite(year, month, day, col = "date", sep = "-")

#left_join function detects similar words (combines dataset) under same columns.
SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy) 

  












