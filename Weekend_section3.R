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

SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy, by = c("site", "src", "date"))



#Tidier functions

# Load the RData file
load("data/SACTNmonthly_v4.0.RData")
SACTN <- SACTNmonthly_v4.0 # Copy the data as a dataframe [assigning new name]
rm(SACTNmonthly_v4.0)  # Remove the original data

SACTN %>% 
  filter(site == "Amanzimtoti") #extract from site column and replace with different site

SACTN %>% 
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1) #straight line symbol means "or".
#lubridate allows you to use dates, month and year if its not in the format
# we use the numbers because of the combined dates in one column
#filter site "pollock beach" select month in the date column which is the last month ofthe year or select month in the date column but only the first month

SACTN %>% 
  arrange(depth, temp) #arrange columns from the lowest value to the highest value

SACTN %>% 
  arrange(desc(temp)) #arrange columns from highest to the lowest value

SACTN %>% 
  filter(site == "Humewood", year(date) == 1990) #filter date column to the year "1990" of the site "Humewood"

SACTN %>% 
  filter(site == "Humewood", year(date) == 1992) #filter date column to the year "1992" of the site "Humewood"

humewood_90s <- SACTN %>%
  filter(site == "Humewood", year(date) %in% seq(1990, 1999, 1))

SACTN %>%
  filter(site == "Port Nolloth", # extract site Port Nolloth
         src == "DEA", # specify the source
         temp <= 11 | # Temperatures below 11°C
           is.na(temp)) # Includes the missing values
  



# Select columns individually by name
try1 <- SACTN %>% 
  select(site, src, date, temp)

# Select all columns between site and temp like a sequence
try2 <- SACTN %>% #new data frame
  select(site:temp) #from site to temp

# Select all columns except date and depth
try3 <- SACTN %>% 
  select(-date, -depth) #remove date and depth column

# Select all columns except those within a given sequence
# Note that the '-' goes outside of a new set of brackets
# that are wrapped around the sequence of columns to remove
try4 <- SACTN %>% 
  select(-(date:depth)) # ":" exlude from date to depth

try_5 <- SACTN %>% #select data set #pipe function "of then"
  mutate(kelvin = temp + 273.15) #fix error by removing one bracket #kelvin is the column we are mutating [creating new column] adding each temp value by 273.15 and name it kelvin [mutate]

SACTN %>%
  select(type, src, everything()) # Use the "everything" function to grab all columns

SACTN %>%
  select(temp:type, everything(), -src) # Use the "everything" function to grab all columns, excluding the source column

SACTN %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE)) #mean temperature excluding the "NA" values

SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE), # summarise function require a name for a column
            sd_temp = sd(temp, na.rm = TRUE),       # na.rm- NA value affect dataset that means to remove NA values.
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))



#Tidiest data

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


# Create groupings based on temperatures and depth
SACTN_temp_group <- SACTN %>%
  group_by(round(temp), depth) #create groups based on temperature and depth

SACTN_depth_mean_2 <- SACTN %>% # Choose a dataframe
  group_by(depth) %>% # Group by depth column
  summarise(mean_temp = mean(temp, na.rm = TRUE), # Calculate the mean
            count = n()) # Count observations
  

SACTN_30_years <- SACTN %>% 
  group_by(site, src) %>%
  filter(n() > 360)       #only selecting data greater than 30 years

SACTN_anom <- SACTN %>%
  group_by(site, src) %>%
  mutate(anom = temp - mean(temp, na.rm = T)) %>%
  select(site:date, anom, depth, type) %>%
  ungroup()

selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood") #concaternate creat sites by selecting it

SACTN %>%
  filter(site == "Paternoster" | site == "Oudekraal") %>%
  group_by(site, src) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE))

SACTN %>% # Choose starting dataframe
  filter(site %in% c("Bordjies", "Tsitsikamma", "Humewood", "Durban")) %>% # Select sites
  select(-depth, -type) %>% # Removing depth and type columns
  mutate(month = month(date), # Create a month column
         index = paste(site, src, sep = "/ ")) %>% # Create individual site column
  group_by(index, month) %>% # Group by individual sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), # Calculate mean temperature
            sd_temp = sd(temp, na.rm = TRUE)) %>% # Calculate standard deviation
  ggplot(aes(x = month, y = mean_temp)) + # Begin with ggplot, switch from '%>%' to '+'
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp),
              fill = "black", alpha = 0.4) + # Create ribbon
  geom_line(col = "red", size = 0.3) + # Create lines graph
  facet_wrap(~index) + # Facet individual sites
  scale_x_continuous(breaks = seq(2, 12, 4)) + # Control x axis ticks
  labs(x = "Month", y = "Temperature (°C)") + # Giving labels
  theme_dark() # Give a theme

SACTN %>% #select data set
  filter(site %in% selected_sites) %>% #want to filter 4 sites in selected sites #%in%: in 
  group_by(site, src) %>%     #group sites together and sources together
  summarise(mean_temp = mean(temp, na.rm = TRUE), 
            sd_temp = sd(temp, na.rm = TRUE))

SACTN %>% 
  filter(site == "Port Nolloth") # selected only one site




  












