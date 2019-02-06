#Exercise 2
#Question 3
#Rasheeqa Isaacs
#5 Feb 2019

#load libraries
library(tidyverse)
library(ggpubr)
library(scales)

ToothGrowth <- datasets::ToothGrowth #loading the ToothGrowth dataset

Toothgrowth1 <- ToothGrowth %>%
  group_by(supp, dose) %>%  #grouping by supp and dose
  summarise (mean_length = mean(len),  #calculate the mean length
            sd_length = sd(len)) #standard deviation of length

ToothGrowth1 <- ToothGrowth %>%
  ggplot(data = Toothgrowth1, aes(x = dose, y = mean_length, fill = supp)) +  #parent line
  geom_col(aes(fill = supp), position = "dodge", colour = "black") +  #plotting the columns and using 
  geom_errorbar(aes(ymin = mean_length - sd_length,  # plotting the error bars using ymin= mean-sd
                    ymax = mean_length + sd_length),  #plotting error bars using ymax= mean+sd
                    position = "dodge") + 
  labs(x = "Dose (mg/d)", y = "Tooth Length (m)") + #adding labels to the x and y axis
  ggtitle("Showing the rate of vitamin c dosage and tooth length")  #adding a title to the graph


#Exploring data

head(ToothGrowth, n = 4) #show first 4 rows 
tail(ToothGrowth, n = 6)#show last 6 rows

glimpse(ToothGrowth)  #shows the structure of the data and data that is contained in each variable 
  
ToothGrowth_select <- ToothGrowth %>%
  select(len, supp) %>% #select sites len and supp
  slice(4:10) #select from row 4 to 10

  
  
  
  