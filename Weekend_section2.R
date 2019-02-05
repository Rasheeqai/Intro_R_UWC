#Weekend Homework Section 2
#Rasheeqa Isaacs
#1 Feb 2019

# Section 2: 
# Make use of the ecklonia.csv dataset:
# Explore the data (Hint* head, tail, glimpse functions)
# Demonstrate the dimensions of the dataset [dim] function
# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Make use of the ggarrange function and arrange these three graphs created above into 1 plot
# All graphs must have labels as well as titles
# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites (Hint* group_by site)
# Calculate standard error
# Determine the min and maximum frond length and stipe length
# Determine the overall summary of the dataset

#hypothesis for each graph 
#summarise[calc mean, median, mode, var,sd] sd+var= se

#loading library
library(tidyverse)
library(ggpubr)
eck <- read.csv("data/ecklonia.csv")

head(eck, n=10) #shows the first 10 rows
tail(eck, n=4) #shows the last 4 rows

glimpse(eck) #shows the structure of the data and data that is contained in each variable 

eck_select <- eck %>%
  select(site, frond_length) %>% #select wo columns of data
  slice(6:14)  #selected rows 6 to 14 of the data

eck_bould <- eck %>%
  filter(site == "Boulders Beach") #extract the data for the site Boulders Beach

eck %>%
  filter(frond_length == min(frond_length)) #selected the row with minimum frond length

dim(eck) 
  
line_graph <- ggplot(data= eck, aes(x = stipe_diameter, y = stipe_length)) +
  geom_point(colour = "deeppink2") +   #plotting points and add colour
  geom_line(colour = "chocolate3") +  #plot line graph and add colour 
  labs(x = "stipe_diameter", y = "stipe_length (m)") + #adding labels to axis
  ggtitle("Stipe Length in various sites")  #add title to graph

#Hypothesis: Stipe diameter is not influenced by the stipe length

box_plot1 <- ggplot(data = eck, aes(x = stipe_diameter, y = stipe_length)) + #parent line
  geom_boxplot(aes(fill = stipe_diameter, group = site)) +   #plotting the boxplot using stipe diameter and group by site
  labs(x = "Stipe Diameter", y = "Stipe length (m)") +  #add labels to the x and y axis
  ggtitle("The relationship between stipe diameter and stipe length")  #add title to the boxplot

#Hypothesis: The longer the stipe the bigger the diameter

bar_graph <- ggplot(eck) +
  geom_bar(aes(x = stipe_diameter)) +
  ggtitle("The Stipe Diameter of Boulders Beach and Batsata Rock")

#Hypothesis: Stipe Diameter will vary with the Stipe Length 


plot1 <- ggarrange(line_graph, box_plot1, bar_graph)  #arranging the graphs
plot1          
          
eck %>%
  group_by(site) %>% 
  summarise(avrg_stipe_d = mean(stipe_diameter), #calculate mean stipe diameter
            min_stipe_d = min(stipe_diameter),  #calculate the minimum value
            max_stipe_d = max(stipe_diameter),  #calculate the maximum value
            med_stipe_d = median(stipe_diameter), #calculate the median
            var_stipe_d = var(stipe_diameter),  #calculate the variance
            n_stipe_d = n()) %>%  #number of values
  mutate(se_stipe_d = sqrt(var_stipe_d/n_stipe_d))  #adding a column and calculating standard error


eck %>%
  group_by(site) %>% 
  summarise(avrg_stipe_l = mean(stipe_length), #calculate mean stipe length
            min_stipe_l = min(stipe_length),  #calculate the minimum value
            max_stipe_l = max(stipe_length),  #calculate the maximum value
            med_stipe_l = median(stipe_length),  #calculate the median
            var_stipe_l = var(stipe_length), #calculate the variance
            n_stipe_l = n()) %>%  #number of values
  mutate(se_stipe_l = sqrt(var_stipe_l/n_stipe_l))  #adding a column and calculating standard error


eck %>%
  group_by(site) %>% 
  summarise(min_stipe_l1 = min(stipe_length),  #minimum value of stipe length
            max_stipe_l1 = max(stipe_length))  #maximum value of stipe length


eck %>%
  group_by(site) %>% #grouping by site
  summarise(min_frond_l = min(frond_length),  #minimum value of frond length
            max_frond_l = max(frond_length))  #maximum value of frond length


summary(eck) #summary of overall dataset
  



  

  








