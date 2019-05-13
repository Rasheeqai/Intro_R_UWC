#Rasheeqa Isaacs
#R_Assignment 
#13 May 2019

#Picking a dataset
library(help = "datasets")

#Load libraries
library(tidyverse)
library(plotly)
library(ggpubr)
library(corrplot)
library(ggplot2)
library(fitdistrplus)
library(rmarkdown)
library(e1071)
library(RColorBrewer)
library(ggthemes)
library(vegan)

#load dataset
women_america <- read.csv("rasheeqa_isaacs/women.csv")

#Changing data from non-normal to normal data
women_america <- decostand(women_america, "hellinger")

#Exploring data
#displayng the first 5 rows
head(women_america, n = 5)

#displaying the last 4 rows
tail(women_america, n = 4)

#Checking the structure of the data 
glimpse(women_america)

#The names of the columns in the dataset
names(women_america)

#dimension of the dataset
dim(women_america)

#summary of the dataset
summary(women_america)

#the number of columns
ncol(women_america)

#the number of rows
nrow(women_america)

#selecting from row 5 to 12
women_america %>%
slice(5:12)

#selecting the tallest person that weighs the most
women_america %>%
  filter(height == max(height))

#selecting the shortest person that weighs the least
women_america %>%
  filter(height == min(height))

#calculating the mean, median and standard deviation of the weight of women.
women_america %>% 
  summarise(avrg_wa = mean(weight),
            med_wa = median(weight),
            sd_wa = sd(weight))

#calculating the mean, median and standard deviation of the height of women
women_america %>% 
  summarise(avrg_wa = mean(height),
            med_wa = median(height),
            sd_wa = sd(height))

#Measuring the skewness of the data 
skewness(women_america$weight)
#Results shows that the data is right-skewed

#Measuring Kurtosis of the data
kurtosis(women_america$weight)
#Results show that the data has a thin-tailed (platykurtic) distribution.

#Spitting out the quantiles
quantile(women_america$weight)

#Testing if the data is normal using the Shapiro Test
shapiro.test(women_america$weight)

#OR

women_america %>% 
  summarise(norm_wt = as.numeric(shapiro.test(weight)[2]))
#Results show p-value is 0.6986 which makes the data of normal distribution

#Testing for homoscedasticity
women_america %>%
  summarise(var_wt = var(weight))

#Testing if it is normal data using the Cullen and Frey graph
descdist(women_america$weight)

#Creating a new theme for the graphs
theme_new <- theme_update(panel.background = element_rect(fill="darkseagreen1"))

#Created a density plot to visualize the data
ggplot(data = women_america, aes(x = weight)) +   #selecting data and choosing the independent variable for the graph 
  geom_density(colour = "green", fill = "green", alpha = 0.6) +   #creating a density plot and changing the colour of the graphs data to green
  ggtitle("The average weight of American women between ages 30-39") +  #adding a title to the graph
  labs(x = "weight", y = "") +  #adding labels to the graph
  theme_new   #adding th new theme that was made
  
#creating a line graph visualising the data
ggplot(data = women_america, aes(x = weight, y = height)) +
 geom_point(color = "deepskyblue2")+    #plotting data points and adding a colour
  geom_line(color = "deeppink")+   #plotting line data and adding a colour
  labs(x = "weight (lbs)", y = "height (in)") +    #adding labels
  ggtitle("The weight of American women between ages 30-39") +   #adding a title to the graph
  theme(legend.position = c(0.9, 0.2)) +   
  theme_new      #added new theme

#creating a bar graph to visualize the weight of individuals
women_america1 <- data.frame(x = 1:15, y = 115:164)
ggplot(women_america1, aes (x, y, fill= "weight")) +
  geom_bar(stat="identity") +
  labs(x = "Number of individuals", y = "weight (lbs)") +
  ggtitle("The weight of American women between 30-39 years old") +
  theme_new

#creating a bar graph to visualize the height of individuals
women_america2 <- data.frame(x = 1:15, y = 58:72)
ggplot(women_america2, aes (x, y, fill = "height")) +
  geom_bar(stat="identity") +
  labs(x = "Number of individuals", y = "height (in)") +
  ggtitle("The height of American women between 30-39 years old") +
  theme_new

#ANOVA Test
#Hypothesis
#H0: The average weight of women do not increase with height.
#H1: The average weight of women increase with height.
women.aov <- aov(height~weight, data = women_america)
summary(women.aov)
#the p-value is less than 0.05; there is a significant difference.
#the alternative hypothesis is accepted and the null hypothesis is rejected. 

#TukeyHSD
TukeyHSD(women.aov)
#Does not work because data is to little/small.

#CORRELATIONS  
#Pearson correlation
cor.test(x = women_america$height, women_america$weight,
         use = "everything", method = "pearson")

women_pearson <- cor(women_america)
women_pearson

#One panel visual would be done to view the statistical data
#Relationship between height and weight of women
#calculating Pearson r-value
r_print <- paste0("r = ",
                  round(cor(x = women_america$height, women_america$weight),2))

#Create a single panel visual showing one correlation
ggplot(data = women_america, aes(x = height, y = weight)) +  #choosing the data frame and selecting the specific variables for the x and y axis
  geom_smooth(method = "lm", colour = "maroon2", se = F) +   #creating a linear model to plot data
  geom_point(colour = "darkorchid1") +   #plotting the data point and giving it a colour
  geom_label(x = 0.54, y = 0.812, label = r_print) +  #position the r value on the graph
  labs(x = "height (in)", y = "weight (lbs)") +  #adding label to the graph
  theme_new +  #added the new theme
  ggtitle("One panel visual")  #adding a title to the graph

#Chi-squared test
women_america %>% 
  chisq.test(women_america)
#Result: X-squared = 24.526, df = 28, p-value = 0.6535







