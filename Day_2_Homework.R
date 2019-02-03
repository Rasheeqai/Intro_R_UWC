#Day2
#Homework of Day2
#Rasheeqa Isaacs
#30 Jan 2019

#Exercise 1
#choose 3 data sets e.g. chicks <- datasets:: (datasets will pop up)
#2 graphs per dataset (make a question from the graph= hypothesis)
#of the 3 datasets mean of one column in those datasets


#Loading libraries
library(tidyverse)

library(ggpubr)

# 1.Dataset 
car <- datasets::cars

#Specifying dataset
#Creating a line graph
car_plot <- ggplot(data = car, aes(x = speed, y = dist))+
  geom_point(colour = "blue")+
  geom_line(colour = "red") +
  labs(x = "speed (m)", y = "distance (km)") +
  ggtitle("Car_plot")


#creating a linear model
car_plot1 <- ggplot(data = car, aes(x = speed, y = dist)) +
  geom_point(colour = "green") +
  geom_smooth(method = "lm") +
  labs(x = "speed (m)", y = "distance (km)") +
  ggtitle("Car_plot1")
  

#Hypothesis: The higher the speed the more the distance covered by the car.

#the mean of the column "speed" in the cars dataset
car_avrg_sp <- cars %>%
  summarise(avrg_dist= mean(speed))


# 2.Dataset

beaver_1 <- datasets::beaver1



#creating a linear model
beaver_plot1 <- ggplot(data = beaver_1, aes(x = time, y = temp, colour = day)) +
  geom_point(colour = "purple") +
  geom_smooth(method = "lm") +
  labs(x = "Time", y = "Temp") +
  ggtitle("Beaver_plot1")

#Hypothesis: The temperature will vary by increasing and decreasing as time passes. 

#creating a line graph


beaver_plot2 <- ggplot(data = beaver_1, aes(x = time, y = temp, colour = day)) +
  geom_point(colour = "blue")+
  geom_line(colour = "orange") +
  labs(x = "Time (years)", y = "Temp") +
  ggtitle("Beaver_plot2")

#Hypothesis: The temperature fluctuates as time pass.


#the mean of the column "temp" in the beaver dataset 
beaver_avrg_temp <- beaver1 %>%
  summarise(avrg_beav= mean(temp))


# 3.Dataset

iris_1 <- datasets::iris

#Creating a linear model

iris_plot1 <- ggplot(data = iris_1, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(colour = "brown") +
  geom_smooth(method = "lm") +
  facet_wrap(~Species, ncol = 4)
  labs(x = "P. Length", y = "P.Width") +
  ggtitle("Iris_plot1") 

#Hypothesis: The species virginica has the largest petal width and length


  #Creating line graph
  
  iris_plot2 <-ggplot(data = iris_1, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point(colour = "red")+
  geom_line(colour = "green") +
  facet_wrap(~Species, ncol = 4) +
  labs(x = "S.Length (m)", y = "S.Width") +
  ggtitle("Iris_plot2")

#Hypothesis: The species setosa has the highest sepal width and length 


#the mean of the column "Petal_Length" in the iris dataset

iris_avrg_pl <- iris %>%
  summarise(avrg_iris= mean(Sepal.Length))



#Reading the Lamanaria csv. file

lam <- read_csv("data/laminaria.csv")


#selecting sites and blade_length. Only using data between rows 54 to 74.
lam_plot1 <- lam %>% 
  select(site, blade_length) %>%
  slice(54:74)


#plotting a line graph
lam_plot_1 <- ggplot(data = lam_plot1, aes(x = site, y = blade_length)) +
  geom_point(colour = "purple")+
  geom_line(colour = "red") +
  labs(x = "site", y = "blade_length(m)") +
  ggtitle("Lamanaria")



#grouping sites and selecting only thtallus_mass
lam_plot2 <- lam %>%
  group_by(site) %>% 
  select(site, thallus_mass)


#plotting a linear model
lam_plot_2 <- ggplot(data = lam_plot2, aes(x = site, y = thallus_mass)) +
  geom_point(colour = "blue")+
  geom_smooth(colour = "purple") +
  labs(x = "site", y = "thallus_mass(g)") +
  ggtitle("Lamanaria_1")


# [A.A]
# NIce graphs
# added new features
# Neat script, the graphs are however lacking hypothesis
# THe visualisations were suppose to answer each of the hypothesis
  
  
  
  
  
  
  
  
  

  
  
  


  
  
  
  



          
         
  

