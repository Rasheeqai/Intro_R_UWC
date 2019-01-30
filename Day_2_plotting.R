#plotting in R using ggplot2
#day2
#Rasheeqa Isaacs
#30th Jan 2019

#load libraries
library(tidyverse)

#datasets
chicks <- datasets::ChickWeight
??Chickweight

#specifying dataset 
ggplot(data = chicks, aes(x = Time, y = weight))+
  geom_point()+
  geom_line(aes(group = Chick))

#don't have to specify data set already specified in previous coding
ggplot(chicks, aes( x = Time, y = weight, colour = Diet)) +
  geom_point()+
  geom_line(aes(group = Chick))

# Same inputs but changing graph type using geom_smooth
# lm= linear model
ggplot(chicks, aes(x = Time, y = weight, colour = Diet))+
  geom_point() +
  geom_smooth(method = "lm")

# Diet 3 has the most growth

ggplot(chicks, aes( x = Time, y = weight, colour = Diet)) +
  geom_point(color = "blue")+
  geom_line(aes(group = Chick))

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) +
  geom_smooth(method = "lm") +
  labs(x = "Days", y = "Mass (kg)") +
  ggtitle("Sea Point") +
  theme_bw()
# aes (control output for x and y) 
# heavier weight of chick the larger the point
# every size of point look at weight column
#Adding labels (changing x and y names from...
#Changed the theme to black and white (theme(bw))

lam <- read.csv("data/data/laminaria.csv")

ggplot(data = lam, aes(x = site, y = stipe_mass, colour = region)) +
  geom_point(aes(size = stipe_mass)) +
  geom_smooth(method = "lm") +
  labs(x = "")



#Facetting in ggplot
library(ggpubr)

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Diet, ncol = 4)
# facet_wrap uses tilda(~) to split into columns or rows using "ncol" or "nrow" 

chick_2 <- chicks %>%
  filter(Time == 21)

#double clink on plot_1 and then press control enter graph will appear (same for all plots)
plot_1 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "weight") +
  ggtitle("A")
plot_1

plot_2 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle("B")
plot_2

# position = "dodge", binwidth = 100 (plot data beside each other for histogrms)
plot3 <-  ggplot(data = chick_2, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")


plot4 <- ggplot(data = chick_2, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")


plot_combined <- ggarrange(plot_1, plot_2, plot3, plot4)


#3rd Library
library(boot)

urine <- boot::urine
??urine

urine %>%
  select(-cond)
#remember to assign a name or it will not save in the environment


#create a quick scatterplot
#specify data. "aes" control x and y. 
#comparing through colour variation.
ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond))

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = as.factor(r)))







#Exercise 1
#choose 3 data sets e.g. chicks <- datasets:: (datasets will pop up)
#2 graphs per dataset (make a question from the graph= hypothesis)
#of the 3 datasets mean of one column in those datasets

Airpl <- datasets::AirPassengers