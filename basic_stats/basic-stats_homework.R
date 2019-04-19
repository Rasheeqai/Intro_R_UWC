#Rasheeqa Isaacs
#Biostats Homework
#19 Apr 2019

# Load libraries
library(tidyverse)
library(ggpubr)
library(corrplot)
library(ggpubr)
library(reshape2)
library(plotly)

#Exercise 9.6: Correlation
#producing a heatmap using ggplot

#Load data
ecklonia <- read_csv("data/ecklonia.csv")

head(ecklonia) #shows first six rows
tail(ecklonia) #shows last six rows
head(ecklonia, n=3) #shows first 3 rows
tail(ecklonia, n=3) #shows last 3 rows

glimpse(ecklonia)  #showing the data within the ecklonia csv. file.
dim(ecklonia)  
summary(ecklonia)

ecklonia %>%   #letting R know what dataframe is being used
  select(site, stipe_length) %>%   #select a specific column
  slice(13:24)   #select specific rows between 13 and 24

ecklonia_bat <- ecklonia %>%  #creating a new dataset
  filter(site == "Batsata Rock")

ecklonia_sub <- ecklonia %>%        #creating a new dataset
  select(-species, - site, - ID)   #removing the columns species, site and ID

new_ecklonia <- cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
                         use = "everything", method = "pearson")

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

heatmap_ecklonia <- melt(ecklonia_pearson)   #Assingning a name to the ecklonia dataset


#Creating a heatmap using the ecklonia dataset
ggplot(heatmap_ecklonia, aes(x = Var1, y = Var2, fill = value)) + 
  geom_tile() +
  scale_fill_gradient(low = "yellow3", high = "red2", name = "Pearson Correlation") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  ggtitle("The Heatmap of ecklonia") +
  labs(x = "Variable 1", y = "Variable 2")


#Exercise 7.4 (ANOVA)
#Exercise 7.4.1.
#loading a  pig dataset
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

#making a dataframe
bacon <- as.tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))),
  mass = c(feed_1, feed_2, feed_3, feed_4)))

#Performing an ANOVA test
#H0: There is no difference in the pigs mass after being fed one of the four diets.
#H1: There is a difference in the pigs mass after being fed one of the four diets.

pigs.aov1 <- aov(mass ~ feed, data = bacon)
summary(pigs.aov1)

#the null hypothesis is rejected; the p-value is more than 0.05
#As a result, there is a difference in pig mass when fed on of the four diets.

#We can show the differences with a boxplot 
#Creating a boxplot
ggplot(data = bacon, aes(x = feed, y = mass, fill = feed )) +
  geom_boxplot(notch = FALSE)

#RESULTS show great variation in the four diets

#Exercise 7.4.2.
#loading toothgrowth dataset
teeth <- datasets::ToothGrowth
#H0: There is no difference in the tooth length of cats receiving different vitamin C doses
#H1: There is a difference in the tooth length of cats receiving different vitamin C doses

#Exctracting the "supp" column
teeth_supp <- teeth %>%
  filter(supp == "VC")

#ANOVA Test
teeth.aov1 <- aov(len ~ as.factor(dose), data = teeth_supp)  
summary(teeth.aov1)  #summarising the anova test values

#Results show that the null hypothesis is accepted; p-value is more than 0.05
#Therefore there is no difference in toothlength of cats when receiving different vitamin c dosages.

#Exercise 7.4.3.
#Two-Way ANOVA Test

teeth1 <- datasets::ToothGrowth

#hypothesis
# H0: Vitamin C dosages has NO effect on the length of teeth
# H1: Vitamin C dosage has an effect on the length of teeth

teeth_aov <- (aov(len ~ supp, data = teeth))
summary(teeth_aov)


#Creating a box plot
ggboxplot(teeth, x = "dose", y = "len", color = "supp",
          palette = c("#EC9161", "#51B792"))

teeth.aov2 <- aov(len ~ supp + dose, data = teeth)

#summary of ANOVA test
summary(teeth.aov2)

#Results conclude that the supp value is significant; whereas dose is not significant as its value is more than 0.05  
TukeyHSD(teeth.aov2)

#Exercise 6.7.1
r_dat <- data.frame(dat = c(rnorm(n = 700, mean = 10, sd = 3)))

#H0: there are more males in honours at UWC than females

shapiro.test(r_dat1$dat)

#Produce a t-test
t.test(r_dat1$dat, mu = 10)

#Null hypothesis is rejected; p- value is more than 0.05.


#Exercise 6.7.2.

People <- matrix(c(1, 3, 5, 7), ncol = 2)
colnames(People) <- c("Females", "Males")
rownames(People) <- c("gain", "loss")

#H0: Males will not gain more weight than females.
#H1: Males will gain more weight than females.

prop.test(People)























