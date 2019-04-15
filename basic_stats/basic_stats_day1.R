#Rasheeqa Isaacs
#09 April 2019
#basic stats Day 1

#load libraries
library(tidyverse)

#load chickweight
chicks <- as_tibble(ChickWeight)

#type in head
head(chicks)
head (chicks, n = 20)  #first 20 rows
tail(chicks, n = 10)   #last 10 rows
colnames(chicks)       #display column names
names <- colnames(chicks)  #rename data to names
summary(chicks)  #summary of the data
dim(chicks)  #dimension of the data
glimpse(chicks)  #giving a glimpse of the data
ncol(chicks)  #the number of columns
nrow(chicks)  #the number of rows
mean(chicks$weight)

chicks %>%
  group_by(Diet) %>%   #group by diet
  filter(Time == 0) %>%  #filter by time
  summarise(mean_weight = mean(weight),    #summarise using mean weight and the function standard deviation 
            sd_weight = sd(weight)) %>%
  ungroup()   #ungroup to simplify the data

chicks %>%
  summarise(length = n())

length(chicks$weight)


#the mean
nums <- c(13, 666, 13, 776, 35, 13)  #single vector of numbers

(mean <- sum(nums) / length(nums)) #longer way of caculating the mean 
mean(nums) 

round(mean(chicks$weight), 1)   #rounds off to one decimal place

chicks %>%
  summarise(mean_weight = mean(weight))   #an individual chickens size


#What happens when there are missing values (NA)?
numbers <- c(6, 65, 84, NA, 45, 4)   
mean(numbers)       
mean(numbers, na.rm = TRUE)     
mean(na.omit(numbers))     #removing NA values


#median
nums2 <- c(5, 2, 6, 13, 1)  #vector of numbers
mean(nums2)  #the mean/average of values
median(nums2)  #median/middle value

nums3 <- c(5, 2, 6, 13, 1, 13)  #change the original values and add additional value
mean(nums3)       #the mean/average of values
median(nums3)  #median/middle value


chicks %>%
  summarise(sd_wt = sd(weight))  #the standard deviation of the chick data


#Quantile calculation
chicks %>%    
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = quantile(weight, p = 0.75),
            max_wt = max(weight))


range(chicks$weight)  #calculating the range of the chick data
min(chicks$weight)   #calculating the minimum value
max(chicks$weight)   #calculating the maximum value


summary(chicks$weight) 
  
sum_chicks <- chicks %>%
  filter(Time <=10) %>%
  group_by(Diet) %>%
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = median(weight, p = 0.75),
            max_wt = max(weight))

library(ggpubr)
library(RColorBrewer)
library(ggthemes)

iris

iris.cnt <- iris %>%
  count(Species) %>%    # creates a column, n, with the counts
  mutate(prop = n / sum(n))   # creates the relative proportion of each species #mutation which creates a new column
iris.cnt           
 
plt1 <- ggplot(data = iris.cnt, aes(x = "", y = n, fill = Species)) +  #do not need a name at the x axis because it will automatically use the species in the data
  geom_bar(width = 1, stat = "identity") +  #   
  labs(title = "Stacked bar graph", subtitle = "cumulative sum",
       x = NULL, y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

plt2 <- ggplot(data = iris.cnt, aes(x = "", y = prop, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()


plt3 <- plt2 + coord_polar("y", start = 0) +
  labs(title = "The distribution of...", subtitle = "",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Pinks") +
  theme_minimal()


plt4 <- ggplot(data = iris, aes(x = Species, fill = Species)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Side-by-side bars", subtitle = "n per species", y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()

ggarrange(plt1, plt2, plt3, plt4, nrow = 2, ncol = 2, labels = "AUTO")  #arranging the four plots


faithful #faithful data
summary(faithful)

hist1 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(colour = "black", fill = "salmon", alpha = 1) +  #alpha is the transparency of the bar
  labs(title = "Old Faithful data",
  subtitle = "A vanilla frequency histogram",
  x = "Eruption duration (min)",
  y = "Count") + theme_pubclean()  #theme_pubclean is the changing of the bachground of the graph


hist2 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = ..density..),  #have to have the two dots by density
                 
                 position = 'identity', binwidth = 1,   #changing binwidth change the width to make the columns in the graph broader
                 colour = "black", fill = "salmon", alpha = 0.6) +
  
  labs(title = "Old Faithful data",
       subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)",
       y = "Count") + theme_pubclean()

nrow(faithful)


hist3 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = 0.5 * ..density..),
                 position = 'identity', binwidth = 0.5,
                 colour = "black", fill = "salmon", alpha = 0.6) +
  
  labs(title = "Old Faithful data",
       subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)",
       y = "Relative contribution") + theme_pubclean()


hist4 <- ggplot(data = faithful, aes(x = eruptions)) +
  stat_ecdf() +
  labs(title = "Old Faithful data",
       subtitle = "ECDF",
       x = "Eruption duration (min)",
       y = "Relative contribution") + theme_pubclean()


ggarrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2, labels = "AUTO")


iris.long <- iris %>%
  gather(key = "variable", value = "size", -Species)
ggplot(data = iris.long, aes(x = size)) +
  geom_histogram(position = "dodge", # ommitting this creates a stacked histogram
                 
                 colour = NA, bins = 20,
                 aes(fill = Species)) +
  
  facet_wrap(~variable) +
  labs(title = "Iris data",
       subtitle = "Grouped frequency histogram",
       x = "Size (mm)",
       y = "Count") +
  theme_pubclean()

summary(iris)

plt1 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(show.legend = FALSE, notch = TRUE) + theme_pubclean() +  #specify geom_boxplot to create boxplot graph
  labs(y = "Sepal length (mm)") +
  theme(axis.text.x = element_text(face = "italic"))

plt2 <- ggplot(data = iris.long, aes(x = Species, y = size)) +
  geom_boxplot(fill = "red", alpha = 0.4, notch = TRUE) +
  geom_jitter(width = 0.1, shape = 21, colour = "blue", fill = NA, alpha = 0.2) +  #alpha is the transparency of the bins
  facet_wrap(~variable, nrow = 1) +
  labs(y = "Size (mm)") + theme_pubclean() +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))

ggarrange(plt1, plt2, nrow = 2, ncol = 1, labels = "AUTO")   #arranging the graphs into 2 rows and a single column


plt1 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point() +   #creating a scatterplot
  labs(x = "Petal length (mm)", y = "Petal width (mm)") +
  theme(legend.position = c(0.18, 0.85)) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  theme_pubclean()

plt2 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) +   
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +  
  labs(x = "Petal length (mm)", y = "Petal width (mm)") +  #labelling the x and y axis 
  theme_pubclean()  #the changing of the background of the graph

ggarrange(plt1, plt2, ncol = 2, nrow = 1, labels = "AUTO") #arranging the plots in two columns and a single row

facet.names <- c(Petal.Length = "Petal length",
                 Petal.Width = "Petal width",
                 Sepal.Length = "Sepal length",
                 Sepal.Width = "Sepal width")    #make lables for the facets

iris.long %>%
  group_by(Species, variable) %>%       #grouping by species and variables
  summarise(mean.size = mean(size),         #summarising mean and standard deviation  
            sd.size = sd(size)) %>%
  ggplot(aes(x = Species, y = mean.size)) +  #creating a ggplot 
  geom_bar(stat = "identity") +              #creating bargraph
  geom_errorbar(aes(ymin = mean.size - sd.size, ymax = mean.size + sd.size), width = 0.2) +  #adding error bars with the width.
  facet_wrap(~variable, labeller = labeller(variable = facet.names)) +   
  labs(y = "Size (mm)", title = "A box plot...", subtitle = "...of the Iris data") +   #adding labels to the y axis, adding the title and subtitle.
  theme(axis.text.x = element_text(face = "italic")) 


#Violin graph
vio1 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin() +
  theme_pubclean() + theme(legend.position = "none") +
  labs(title = "Iris data",
       subtitle = "Basic violin plot", y = "Sepal length (mm)") +
  theme(axis.text.x = element_text(face = "italic"))

vio2 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_violin(show.legend = FALSE, draw_quantiles = c(0.25, 0.5, 0.75)) +
  theme_pubclean() + theme(legend.position = "none") +
  labs(title = "Iris data",
       subtitle = "Violin plot with quartiles", y = "Sepal length (mm)") +
  theme(axis.text.x = element_text(face = "italic"))

vio3 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, colour = Species)) +
  geom_violin(fill = "grey70") +
  geom_boxplot(width = 0.1, colour = "grey30", fill = "white") +
  theme_pubclean() + theme(legend.position = "none") +
  labs(title = "Iris data",
       subtitle = "Box plots nested within violin plots", y = "Sepal length (mm)") +
       theme(axis.text.x = element_text(face = "italic"))

vio4 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, colour = Species)) +
  geom_violin(fill = "grey70") +
  geom_boxplot(width = 0.1, colour = "black", fill = "white") +
  geom_jitter(shape = 1, width = 0.1, colour = "red", alpha = 0.7, fill = NA) +
  theme_pubclean() + theme(legend.position = "none") +
  labs(title = "Iris data",
       subtitle = "Violins, boxes, and jittered data", y = "Sepal length (mm)") +
  theme(axis.text.x = element_text(face = "italic"))

ggarrange(vio1, vio2, vio3, vio4, ncol = 2, nrow = 2, labels = "AUTO")






























































  
  


  





