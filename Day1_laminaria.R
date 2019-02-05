# Day1
# Laminaria dataset exploring and learning
# Rasheeqa Isaacs
# 29 Jan 2019

#loading libraries

library(tidyverse)
lam <- read_csv("data/laminaria.csv")
lam <- read_csv("data/data/laminaria.csv")
head(lam) #shows first six rows
tail(lam) #shows last six rows
head(lam, n=3) #shows first 3 rows
tail(lam, n=3) #shows last 3 rows

glimpse(lam)

lam_select <- lam %>% # Tell R which dataframe we are using
  select(site, total_length) %>% # Select only specific columns
  slice(54:80)  # Select specific rows

lam_kom <- lam %>%  #rows belonging to the kommetjie site
  filter(site =="Kommetjie")   #filter: is extracting data

# In the laminaria dataset select only site and blade_length column
# filter only for Sea Point

lam_try <- lam %>%
  select(site, blade_length) %>%
  filter(site== "Sea Point")

#tell R what dataset to use
#Selecting row with max total length

lam %>%
  filter(total_length == max(total_length))

summary(lam)
dim(lam)

#create summary of mean, median and standard deviation for blade length.

#tell R what dataset to use
lam %>%
  summarise(avrg_bl = mean(blade_length),     # Calculate mean blade length
            med_bl = median(blade_length),
            sd_bl = sd(blade_length))

#group by group sites together
#to produce new column use the mutate function

lam %>%    # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>%          # Group the dataframe by site
  summarise(var_bl = var(blade_length), # Calculate variance
            n = n()) %>%              # Count number of values
  mutate(se = sqrt(var_bl/n))         #mutate: add columns   # Calculate se

lam_2 <- lam %>%
  select(-blade_thickness, -blade_length)

lam %>%
  select(blade_length) %>%
  na.omit %>%        #removing NA
  summarise(n = n()) # number of enteries (sample size)


lam %>%
  select(blade_length) %>%
  na.omit %>% 
  summarise(n = n())

#ggplot found within tidyverse
# aes (control output for x and y) 
ggplot(data = lam, aes(x = stipe_mass, y = stipe_length)) +  
  geom_point(shape = 21, colour = "salmon", fill = "white") +   
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)")

## Exercise:

# 1. Create a new data frame from the `laminaria` dataset that meets the following criteria: contains only the `site` column and a new column called `total_length_half` containing values that are half of the `total_length`. In this `total_length_half` column, there are no `NA`s and all values are less than 100.
# **Hint**: think about how the commands should be ordered to produce this data frame!
#   
# 2. Use `group_by()` and `summarize()` to find the mean, min, and max blade_length for each site. Also add the number of observations (hint: see `?n`).
# 
# 3. What was the heaviest stipe measured in each site? Return the columns `site`, `region`, and `stipe_length`.


lam %>% # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>% #Group by site
  summarise (avrg_bl= mean(blade_length),
            min_bl = min(blade_length),
            max_bl = max(blade_length),
            n = n())  # number of enteries (sample size)

total_length_half <- lam %>%
  mutate(total_length_half = total_length / 2)%>% 
  filter(total_length_half < 100)%>% 
  select(site, total_length_half)

lam %>% # Tell R that we want to use the 'laminaria' dataframe
  group_by(site) %>%  #Group by site
  filter(stipe_mass == max(stipe_mass)) %>% 
  select(site, region, stipe_length)
  
  #create
  


  



