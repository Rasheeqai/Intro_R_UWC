#Weekend Homework Section 4:
  # Make use of any two built in datasets:
  # Make use of the summarise, select, group_by functions
  # Create at least two visualisations that were not done in the Intro R workshop-  any graphs but not the ones we did in class e.g. bar graph

  #adding datasets
beaver <- datasets::beaver1  
CO21<- datasets::CO2

beaver %>%
  group_by(day) %>% 
  summarise(avrg_temp= mean(temp), #calculate mean stipe diameter
            min_temp = min(temp),  #calculate the minimum value
            max_temp = max(temp),  #calculate the maximum value
            med_temp = median(temp), #calculate the median
            var_temp = var(temp),  #calculate the variance
            n_temp = n()) %>%  #number of values
  mutate(se_beaver_temp = sqrt(var_temp/n_temp))  #adding a column and calculating standard error

beaver_select <- beaver %>%
  select(day, temp) %>%  #selecting 
  filter(day== "347")   #extract the CO2 uptake data of Mississippi  

beaver21 <- beaver %>% 
  select(-activ, -day)  #removing activ and day columns

bar_graph_beav <- ggplot(beaver) +
  geom_bar(aes(x = temp)) +  #plotting bar graph of temperature
  ggtitle("The temperature of beaver1") #adding title

summary(beaver)
dim(beaver)

CO21 %>% 
  group_by(Type) %>% 
  summarise(avrg_uptake= mean(uptake), #calculate mean stipe diameter
            min_uptake = min(uptake),  #calculate the minimum value
            max_uptake = max(uptake),  #calculate the maximum value
            med_uptake = median(uptake), #calculate the median
            var_uptake = var(uptake),  #calculate the variance
            n_uptake = n()) %>%  #number of values
  mutate(se_CO2_uptake = sqrt(var_uptake/n_uptake))  #adding a column and calculating standard error

CO2_select <- CO21 %>% # Tell R which dataframe we are using
  select(Type, conc) %>% # Select only specific columns
  slice(4:17)  # Select specific rows 

CO2_M <- CO2 %>%
  select(Type, uptake) %>%  #selecting 
  filter(Type== "Mississippi")   #extract the CO2 uptake data of Mississippi

co22 <- CO21 %>%
  select(-Plant, -conc) #removing plant and conc columns

summary(CO21) 
dim(CO21)

histogram_graph_co2 <- ggplot(CO21) +
  geom_histogram(aes(x = uptake)) + #plotting bar graph of uptake of CO2
  ggtitle("The uptake of CO2")













