#Day_4
#Chapter 10
#Tidy_data
#1 Feb 2019


load("data/SACTN_mangled.RData")

#UTR- underwater temperature recorder

ggplot(data = SACTN1, aes(x = date, y = temp)) +       #specify dataset 
  geom_line(aes(colour = site, group = paste0(site, src))) +  #edit function must use aes. Use colour to make each site diff. colour. group = paste0 to include more than one variable it by site and source. 
  labs(x = "", y = "Temperature (°C)", colour = "site") + #to add labels
  theme_bw()
  

#Use the gather function to group sources "src" together in one column
SACTN2_tidy <- SACTN2 %>%
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")


#Changing SACTN3- 2 columns into one column "depth" labelled SACTN_tidy
#

SACTN3_tidy <- SACTN3 %>% 
  spread(key = var, value = val)


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









