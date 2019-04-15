#Rasheeqa Isaacs
#15 Apr 2019
#basic stats day 2

library(fitdistrplus)
library(logspline)
library(ggplot2)
library(tidyverse)


#generating normal log data
y <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)

par(mfrow = c(2, 2))   #parameter with 2 columns and 2 rows
plot(x = c(1:length(y)), y = y)  
hist(y)                             #producing histogram (function)
descdist(y, discrete = FALSE, boot = 100)

y1 <- c(18,9,31,7,47,28,20,300,19,6,19,21,99,85,52,68,69,3,48,116,15,27,51,100,105,99,73,58,1,89,222,56,27,36,300,121,5,42,184,88,24,127,67,93,85,60,92,23,39,140,60,71,333,42,16,51,151,625,624,200,350,4,105,199,88,742)

y2 <- rnorm(10, 13, 2)
length(y2)
mean(y2)
sd(y2)
hist(y2)
descdist(y2, discrete = FALSE)


y4 <-rnorm(n = 200, m = 13, sd = 2)
par(mfrow = c(2, 2))
# using some basic base graphics as ggplot2 is overkill;
# we can get a histogram using hist() statement
hist(y4, main = "Histogram of observed data")  #first graph
plot(density(y4), main = "Density estimate of data")   #second graph
plot(ecdf(y4), main = "Empirical cumulative distribution function")  #third graph
# standardise the data
z.norm <- (y4 - mean(y4)) / sd(y4)

# make a qqplot (4th graph)
qqnorm(z.norm)

# add a 45 degree reference line
abline(0, 1)  


#Chapter 6- Inferences
#generating data, create long dataframe. 

set.seed(666)  
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))   #create long dataframe 

tail(r_dat)

#creating a histogram
h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h

#the p-value isnot less than 0.05 therefore 

shapiro.test(r_dat$dat)  # "$" chooses particular columns   #spits out 2 values 

r_dat %>%
  group_by(sample) %>%
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2]))  #2 means we only want 


r_dat %>%
  group_by(sample) %>%
  summarise(norm_dat = (shapiro.test(dat)[2]))

out <- shapiro.test(r_dat$dat) #test for null hypothesis  that sample comes from normal distributed data. 
str(out)
out$p.value

str(r_dat)
r_dat$dat
r_dat$sample

r_dat %>%
  group_by(sample) %>%
  summarise(p_val = as.numeric(shapiro.test(dat)[2]))


#to check the variance of sample data
r_dat %>%
  group_by(sample) %>%
  summarise(sample_var = var(dat))


#sample of 20
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    
                    sample = "A")
 
#normality
shapiro.test(r_one$dat)

t.test(r_one$dat, mu = 20)

ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightsalmon") +
  # population mean (mu) = 20
  geom_hline(yintercept = 20, colour = "blue",
             size = 1, linetype = "dashed") +
  # population mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red",
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()

#one-sided sample t-test
t.test(r_one$dat, mu = 30, alternative = "less") 

t.test(r_one$dat, mu = 30, alternative = "greater")


#two sample t-test
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

t.test(dat ~ sample, data = r_two, var.equal = TRUE) # ~ are data different from each other 


#cHAPTER 7: ANOVA

chicks <- as_tibble(ChickWeight)

chicks_sub <- chicks %>%
  filter(Diet %in% c(1, 2), Time == 21)  #took out chicks from 21 and only diets 1 and 2 [independent because they are different chickens weighed on the same day] [two samples, two sided test]
#group A will have a greater mass than group B
  
t.test(weight ~ Diet, data = chicks_sub)   #producing a t-test of chicks weight [not reject null hypothesis]                                        
library(tidyverse)
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))  #weight influenced by diet; only want to look at chicks and day 21
summary(chicks.aov1)

#arrive at suggestion to determine which diet will cause a difference, devise a graph that will hint at the difference [create boxplot for each diet without the knotch]
#creating a notch boxplot
chicks1 <- ggplot(data = chicks, aes(x = Diet, y = weight))+
                    geom_boxplot(aes (fill = Diet), notch = TRUE)+
  ggtitle("Variation of the four diets")

TukeyHSD(chicks.aov1)

plot(TukeyHSD(chicks.aov1, ordered = TRUE)) #Graph representing TukeyHSD

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0))))  #if there is an effect on there weight
 
plot(TukeyHSD(aov(weight ~ Diet, data = filter(chicks, Time %in% c(0)))))

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2))))          

#2 null hypothesis [reject both null hypothesis] because as time moves along the weight will iincrease and will not stay the same
summary(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))  
  
#interaction between diet and time
summary(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 21)))) 

summary(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(10, 21))))

TukeyHSD(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(20, 21))))








