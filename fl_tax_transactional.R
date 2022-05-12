#Dalton Anderson
#Last grade was told my format was wrong so I will try another format this time

#install.packages("tidyverse")

library(readxl)
library(dplyr)
library(ggplot2)

#1.) Load in dataset
#Import data
fl_taxes <- read_excel("6304 Module 3 Assignment Data.xlsx")

#2.) Map data by facility
#Create subsets
Bunny = subset(fl_taxes,fl_taxes$facility == 'Bunnyville')
Hooter = subset(fl_taxes,fl_taxes$facility == 'Hooterville')
Oblong = subset(fl_taxes,fl_taxes$facility == 'Oblong')
Pixley = subset(fl_taxes,fl_taxes$facility == 'Pixley')

#3.) Create random sampleple sets by unumber
set.seed(59657076)
sample <- Bunny
Bunny_Sam = sample_n(sample, 70)

set.seed(59657076)
sample <- Hooter
Hooter_Sam = sample_n(sample, 70)

set.seed(59657076)
sample <- Oblong
Oblong_Sam = sample_n(sample, 70)

set.seed(59657076)
sample <- Pixley
Pixley_Sam = sample_n(sample, 70)

head(Hooter_Sam)
head(Oblong_Sam)

#Analysis

#1.) sing your 70-case sample, construct a 90% confidence interval on the population mean
#transaction time for Hooterville.

Hooter_Results = t.test(Hooter_Sam$transaction.time, conf.level = .90)

#2.) Assuming the data in the primary Hooterville data frame represents the population, 
#does your 90% confidence interval include the true population mean on the transaction time 
#variable?
  
mean(Hooter$transaction.time)
Hooter_Results$statistic
Hooter_Results$conf.int
#Yes my seed includes the the population mean within the sample confidence interval
# upper of 9.35 lower of 6.25 population mean of 8.40
t.test(Oblong_Sam$transaction.time, conf.level = .90)

#3.)	Use R and your reduced 70-case data set for Oblong.  
#Can you say (α = .05) that the population mean transaction time is greater than 8 minutes?
#How about greater than 9 minutes and 15 seconds?

t.test(Oblong_Sam$transaction.time, mu = 8, alternative = 'greater')
t.test(Oblong_Sam$transaction.time, mu = 9.15, alternative = 'greater')

#4.)	Referencing Part 3 above, what “test against” (mu) value
#in a two-tailed hypothesis test would yield p = .05 in a two-tailed hypothesis test
#on the Oblong transaction time?
t.test(Oblong_Sam$transaction.time, mu = 5.045807, alternative = 'two.sided')
#It is going to be the lower bound confidence interval.

#5).	Using R and your sample 70-case data sets, show comparative notched boxplots
#of the four facilities’ transaction time variable.
#Your boxplots should be displayed side by side in a single graphic with an appropriate title
#and x-axis labels.  Do these plots indicate a possible difference between the transaction times 
#for the two facilities?  Do these plots indicate a difference in skewness or number of 
#potential outliers between Hooterville and Pixley?

#Good evening everyone,In Question 5  there has been a small mistake 
#Using R and your sample 70-case data sets, show comparative notched boxplots
#of the four facilities’ transaction time variable. 
#Your boxplots should be displayed side by side in a single graphic with an appropriate 
#title and x-axis labels. Do these plots indicate a possible difference between
#the transaction times for the two facilities? Do these plots indicate a difference in 
#skewness or number of potential outliers between Hooterville and Pixley?

#Create master dataset
Master_Sam1 = full_join(Bunny_Sam, Hooter_Sam, copy = FALSE)
Master_Sam2 = full_join(Oblong_Sam, Pixley_Sam, copy = FALSE)
Master_Sam = full_join(Master_Sam1, Master_Sam2, copy = FALSE)

#Plot dataset
ggplot(data = Master_Sam, mapping = aes(x = facility, y = transaction.time)) + 
  geom_boxplot(notch = TRUE,fill="slategrey", colour="darkslategrey") +
  geom_jitter(color="darkslategrey", size=0.9) +
  stat_summary(fun.y=mean, geom="point", shape=13, size=2, color="peru", fill="red")
labs(x= 'Facility', y = 'Transaction Time (mins)',title = 'FL Tax Office Transaction Times')

#I would not say it tells, but shows you.Yes, the boxplot shows you the the outlines

#6.)	Using R and your sample 70-case data sets, does there appear to be a statistically
#significant difference (α = .05) between the mean transaction times for
#Hooterville and Bunnyville

t.test(Bunny_Sam$transaction.time)
t.test(Hooter_Sam$transaction.time)
t.test(Hooter_Sam$transaction.time,Bunny_Sam$transaction.time)

#Yes Bunnyville has a much confidence interval range than Hooterville

