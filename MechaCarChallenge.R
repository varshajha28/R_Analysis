library(tidyverse)
library(jsonlite)
library(ggplot2)
#Import Mechacar Data and its analysis
MechaCar_table <- read.csv(file='MechaCar_mpg.csv',sep=",", header = T)
#generate multiple linear regression model
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table) 
#generate summary statistics
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD,data=MechaCar_table))
#generate single linear regression model between vehicle length and mpg
lm(mpg ~ vehicle.length,data=MechaCar_table) 
#generate single linear regression model between ground clearence and mpg
lm(mpg ~ ground.clearance,data=MechaCar_table)
#generate summary statistics for vehicle.length
summary(lm(mpg ~ vehicle.length,data=MechaCar_table)) 
#generate summary statistics for ground.clearance
summary(lm(mpg ~ ground.clearance,data=MechaCar_table)) 


#Import Suspension Coil data and summary.
Suspension_table <- read.csv(file='Suspension_Coil.csv',sep=",", header = T)
#Suspension_summary 
Suspension_table %>%
  group_by(Manufacturing_Lot) %>%
  summarise(PSI_mean= mean(PSI), PSI_sd = sd(PSI), PSI_median = median(PSI), PSI_variance = var(PSI))

#Suspension Coils T-test
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot1")$PSI,mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot2")$PSI,mu = 1500)
t.test(subset(Suspension_table,Manufacturing_Lot=="Lot3")$PSI,mu = 1500)

sample_table <- Suspension_table %>% sample_n(50) #generate 50 randomly sampled data points
sample_table2 <- Suspension_table %>% sample_n(50) #generate another 50 randomly sampled data points
t.test(sample_table$PSI,sample_table2$PSI) #compare means of two samples
t.test(sample_table$PSI, mu = 1500)
t.test(sample_table2$PSI, mu = 1500)

#Design the own study:

#Determine the dataset to be considered for the test.
head(mtcars)
head(mpg)
data()
head(cars)
#generate multiple linear regression model to determine the factor for the study.
lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars) 
summary_table_mtcars<-summary(lm(mpg ~ hp + cyl + disp + drat+ wt + qsec + vs + am + gear + carb, data=mtcars)) #generate multiple linear regression model
summary_table_mtcars
#generate single linear regression model
lm(mpg ~ hp,data=mtcars) 
summary(lm(mpg ~ hp,data=mtcars)) #generate single linear regression model

#Determine the mean of the hp value for the t-test 
mtcars %>%
  summarise(hp_mean= mean(hp), hp_sd = sd(hp), hp_median = median(hp), hp_variance = var(hp))
sample_table_mtcars <- mtcars %>% sample_n(16) #generate 16 randomly sampled data points
t.test(sample_table_mtcars$hp, mu = 146)

#Correlation between hp and mpg.
cor(mtcars$mpg, mtcars$hp)
