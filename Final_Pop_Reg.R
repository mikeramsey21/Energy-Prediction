###################################################
# Michael E. Ramsey
# APPM 5590
# Statistical Modeling
# Final Project

# Date Created: 5/2/18
# Last Updated: 9/3/18
###################################################

# This is an r-script to analyze the data presented for my
# final project for my statistical modeling class. We are presented 
# with two years of monthly temperature and energy use for 50 US
# cities. Our goal is to predict the energy consumption for a third
# year of Denver temperature values

####### Datasets #######
# "FinalTempData1.csv" contains two years of monthly temperature data
# in Farenheight for 50 US cities. We also have the variables of city
# population and Region (Mountain West, West, Northeast, Southeast)

# "EnergyData6.csv" contains two years of monthly energy consumption 
# data for the 50 US cities present in "FinalTempData1.csv"

# "FinalTempYr3.csv" contains a third year of temperature data for the
# City of Denver. Our goal is to predict the energy consumption for 
# Denver for these months based on the temperatures given

####### Workspace Items ########
# DATA FRAMES
# temp:         Two years of monthly temperature data for 50 US cities
# energy:       Two years of monthly energy data for 50 US cities
# temp3:        A year of monthly temperature values for Denver
# energy1:      A tidy version of "energy" data frame
# temp1:        A tidy version of "temp" data frame
# data_long:    Our primary dataset of use. It contains the data combined 
#               from "temp1" and "energy1"
# subdata_long: The data for Denver from "data_long"
# DenTemp:      Available data and energy values for all three years of Denver
# Data_long2:   The data in data_long with the log-energy

# ALL MODELS IN THIS R SCRIPT USE TEMPERATURE, REGION, AND POPULATION AS PREDICTORS

# MODELS
# poolm:    A model where all of the temperature data is pooled to fit regression
# poolm2:   A pooled linear model with outliers removed

# NOTE: All outliers removed beyond this point. Continued models without outliers

# poolm3:   A pooled linear model with the log-energy
# stratlm:  A stratified linear model (each city has own line)
# stratlm2: A stratified linear model with the log-energy
# melm:     Mixed effect linear model
# melm2:    Mixed effect linear model with log-energy
# plm:      Pooled Poisson Linear Model
# plm2:     Stratified Poisson Linear model
# meplm:    Mixed effect poisson linear model
# zglm:     Zero-Inflated mixed effect poisson linear model

# VALUES
# pred_den: Energy prediction values for Denver. Make sure you run this for each 
#           model before plotting
# prd: Predicted Denver values for "Best" model

# Import necessary libraries
library(tidyverse)
library(reshape2)
library(ggplot2)
library(dplyr)
library(broom)
library(lme4)
library(gvlma)
library(glmmTMB)
library(AER)

################################################################
# Data Processing

# Import the the datasets
# Denver is row 13 in the 'energy' data frames
temp = read.table('Data/FinalTempdata1.csv', sep=",", header=TRUE)
energy = read.table('Data/EnergyData6.csv', sep=",", header=TRUE)
temp3 = read.table('Data/FinalTempYr3.csv', sep=",", header=TRUE)

# Pool all of the data
temp1 <- melt(temp,id.vars = c("City","Region","Pop"))
energy1 <- melt(energy)
data_long <- cbind(temp1, energy1$value)
colnames(data_long) <- c('City','Region','Pop','Month','Temp','Energy')

# Get data for Denver
subdata_long <- data_long[data_long$City =='Denver',]

# Fit a pooled linear model
################################################################
# Data Analysis

# Fit a pooled linear model
poolm <- lm(Energy~Temp+Pop+as.factor(Region), data = data_long)
summary(poolm) #Everything significant

# Plot the pooled data and line
ggplot(data=data_long, aes(x=Temp, y=Energy)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = T) +
  ggtitle('Temperature vs. Energy Consumption') +
  xlab('Temperature') +
  ylab('Energy Usage')

# Look at diagnostic
gvlma(poolm)
plot(poolm)
# Homoskedacity violated
# Linearity violated
# Normality violated
# Outliers 638,648,1198

# Predict values for Denver
pred_den <- predict(poolm, subdata_long)

# Plot the prediction vs fitted values
qplot(data_long$Energy,predict(poolm)) +
  ggtitle('Predicted Energy vs. Actual Energy') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red') +
  geom_point(aes(x=subdata_long$Energy,y=unname(pred_den)),col = 'red')

# Conclusion- remove outliers and try again
data_long <- data_long[data_long$Energy<= 40,]

# Fit a pooled linear model
poolm2 <- lm(Energy~Temp+Pop+as.factor(Region), data = data_long)
summary(poolm2) #Everything significant

# Plot the pooled data and line
ggplot(data=data_long, aes(x=Temp, y=Energy)) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = T) +
  ggtitle('Temperature vs. Energy Consumption') +
  xlab('Temperature') +
  ylab('Energy Usage')

# Look at diagnostic plots
gvlma(poolm2)
plot(poolm2)
# All assumptions still bad

# Predict values for Denver
pred_den <- predict(poolm2, subdata_long)

# Plot the prediction vs fitted values
qplot(data_long$Energy,predict(poolm2)) +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red') +
  geom_point(aes(x=subdata_long$Energy,y=unname(pred_den)),col = 'red') +
  ggtitle('Predicted Energy vs. Actual Energy') 

# Conclusion: Still bad, try taking the log
data_long2 <- data_long[data_long$Energy > 0,]

# Fit a pooled linear model
poolm3 <- lm(log(Energy)~Temp+Pop+as.factor(Region), data = data_long2)
summary(poolm3) #Everything significant

# Plot the pooled data and line
ggplot(data=data_long2, aes(x=Temp, y=log(Energy))) + 
  geom_point() + 
  geom_smooth(method = 'lm', se = T) +
  ggtitle('Temperature vs. Energy Consumption') +
  xlab('Temperature') +
  ylab('Energy Usage')

# Look at diagnostic plots
gvlma(poolm3)
plot(poolm3)
# All assumptions still bad

# Predict values for Denver
pred_den <- predict(poolm3, subdata_long)

# Plot the prediction vs fitted values
qplot(data_long2$Energy,exp(predict(poolm3))) +
  geom_point(aes(x=subdata_long$Energy,y=exp(unname(pred_den))),col = 'red') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  ggtitle('Predicted Energy vs. Actual Energy') +
  geom_abline(slope=1, intercept=0, color = 'red')

# Conclustion: Try a stratified linear model

# Fit a stratified linear model
################################################################
stratlm <- lm(Energy~Pop+as.factor(Region)+Temp*as.factor(City), data = data_long)
summary(stratlm) # Not everything significant

# Plot the stratified results
ggplot(data=data_long, aes(x=Temp, y=Energy, color = factor(City))) + 
  geom_point() + 
  geom_smooth(method = 'lm',se = F) +
  ggtitle('Temperature vs. Energy Consumption') +
  xlab('Temperature') +
  ylab('Energy Usage') +
  scale_color_discrete(name="City")

# Look at diagnostic plots
gvlma(stratlm)
plot(stratlm)
# Everything is still really bad

# Predict values for Denver
pred_den <- predict(stratlm,subdata_long)

# Plot the prediction vs fitted values
qplot(data_long$Energy,predict(stratlm)) +
  geom_point(aes(x=subdata_long$Energy,y=unname(pred_den)),col = 'red') +
  ggtitle('Predicted Energy vs. Actual Energy') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red')

# Conclusion: Still bad, try taking the log

# Fit the linear model
stratlm2 <- lm(log(Energy)~Pop+as.factor(Region)+Temp*as.factor(City), data = data_long2)
summary(stratlm2) # Not everything significant

# Plot the results
ggplot(data=data_long2, aes(x=Temp, y=log(Energy), color = factor(City))) + 
  geom_point() + 
  geom_smooth(method = 'lm',se = F) +
  ggtitle('Temperature vs. Energy Consumption') +
  xlab('Temperature') +
  ylab('Energy Usage') +
  scale_color_discrete(name="City")

# Look at diagnostic plots
gvlma(stratlm2)
plot(stratlm2)
# Everything is still really bad

# Predict values for Denver
pred_den <- predict(stratlm2,subdata_long)

# Plot the prediction vs fitted values
qplot(data_long2$Energy,exp(predict(stratlm2))) +
  geom_point(aes(x=subdata_long$Energy,y=exp(unname(pred_den))),col = 'red') +
  ggtitle('Predicted Energy vs. Actual Energy') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red')
# Log is better

# Conclusion: Onto mixed effect linear regression

# Fit a mixed-effect linear model
################################################################
melm <- lmer(Energy ~ Pop+as.factor(Region)+Temp +(1 + Temp | City), 
             data = data_long)
summary(melm)

# Plot the results - same as stratified analysis
ggplot(data=data_long, aes(x=Temp, y=Energy, color = factor(City))) + 
  geom_point() + 
  geom_smooth(method = 'lm',se = F) +
  ggtitle('Temperature vs. Energy Consumption') +
  xlab('Temperature') +
  ylab('Energy Usage') +
  scale_color_discrete(name="City")

# Look at diagnostic plots
plot(melm)
# Things might be a little better

# Predict values for Denver
pred_den <- predict(melm,subdata_long)

# Plot the prediction vs fitted values
qplot(data_long$Energy,fitted.values(melm)) +
  geom_point(aes(x=subdata_long$Energy,y=unname(pred_den)),col = 'red') +
  ggtitle('Predicted Energy vs. Actual Energy') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red')

# Conclusion: Try taking the log
melm2 <- lmer(log(Energy) ~ Pop+as.factor(Region)+Temp +(1 + Temp | City), 
              data = data_long2)
summary(melm2)

# Plot the results - same as stratified analysis
ggplot(data=data_long2, aes(x=Temp, y=log(Energy), color = factor(City))) + 
  geom_point() + 
  geom_smooth(method = 'lm',se = F) +
  ggtitle('Temperature vs. Energy Consumption') +
  xlab('Temperature') +
  ylab('Energy Usage') +
  scale_color_discrete(name="City")

# Look at diagnostic plots
plot(melm2)
# Things might be a little better

# Predict values for Denver
pred_den <- predict(melm2,subdata_long)

# Plot the prediction vs fitted values
qplot(data_long2$Energy,exp(fitted.values(melm2))) +
  geom_point(aes(x=subdata_long$Energy,y=exp(unname(pred_den))),col = 'red') +
  ggtitle('Predicted Energy vs. Actual Energy') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red')

# Conclusion: Better

# Fit a poisson model
################################################################
plm <- glm(Energy ~ Pop+as.factor(Region)+Temp, 
           family="poisson", data=data_long)
summary(plm)

# Look at diagnostic plots
plot(plm)
# Things seem better

# Predict values for Denver
pred_den <- predict(plm,subdata_long)

# Plot the prediction vs fitted values
qplot(data_long$Energy,fitted.values(plm)) +
  geom_point(aes(x=subdata_long$Energy,y=exp(unname(pred_den))),col = 'red') +
  ggtitle('Predicted Energy vs. Actual Energy') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red')
# Looks really bad

# Conclusion: Try again with city
plm2 <- glm(Energy ~ Pop+as.factor(Region)+Temp*as.factor(City), 
            family="poisson", data=data_long)
summary(plm2)

# Look at diagnostic plots
plot(plm2)
# Things seem better

# Predict values for Denver
pred_den <- predict(plm2,subdata_long)

# Plot the prediction vs fitted values
qplot(data_long$Energy,fitted.values(plm2)) +
  geom_point(aes(x=subdata_long$Energy,y=exp(unname(pred_den))),col = 'red') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red') +
  ggtitle('Predicted Energy vs. Actual Energy (Gwh')
# Looks really good! - Best model so far

# Fit a mixed effect poisson
################################################################  
meplm <- glmer(Energy ~ Pop+as.factor(Region)+Temp + (1 + Temp | City), 
               family="poisson", data=data_long)
summary(meplm)

# Look at diagnostic plots
plot(meplm)
# Things seem better

# Predict values for Denver
pred_den <- predict(meplm,subdata_long)

# Plot the prediction vs fitted values
qplot(data_long$Energy,fitted.values(meplm)) +
  geom_point(aes(x=subdata_long$Energy,y=exp(unname(pred_den))),col = 'red') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red') + 
  ggtitle('Predicted Energy vs. Actual Energy')
# Looks really good! - Best model so far

# Fit a zero-inflated mixed poisson
################################################################  
zglm <- glmmTMB(Energy ~ Pop+as.factor(Region)+Temp + (1 + Temp | City), 
                family="poisson", data=data_long,
                zi=~Temp)
summary(zglm)

# Predict values for Denver
pred_den <- predict(zglm,subdata_long)

# Plot the prediction vs fitted values
qplot(data_long$Energy,exp(predict(zglm))) +
  geom_point(aes(x=subdata_long$Energy,y=exp(unname(pred_den))),col = 'red') +
  xlab('Acutal Energy Values') +
  ylab('Predicted Energy Values') +
  geom_abline(slope=1, intercept=0, color = 'red') + 
  ggtitle('Predicted Energy vs. Actual Energy')
# Conclusion - didn't really change the model that much

# Compare residuals of all the constructed models
################################################################

# Compare SSR
sum((data_long$Energy - predict(poolm,data_long))^2)
sum((data_long$Energy - predict(poolm2,data_long))^2)
sum((data_long$Energy - exp(predict(poolm3,data_long)))^2)
sum((data_long$Energy - predict(stratlm,data_long))^2)
sum((data_long$Energy - exp(predict(stratlm2,data_long)))^2)
sum((data_long$Energy - predict(melm,data_long))^2)
sum((data_long$Energy - exp(predict(melm2,data_long)))^2)
sum((data_long$Energy - exp(predict(plm,data_long)))^2)
sum((data_long$Energy - exp(predict(plm2,data_long)))^2) #Best
sum((data_long$Energy - exp(predict(meplm,data_long)))^2)
sum((data_long$Energy - predict(zglm,data_long))^2)

# Find best prediction for denver
sum((subdata_long$Energy - predict(poolm,subdata_long))^2)
sum((subdata_long$Energy - predict(poolm2,subdata_long))^2)
sum((subdata_long$Energy - exp(predict(poolm3,subdata_long)))^2)
sum((subdata_long$Energy - predict(stratlm,subdata_long))^2)
sum((subdata_long$Energy - exp(predict(stratlm2,subdata_long)))^2)
sum((subdata_long$Energy - predict(melm,subdata_long))^2)
sum((subdata_long$Energy - exp(predict(melm2,subdata_long)))^2)
sum((subdata_long$Energy - exp(predict(plm,subdata_long)))^2)
sum((subdata_long$Energy - exp(predict(plm2,subdata_long)))^2) #Best
sum((subdata_long$Energy - exp(predict(meplm,subdata_long)))^2)
sum((subdata_long$Energy - predict(zglm,subdata_long))^2)