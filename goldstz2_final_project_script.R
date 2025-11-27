## Project:  STA 145, Fall 2025, Final Project
# Located:   Posit Cloud
# File Name: int_data
# Date:      2025_11_26
# Who:       Zachary Goldstein



## Load packages
# NOTE: Run base.R if these commands return an error!
library(readr)
library(dplyr)
library(tidytext)
library(tidyverse)
library(ggplot2)
library(haven)
library(forcats)
library(psych)

# Load data 
library(readr)
dataset <- read_csv("goldstz2_Sheet 1.csv")
View(dataset)

##################################################################################
############### Table 1: descriptive statistics    ####################   
##################################################################################
table(dataset$interceptions_caught)
mean(dataset$interceptions_caught)
sd(dataset$interceptions_caught)

table(dataset$interceptions_thrown)
mean(dataset$interceptions_thrown)
sd(dataset$interceptions_thrown)

table(dataset$season)

table(dataset$game)

table(dataset$win)
mean(dataset$win)
sd(dataset$win)

##################################################################################
#################### Figure 1: boxplot             ####################   
##################################################################################
# BOX PLOT
lm(interceptions_caught ~ season, data = dataset)
aov(interceptions_caught ~ season, data = dataset)
summary(interceptions_caught ~ season, data = dataset)
boxplot(interceptions_caught ~ season, data = dataset)

lm(interceptions_caught ~ win, data = dataset)
aov(interceptions_caught ~ win, data = dataset)
summary(interceptions_caught ~ win, data = dataset)
boxplot(interceptions_caught  ~ win, data = dataset)

##################################################################################
####################   Figure 2: scatter plot             ####################   
##################################################################################
linear_plot <- plot(data$interceptions_caught, data$win)
print(linear_plot)

# add x line and y line for means
meany <- mean(data$win)
meanx <- mean(data$interceptions_caught)

abline(c = meanx, col = "black")
abline(h = meany, col = "black")

linear_relationship <- lm(data$win ~ interceptions_caught, data = data)
summary(linear_relationship)


abline(linear_relationship, col = "red")
##################################################################################
####################  Figure 3: residual plot                ####################   
##################################################################################
# Plot the residuals
plot(data$win, residuals(linear_relationship))

# Add a horizontal line at zero to indicate the baseline
abline(h = 0, col = "red")

##################################################################################
####################  Table 2: contingency table                ####################   
##################################################################################
table(data$interceptions_thrown, data$win)
chisq.test(data$interceptions_thrown, data$win)