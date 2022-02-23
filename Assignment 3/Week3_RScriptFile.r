### ================================
# Title: Discussion Section Week 3
# Name: Head TA
### ================================

# Topics for this week:
# - Creating new variables for prediction
# - Predicting the outcome for out-of-sample variables

# Set Working Directory
setwd("YOUR DIRECTORY")

# Load Packages
library("ggplot2") # This package is used to graph data
library("magrittr") # This is used to write more readable code
library("dplyr") # This is a set of useful functions for working with data

# Load the Data
titanic2 <- read.csv("titanic2.csv")


### ================================
# Regression as Prediction
### ================================
# Our dataset has two new passengers, but we don't know whether they survived.
# Using the other passengers, we want to build a model to predict
# whether these two new passengers survived.

# Before I build a model, I want to generate some new variables might be useful

# total number of famiy members aboard
titanic2$fam_total = titanic2$sibsp + titanic2$parch 

# equals one if they have any family aboard
titanic2$fam_dummy = as.numeric(titanic2$fam_total > 0) 

# equals one if they are in first class
titanic2$first_class = as.numeric(titanic2$pclass == 1) 

# equals one if they are in second class
titanic2$sec_class = as.numeric(titanic2$pclass == 2)

# equals one if they are in a male
titanic2$male = as.numeric(titanic2$sex == "male")

# equals one if they are in first class AND they are a male
titanic2$first_class_male = titanic2$male * titanic2$first_class 


### ================================
### Train a model to make 
### out-of-sample predictions
### ================================

# Split our data into in sample and out of sample
out_of_sample = subset(titanic2, titanic2$name == "Dawson, Jack" | titanic2$name == "DeWitt Bukater, Rose")
in_sample = subset(titanic2, titanic2$name != "Dawson, Jack" & titanic2$name != "DeWitt Bukater, Rose")


# Train a regression model using the in_sample data
model1 = lm(survived ~ male + first_class + first_class_male, data = in_sample)
summary(model1) 

# What are model 1's predictions for Jack and Rose (in the out_of_sample data)?
# Save predictions as a variable in the out of sample dataset
out_of_sample$model1_predictions <- predict(model1, out_of_sample)

# Print predictions
out_of_sample %>% select(c(name, model1_predictions))

# Jack's probability of surviving is approx 0.141 or 14.1%
# Rose's probability of surviving is approx 0.968 or 96.8%


