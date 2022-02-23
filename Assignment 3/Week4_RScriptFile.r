### ================================
# Title: Discussion Section Week 3
# Name: Head TA
### ================================

# Topics for this week:
# - Intro to Loops
# - Comparing Regression Models
# - Calculating out-of-sample MSE

# Set Working Directory
setwd("YOUR DIRECTORY")

# Load Packages
library("magrittr") # This is used to write more readable code
library("dplyr") # This is a set of useful functions for working with data

# Load the Data
titanic3 <- read.csv("titanic3.csv")

# Remember that our new dataset has two passengers (Jack and Rose) and we don't know 
# if they survived. So we want to train a model to predict their probability of surviving

# We use all the rest of the data (we which call our in sample data) to train a model
# The we use that model to predict survival for Jack and Rose (who comprise our out of sample data)
# for any given model, the out of sample data are any data not use to train the model


### ================================
### Train a model to make 
### out-of-sample predictions
### ================================

# Split our data into in sample and out of sample
out_of_sample = subset(titanic2, titanic2$name == "Dawson, Jack" | titanic2$name == "DeWitt Bukater, Rose")
in_sample = subset(titanic2, titanic2$name != "Dawson, Jack" & titanic2$name != "DeWitt Bukater, Rose")



### ================================
### Model 1

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



### ================================
### Model 2

# Train a regression model using the in_sample data
model2 = lm(survived ~ male + first_class + sec_class + fam_dummy, data = in_sample)
summary(model2) 

# What are model 1's predictions for Jack and Rose (in the out_of_sample data)?
# Save predictions as a variable in the out of sample dataset
out_of_sample$model2_predictions <- predict(model2, out_of_sample)

# Print predictions
out_of_sample %>% select(c(name, model2_predictions))

# Jack's probability of surviving is approx 0.087 or 8.7%
# Rose's probability of surviving is approx 0.926 or 92.6%


### We don't know which model gives us more accurate predictions 
### Because we don't know whether or not Jack and Rose survived.
### In the next section, we will learn one way of evaluating
### The model's effectiveness.





### ================================
### Leave-One-Out Exercise
### ================================

### In this section, we will test how accurate our models are
### by removing one observation, training a model, then testing 
### how well our model predicts the observation we left out.


### Lets split our data into training and testing
# We we start by removing the first observation
training <- in_sample[-1, ]
# and saving it in a dataframe by itself
testing <- in_sample[1, ]

### Model 1
# now re-estimate model 1 with the new training data
model1 = lm(survived ~ male + first_class + first_class_male, data = training)
# predict the outcome for the observation we left out
predict(model1, testing)
# find the square error of this prediction (the difference between the truth and the prediction)^2
error_model1 <- (testing$survived - predict(model1, testing))^2

### Model 2
# now re-estimate model 2 with the new training data
model2 = lm(survived ~ male + first_class + sec_class + fam_dummy, data = training)
# predict the outcome for the observation we left out
predict(model2, testing)
# find the square error of this prediction (the difference between the truth and the prediction)^2
error_model2 <- (testing$survived - predict(model2, testing))^2

# Which model had a larger square error for this one observation?
error_model1
error_model2

# Repeat this exercise for every observation using a loop and 
# average all the square errors for each model


### ================================
### Build practice building loops
### ================================

# Print each number in a loop
for (i in c(1,2,3)){
  print(i)
}

# Let's initialize an empty vector to store numbers in 
test <- c()

# Now we will add numbers to this vector one at a time in a loop
for (i in c(1,2,3)){
  
  test <- c(test, i)
  
}

# Save a list of colors
colors <- c("red", "blue")

# Print them in a sentence
for (j in colors){
  
  print(paste0("My shirt is ", j))
  
}


### ========================================
### Repeat Leave-one-out Exercise in a Loop
### ========================================

# Where to store the square errors for model 1?
errors1 <- c()

# Where to store the square errors for model 2?
errors2 <- c()

# Start the Loop
for (i in c(1:891)){
  
  ### Lets split our data into training and testing
  training <- in_sample[-i, ]
  testing <- in_sample[i, ]
  
  ### Model 1
  model1 = lm(survived ~ male + first_class + first_class_male, data = training)
  temp <- (testing$survived - predict(model1, testing))^2
  errors1 <- c(errors1, temp)
  
  ### Model 2
  # now re-estimate model 2 with the new training data
  model2 = lm(survived ~ male + first_class + sec_class + fam_dummy, data = training)
  temp <- (testing$survived - predict(model2, testing))^2
  errors2 <- c(errors2, temp)
  
}

# Now we can see which model has smaller mean squared errors on average
mean(errors1)
mean(errors2)



