##########################
# PBPL 26400
# Week 6
##########################

### Topics for this week
# Estimate the intent to treat
# Find the compliance rate
# Estimate the CATE
# Think about the ways attrition can effect your estimate of the CATE


# Install Packages #(uncomment and run each line to install - you only need to install once)
# Install.packages("magrittr")
# Install.packages("dplyr")

# Load Packages
library("magrittr")
library("dplyr")

# Set Working Directory (replace "YOUR DIRECTORY" with the place where your data are stored)
setwd("C:/Users/emmea/OneDrive - The University of Chicago/PBPL264/2022/Discussion")

# Load data on high school seniors in High School A
data <- read.csv("scholarship.csv")

# Description of Variables
# studentID: 
#     ID number for each student
# female: 
#     equals 1 if for female students
# assign_treatment: 
#     equals 1 if student was assigned a treatment (offered a scholarship at College X)
# treated: 
#     equals 1 if student was treated (used scholarship at College X)
# graduate: 
#     equals 1 if student graduated from any college


##### TREATMENT
# High school A was given enough money to offer 4,000 seniors a scholarship to cover
# all college expenses if they attend College X

# The school decides to randomly assign seniors to get these scholarships.
# Being offered a scholarship is our "Treatment Assignment"
# Using the scholarship at College X is our "Treatment"

##### NON-COMPLIANCE
# Some students might not want to go to College X and some students might not want to
# go to College at all, so not all students who are offered a scholarship will use it.
# Lastly, some students who were not randomly assigned the scholarship, managed to get
# one anyway

##### OUTCOMES
# We want to see if this treatment increases the probablity that a student graduates
# from any college

##### ATTRITION
# Note that for some of the students in this experiment, we were unable to track
# down whether or not they graduated from college


#############################
# Difference in Means 
# between Treated and Control
#############################

# What is the probability of graduating college for people who were treated?
mean(data$graduate[data$treated == 1], na.rm=T)

# What is the probability of graduating college for people who were not treated?
mean(data$graduate[data$treated == 0], na.rm = T)

# What is the implied effect of the treatment using the two values above
0.4431149 - 0.3634343 

# We can get this same number by regressing graduate on treated
summary(lm(graduate ~ treated, data))

# Is this implied effect statistically significant?



##########################
# Calculating the CATE
##########################
# Describe which students are Compliers, Always Takers, and Never Takers.

# Calculate the CATE to account for non-compliers 

# First calculate the compliance rate manually by subtracting 
# the percent of people treated who were assigned treatment
# the percent of people treated who were not assigned treatment
c <- sum(data$assign_treatment == 1 & data$treated == 1) / sum(data$assign_treatment == 1)
D <- sum(data$assign_treatment == 0 & data$treated == 1) / sum(data$assign_treatment == 0)
C-D
0.889 -  0.3758333

# Now calculate the compliance rate using OLS (first stage).
first_stage <- lm(treated ~ assign_treatment, data)
# The estimated compliance rate is the coefficient on assign_treatment
summary(first_stage)
# Save the estimated compliance rate since we will use it a lot
compliance_rate <- 0.513167


# Next estimate the intent-to-treat effect (reduced form)
reduced_form <- lm(graduate ~ assign_treatment, data)
# The estimated intent-to-treat is the coefficient on assign_treatment
summary(reduced_form)


# Divide the intent-to-treat by the estimated compliance rate to get the CATE
0.088464 / compliance_rate




##############################
# What to do about Attrition?
##############################

##### Case 1
# Estimate the CATE assuming that all of the 
# students with missing graduation data graduated

# Copy the graduation variable
data$graduate_case1 <- data$graduate
# Replace all the NA values with 1
data$graduate_case1[is.na(data$graduate_case1)] <- 1

# Estimate the new ITT (which is  0.07808)
lm(graduate_case1 ~ assign_treatment, data)

# Divide the new ITT by the compliance rate to get the new CATE
0.07808 / compliance_rate



##### Case 2
# Estimate the CATE assuming that all of the 
# students with missing graduation data did not graduate

# Copy the graduation variable
data$graduate_case2 <- data$graduate

# Replace all the NA values with 0
data$graduate_case2[is.na(data$graduate_case2)] <- 0

# Estimate the new ITT (which is 0.08125)
lm(graduate_case2 ~ assign_treatment, data)

# Divide the new ITT by the compliance rate to find the CATE under this assumption
0.08125 / compliance_rate






