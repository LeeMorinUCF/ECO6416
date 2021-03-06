##################################################
# 
# ECO 6416.0028 Applied Business Research Tools
# 
# OLS Regression Demo
# Effects of Omitted Variables and 
# Testing for Misspecification from Omitted Variables
# 
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
# 
# September 24, 2018
# 
##################################################
# 
# ECO6416_Model_Testing gives an example of regression model testing
#   by first estimateing a model with omitted variables on a training dataset
#   and then detecting the misspecification on a testing dataset.
# 
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)

# Or do this in one step (using buttons in  File panel).
setwd("~/Teaching/ECO6416_Fall2019/Module03")

# Read function for sampling data. 
source('ECO6416_Sim_Data.R')
# This is the same as running the ECO6416_Sim_Data.R script first.
# It assumes that the script is saved in the same working folder.


##################################################
# Setting the Parameters
##################################################

# Dependent Variable: Property values (in Millions)

beta_0          <-   0.10    # Intercept
beta_income     <-   5.00    # Slope ceofficient for income
beta_cali       <-   0.25    # Slope coefficient for California
beta_earthquake <- - 0.50    # Slope coefficient for earthquake
# beta_earthquake <- - 0.00    # Slope coefficient for earthquake

# Distribution of incomes (also in millions).
avg_income <- 0.1
sd_income <- 0.01

# Fraction of dataset in California.
pct_in_cali <- 0.5

# Frequency of earthquakes (only in California).
# prob_earthquake <- 0.05
prob_earthquake <- 0.15 # More earthquakes to illustrate. 

# Additional terms:
sigma_2 <- 0.1        # Variance of error term
num_obs <- 200        # Number of observations in entire dataset
num_obs_estn <- 100   # Number of observations for estimation.
# Notice num_obs is twice as large, saving half for out-of-sample testing.

# Select the variables for estimation at random. 
obsns_for_estimation <- runif(num_obs) < num_obs_estn/num_obs
# Test how many are in each sample. 
table(obsns_for_estimation)



##################################################
# Generating the Data
##################################################

# Call the housing_sample function from ECO6416_Sim_Data.R. 
housing_data <- housing_sample(beta_0, beta_income, beta_cali, beta_earthquake, 
                               avg_income, sd_income, pct_in_cali, prob_earthquake, 
                               sigma_2, num_obs)

# Summarize the data.
summary(housing_data)

# Check that earthquakes occurred only in California:
table(housing_data[, 'in_cali'], housing_data[, 'earthquake'])
# Data errors are the largest cause of problems in model-building.

# Check for the subsamples for estimation and testing. 
# Estimation sample:
table(housing_data[obsns_for_estimation, 'in_cali'], 
      housing_data[obsns_for_estimation, 'earthquake'])
# Testing sample:
table(housing_data[!obsns_for_estimation, 'in_cali'], 
      housing_data[!obsns_for_estimation, 'earthquake'])
# ! means 'not'.


##################################################
# Estimating the Regression Model
# Model 1: Omitting One Variable
# We know from Assignment 1 that this is the wrong model
# but we can use it to demonstrate the approach to model testing. 
##################################################

# Estimate a regression model.
lm_no_earthquakes <- lm(data = housing_data[obsns_for_estimation, ], 
                        # Notice only first set of observations (training dataset).
                        formula = house_price ~ income + in_cali) # earthquake removed.

# Output the results to screen.
summary(lm_no_earthquakes)


##################################################
# Calculate the predictions of the 'No earthquakes' model
##################################################

# Store predictions for the entire dataset (both training and testing)
# based on the model built on only the training dataset. 
housing_data[, 'prediction'] <- predict(lm_no_earthquakes, 
                                        newdata = housing_data) # All observations, including testing sample. 


##################################################
# Testing the Regression Model
# Model 2: All Variables Included
# Note that the prediction is also included
##################################################

# Estimate a regression model.
lm_testing_model_1 <- lm(data = housing_data[!obsns_for_estimation, ], 
                    # Notice only second set of observations (testing dataset).
                    formula = house_price ~ prediction + in_cali + earthquake)
# Dropped income because of multicollinearity. 
# Want to test accuracy for california and relevance of earthquakes.

# Output the results to screen.
summary(lm_testing_model_1)



##################################################
# Bonus round: Notice what happened above. 
# Try a different version of the testing model (dropping income).
##################################################

# Estimate a regression model.
lm_testing_model_2 <- lm(data = housing_data[(num_obs_estn + 1):num_obs, ], 
                         # Notice only second set of observations (testing dataset).
                         formula = house_price ~ prediction + in_cali + earthquake)


# Output the results to screen.
summary(lm_testing_model_2)



##################################################
# 
# Exercise 1 (in class):
# 
# Observe the values of the coefficients for california and earthquakes.
# Then compare these to the bias recorded for the first (misspecified) regression.
# 
##################################################




##################################################
# 
# Exercise 2 (after class):
# 
# Estimate the true model (including earthquakes).
# Then perform the second regression to test the correct model. 
# Observe the values of the coefficients for california and earthquakes.
# Compare the results with the test of the incorrect model above.
# 
##################################################



##################################################
# End
##################################################
