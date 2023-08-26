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
# College of Business
# University of Central Florida
#
# August 25, 2023
#
##################################################
#
# ECO6416_Model_Testing gives an example of regression model testing
#   by first estimating a model with omitted variables on a training dataset
#   and then detecting the misspecification on a testing dataset.
#
# This version extends the types of models built to provide more examples
#   of when the initial model needs adjustment or not.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# You need to set the working directory to the location
# of your files.
# setwd("/path/to/your/folder")
# Find this path as follows:
# 1. Click on the "File" tab in the bottom right pane.
# 2. Browse to the folder on your computer that contains your R files.
# 3. Click the gear icon and choose the option "Set as Working Directory."
# 4. Copy the command from the Console in the bottom left pane.
# 5. Paste the command below:

setwd("C:/Users/le279259/OneDrive - University of Central Florida/Desktop/ECO6416_Demos")


# Now, RStudio should know where your files are.



# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)


# Read function for sampling data.
source('ECO6416_tools.R')
# This is the same as running the ECO6416_tools.R script first.
# It assumes that the script is saved in the same working folder.

# The file ECO6416_tools.R must be in the working directory.
# If you an error message, make sure that the file is
# located in your working directory.
# Also make sure that the name has not changed.


##################################################
# Setting the Parameters
##################################################

# Dependent Variable: Automobile values

beta_0          <-   50000     # Intercept
beta_mileage    <- -  0.20     # Slope coefficient for mileage
beta_accident   <- -  5000     # Slope coefficient for accident
beta_damage     <- - 20000     # Slope coefficient for damage
# beta_damage     <-       0   # Alternate Slope coefficient for damage

# Distribution of mileage.
avg_mileage <- 50000
sd_mileage  <- 10000

# Extra parameter for measurement error in mileage.
measurement_error_mileage <- 0.01

# Fraction of dataset in an accident.
pct_accident <- 0.4

# Frequency of damages (only after an accident).
prob_damage <- 0.20 # More damage to illustrate the difference.

# Additional terms:
sigma_2 <- 4000    # Variance of error term
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

# Call the other_sample() function from ECO6416_tools.R.
car_data <- other_sample(beta_0, beta_mileage, beta_accident, beta_damage,
                         avg_mileage, sd_mileage, pct_accident, prob_damage,
                         sigma_2, num_obs)

# Summarize the data.
summary(car_data)

# Check that damages occurred only in accidents:
table(car_data[, 'accident'], car_data[, 'damage'])
# Data errors are the largest cause of problems in model-building.

# Check for the subsamples for estimation and testing.
# Estimation sample:
table(car_data[obsns_for_estimation, 'accident'],
      car_data[obsns_for_estimation, 'damage'])
# Testing sample:
table(car_data[!obsns_for_estimation, 'accident'],
      car_data[!obsns_for_estimation, 'damage'])
# ! means 'not'.
# So, !obsns_for_estimation means to include only the
# observations left out for testing the model.

# Run it again up to here if you don't observe damages
# in both samples.



##################################################
# Estimating the Regression Model
# Model 1: Omitting One Variable
# We know from Assignment 1 that this is the wrong model
# but we can use it to demonstrate the approach to model testing.
##################################################

# Estimate a regression model.
lm_no_damages <- lm(data = car_data[obsns_for_estimation, ],
                        # Notice only first set of observations (training dataset).
                        formula = car_price ~ mileage + accident) # damage removed.

# Output the results to screen.
summary(lm_no_damages)


##################################################
# Calculate the predictions of the 'No damages' model
##################################################

# Store predictions for the entire dataset (both training and testing)
# based on the model built on only the training dataset.
car_data[, 'prediction'] <- predict(lm_no_damages,
                                        newdata = car_data) # All observations, including testing sample.


##################################################
# Testing the Regression Model
# Testing Model 1: All Variables Included
# Note that the prediction is also included
##################################################

# Estimate a regression model.
lm_testing_no_damages_1 <- lm(data = car_data[!obsns_for_estimation, ],
                    # Notice only second set of observations (testing dataset).
                    formula = car_price ~ prediction + accident + damage)
# Dropped mileage because of multicollinearity.
# Want to test accuracy for accidents and relevance of damages.

# Output the results to screen.
summary(lm_testing_no_damages_1)





##################################################
#
# Exercise 1:
#
# Observe the values of the coefficients for accidents and damages.
# Then compare these to the bias recorded for the first (misspecified) regression.
#
##################################################




##################################################
#
# Exercise 2:
#
# Estimate the true model (including damages, see below).
# Then perform the second regression to test the correct model.
# Observe the values of the coefficients for accidents and damages.
# Compare the results with the test of the incorrect model above.
#
##################################################


##################################################
# Estimating the Regression Model
# Model 2: Full Model, with no omitted variable
# We know from the parameter values that this is the correct model
# but we can use it to demonstrate the approach to model testing.
##################################################


# Estimate a regression model.
lm_full_model <- lm(data = car_data[obsns_for_estimation, ],
                        # Notice only first set of observations (training dataset).
                        formula = car_price ~ mileage + accident + damage)

# Output the results to screen.
summary(lm_full_model)


# Store predictions for the entire dataset (both training and testing)
# based on the model built on only the training dataset.
car_data[, 'prediction_full'] <- predict(lm_full_model,
                                        newdata = car_data) # All observations, including testing sample.


##################################################
# Testing the Regression Model
# Model 2: Full Model, with no omitted variable
# Note that the prediction is also included
##################################################

# Estimate a regression model.
lm_testing_full_model_1 <- lm(data = car_data[!obsns_for_estimation, ],
                         # Notice only second set of observations (testing dataset).
                         formula = car_price ~ prediction_full + accident + damage)
# Dropped mileage because of multicollinearity.
# Want to test accuracy for accidents and relevance of damages.

# Output the results to screen.
summary(lm_testing_full_model_1)



##################################################
# Bonus round: Notice what happened above.
# Try a different version of the testing model.
# Subract predictions from actual house prices.
# Use the difference as the regressand.
##################################################

car_data[, 'prediction_diff_full']  <- car_data[, 'car_price'] -
  car_data[, 'prediction_full'] # Predictions from full model.

# Estimate a regression model.
lm_testing_full_model_2 <- lm(data = car_data[!obsns_for_estimation, ],
                         # Notice only second set of observations (testing dataset).
                         formula = prediction_diff_full ~ mileage + accident + damage)


# Output the results to screen.
summary(lm_testing_full_model_2)



##################################################
#
# Exercise 3:
#
# Observe the values of the coefficients for accidents and damages.
# Notice what happens when the correct model has been estimated.
#
##################################################


##################################################
# Now use this approach to reconsider the original
# (misspecified) model in which we omitted the
# damage variable.
##################################################


car_data[, 'prediction_diff']  <- car_data[, 'car_price'] -
  car_data[, 'prediction'] # Predictions from no_damages model.

# Estimate a regression model.
lm_testing_no_damages_2 <- lm(data = car_data[!obsns_for_estimation, ],
                              # Notice only second set of observations (testing dataset).
                              formula = prediction_diff ~ mileage + accident + damage)


summary(lm_testing_no_damages_2)



##################################################
#
# Exercise 4:
#
# Observe the values of the coefficients for accidents and damage.
# Then compare these to the bias recorded for the first (misspecified) regression.
#
##################################################




##################################################
# End
##################################################
