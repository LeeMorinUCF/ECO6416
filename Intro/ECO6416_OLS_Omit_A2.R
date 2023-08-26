##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Regression with Simulated Data: Omitted Variables
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
# ECO6416_OLS_Omit uses simulated data to create an example
#   that illustrates the change in estimates resulting from
#   omitted variables.
#
# Dependencies:
#   ECO6416_tools.R
#
##################################################

##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# RStudio does its work in a working directory,
# which is a folder on your computer.
# Display the current path to working directory, with getwd():
getwd()


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

# Fraction of dataset in an accident.
pct_accident <- 0.4

# Frequency of damages (only after an accident).
prob_damage <- 0.15

# Additional terms:
sigma_2 <- 4000    # Variance of error term
num_obs <- 100      # Number of observations in dataset


##################################################
# Generating the Data
##################################################

# Call the new_sample function from ECO6416_tools.R.
car_data <- other_sample(beta_0, beta_mileage, beta_accident, beta_damage,
                         avg_mileage, sd_mileage, pct_accident, prob_damage,
                         sigma_2, num_obs)


# Summarize the data to inspect for data quality.
summary(car_data)

# Check that damages occurred only in accident:
table(car_data[, 'accident'], car_data[, 'damage'])
# Data errors are the most frequent cause of problems in model-building.

# Run it again if no damages occurred.


##################################################
# Estimating the Regression Model
# Model 1: All Variables Included
##################################################

# Estimate a regression model.
lm_full_model <- lm(data = car_data,
                    formula = car_price ~ mileage + accident + damage)

# Output the results to screen.
summary(lm_full_model)


##################################################
# Estimating the Regression Model
# Model 2: Omitting One Variable
##################################################

# Estimate a regression model.
lm_no_damages <- lm(data = car_data,
                        formula = car_price ~ mileage + accident) # damage removed.

# Output the results to screen.
summary(lm_no_damages)


##################################################
#
# Exercise:
#
# Observe the values of the coefficient for damage.
# Then compare the change in coefficient on accidents
# with and without the damage variable.
#
# Then set the damage coefficient to zero and repeat the exercise.
# Compare the change in coefficient on accidents
# with and without the damage variable.
#
##################################################



##################################################
# End
##################################################
