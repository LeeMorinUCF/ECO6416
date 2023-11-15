##################################################
#
# ECO 6416: Applied Business Research Tools
#
# OLS Regression Demo
# Detecting and Correcting for Serial Correlation
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# November 15, 2023
#
##################################################
#
# ECO6416_Serial_Correlation_by_Differencing gives an example 
#   of an OLS regression model with serial correlation, 
#   detects the serial correlation, adjusts for it 
#   by taking first differences of the series, 
#   and reducing the problem of serial correlation,
#   instead of correcting for it by
#   calculating Newey-West standard errors, 
#   and corrects it with GLS estimation.
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


# The csv file used below must be in the working directory.
# If you an error message, make sure that the file is
# located in your working directory.
# Also make sure that the name has not changed.



# Load library for Newey-West sandwich estimator
# for standard errors for serial correlation in errors.
# Download and install the package the first time:
# install.packages('sandwich')
# Declare that you will use this library every time:
library(sandwich)

# install.packages('lmtest')
library(lmtest)


##################################################
# Loading the Data and Conducting Initial Assessment
##################################################

# Variable definitions in CONS9.csv:
# year: year t
# con: Real personal consumption expenditures in year t, billions of 2009 dollars.
# dpi: Real disposable personal income in year t, billions of 2009 dollars.
# aaa: The real interest rate on AAA corporate bonds in year t.

cons_data <- read.csv('CONS9.csv')

# Inspect the contents.
summary(cons_data)
# Make sure there are no problems with the data.

# Inspect the dependent variable.
hist(cons_data[, 'con'])
plot(cons_data[, 'con'])
lines(cons_data[, 'dpi'])

# There is an obvious increasing trend over time.

# Inspect the correlations between numeric explanatory variables.
list_of_variables <- colnames(cons_data)[3:4]
cor(cons_data[, list_of_variables])
# Be aware of any explanatory variables that are highly correlated
# (both positively and negatively) with each other.



# Calculate N and K (we'll need them later).
# N:
num_obs <- nrow(cons_data)
# K:
num_vars <- length(list_of_variables)


##################################################
# Generating New Variables
##################################################

# You can create new variables in two ways:
# 1. Add commands within this program
# 2. Create new columns in a spreadsheet
#   (but you would need to re-load the dataset
#   after adding variables this way)




##################################################
# Estimating a Regression Model
# Model 1: Linear model for Aggregate Consumption
# Start with a full model that includes all variables.
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = cons_data,
                 formula = con ~ dpi + aaa)

# Output the results to screen.
summary(lm_model_1)



##################################################
# Calculate and inspect the residuals
##################################################

# First calculate the predictions.
cons_data[, 'con_hat'] <- predict(lm_model_1)

# Then calculate the residuals, the difference
# between the observed con and the predictions.
cons_data[, 'con_resid'] <- cons_data[, 'con'] - cons_data[, 'con_hat']

# Plot the residuals over time.
plot(cons_data[, 'year'], cons_data[, 'con_resid'],
     main = c('OLS Residuals over Time', 
              '(Differenced Series)'),
     xlab = 'Year',
     ylab = 'Residual',
     type = 'l',
     lwd = 2,
     col = 'blue')
# Looks like positive serial correlation.


# Generate a lagged version of the residuals.
cons_data[, 'con_resid_lag'] <- c(NA, cons_data[1:(num_obs - 1), 'con_resid'])
# View the data to verify.

# Plot the residuals to check for a relationship.
plot(cons_data[, 'con_resid_lag'], cons_data[, 'con_resid'],
     main = c('OLS Residuals vs Lagged Residuals', 
              '(Original Series)'),
     xlab = 'Lagged Residual',
     ylab = 'Residual',
     col = 'blue')
# Again, looks like positive serial correlation.



# Formal test with Durbin-Watson statistic.
# Formula is just like lm() function for regression.
dwtest(data = cons_data,
       formula = con ~ dpi + aaa)


# Formal test with LM statistic.
# First run regression of residuals on all variables
# and lagged errors.
lm_model_lm_test <- lm(data = cons_data,
                       formula = con_resid ~ dpi + aaa + con_resid_lag)

# Output the results to screen as usual.
summary(lm_model_lm_test)

# Test for coefficient on lagged residual is N*R^2.
# Compare to critical value for chi-squared with 1 degree of freedom.
lm_test_summ <- summary(lm_model_lm_test)
print(num_obs*lm_test_summ$r.squared)
# Much bigger than 3.84.
# Conclusion: Reject H_O: No serial correlation.


##################################################
# Conclusion:
# Serial correlation is a problem
##################################################




##################################################
# The Street-Smart Solution:
# Model the Changes in the Series, not the Levels
##################################################


# To address the problem of serial correlation, 
# take first differences to measure the changes in each series over time. 

# Subtract consumption last year from current consumption.
cons_data[, 'con_diff'] <- cons_data[, 'con'] - 
  c(NA, cons_data[1:(num_obs - 1), 'con'])
# Repeat for other variables dpi and aaa.
cons_data[, 'dpi_diff'] <- cons_data[, 'dpi'] - 
  c(NA, cons_data[1:(num_obs - 1), 'dpi'])
cons_data[, 'aaa_diff'] <- cons_data[, 'aaa'] - 
  c(NA, cons_data[1:(num_obs - 1), 'aaa'])


##################################################
# Estimating a Regression Model
# Model 1: Linear model for Aggregate Consumption
# Start with a full model that includes all variables.
##################################################

# Estimate a regression model.
lm_model_diff_1 <- lm(data = cons_data,
                 formula = con_diff ~ dpi_diff + aaa_diff)

# Output the results to screen.
summary(lm_model_diff_1)



##################################################
# Calculate and inspect the residuals
##################################################

# First calculate the predictions.
cons_data[, 'con_diff_hat'] <- c(NA, predict(lm_model_diff_1))

# Then calculate the residuals, the difference
# between the observed con and the predictions.
cons_data[, 'con_diff_resid'] <- cons_data[, 'con_diff'] - cons_data[, 'con_diff_hat']

# Plot the residuals over time.
plot(cons_data[, 'year'], cons_data[, 'con_diff_resid'],
     main = c('OLS Residuals over Time', 
              '(Differenced Series)'),
     xlab = 'Year',
     ylab = 'Residual',
     type = 'l',
     lwd = 2,
     col = 'blue')
# Looks much more stable over time.
# No obvious sign of serial correlation.


# Generate a lagged version of the residuals.
cons_data[, 'con_diff_resid_lag'] <- c(NA, cons_data[1:(num_obs - 1), 'con_diff_resid'])
# View the data to verify.

# Plot the residuals to check for a relationship.
plot(cons_data[, 'con_diff_resid_lag'], cons_data[, 'con_diff_resid'],
     main = c('OLS Residuals vs Lagged Residuals', 
              '(Differenced Series)'),
     xlab = 'Lagged Residual',
     ylab = 'Residual',
     col = 'blue')
# Again, no obvious sign of serial correlation, if any.



# Formal test with Durbin-Watson statistic.
# Formula is just like lm() function for regression.
dwtest(data = cons_data,
       formula = con_diff ~ dpi_diff + aaa_diff)
# Did not reject null of no serial correlation.


# Formal test with LM statistic.
# First run regression of residuals on all variables
# and lagged errors.
lm_model_diff_lm_test <- lm(data = cons_data,
                       formula = con_diff_resid ~ dpi_diff + aaa_diff + con_diff_resid_lag)

# Output the results to screen as usual.
summary(lm_model_diff_lm_test)
# Note that coefficient on lagged residual is zero, 
# which is a good sign, but the formal test is as follows.


# Test for coefficient on lagged residual is N*R^2.
# Compare to critical value for chi-squared with 1 degree of freedom.
lm_test_diff_summ <- summary(lm_model_diff_lm_test)
print(num_obs*lm_test_diff_summ$r.squared)
# Much smaller than 3.84, the critical value.
# Conclusion: Accept H_O: No serial correlation.



##################################################
# Conclusion:
# No serial correlation: No need to solve the problem,
# or to correct for it, because our modeling decisions
# prevented the problem.
##################################################




##################################################
# End
##################################################
