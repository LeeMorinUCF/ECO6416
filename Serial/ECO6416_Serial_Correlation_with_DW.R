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
# College of Business Administration
# University of Central Florida
# 
# November 9, 2018
# 
##################################################
# 
# ECO6416_Serial_Correlation gives an example of an OLS regression model
#   with serial correlation and corrects it with GLS estimation
#   and by calculating Newey-West standard errors.
# 
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Load library for Newey-West sandwich estimator
# for standard errors for serial correlation in errors.
library(sandwich)


##################################################
# Setting the Parameters
##################################################


# Set path for working directory.
# Put files on desktop in a folder called ECO6416
wd_path <- 'C:/Users/le279259/Desktop/ECO6416'
# Modify the above line according to the specific path on your computer,
# as in:
# wd_path <- 'C:/Users/name/of/your/path'

# Set the working directory to this path. 
setwd(wd_path)

# Verify that the path was assigned correctly. 
getwd()


##################################################
# Loading the Data and Conducting Initial Assessment
##################################################

# Variable definitions in CONS9.csv:
# year: year t
# con: Real personal consumption expenditures in year t, billions of 2009 dollars. 
# dpi: Real disposable personal income in year t, billions of 2009 dollars.
# aaa: The real interest rate on AAA corporate bonds in year t. 

cons_data <- read.csv('CONS9.csv')
# cons_data <- read.csv("C:/Users/le279259/Desktop/ECO6416/CONS9.csv")

# Inspect the contents.
summary(cons_data)
# Make sure there are no problems with the data. 

# Inspect the dependent variable. 
hist(cons_data[, 'con'])

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
     main = 'OLS Residuals over Time', 
     xlab = 'Year', 
     ylab = 'Residual')
# Looks like positive serial correlation. 


# Generate a lagged version of the residuals. 
cons_data[, 'con_resid_lag'] <- c(NA, cons_data[1:(num_obs - 1), 'con_resid'])
# View the data to verify.

# Plot the residuals to check for a relationship.
plot(cons_data[, 'con_resid_lag'], cons_data[, 'con_resid'], 
     main = 'OLS Residuals vs Lagged Residuals', 
     xlab = 'Lagged Residual', 
     ylab = 'Residual')
# Again, looks like positive serial correlation. 


# Formal test with Durbin-Watson statistic.
# Formula is just like lm() function for regression.
dwtest(data = cons_data, 
       formula = con ~ dpi + aaa)




##################################################
# One Solution:
# Calculate the Newey-West Standard Errors
##################################################

# Standard erros are the square root of the 
# Newey-West covariance matrix. 


NW_se <- sqrt(diag(NeweyWest(lm_model_1)))

# Compare these to the original SEs from the OLS regression.
summary(lm_model_1)


# Need to recalculate the t-statistics and p-values.
lm_stats_NW <- coef(summary(lm_model_1))
# Replace the SEs.
lm_stats_NW[, 'Std. Error'] <- NW_se
# Recalculate the t-stats.
lm_stats_NW[, 't value'] <- lm_stats_NW[, 'Estimate'] / lm_stats_NW[, 'Std. Error']
# Recalculate the p-values.
lm_stats_NW[, 'Pr(>|t|)'] <- 2*pt(- abs(lm_stats_NW[, 't value']),
                                  df = num_obs - num_vars - 1)

# Display the adjusted regression statistics. 
print(lm_stats_NW)
# Compare to the originals.
coef(summary(lm_model_1))

# The coefficient on AAA is no longer significant.






##################################################
# Another Solution:
# Estimate a GLS Regression Model
##################################################

# Three steps:
# 1. Estimate model of residuals on lagged residuals.
# 2. Generate adjusted variables. 
# 3. Estimate GLS regression model. 


##################################################
# Estimating a Regression Model
# Model 2: Linear model for the Residuals
##################################################

# Estimate a regression model.
lm_model_2 <- lm(data = cons_data, 
                 formula = con_resid ~ con_resid_lag)

# Output the results to screen.
summary(lm_model_2)

# Obtain estimate of autocorrelation parameter.
rho <- coef(summary(lm_model_2))['con_resid_lag', 'Estimate']

##################################################
# Create the adjusted GLS variables
##################################################

# Each variable is the original, less rho times the lagged value. 
cons_data[, 'con_GLS'] <- cons_data[, 'con'] - 
  rho*c(NA, cons_data[1:(num_obs - 1), 'con'])

cons_data[, 'dpi_GLS'] <- cons_data[, 'dpi'] - 
  rho*c(NA, cons_data[1:(num_obs - 1), 'dpi'])

cons_data[, 'aaa_GLS'] <- cons_data[, 'aaa'] - 
  rho*c(NA, cons_data[1:(num_obs - 1), 'aaa'])

# The intercept coefficient is not important but it doesn't hurt 
# to change the units to match the original regression. 
cons_data[, 'Intercept'] <- 1 - rho


##################################################
# Estimating a GLS Regression Model
# Model 3: Linear model for Aggregate Consumption
# GLS Regression Adjusted for Serial Correlation
##################################################


# Estimate a regression model.
lm_model_3 <- lm(data = cons_data, 
                 formula = con_GLS ~ Intercept + dpi_GLS + aaa_GLS - 1)
# Note: The '- 1' excludes the intercept, since we have included an adjusted intercept.


# Output the results to screen.
summary(lm_model_3)

# Compare this to the original estimates.
summary(lm_model_1)




##################################################
# Calculate and inspect the GLS residuals
##################################################

# First calculate the predictions.
cons_data[, 'con_hat_GLS'] <- c(NA, predict(lm_model_3))

# Then calculate the residuals, the difference
# between the observed con and the predictions.
cons_data[, 'con_resid_GLS'] <- cons_data[, 'con_GLS'] - cons_data[, 'con_hat_GLS']

# Plot the residuals over time. 
plot(cons_data[, 'year'], cons_data[, 'con_resid_GLS'], 
     main = 'GLS Residuals over Time', 
     xlab = 'Year', 
     ylab = 'Residual')
# Not much remains of the positive serial correlation. 


# Generate a lagged version of the residuals. 
cons_data[, 'con_resid_lag_GLS'] <- c(NA, cons_data[1:(num_obs - 1), 'con_resid_GLS'])
# View the data to verify.

# Plot the residuals to check for a relationship.
plot(cons_data[, 'con_resid_lag_GLS'], cons_data[, 'con_resid_GLS'], 
     main = 'GLS Residuals vs Lagged Residuals', 
     xlab = 'Lagged Residual', 
     ylab = 'Residual')
# Not much trace of the positive serial correlation. 




##################################################
# End
##################################################