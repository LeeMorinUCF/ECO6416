##################################################
#
# ECO 6416: Applied Business Research Tools
#
# OLS Regression Demo
# Detecting and Correcting for Heteroskedasticity
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# November 19, 2018
#
##################################################
#
# ECO6416_Heteroskedasticity gives an example of an OLS regression model
#   with heteroskedasticity and corrects it with GLS estimation
#   and by calculating White standard errors.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Install required libraries.
# Will have a command like the following.
# library(name_of_R_package)
# After installing the package before the first time:
# install.packages('name_of_R_package')


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

# Variable definitions in PLANES10.csv:
# lnprice: The log of the price of an airplane
# lncieling: The log of the maximum flying height of an airplane, in feet
# lncruise: The log of the cruising speed, in MPH
# lnhorse: The log of the horsepower of the engine
# fixgear: An indicator for fixed landing gear (i.e. not retractable)
# lnfuel: The log of the volume of the fuel tank, in gallons
# pass: The number of passengers an airplane can accommodate
# tdrag: An indicator that a wheel is on the tail (a tail-dragger)
# wtop: An indicator that the wings are above the fuselage
# lnage: The log of the age of the aircraft, in years

plane_data <- read.csv('PLANES10.csv')
# plane_data <- read.csv("C:/Users/le279259/Desktop/ECO6416/PLANES10.csv")

# Inspect the contents.
summary(plane_data)
# Make sure there are no problems with the data.

# Inspect the dependent variable.
hist(plane_data[, 'lnprice'])

# Age is already transformed into log form.
# Transform back to age to view the
plane_data[, 'age'] <- exp(plane_data[, 'lnage'])
plane_data[, 'price'] <- exp(plane_data[, 'lnprice'])

# Check the
hist(plane_data[, 'price'])

# Inspect the correlations between numeric explanatory variables.
list_of_variables <- c('lnceiling', 'lncruise',
                         'lnhorse', 'fixgear', 'lnfuel',
                         'pass', 'tdrag', 'wtop', 'lnage')
cor(plane_data[, list_of_variables])
# Be aware of any explanatory variables that are highly correlated
# (both positively and negatively) with each other.


# Need the number of observations and variables:
num_obs <- nrow(plane_data)
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
# Model 1: Linear model for the Log of Airplane Prices
# Start with a full model that includes all variables.
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = plane_data,
                 formula = lnprice ~ lnceiling + lncruise +
                   lnhorse + fixgear + lnfuel +
                   pass + tdrag + wtop + lnage)

# Output the results to screen.
summary(lm_model_1)




##################################################
# Calculate and inspect the residuals
##################################################

# First calculate the predictions.
plane_data[, 'lnprice_hat'] <- predict(lm_model_1)
plane_data[, 'price_hat'] <- exp(plane_data[, 'lnprice_hat'])

summary(plane_data[, c('price', 'price_hat')])

# Then calculate the residuals, the difference
# between the observed con and the predictions.
plane_data[, 'lnprice_resid'] <- plane_data[, 'lnprice'] - plane_data[, 'lnprice_hat']

# Plot the residuals over passenger capacity.
plot(plane_data[, 'pass'], plane_data[, 'lnprice_resid'],
     main = 'OLS Residuals vs Passenger Capacity',
     xlab = 'Number of Passengers',
     ylab = 'Residual')
# Looks like slightly more variation with higher capacity.


# Plot the residuals against cruising speed.
plot(plane_data[, 'lncruise'], plane_data[, 'lnprice_resid'],
     main = 'OLS Residuals vs Cruising Speed',
     xlab = 'Log of Cruising Speed',
     ylab = 'Residual')
# Looks like slightly more variation with higher cruising speed.



##################################################
# Estimating a Regression Model for the Squared Errors
# Use a full model that includes all variables.
##################################################

# First generate a variable with squared residuals.
plane_data[, 'lnprice_squared_resid'] <- plane_data[, 'lnprice_resid']^2

# Estimate a regression model.
het_model_1 <- lm(data = plane_data,
                  formula = lnprice_squared_resid ~
                    lnceiling + lncruise +
                    lnhorse + fixgear + lnfuel +
                    pass + tdrag + wtop + lnage)

# Output the results to screen.
summary(het_model_1)




##################################################
# Double Check:
# Calculate the White Standard Errors
##################################################

# Standard erros are the square root of the
# White Heteroskedasticity-Corrected Covariance Matrix.
# Download and install this the first time it is used.
# install.packages('sandwich')
library(sandwich)

White_se <- sqrt(diag(vcovHC(lm_model_1)))


# Compare these to the original SEs from the OLS regression.
summary(lm_model_1)



# Need to recalculate the t-statistics and p-values.
lm_stats_White <- coef(summary(lm_model_1))
# Replace the SEs.
lm_stats_White[, 'Std. Error'] <- White_se
# Recalculate the t-stats.
lm_stats_White[, 't value'] <- lm_stats_White[, 'Estimate'] / lm_stats_White[, 'Std. Error']
# Recalculate the p-values.
lm_stats_White[, 'Pr(>|t|)'] <- 2*pt(- abs(lm_stats_White[, 't value']),
                                  df = num_obs - num_vars - 1)

# Display the adjusted regression statistics.
print(lm_stats_White)
# Compare to the originals.
coef(summary(lm_model_1))


# Some of the standard errors and t-statistics are now different.
# Does it change any of the results?


##################################################
# End
##################################################

