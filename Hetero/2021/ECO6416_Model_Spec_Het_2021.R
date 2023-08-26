##################################################
#
# ECO 6416: Applied Business Research Tools
#
# OLS Regression Demo
# Examples of Model Specification
# to Correct for Heteroskedasticity
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# September 10, 2020
#
##################################################
#
# ECO6416_Model_Spec_Het gives examples of OLS regression models
#   by considering a number of different model specifications
#   to correct for heteroskedasticity.
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
# setwd('~/path/to/your/folder')
# Make sure to spell it correctly and use forward slashes.
# Make sure to have both open and closed quotes.
# Both single and double quotes are appropriate,
# as long as they are the same on both sides.
# For example:
setwd("C:/Users/le279259/OneDrive - University of Central Florida/Desktop/ECO6416_Demos")
# Note that my folder is different because it depends on where I store my files.

# If an error message is displayed, verify the path and run setwd() again.

# Verify that it changed the path correctly.
getwd()


##################################################
# Loading the Data
##################################################

tractor_sales <- read.csv('TRACTOR7.csv')

# Inspect the contents.
summary(tractor_sales)
# Make sure there are no problems with the data.


##################################################
# Generating New Variables
##################################################

# You can create new variables in two ways:
# 1. Add commands within this program
#     (as for log_saleprice and squared_horsepower below).
# 2. Create new columns in a spreadsheet.
#   (but you would need to re-load the dataset
#   after adding variables this way)

hist(tractor_sales[, 'saleprice'])
# Notice that there are some very large values.
# Consider taking logs to bring outliers closer to the others.

tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])

# Now plot the histogram for log of saleprice:
hist(tractor_sales[, 'log_saleprice'])
# Much better behaved. Looks almost normal.

# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2


##################################################
# Estimating a Regression Model
# Model 1: Linear model for dollar sale price
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = tractor_sales,
                 formula = saleprice ~ horsepower + age + enghours +
                   diesel + fwd + manual + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_1)



##################################################
# Calculate and inspect the residuals
##################################################

# First calculate the predictions.
tractor_sales[, 'saleprice_hat'] <- predict(lm_model_1)

# Then calculate the residuals, the difference
# between the observed con and the predictions.
tractor_sales[, 'saleprice_resid'] <- tractor_sales[, 'saleprice'] - tractor_sales[, 'saleprice_hat']

# Plot the residuals over Predicted Price.
plot(tractor_sales[, 'saleprice_hat'],
     tractor_sales[, 'saleprice_resid'],
     main = 'OLS Residuals vs Predicted Price',
     xlab = 'Predicted Price',
     ylab = 'Residual')
# Residuals have a strong pattern.



##################################################
# Estimating a Regression Model
# Model 2: Linear model for log of dollar sale price
##################################################

# Estimate a regression model.
lm_model_2 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + manual + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_2)



##################################################
# Calculate and inspect the residuals
##################################################

# First calculate the predictions.
tractor_sales[, 'log_saleprice_hat'] <- predict(lm_model_2)

# Then calculate the residuals, the difference
# between the observed con and the predictions.
tractor_sales[, 'log_saleprice_resid'] <- tractor_sales[, 'log_saleprice'] - tractor_sales[, 'log_saleprice_hat']

# Plot the residuals over Predicted Price.
plot(tractor_sales[, 'log_saleprice_hat'],
     tractor_sales[, 'log_saleprice_resid'],
     main = 'OLS Residuals vs Log of Predicted Price',
     xlab = 'Log of Predicted Price',
     ylab = 'Residual')
# Residuals are negative for the highest predictions.

# Plot the residuals over horsepower.
plot(tractor_sales[, 'horsepower'],
     tractor_sales[, 'log_saleprice_resid'],
     main = 'OLS Residuals vs Horsepower',
     xlab = 'Horsepower',
     ylab = 'Residual')
# Residuals are increasing for low values of horsepower
# and negative with high horsepower.


##################################################
#
# Estimating a Regression Model
# Model 3: Linear model for log of dollar sale price
#     Polynomial Functional Form for Horsepower
#
# Consider a polynomial functional form for horsepower.
# Idea: Horsepower improves performance up to a limit,
# then extra power does not add value, only consumes more fuel.
#
# 1. Generate the squared variable.
# 2. Hypothesize the signs.
# 3. Add the squared horsepower term to the regression equation.
# 4. Estimate the revised model.
# 5. Analyze the resulting estimates.
# 6. Make recommendation for the new model.
#
##################################################


# Estimate a regression model.
lm_model_3 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_3)



##################################################
# Calculate and inspect the residuals
##################################################

# First calculate the predictions.
tractor_sales[, 'log_saleprice_hat_2'] <- predict(lm_model_3)

# Then calculate the residuals, the difference
# between the observed con and the predictions.
tractor_sales[, 'log_saleprice_resid_2'] <- tractor_sales[, 'log_saleprice'] - tractor_sales[, 'log_saleprice_hat_2']

# Plot the residuals over Predicted Price.
plot(tractor_sales[, 'log_saleprice_hat_2'],
     tractor_sales[, 'log_saleprice_resid_2'],
     main = 'OLS Residuals vs Log of Predicted Price',
     xlab = 'Log of Predicted Price',
     ylab = 'Residual')
# Residuals look homoskedastic.


# Plot the residuals over horsepower.
plot(tractor_sales[, 'horsepower'],
     tractor_sales[, 'log_saleprice_resid_2'],
     main = 'OLS Residuals vs Horsepower',
     xlab = 'Horsepower',
     ylab = 'Residual')

# It seems that the change in functional form also
# improved the distribution of the error terms.



##################################################
# End
##################################################
