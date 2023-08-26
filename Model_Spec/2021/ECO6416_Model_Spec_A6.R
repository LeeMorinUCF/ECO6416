##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Examples of Model Specfication
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# September 9, 2020
#
##################################################
#
# ECO6416_Model_Spec_A6 gives examples of OLS regression models
#   by considering a number of different model specifications.
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


##################################################
# Setting the Parameters
##################################################


# Set path for working directory.
# Put files on desktop in a folder called ECO6416
# wd_path <- 'C:/Users/le279259/Desktop/ECO6416/Model_Spec'
# Modify the above line according to the specific path on your computer,
# as in:
# wd_path <- 'C:/Users/name/of/your/path'

# Set the working directory to this path.
# setwd(wd_path)

# Or set the working directory in one command:
setwd("C:/Users/le279259/Desktop/ECO6416/Model_Spec")

# Verify that the path was assigned correctly.
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
# Estimating a Regression Model
# Model 4: Linear model for log of dollar sale price
# Add omitted variable: enclosed cab indicator
##################################################


# Estimate the regression model.
lm_model_4 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_4)


##################################################
# Estimating a Regression Model
# Model 5: Linear model for log of dollar sale price
# Omit seasonal indicators
##################################################

# Estimate a regression model.
lm_model_5 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_model_5)

##################################################
#
# Exercise: Test exclusion of seasonal indicators
#   An example of joint hypothesis testing.
#
# The unconstrained RSS is calculated from the model
# that includes seasonal indicators:
RSS_unconstrained <- sum(lm_model_4$residuals^2)
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that excludes seasonal indicators:
RSS_constrained <- sum(lm_model_5$residuals^2)
print(RSS_constrained)
#
# Follow the approach for conducting the F-test on p. 143.
# Do used tractor prices follow a seasonal pattern?
#
##################################################





##################################################
# Estimating a Regression Model
# Model 6: Linear model for log of dollar sale price
# Interact Slope Indicator for Diesel with Engine Hours
##################################################

# Estimate a regression model.
lm_model_6 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours + diesel*enghours + # Note the added term.
                   diesel + fwd + manual + johndeere + cab)

# Output the results to screen.
summary(lm_model_6)

# Does an additional hour of use affect a diesel-powered tractor
# differently than a gasoline-powered tractor?


##################################################
# End
##################################################