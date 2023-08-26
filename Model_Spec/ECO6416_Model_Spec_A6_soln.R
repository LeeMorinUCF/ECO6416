##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Examples of Model Specification
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# August 26, 2023
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


# The csv file used below must be in the working directory.
# If you an error message, make sure that the file is
# located in your working directory.
# Also make sure that the name has not changed.



##################################################
# Loading the Data
##################################################

tractor_sales <- read.csv('TRACTOR_A6.csv')

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
                   diesel + fwd + manual + cab + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_1)


##################################################
# Estimating a Regression Model
# Model 2: Linear model for log of dollar sale price
##################################################

# Estimate a regression model.
lm_model_2 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + 
                   age + enghours +
                   diesel + fwd + manual + cab + johndeere +
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
                   diesel + fwd + manual + cab + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_3)





##################################################
# Estimating a Regression Model
# Model 4: Linear model for log of dollar sale price
# Omit seasonal indicators
##################################################

# Estimate a regression model.
lm_model_4 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + manual + cab + johndeere)

# Output the results to screen.
summary(lm_model_4)

##################################################
#
# Exercise: Test exclusion of seasonal indicators
#   An example of joint hypothesis testing.
#
# The unconstrained RSS is calculated from the model
# that includes seasonal indicators:
RSS_unconstrained <- sum(lm_model_3$residuals^2)
print(RSS_unconstrained)
#
# The constrained RSS is calculated from the model
# that excludes seasonal indicators:
RSS_constrained <- sum(lm_model_4$residuals^2)
print(RSS_constrained)
#
# Follow the approach for conducting the F-test on p. 143.
# Do used tractor prices follow a seasonal pattern?
#
##################################################

# Need sample size and number of variables.

num_obs <- nrow(tractor_sales)
num_vars <- 13 # All parameters, including the constant and seasonal indicators.
num_restr <- 3 # Three restrictions for seasonal indicators.


F_stat <- (RSS_constrained - RSS_unconstrained)/num_restr /
  RSS_unconstrained*(num_obs - num_vars - 1)
print(F_stat)


# You can look in the tables in the textbook
# but R can calculate critical values.
F_critical_1 <- qf(p = 0.01,
                   df1 = num_restr, df2 = (num_obs - num_vars - 1),
                   lower.tail = FALSE)
F_critical_5 <- qf(p = 0.05,
                   df1 = num_restr, df2 = (num_obs - num_vars - 1),
                   lower.tail = FALSE)
F_critical_10 <- qf(p = 0.10,
                    df1 = num_restr, df2 = (num_obs - num_vars - 1),
                    lower.tail = FALSE)

print("Critical value of F-statistic:")
print("at the 1% level")
print(F_critical_1)
print("at the 5% level")
print(F_critical_5)
print("at the 10% level")
print(F_critical_10)


# This places the F-statistic above the critical values for the
# 1 percent level of significance.
# Conclude that the seasonal indicators should be included in the model.



##################################################
# Estimating a Regression Model
# Model 5: Linear model for log of dollar sale price
# Interact Slope Indicator for johndeere with Engine Hours
##################################################

# Estimate a regression model.
lm_model_5 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours + johndeere*enghours + # Note the added term.
                   diesel + fwd + manual + cab + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_5)

# Does an additional hour of use affect the price of a John Deere tractor
# differently than tractors of other brands?


##################################################
# End
##################################################