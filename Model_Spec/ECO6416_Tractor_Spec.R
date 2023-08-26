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
# ECO6416_Tractor_Spec gives examples of OLS regression models
#   by considering a number of different model specifications
#   on a dataset of prices from the sale of used tractors.
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


summary(tractor_sales[, 'saleprice'])

summary(tractor_sales[tractor_sales[, 'johndeere'] == 1, 'saleprice'])
summary(tractor_sales[tractor_sales[, 'johndeere'] == 0, 'saleprice'])


##################################################
# Estimating a Regression Model
# Model 3: Linear model for log of dollar sale price
# Omit seasonal indicators
##################################################

# Estimate a regression model.
lm_model_3 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + manual + johndeere)

# Output the results to screen.
summary(lm_model_3)


##################################################
# Estimating a Regression Model
# Model 3: Linear model for log of dollar sale price
# Omit seasonal indicators and transmission type
##################################################

# Estimate a regression model.
lm_model_4 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + age + enghours +
                   diesel + fwd + johndeere)

# Output the results to screen.
summary(lm_model_4)


##################################################
#
# Exercise:
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
lm_model_5 <- lm(data = tractor_sales,
                 formula = log_saleprice ~ horsepower + squared_horsepower +
                   age + enghours +
                   diesel + fwd + johndeere)

# Output the results to screen.
summary(lm_model_5)


##################################################
# End
##################################################