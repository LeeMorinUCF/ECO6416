##################################################
#
# ECO 6416: Applied Business Research Tools
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
# September 10, 2020
#
##################################################
#
# ECO6416_Life_Exp_a5_w_VIF gives examples of OLS regression models
#   by considering a number of different model specifications.
# This version also considers multicollinearity
# by calculating variance inflation factors.
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
# Loading the Data and Conducting Initial Assessment
##################################################

life_exp_data <- read.csv('LIFE5.csv')
# life_exp_data <- read.csv("C:/Users/le279259/Desktop/ECO6416_Demos/Data/LIFE5.csv")

# Inspect the contents.
summary(life_exp_data)
# Make sure there are no problems with the data.

# Inspect the dependent variable.
hist(life_exp_data[, 'lifeexpect'])

# Inspect the correlations between numeric explanatory variables.
list_of_variables <- c('medinc', 'uninsured', 'smoke',
                       'obesity', 'teenbirth', 'gunlaw', 'metro')
cor(life_exp_data[, list_of_variables])
# Be aware of any explanatory variables that are highly correlated
# (both positively and negatively) with each other.


##################################################
# Estimating Variance Inflation Factors
##################################################

# Define a data frame for storing the R^2 and VIF statistics.
vif_results <- data.frame(r_squared = numeric(length(list_of_variables)))
rownames(vif_results) <- list_of_variables

# Estimate regression models in a loop in explanatory variables.
for (variable_name in list_of_variables) {


  # Specify the formula for the VIF regression.
  other_variables <- list_of_variables[list_of_variables != variable_name]

  vif_formula <- as.formula(paste0(variable_name, ' ~ ',
                                   paste(other_variables, collapse = ' + ')))

  # Print out the regression results.
  print('Now estimating the VIF regression: ')
  print(vif_formula)

  # Estimate the regression of this variable on all the others.
  lm_model_vif <- lm(data = life_exp_data,
                   formula = vif_formula)

  print(summary(lm_model_vif))

  # Store the R^2.
  vif_results[variable_name, 'r_squared'] <- summary(lm_model_vif)$r.squared

}

# Calculate the VIF from the R^2 statistics.
vif_results[, 'VIF'] <- 1/(1 - vif_results[, 'r_squared'])

# The resulting R^2 values are:
print(vif_results)


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
# Model 1: Linear model for life expectancy
# Start with a full model that includes all variables.
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = life_exp_data,
                 formula = lifeexpect ~ medinc + uninsured +
                   smoke + obesity + teenbirth + gunlaw + metro)

# Output the results to screen.
summary(lm_model_1)



##################################################
# Estimating a Regression Model
# Model 2: Linear model for life expectancy
# Trimming down the model by omitting variables.
##################################################

# Estimate a regression model.
lm_model_2 <- lm(data = life_exp_data,
                   # Remove some variables from here:
                   formula = lifeexpect ~ medinc +
                   smoke + obesity + teenbirth + gunlaw + metro)
# Note that the '+' signs allow for the formula to continue
# to the next line.

# Output the results to screen.
summary(lm_model_2)

# Be sure to consider the 4 important specification criteria (p. 166)
# for each variable under consideration.


##################################################
# Estimating a Regression Model
# Model 3: Linear model for life expectancy
#
##################################################

# Consider further exclusions before arriving at
# the simplest model.


# Estimate a regression model.
lm_model_3 <- lm(data = life_exp_data,
                 # Remove some variables from here:
                 formula = lifeexpect ~ # medinc +
                   smoke + obesity + teenbirth + gunlaw + metro)


# Output the results to screen.
summary(lm_model_3)


##################################################
# End
##################################################
