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
# ECO5445_Model_Spec gives examples of OLS regression models
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
wd_path <- 'C:/Users/le279259/Desktop/ECO6416_Demos/Module02'
# Modify the above line according to the specific path on your computer,
# as in:
# wd_path <- 'C:/Users/name/of/your/path'

# Set the working directory to this path.
setwd(wd_path)

# Or just run the same command that appears after
# Clicking "More > Set as Working Directory".
setwd("C:/Users/le279259/Desktop/ECO6416_Demos/Module02")

# Verify that the path was assigned correctly.
getwd()


##################################################
# Loading the Data and Conducting Initial Assessment
##################################################

life_exp_data <- read.csv('LIFE5.csv')

# Inspect the contents.
summary(life_exp_data)
# Make sure there are no problems with the data.

# Inspect the dependent variable.
hist(life_exp_data[, 'lifeexpect'])

# Inspect the correlations between numeric explanatory variables.
cor(life_exp_data[, c('medinc', 'uninsured', 'smoke',
                      'obesity', 'teenbirth', 'gunlaw', 'metro')])
# Be aware of any explanatory variables that are highly correlated
# (both positively and negatively) with each other.

##################################################
# Generating New Variables
##################################################

# For your Research Report:
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
                 formula = lifeexpect ~
                   medinc + uninsured +
                   smoke + obesity + teenbirth +
                   gunlaw + metro)
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


##################################################
# End
##################################################