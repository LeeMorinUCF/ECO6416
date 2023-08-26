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
# ECO6416_Life_Exp is an example of variable selection
#   in OLS regression models.
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