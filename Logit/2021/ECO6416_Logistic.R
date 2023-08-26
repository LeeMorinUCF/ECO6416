##################################################
#
# ECO 6416: Applied Business Research Tools
#
# Logistic Regression Demo
# Examples of Model Specification
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
# ECO6416_Logistic gives a examples of logistic regression models
#   and compares with linear model specifications.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))


# Load library for computing the AUROC.
# install.packages('pROC)
library(pROC)

##################################################
# Setting the Parameters
##################################################


# Set path for working directory.
# One option: Put files on desktop in a folder called Data
# wd_path <- 'C:/Users/le279259/Desktop/Data'
# Other option: Put files in a path and set it as your working directory.
wd_path <- 'C:/Users/le279259/Desktop/ECO6416/Logit'
# Set the working directory to this path.
setwd(wd_path)

# Verify that the path was assigned correctly.
getwd()


##################################################
# Loading the Data
##################################################

# A sample dataset to predict employment among women.
female_employment <- read.csv('WOMEN13.csv')

# Inspect the contents.
summary(female_employment)

# Variables include:
# D to indicate whether a woman is in the labor force
# M to indicate that the woman is married
# S is the number of years of schooling
# A is the woman's age




##################################################
# Generating New Variables
##################################################

# Can do this in a spreadsheet or within this program.


##################################################
# Estimating a Linear Regression Model
# Model 1: Linear probability model for female employment
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = female_employment,
                 formula = D ~ M + S)

# Output the results to screen.
summary(lm_model_1)

# Calculate the predictions of this model.
female_employment[, 'D_hat_lm'] <- predict(lm_model_1)

summary(female_employment[, 'D_hat_lm'])
# Does anything look unusual?


##################################################
# Estimating a Logistic Regression Model
# Model 2: Logistic model for female employment
##################################################

# Estimate a logistic regression model.
logit_model_1 <- glm(data = female_employment,
                  formula = D ~ M + S,
                  family = 'binomial')

# Output the results to screen.
summary(logit_model_1)


# Calculate the predictions of this model.
female_employment[, 'D_hat_logit'] <- predict(logit_model_1, type = 'response')

summary(female_employment[, 'D_hat_logit'])
# Does this look better?


##################################################
# Compare quality of prediction with AUROC
# The Area Under the ROC Curve
##################################################

# Calculate the AUROC for the logistic model.
roc(response = female_employment[, 'D'],
    predictor = female_employment[, 'D_hat_logit'])


# Compare this to the estimate for the linear model.
roc(response = female_employment[, 'D'],
    predictor = female_employment[, 'D_hat_lm'])
# Ironic, since it actually provides better ordering in this case.
# However, it provides negative predictions of the probabilities.


# This statistic is best used to compare different models
# with different predictor variables,
# as in the following exercise.


##################################################
#
# Exercise:
# See if you can improve upon this model.
#
#
##################################################



##################################################
# End
##################################################