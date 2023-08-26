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
# ECO6416_Logistic gives an example of a logistic regression models
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

# A sample dataset to predict patients' choice of hospital.
hospital_choice <- read.csv('HOSPITAL13.csv')

# Inspect the contents.
summary(hospital_choice)

# Variables include:
# D to indicate whether a patient chose Cedars Sinai, 0 if they chose UCLA
# DISTANCE is the excess distance from the patients home to Cedars Sinai
#   minus the distance to UCLA
# INCOME is the average income in the zip code in thousands
# OLD is an indicator for whether the patient is older than 75



# Compare the distributions of variables
# for classes of the dependent variable.
summary(hospital_choice[hospital_choice[, 'D'] == 0, ])
summary(hospital_choice[hospital_choice[, 'D'] == 1, ])


##################################################
# Generating New Variables
##################################################

# Can do this in a spreadsheet or within this program.


##################################################
# Estimating a Linear Regression Model
# Model 1: Linear probability model for hospital choice
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = hospital_choice,
                 formula = D ~ DISTANCE + INCOME + OLD)

# Output the results to screen.
summary(lm_model_1)

# Calculate the predictions of this model.
hospital_choice[, 'D_hat_lm'] <- predict(lm_model_1)

summary(hospital_choice[, 'D_hat_lm'])
# Does anything look unusual?


##################################################
# Estimating a Logistic Regression Model
# Model 2: Logistic model for hospital choice
##################################################

# Estimate a logistic regression model.
logit_model_1 <- glm(data = hospital_choice,
                     formula = D ~ DISTANCE + INCOME + OLD,
                     family = 'binomial')

# Output the results to screen.
summary(logit_model_1)


# Calculate the predictions of this model.
hospital_choice[, 'D_hat_logit'] <- predict(logit_model_1, type = 'response')

summary(hospital_choice[, 'D_hat_logit'])
# Does this look better?


##################################################
# Compare quality of prediction with AUROC
# The Area Under the ROC Curve
##################################################

# Calculate the AUROC for the logistic model.
roc(response = hospital_choice[, 'D'],
    predictor = hospital_choice[, 'D_hat_logit'])


# Compare this to the estimate for the linear model.
roc(response = hospital_choice[, 'D'],
    predictor = hospital_choice[, 'D_hat_lm'])


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