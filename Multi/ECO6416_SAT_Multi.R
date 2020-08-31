##################################################
# 
# ECO 6416: Applied Business Research Tools
# 
# Regression Demo
# Examples of Multicollinearity
# 
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
# 
# November 17, 2019
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


##################################################
# Setting the Parameters
##################################################


# Set path for working directory.
# One option: Put files on desktop in a folder called Data
wd_path <- 'C:/Users/le279259/Desktop/ECO6416'
# Other option: Put files in a path and set it as your working directory. 
# wd_path <- 'C:/Users/le279259/Documents/Teaching/ECO6416_Fall2018/Data'
# Set the working directory to this path. 
setwd(wd_path)

# Verify that the path was assigned correctly. 
getwd()


##################################################
# Loading the Data
##################################################

# A sample dataset to predict employment among women. 
sat <- read.csv('SAT8.csv')


# Inspect the contents.
summary(sat)

# Variables include:
# AP to indicate whether a student has taken an AP course
# APENG to indicate whether a student has taken an AP English course
# APMATH to indicate whether a student has taken an AP Math course
# ESL to indicate whether English is not the student's first language
# GEND is a male dummy variable
# GPA is the student's GPA
# PREP to indicate whether a student has taken an SAT prep course
# RACE to indicate whether a student is Asian
# SAT is the student's SAT score


# Compare the distributions of variables 
# for classes of the GEND indicator. 
summary(sat[sat[, 'GEND'] == 0, ])
summary(sat[sat[, 'GEND'] == 1, ])

# Compare the relationship between RACE and ESL.
table(sat[, 'RACE'], sat[, 'ESL'])



##################################################
# Generating New Variables
##################################################

# Can do this in a spreadsheet or within this program. 


##################################################
# Estimating a Linear Regression Model
# Model 1: Linear probability model for female employment
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = sat, 
                 formula = SAT ~ GPA + AP + GEND + ESL + RACE
                 # formula = SAT ~ GPA + AP + GEND + ESL
                 # formula = SAT ~ GPA + AP + GEND + RACE
                 )

# Output the results to screen.
summary(lm_model_1)

# Calculate the predictions of this model.
sat[, 'SAT_hat_lm'] <- predict(lm_model_1)

summary(sat[, 'SAT_hat_lm'])


##################################################
# Look for a pattern in the residuals
##################################################


# Calculate the residuals.
sat[, 'SAT_res_lm'] <- sat[, 'SAT'] - sat[, 'SAT_hat_lm']

# Compare the residuals by gender.
summary(sat[sat[, 'GEND'] == 0, 'SAT_res_lm'])
summary(sat[sat[, 'GEND'] == 1, 'SAT_res_lm'])

# Compare the residuals by race.
summary(sat[sat[, 'RACE'] == 0, 'SAT_res_lm'])
summary(sat[sat[, 'RACE'] == 1, 'SAT_res_lm'])


##################################################
# End
##################################################