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
# September 10, 2020
#
##################################################
#
# ECO6416_SAT_Multi gives an example of a set of
# regression models with multicollinearity.
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

# A sample dataset to predict SAT scores.
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


##################################################
# Calculate variance inflation factors
##################################################

# Select from the variables in the dataset.
print(colnames(sat))


# Define the list of variables to calculate VIFs.
list_of_variables <- c("AP", "APENG", "APMATH",
                       "GPA", "PREP",
                       "ESL", "GEND", "RACE")
# You can modify this to suit your own list of variables
# as follows:
# list_of_variables <- c('my_variable_1', 'my_variable_2', 'my_variable_3')
# Also, replace housing_data with the name of your dataset below,
# in the line
# lm_model_vif <- lm(data = my_dataset_name, ...)




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
  lm_model_vif <- lm(data = sat, # Replace with name of your dataset.
                     formula = vif_formula)

  print(summary(lm_model_vif))

  # Store the R^2.
  vif_results[variable_name, 'r_squared'] <- summary(lm_model_vif)$r.squared

}

# Calculate the VIF from the R^2 statistics.
vif_results[, 'VIF'] <- 1/(1 - vif_results[, 'r_squared'])

# The resulting R^2 values are:
print(vif_results)


# Some of the VIFs are very high values that indicate that
# multicollinearity will be a problem in this dataset.

##################################################
# Inspect the data to analyze the reasons for multicollinearity.
##################################################


# RACE and ESL had high and similar VIF patterns.
# Compare the relationship between RACE and ESL.
table(sat[, 'RACE'], sat[, 'ESL'])
# Very high correlation.

# Check the correlation of measures of academic success.
cor(sat[, c("AP", "APENG", "APMATH",
            "GPA", "PREP")])
# Pairwise correlation is somewhat high but
# misses the combined effect of all variables together.


# On the other extreme,
# compare the distributions of variables
# for classes of the GEND indicator.
summary(sat[sat[, 'GEND'] == 0, ])
summary(sat[sat[, 'GEND'] == 1, ])
# No major differences as VIF was low for GEND.


##################################################
# Generating New Variables
##################################################

# Can do this in a spreadsheet or within this program.


##################################################
# Estimating a Linear Regression Model
# Model 1: Linear regression for SAT scores
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = sat,
                 # formula = SAT ~ GPA + GEND + ESL + RACE
                 # formula = SAT ~ AP + GEND + ESL + RACE
                 # formula = SAT ~ APENG + GEND + ESL + RACE
                 # formula = SAT ~ APMATH + GEND + ESL + RACE
                 # formula = SAT ~ APMATH + GEND + RACE
                 formula = SAT ~ APMATH + GEND + ESL
)

# Output the results to screen.
summary(lm_model_1)


##################################################
# Estimating a Linear Regression Model
# Model 2: Linear regression for SAT scores
# Include more variables for better measurement of
# scholastic aptitude.
##################################################

# Estimate a regression model.
lm_model_2 <- lm(data = sat,
                 formula = SAT ~ GPA + AP + GEND + ESL + RACE
                 # formula = SAT ~ GPA + AP + GEND + ESL
                 # formula = SAT ~ GPA + AP + GEND + RACE
)

# Output the results to screen.
summary(lm_model_2)

# Calculate the predictions of this model.
sat[, 'SAT_hat_lm'] <- predict(lm_model_2)

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
