##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Regression with Simulated Data
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# September 7, 2021
#
##################################################
#
# ECO6416_OLS_Sim gives an example of OLS regression
#   using simulated data.
#
# Dependencies:
#   ECO6416_tools_2.R
# Note that this is version 2, which is modified
# from the last version.
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



# R uses libraries, which we will use in future sessions.
# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)
# We will use this later in the course.


# Read function for sampling data.
source('ECO6416_tools_2.R')
# This is the same as running the ECO6416_tools_2.R script first.
# It assumes that the script is saved in the same working folder.

# The file ECO6416_tools_2.R must be in the working directory.
# If you an error message, make sure that the file is
# located in your working directory.
# Also make sure that the name has not changed.


##################################################
# Setting the Parameters
##################################################

# Dependent Variable: Property values (in Millions)

beta_0          <-   0.10    # Intercept
beta_income     <-   5.00    # Slope coefficient for income
beta_cali       <-   0.25    # Slope coefficient for California
beta_earthquake <- - 0.50    # Slope coefficient for earthquake
# beta_earthquake <- - 0.00    # Slope coefficient for earthquake

# Distribution of incomes (also in millions).
avg_income <- 0.1
sd_income <- 0.01

# Fraction of dataset in California.
pct_in_cali <- 0.5

# Frequency of earthquakes (only in California).
prob_earthquake <- 0.05

# Additional terms:
sigma_2 <- 0.1      # Variance of error term
num_obs <- 100      # Number of observations in dataset


##################################################
# Generating the Data
##################################################

# Call the housing_sample function from ECO6416_Sim_Data.R.
housing_data <- housing_sample(beta_0, beta_income, beta_cali, beta_earthquake,
                               avg_income, sd_income, pct_in_cali, prob_earthquake,
                               sigma_2, num_obs)


# Summarize the data to inspect for data quality.
summary(housing_data)

# Check that earthquakes occurred only in California:
table(housing_data[, 'in_cali'], housing_data[, 'earthquake'])
# Data errors are the most frequent cause of problems in model-building.



##################################################
# Estimating the Regression Model
##################################################

# Estimate a regression model.
lm_model <- lm(data = housing_data,
               formula = house_price ~ income + in_cali + earthquake)

# Output the results to screen.
summary(lm_model)




##################################################
# Generate another dataset and save it
##################################################

# Call the housing_sample function from ECO6416_Sim_Data.R.
housing_data_2 <- housing_sample(beta_0, beta_income, beta_cali, beta_earthquake,
                               avg_income, sd_income, pct_in_cali, prob_earthquake,
                               sigma_2, num_obs)

# Save this to disk.
write.csv(housing_data_2, file = 'housing_data.csv')



##################################################
# Read the dataset and run another regression
# This is the same process you will follow when
# reading in a dataset obtained from any other source.
##################################################

# Read the newly saved dataset.
housing_data_3 <- read.csv(file = 'housing_data.csv')

# Estimate a regression model.
lm_model_3 <- lm(data = housing_data_3,
               formula = house_price ~ income + in_cali + earthquake)

# Output the results to screen.
summary(lm_model_3)


##################################################
# End
##################################################

