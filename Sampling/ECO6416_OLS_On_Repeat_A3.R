##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Simulation with repeated estimation
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# August 25, 2023
#
##################################################
#
# ECO6416_OLS_On_Repeat gives an example of OLS regression
#   using simulated data.
#   It repeats the estimation several times to get a
#   distribution of estimates.
#
# Dependencies:
#   ECO6416_tools.R
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


# Read function for sampling data.
source('ECO6416_tools.R')
# This is the same as running the ECO6416_tools.R script first.
# It assumes that the script is saved in the same working folder.

# The file ECO6416_tools.R must be in the working directory.
# If you an error message, make sure that the file is
# located in your working directory.
# Also make sure that the name has not changed.


##################################################
# Setting the Parameters
##################################################

# Dependent Variable: Automobile values

beta_0          <-   50000     # Intercept
beta_mileage    <- -  0.20     # Slope coefficient for mileage
beta_accident   <- -  5000     # Slope coefficient for accident
beta_damage     <- - 20000     # Slope coefficient for damage
# beta_damage     <-       0   # Alternate Slope coefficient for damage

# Distribution of mileage.
avg_mileage <- 50000
sd_mileage  <- 10000

# Extra parameter for measurement error in mileage.
measurement_error_mileage <- 10000

# Fraction of dataset in an accident.
pct_accident <- 0.4

# Frequency of damages (only after an accident).
prob_damage <- 0.10

# Additional terms:
sigma_2 <- 4000    # Variance of error term
num_obs <- 100      # Number of observations in dataset

# Set the number of replications in the simulation.
num_replications <- 1000


##################################################
# Generating the Fixed Data
##################################################

# Call the other_sample() function from ECO6416_tools_2.R.
car_data <- other_sample(beta_0, beta_mileage, beta_accident, beta_damage,
                         avg_mileage, sd_mileage, pct_accident, prob_damage,
                         sigma_2, num_obs)


# Summarize the data to inspect for data quality.
summary(car_data)

# Check that damages occurred only in accident:
table(car_data[, 'accident'], car_data[, 'damage'])
# Data errors are the most frequent cause of problems in model-building.

# Run it again if no damages occurred.


##################################################
# Generating Additional Data
# The extra data that is not in the model
##################################################

#--------------------------------------------------
# Assume that true mileage is not observed but some variables
# that are correlated with mileage are available.
#--------------------------------------------------

# mileage measure 1.
car_data[, 'mileage_1'] <- car_data[, 'mileage'] +
  rnorm(n = num_obs, mean = 0, sd = measurement_error_mileage)

# mileage measure 2.
car_data[, 'mileage_2'] <- car_data[, 'mileage'] +
  rnorm(n = num_obs, mean = 0, sd = measurement_error_mileage)


##################################################
# Running a Simulation
# Estimating Again and Again
##################################################

# Set the list of variables for the estimation.
list_of_variables <- c('mileage', 'accident', 'damage')
# list_of_variables <- c('mileage_1', 'accident', 'damage')

# Add beta_0 to the beginning for the full list.
full_list_of_variables <- c('intercept', list_of_variables)

# Create an empty data frame to store the results.
reg_results <- data.frame(reg_num = 1:num_replications)
reg_results[, full_list_of_variables] <- 0
reg_results[, c('mileage', 'mileage_1', 'mileage_2')] <- 0


# Generate repeated realizations of the car_data dataset.
for (reg_num in 1:num_replications) {

  # Print a progress report.
  # print(sprintf('Now estimating model number %d.', reg_num))

  ##################################################
  # Generating the Random Data
  ##################################################

  # Repeat again and again, replacing only the epsilon values.

  # Generate the error term, which includes everything we do not observe.
  car_data[, 'epsilon'] <- rnorm(n = num_obs, mean = 0, sd = sigma_2)

  # Finally, recalculate the simulated value of house prices,
  # according to the regression equation.
  car_data[, 'car_price'] <-
    beta_0 +
    beta_mileage * car_data[, 'mileage'] +
    beta_accident * car_data[, 'accident'] +
    beta_damage * car_data[, 'damage'] +
    car_data[, 'epsilon']
  # Each time, this replaces the car_price with a different version
  # of the error term.


  ##################################################
  # Estimating the Regression Model
  ##################################################

  # Specify the formula to estimate.
  lm_formula <- as.formula(paste('car_price ~ ',
                                 paste(list_of_variables, collapse = ' + ')))

  # Estimate a regression model.
  lm_full_model <- lm(data = car_data,
                      formula = lm_formula)
  # Note that the normal format is:
  # model_name <- lm(data = name_of_dataset, formula = Y ~ X_1 + x_2 + x_K)
  # but the above is a shortcut for a pre-set list_of_variables.

  ##################################################
  # Saving the Results
  ##################################################

  # Save the estimates in the row for this particular estimation.
  reg_results[reg_num, full_list_of_variables] <- coef(lm_full_model)

}


##################################################
# Analyzing the Results
##################################################

#--------------------------------------------------
# Display some graphs
# Click the arrows in the bottom right pane to
# switch between previous figures.
#--------------------------------------------------

# Plot a histogram for each estimate.
# Note that some will be empty if they were not included in the estimation.

hist(reg_results[, 'intercept'],
     main = 'Distribution of beta_0',
     xlab = 'Estimated Coefficient',
     ylab = 'Frequency',
     breaks = 20)

# This will be blank if mileage is not in the regression:
hist(reg_results[, 'mileage'],
     main = 'Distribution of beta_mileage',
     xlab = 'Estimated Coefficient',
     ylab = 'Frequency',
     breaks = 20)

# This will be blank if mileage_1 is not in the regression:
hist(reg_results[, 'mileage_1'],
     main = 'Distribution of beta_mileage_1',
     xlab = 'Estimated Coefficient',
     ylab = 'Frequency',
     breaks = 20)

# This will be blank if mileage_2 is not in the regression:
hist(reg_results[, 'mileage_2'],
     main = 'Distribution of beta_mileage_2',
     xlab = 'Estimated Coefficient',
     ylab = 'Frequency',
     breaks = 20)

hist(reg_results[, 'accident'],
     main = 'Distribution of beta_accident',
     xlab = 'Estimated Coefficient',
     ylab = 'Frequency',
     breaks = 20)

hist(reg_results[, 'damage'],
     main = 'Distribution of beta_damage',
     xlab = 'Estimated Coefficient',
     ylab = 'Frequency',
     breaks = 20)

#--------------------------------------------------
# Output some statistics to screen
#--------------------------------------------------

# Display some statistics for the result.
summary(reg_results[, full_list_of_variables])

# Calculate the average estimates separately.
print('Average value of the coefficients are:')
sapply(reg_results[, full_list_of_variables], mean)

# Calculate the standard deviation of the estimates.
print('Standard Deviations of the coefficients are:')
sapply(reg_results[, full_list_of_variables], sd)



##################################################
# End
##################################################
