##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# Data Mining Demo
# Data Mining over many Irrelevant Variables
# Includes variables with measurement error
# and two highly correlated substitutes.
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
# ECO6416_Data_Mining_A5 gives an example of a simple form
#   of data mining with OLS regression using simulated data.
#   It estimates a model that illustrates the effects of
#   measurement error, correlated variables and irrelevant variables.
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

# Distribution of mileage.
avg_mileage <- 50000
sd_mileage  <- 10000

# Extra parameter for measurement error in mileage.
number_of_mileage_variables <- 2
measurement_error_mileage <- 5000

# Fraction of dataset in an accident.
pct_accident <- 0.4

# Frequency of damages (only after an accident).
prob_damage <- 0.20 # More damage to illustrate the difference.

# Frequency of rainfall (can happen anywhere).
prob_rainfall <- 0.25

# Number of additional (irrelevant) rainfall variables to add to dataset.
number_of_rainfall_variables <- 20

# Additional terms:
sigma_2 <- 4000    # Variance of error term
num_obs <- 200        # Number of observations in entire dataset
num_obs_estn <- 100   # Number of observations for estimation.
# Notice num_obs is twice as large, saving half for out-of-sample testing.

# Select the variables for estimation at random.
obsns_for_estimation <- runif(num_obs) < num_obs_estn/num_obs
# Test how many are in each sample.
table(obsns_for_estimation)



##################################################
# Generating the Data
# The relevant data in the model
##################################################

# Call the other_sample function from ECO6416_tools_3.R.
car_data <- other_sample(beta_0, beta_mileage, beta_accident, beta_damage,
                               avg_mileage, sd_mileage, pct_accident, prob_damage,
                               sigma_2, num_obs,
                               number_of_mileage_variables, measurement_error_mileage,
                               number_of_rainfall_variables, prob_rainfall)

# Summarize the data.
summary(car_data)

# Check that damages occurred only in accidents:
table(car_data[, 'accident'], car_data[, 'damage'])
# Data errors are the largest cause of problems in model-building.

# Check the subsamples for estimation and testing.
# Estimation sample:
table(car_data[obsns_for_estimation, 'accident'],
      car_data[obsns_for_estimation, 'damage'])
# Testing sample:
table(car_data[!obsns_for_estimation, 'accident'],
      car_data[!obsns_for_estimation, 'damage'])
# ! means 'not'.
# So, !obsns_for_estimation means to include only the
# observations left out for testing the model.

# Run the script again if there are not damages
# in both samples.


##################################################
# Generating Additional Data
# The extra data that is not in the model
##################################################

#--------------------------------------------------
# Assume that true mileage is not observed but some variables
# that are correlated with mileage are available.
#--------------------------------------------------

mileage_variable_list <- sprintf('mileage_%d', seq(1:number_of_mileage_variables))
# These variables are created in the ECO6416_tools_3.R script.


# Check how strongly the data are correlated.
cor(car_data[, c('mileage', 'mileage_1', 'mileage_2')])

correl_mileage_1_2 <- cor(car_data[, 'mileage'],
                         car_data[, 'mileage_1'])
plot(car_data[, 'mileage'], car_data[, 'mileage_1'],
     main = c('Scattergraph of two measures of mileage',
              sprintf('(r = %f)', correl_mileage_1_2)),
     xlab = 'mileage',
     ylab = 'mileage 1')

#--------------------------------------------------
# Further, assume that many rainfall variables
# are available for the estimation, even though
# they do not appear in the model (irrelevant variables).
#--------------------------------------------------

rainfall_variable_list <- sprintf('rainfall_%d', seq(1:number_of_rainfall_variables))
# These variables are also created in the ECO6416_tools_3.R script.

# Summarize the data.
summary(car_data)
# Should be many rainfall variables.


# Collect all available variables into a single list.
variable_list <- c(mileage_variable_list, 'accident', 'damage',
                   rainfall_variable_list)
# Note that true mileage is not in this list.
# We are pretending that it is unobserved.


##################################################
# Estimating the True Regression Model
# Model 1: All true Variables Included
##################################################

# Estimate a regression model.
lm_true_model <- lm(data = car_data[obsns_for_estimation, ],
                    formula = car_price ~ mileage + accident + damage)

# Output the results to screen.
summary(lm_true_model)


##################################################
# Estimating the Feasible Regression Model
# Model 2: Include only the available mileage variables.
##################################################

# Estimate a regression model.
lm_feasible_model <- lm(data = car_data[obsns_for_estimation, ],
                        formula = car_price ~ mileage_1 + mileage_2 + accident + damage)

# Output the results to screen.
summary(lm_feasible_model)


##################################################
# Exercise: Simple Data Mining Algorithm
# Estimating the Feasible Regression Model
##################################################

# Start with an empty model.
best_variable_list <- NULL
remaining_variable_list <- variable_list

# Create a data table to store the results.
best_models <- data.frame(num_vars = 1:length(variable_list), # Label with number of variables.
                          best_new_variable = factor(rep('',length(variable_list)),
                                                     levels = c('', variable_list)), # Place to record best model for each.
                          R2_in_sample = numeric(length(variable_list)),
                          R2_out_sample = numeric(length(variable_list))) # Fill this in later.


# For each size of model (number of variables),
# find the one with the highest R^2.
for (best_model_num in 1:length(variable_list)) {

  # Print a header for each estimation.
  print('')
  print('##################################################')
  print('')
  print(sprintf('Now estimating to find the best model with %d variables.', best_model_num))

  # Check that each model is better with than the previous variables.
  best_R2_so_far <- -1
  # The first one is automatically 'the best' so far.
  best_variable_so_far <- ''

  # To find the best model with this number of variables,
  # find the best variable to add next.
  for (test_model in 1:length(remaining_variable_list)) {

    # Get the name of a new candidate variable.
    test_variable_name <- remaining_variable_list[test_model]

    # Create a temporary variable list.
    fmla_string <- sprintf('car_price ~  %s',
                           paste(cbind(best_variable_list, test_variable_name),
                                 sep = '', collapse = ' + '))
    fmla <- as.formula(fmla_string)

    # Estimate the model.
    lm_test_model <- lm(data = car_data[obsns_for_estimation, ],
                        formula = fmla_string)

    # Calculate R^2 both in sample and out of sample.
    test_R2 <- summary(lm_test_model)$adj.r.squared # Pulled from estimation output.
    # Out-of-sample value needs to be calculated directly.
    actual_out <- car_data[!obsns_for_estimation, 'car_price']
    predict_out <- predict(lm_test_model, newdata = car_data[!obsns_for_estimation, ])
    test_R2_out <- 1 - ( sum((actual_out - predict_out )^2) /
                           (num_obs_estn - best_model_num - 1) ) /
      ( sum((actual_out - mean(actual_out))^2) /
          (num_obs_estn - best_model_num - 1) )


    # Compare the R^2 value with the previous 'best' model.
    if (test_R2 > best_R2_so_far) {

      best_variable_so_far <- test_variable_name
      best_R2_so_far <- test_R2
      best_R2_so_far_out <- test_R2_out

    } # else move on to the next test variable.

  }

  # Add the variable name with the highest R^2 as the best new variable.
  best_new_variable <- best_variable_so_far

  best_variable_list <- cbind(best_variable_list, best_new_variable)

  # Remove the chosen variable from consideration in future tests.
  remaining_variable_list <- remaining_variable_list[remaining_variable_list != best_new_variable]


  # Print a progress report for this model.
  print(sprintf('The best model with %d variables is ', best_model_num))
  fmla_string <- sprintf('car_price ~  %s',
                         paste(best_variable_list, sep = '', collapse = ' + '))
  print(fmla_string)
  print(sprintf('with an R-squared of %f.', best_R2_so_far))

  # Record the characteristics of the 'best' model.
  best_models[best_model_num, 'best_new_variable'] <- best_new_variable
  best_models[best_model_num, 'R2_in_sample'] <- best_R2_so_far
  best_models[best_model_num, 'R2_out_sample'] <- best_R2_so_far_out

}




# Print out the table of selected models and R-squared values.
print(best_models)

# Print out the Adjusted R-squared by number of variables.
plot(1:length(variable_list),
     best_models[, 'R2_in_sample'],
     main = 'Adjusted R-squared for Best Models',
     xlab = 'Number of Variables',
     ylab = 'Adjusted R-squared',
     type = 'l', col = 'red', lwd = 3,
     ylim = c(0.35, 0.8))
lines(1:length(variable_list),
      best_models[, 'R2_out_sample'],
      col = 'blue', lwd = 3)
legend('bottomright',
       legend = c('In sample', 'Out of Sample'),
       col = c('red', 'blue'),
       lwd = 3)



##################################################
#
# Pretending that you don't know the true model:
# Which model would you choose based on the in-sample R-squared?
# Which model would you choose based on the out-of-sample R-squared?
#
# After you have made your choices, compare each of these to the true model.
#
##################################################



##################################################
# End
##################################################
