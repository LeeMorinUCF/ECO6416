##################################################
# 
# ECO 6416.0028 Applied Business Research Tools
# 
# Regression modeling over many highly correlated Variables
# Includes variables with measurement error
# and several highly correlated substitutes.
# 
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
# 
# November 8, 2019
# 
##################################################
# 
# ECO6416_Multi_Demo gives an example of regression modelling
#   with highly correlated variables.
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

# Set working directory
# (could do this using buttons in File panel).
setwd("~/Teaching/ECO6416_Fall2019/Module10")

# Read function for sampling data. 
source('ECO6416_Sim_Data_Plus.R')
# This is the same as running the ECO6416_Sim_Data_Plus.R script first.
# It assumes that the script is saved in the same working folder.


##################################################
# Setting the Parameters
##################################################

# Dependent Variable: Property values (in Millions)

beta_0          <-   0.10    # Intercept
beta_income     <-   5.00    # Slope ceofficient for income
beta_cali       <-   0.25    # Slope coefficient for California
beta_earthquake <- - 0.50    # Slope coefficient for earthquake (when active in model)
# beta_earthquake <-   0.00    # Slope coefficient for earthquake (when removed from model)

# Distribution of incomes (also in millions).
avg_income <- 0.1
sd_income <- 0.01

# Extra parameter for measurement error in income. 
number_of_income_variables <- 4
measurement_error_income <- 0.002

# Fraction of dataset in California.
pct_in_cali <- 0.5

# Frequency of earthquakes (only in California).
prob_earthquake <- 0.075

# Frequency of rainfall (can happen anywhere). 
prob_rainfall <- 0.25

# Number of additional (irrelevant) rainfall variables to add to dataset.
number_of_rainfall_variables <- 0

# Additional terms:
sigma_2 <- 0.1        # Variance of error term
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

# Call the housing_sample function from ECO6416_Sim_Data.R. 
housing_data <- housing_sample(beta_0, beta_income, beta_cali, beta_earthquake, 
                               avg_income, sd_income, pct_in_cali, prob_earthquake, 
                               sigma_2, num_obs, 
                               number_of_income_variables, measurement_error_income, 
                               number_of_rainfall_variables, prob_rainfall)

# Summarize the data.
summary(housing_data)

# Check that earthquakes occurred only in California:
table(housing_data[, 'in_cali'], housing_data[, 'earthquake'])
# Data errors are the largest cause of problems in model-building.

# Check for the subsamples for estimation and testing. 
# Estimation sample:
table(housing_data[obsns_for_estimation, 'in_cali'], 
      housing_data[obsns_for_estimation, 'earthquake'])
# Testing sample:
table(housing_data[!obsns_for_estimation, 'in_cali'], 
      housing_data[!obsns_for_estimation, 'earthquake'])
# ! means 'not'.


##################################################
# Generating Additional Data
# The extra data that is not in the model
##################################################

#--------------------------------------------------
# Assume that true income is not observed but some variables 
# that are correlated with income are available. 
#--------------------------------------------------

income_variable_list <- sprintf('income_%d', seq(1:number_of_income_variables))
# These variables are created in the House_Price_Sim_Data.R script. 


# Check how strongly the data are correlated. 
# cor(housing_data[, c('income', 'income_1', 'income_2')])
cor(housing_data[, c('income', income_variable_list)])

correl_income_1_2 <- cor(housing_data[, 'income'], 
                         housing_data[, 'income_1'])
plot(housing_data[, 'income'], housing_data[, 'income_1'],
     main = c('Scattergraph of two measures of income', 
              sprintf('(r = %f)', correl_income_1_2)),
     xlab = 'Income',
     ylab = 'Income 1')
# These variables are highly correlated. 

#--------------------------------------------------
# Calculate variance inflation factors
#--------------------------------------------------

# The entire list of variables includes several similar measurements of income.
list_of_variables <- c(income_variable_list, 'in_cali', 'earthquake')

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
  lm_model_vif <- lm(data = housing_data, 
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
# Estimating the True Regression Model
# Model 1: All true Variables Included
##################################################

# Estimate a regression model.
lm_true_model <- lm(data = housing_data[obsns_for_estimation, ], 
                    formula = house_price ~ income + in_cali + earthquake)

# Output the results to screen.
summary(lm_true_model)


##################################################
# Estimating the Feasible Regression Model
# Model 2: Include only the available income variables. 
##################################################

# Estimate a regression model.
lm_feasible_model <- lm(data = housing_data[obsns_for_estimation, ], 
                        formula = house_price ~ income_1 + income_2 + in_cali + earthquake)

# Output the results to screen.
summary(lm_feasible_model)





##################################################
# Estimating the Feasible Regression Model
# Model 3: Include all of the available income variables. 
##################################################

# Estimate a regression model.
lm_full_model <- lm(data = housing_data[obsns_for_estimation, ], 
                        formula = house_price ~ income_1 + income_2 + income_3 + income_4 + 
                          in_cali + earthquake)

# Output the results to screen.
summary(lm_full_model)





##################################################
# Estimating the Feasible Regression Model
# Model 4: Include only one of the available income variables. 
##################################################

# Estimate a regression model.
lm_new_model <- lm(data = housing_data[obsns_for_estimation, ], 
                    formula = house_price ~ income_4 + 
                      in_cali + earthquake)

# Output the results to screen.
summary(lm_new_model)






##################################################
# End
##################################################