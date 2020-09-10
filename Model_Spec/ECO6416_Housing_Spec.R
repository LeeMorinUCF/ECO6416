##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Specification Testing with Binary Variables
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
# ECO6416_Housing_Spec gives an example of the search for
#   an appriporiate model specification. by introducing
#   binary variables for the intercept and slope.
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

# Dependent Variable: Property values (in Millions)

beta_0          <-   0.05    # Intercept (different from before)
# beta_income     <-   5.00    # Slope ceofficient for income
beta_income_urban     <-   7.00    # Slope ceofficient for income in urban areas
beta_income_rural     <-   3.00    # Slope ceofficient for income in rural areas
beta_cali       <-   0.25    # Slope coefficient for California
beta_earthquake <- - 0.50    # Slope coefficient for earthquake
# beta_earthquake <- - 0.00    # Slope coefficient for earthquake
# New variable: Urban location indicator.
# beta_urban      <- 0.15
beta_urban      <- 0.00

# Distribution of incomes (also in millions).
# avg_income <- 0.1
avg_income_urban <- 0.15 # Incomes are higher in urban areas.
avg_income_rural <- 0.05 # Average of urban and rural are the same.
sd_income_urban <- 0.01
sd_income_rural <- 0.02  # More variation in income in urban areas.

# Fraction of dataset in California.
pct_in_cali <- 0.5

# Fraction of dataset in urban areas.
pct_in_urban <- 0.5

# Frequency of earthquakes (only in California).
prob_earthquake <- 0.10

# Additional terms:
sigma_2 <- 0.15        # Variance of error term
num_obs <- 100      # Number of observations in dataset


##################################################
# Generating the Data
##################################################

housing_data <- data.frame(obsn_num = 1:num_obs, # Label with observation number.
                           house_price = numeric(num_obs), # Fill this in later.
                           income = numeric(num_obs), # Fill this in later.
                           in_cali = numeric(num_obs), # Fill this in later.
                           earthquake = numeric(num_obs), # Fill this in later.
                           in_urban = numeric(num_obs), # Fill this in later.
                           epsilon = rnorm(n = num_obs, mean = 0, sd = sigma_2))

# Mark first set of observations from California with ones,
# the rest at zero.
housing_data[, 'in_cali'] <- 0
housing_data[ housing_data[, 'obsn_num'] <= num_obs*pct_in_cali, 'in_cali'] <- 1

# Mark a set of observations as having an earthquake,
# but only in California.
housing_data[, 'earthquake'] <- 0
housing_data[ runif(num_obs) <= prob_earthquake &
                housing_data[, 'in_cali'] == 1, 'earthquake'] <- 1

# Mark some observations as located in urban areas.
housing_data[, 'in_urban'] <- 0
housing_data[ runif(num_obs) <= pct_in_urban, 'in_urban'] <- 1

# Calculate income, which now depends on income.
# Start with the average values.
housing_data[housing_data[, 'in_urban'] == 1, 'income'] <- avg_income_urban
housing_data[housing_data[, 'in_urban'] == 0, 'income'] <- avg_income_rural
# Add the variation afterward.
# First for urban.
housing_data[housing_data[, 'in_urban'] == 1, 'income'] <-
  housing_data[housing_data[, 'in_urban'] == 1, 'income'] +
  rnorm(n = sum(housing_data[, 'in_urban'] == 1),
        mean = 0, sd = sd_income_urban)
# Then for rural.
housing_data[housing_data[, 'in_urban'] == 0, 'income'] <-
  housing_data[housing_data[, 'in_urban'] == 0, 'income'] +
  rnorm(n = sum(housing_data[, 'in_urban'] == 0),
        mean = 0, sd = sd_income_rural)


# Finally, calculate the simulated value of house prices,
# according to the regression equation.
housing_data[, 'house_price'] <-
  beta_0 +
  beta_urban * housing_data[, 'in_urban'] + # Note the new term for urban.
  # beta_income * housing_data[, 'income'] +
  beta_income_urban * housing_data[, 'income']*housing_data[, 'in_urban'] +
  beta_income_rural * housing_data[, 'income']*(1-housing_data[, 'in_urban']) +
  beta_cali * housing_data[, 'in_cali'] +
  beta_earthquake * housing_data[, 'earthquake'] +
  beta_urban * housing_data[, 'in_urban'] +
  housing_data[, 'epsilon']

# Summarize the data.
summary(housing_data)

# Check that earthquakes occurred only in California:
table(housing_data[, 'in_cali'], housing_data[, 'earthquake'])
# Data errors are the largest cause of problems in model-building.


# Summarize the data separately for urban and rural areas.
summary(housing_data[housing_data[, 'in_urban'] == 1, ])
summary(housing_data[housing_data[, 'in_urban'] == 0, ])




##################################################
# Estimating the Regression Model
# Model 1: Original true model
# (before urban and rural differences)
##################################################

# Estimate a regression model.
lm_orig_model <- lm(data = housing_data,
                    formula = house_price ~ income + in_cali + earthquake)

# Output the results to screen.
summary(lm_orig_model)


##################################################
# Estimating the Regression Model
# Model 2: Urban and rural differences
##################################################

# Estimate a regression model.
lm_urban_model_1 <- lm(data = housing_data,
                     formula = house_price ~ income + in_urban + in_cali + earthquake)

# Output the results to screen.
summary(lm_urban_model_1)


##################################################
# Estimating the Regression Model
# Model 3: Urban and rural differences in the income coefficient
##################################################

# Estimate a regression model.
lm_urban_model_2 <- lm(data = housing_data,
                     formula = house_price ~ income + in_urban + in_cali +
                       in_urban*income + # New interaction term
                       earthquake)

# Output the results to screen.
summary(lm_urban_model_2)


##################################################
#
# Exercise:
#
# Recommend the best model.
#
##################################################



##################################################
# End
##################################################