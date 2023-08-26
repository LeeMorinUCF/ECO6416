##################################################
# 
# ECO 6416: Applied Business Research Tools
# 
# OLS Regression Demo
# Detecting Serial Correlation
# 
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
# 
# November 25, 2018
# 
##################################################
# 
# ECO6416_Durbin_Watson gives two examples of 
#   Durbin-Watson tests, using simulated data,
#   one without and one with serial correlation.
# 
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

##################################################
# Example 1: No serial correlation
# Serail correlation parameter rho = 0
##################################################

# Generate errors (with no serial correlation)
err1 <- rnorm(100)

# Generate regressor and dependent variable
x <- rep(c(-1,1), 50)
y1 <- 1 + x + err1

# Perform Durbin-Watson test
dwtest(y1 ~ x)


##################################################
# Example 2: Strong serial correlation
# Serail correlation parameter rho = 0.9
##################################################

# Generate errors (with strong serial correlation)
err2 <- filter(err1, 0.9, method="recursive")

# Generate regressor and dependent variable
y2 <- 1 + x + err2

# Perform Durbin-Watson test
dwtest(y2 ~ x)

# Perform LM test.
# First run resression of residuals on all variables 
# and lagged errors.
lm_model_lm_test <- lm(data = cons_data, 
                       formula = con ~ dpi + aaa + con_resid_lag)

# Output the results to screen as usual.
summary(lm_model_lm_test)

# Test for coefficient on lagged residual is N*R^2.
# Compare to critical value for chi-squared with 1 degree of freedom. 



##################################################
# End
##################################################
