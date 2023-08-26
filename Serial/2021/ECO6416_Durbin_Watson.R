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

# Declare lmtest package
library(lmtest)
# If this throws an error, run
# install.packages('lmtest')
# without the '#', to install the packages
# (but only the first time).

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


##################################################
# End
##################################################
