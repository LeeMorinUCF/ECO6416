##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Regression with Data Entered Directly into the Script
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business Administration
# University of Central Florida
#
# August 31, 2020
#
##################################################
#
# ECO6416_A1Q1 gives an example of OLS regression
#   using data entered within this script.
#
# Dependencies:
#   None.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory.
# wd_path <- '/path/to/your/folder'
# wd_path <- 'C:/Users/le279259/Desktop/ECO6416_Demos/Module03' # On Windows
# Not required, since this program does not interact with other files.

# setwd(wd_path)

# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)


##################################################
# Enter the dataset and run regression
##################################################

# Enter the numbers in the dataset.
income_data  <- c(6, 8, 8, 7, 7, 12, 9, 8, 9, 10)
agg_pct_data <- c(9, 10, 8, 7, 10, 4, 5, 5, 6, 7)
agg_data <- data.frame(income = income_data*3 + 2,
                       agg_pct = agg_pct_data*2)

# Inspect the data.
summary(agg_data)

# Plot a scattergraph of income and housing prices.
plot(agg_data[, 'income'],
     agg_data[, 'agg_pct'],
     main = 'Aggregate Income vs. Pct. in Agriculture',
     xlab = 'Income',
     ylab = 'Pct. in Agriculture',
     col = 'blue',
     pch = 16)



# Estimate a regression model.
lm_model <- lm(data = agg_data,
                 formula = income ~ agg_pct)

# Output the results to screen.
summary(lm_model)


##################################################
# Estimating from first principles
##################################################

y <- agg_data[, 'income']
x <- agg_data[, 'agg_pct']

y_bar = mean(y)
x_bar = mean(x)

beta_cov_term <- sum( (y - y_bar)*(x - x_bar) )
beta_var_term <- sum( (x - x_bar)^2 )

# Calculate the slope coefficient.
beta_1_hat <- beta_cov_term/beta_var_term

# Calculate the intercept.
beta_0_hat <- y_bar - beta_1_hat*x_bar

# Compare with the output above.




# Calculation of the R-squared.
TSS <- sum((y - y_bar)^2)
y_hat <- beta_0_hat + beta_1_hat*x
# ESS <- sum( (y_hat - y_bar)^2 ) # Not needed.
RSS <- sum( (y - y_hat)^2 )

R_2 <- 1 - RSS/TSS

R_bar_2 <- 1 - (RSS/(10 - 1 - 1)) / (TSS/(10 - 1))


# Prediction:
beta_0_hat + beta_1_hat*10



##################################################
# End
##################################################

