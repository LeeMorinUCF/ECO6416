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
# September 16, 2019
#
##################################################
#
# ECO6416_OLS_Demo gives an example of OLS regression
#   using data imported entered within this script.
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
# wd_path <- '~/Teaching/ECO6416_Fall2019/Module02' # On Windows
# Not required, since this program does not interact with other files.

# setwd(wd_path)

# No libraries required.
# Otherwise would have a command like the following.
# library(name_of_R_package)


##################################################
# Enter the dataset and run regression
##################################################

# Enter the numbers in the dataset.
income_data  <- c(14, 18, 18, 16, 16, 26, 20, 18, 20, 22)
agg_pct_data <- c(9, 10, 8, 7, 10, 4, 5, 5, 6, 7)
agg_data <- data.frame(income = income_data,
                       agg_pct = agg_pct_data)

# Inspect the data.
summary(agg_data)

# Plot a scattergraph of income and housing prices.
plot(agg_data[, 'income'],
     agg_data[, 'agg_pct'],
     main = 'Aggregate Income vs. Pct. in Agriculture',
     xlab = 'Income',
     ylab = 'Pct. in Agriculture',
     col = 'blue')



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

beta_hat <- beta_cov_term/beta_var_term
# Compare with the output above.


##################################################
# End
##################################################

