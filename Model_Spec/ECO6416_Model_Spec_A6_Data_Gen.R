##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Examples of Model Specification
#
# Lealand Morin, Ph.D.
# Assistant Professor
# Department of Economics
# College of Business
# University of Central Florida
#
# August 26, 2023
#
##################################################
#
# ECO6416_Model_Spec_A6 gives examples of OLS regression models
#   by considering a number of different model specifications.
# 
# This script makes adjustments to the dataset
# to change the results from last year's solution key.
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


# Set path for working directory.
# Put files on desktop in a folder called ECO6416
# wd_path <- 'C:/Users/le279259/Desktop/ECO6416/Model_Spec'
# Modify the above line according to the specific path on your computer,
# as in:
# wd_path <- 'C:/Users/name/of/your/path'

# Set the working directory to this path.
# setwd(wd_path)

# Or set the working directory in one command:
# setwd("C:/Users/le279259/Desktop/ECO6416/Model_Spec")
setwd("~/Teaching/UCF_ECO6416/ECO6416_Fall_2023/GitHub/ECO6416/Data")

# Verify that the path was assigned correctly.
getwd()


##################################################
# Loading the Data
##################################################

tractor_sales <- read.csv('TRACTOR7.csv')

# Inspect the contents.
summary(tractor_sales)
# Make sure there are no problems with the data.

# Record the original column names.
orig_cols <- colnames(tractor_sales)

##################################################
# Generating New Variables
##################################################

# Copy the original saleprices into a separate column and replace
# with new prices.
tractor_sales[, 'saleprice_orig'] <- tractor_sales[, 'saleprice']

#--------------------------------------------------
# First change:
# Increase effect of seasonality
#--------------------------------------------------

# Tractors sell for a premium in the fall, when most tractors 
# are used in the harvest, which is time-sensitive.
fall_premium <- 0.15

# Another premium in the spring, when fields are plowed, 
# not as time-sensitive, but the earlier the better.
spring_premium <- 0.08

# Tractors sell for a discount in the winter when not as many are needed, 
# but some are still used for plowing snow, in some places.
winter_discount <- - 0.08



# Make adjustments to prices.
tractor_sales[, 'saleprice_adj'] <- NA
sel_rows <- tractor_sales[, 'spring'] == 0 & 
  tractor_sales[, 'summer'] == 0 & 
  tractor_sales[, 'winter'] == 0
tractor_sales[sel_rows, 'saleprice_adj'] <- round((1 + fall_premium)*tractor_sales[sel_rows, 'saleprice_orig'], 0)

sel_rows <- tractor_sales[, 'spring'] == 1
tractor_sales[sel_rows, 'saleprice_adj'] <- round((1 + spring_premium)*tractor_sales[sel_rows, 'saleprice_orig'], 0)

sel_rows <- tractor_sales[, 'winter'] == 1
tractor_sales[sel_rows, 'saleprice_adj'] <- round((1 + winter_discount)*tractor_sales[sel_rows, 'saleprice_orig'], 0)

sel_rows <- tractor_sales[, 'summer'] == 1
tractor_sales[sel_rows, 'saleprice_adj'] <- tractor_sales[sel_rows, 'saleprice_orig']




# Take logs to bring outliers closer to the others.
tractor_sales[, 'log_saleprice'] <- log(tractor_sales[, 'saleprice'])
tractor_sales[, 'log_saleprice_adj'] <- log(tractor_sales[, 'saleprice_adj'])



# Estimate a regression model to check.
lm_model_1 <- lm(data = tractor_sales,
                 # formula = saleprice ~ 
                   # formula = saleprice_adj ~
                   # formula = log_saleprice ~
                   formula = log_saleprice_adj ~
                   horsepower + age + enghours +
                   diesel + fwd + manual + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_1)


#--------------------------------------------------
# First change:
# Increase effect of seasonality
#--------------------------------------------------


# Create a variable squared_horsepower
# to investigate quadratic relationship of sale price to horsepower.
tractor_sales[, 'squared_horsepower'] <- tractor_sales[, 'horsepower']^2

summary(tractor_sales[, 'saleprice_adj'])

sq_horse_discount <- - 0.0000012
tractor_sales[, 'saleprice_adj'] <- 
  round(tractor_sales[, 'saleprice_adj'] * 
          (1 + sq_horse_discount*tractor_sales[, 'squared_horsepower']), -1)

summary(tractor_sales[, 'saleprice_adj'])


# Recalculate log sale price.
tractor_sales[, 'log_saleprice_adj'] <- log(tractor_sales[, 'saleprice_adj'])



# Estimate a regression model to check.
lm_model_2 <- lm(data = tractor_sales,
                 # formula = saleprice ~
                 # formula = saleprice_adj ~
                 # formula = log_saleprice ~
                 formula = log_saleprice_adj ~
                   horsepower + squared_horsepower + 
                   age + enghours +
                   diesel + fwd + manual + johndeere +
                   spring + summer + winter)

# Output the results to screen.
summary(lm_model_2)


# Record the final adjusted saleprice.
tractor_sales[, 'saleprice'] <- tractor_sales[, 'saleprice_adj']



##################################################
# Saving the New Dataset
##################################################

out_file_name <- 'TRACTOR_A6.csv'
write.csv(tractor_sales[, orig_cols], row.names = FALSE, 
          file = out_file_name)




##################################################
# End
##################################################
