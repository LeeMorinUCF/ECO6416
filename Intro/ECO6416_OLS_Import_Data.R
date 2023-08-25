##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# OLS Regression Demo
# Regression with Data from Spreadsheet
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
# ECO6416_Import_Data gives an example of OLS regression
#   using data imported from a spreadsheet.
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


##################################################
# Read the dataset and run regression
##################################################

# Read the newly saved dataset.
housing_data_3 <- read.csv(file = 'housing_data.csv')

# Inspect the data.
summary(housing_data_3)

# Plot a scattergraph of income and housing prices.
plot(housing_data_3[, 'income'],
     housing_data_3[, 'house_price'],
     main = c('House Prices vs. Income', '(all figures in millions)'),
     xlab = 'Income',
     ylab = 'House Prices',
     col = 'blue')

# Calculate a correlation matrix for selected variables.
corr_matrix <- cor(housing_data_3[, c('house_price', 'income', 'in_cali', 'earthquake')])
print(round(corr_matrix, 3))


# Estimate a regression model.
lm_model_3 <- lm(data = housing_data_3,
                 formula = house_price ~ income + in_cali + earthquake)

# Output the results to screen.
summary(lm_model_3)


##################################################
# End
##################################################

