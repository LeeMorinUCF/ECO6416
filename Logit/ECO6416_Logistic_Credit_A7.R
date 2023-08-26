##################################################
#
# ECO 6416.0028 Applied Business Research Tools
#
# Logistic Regression Demo
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
# ECO6416_Logistic_Credit gives an example of OLS
#   and logistic regression using data imported from a spreadsheet.
#
# The variables in the dataset credit_data.csv are as follows:
#
#   default: 1 if borrower defaulted on a loan
#   bmaxrate: Maximum rate of interest on any part of the loan
#   amount: the amount funded on the loan
#   close: borrower takes the option of closing the listing
#     before it is fully funded
#   bankcardutil: the utilization rate on the borrower's
#     other credit card accounts (the percentage of the balance
#     divided by the credit limit).
#   rating: the borrower's credit rating category
#     which is divided into the following categories
#   AA: borrower's FICO score greater than 760
#   A: borrower's FICO score between 720 and 759
#   B: borrower's FICO score between 680 and 719
#   C: borrower's FICO score between 640 and 679
#   D: borrower's FICO score between 600 and 639
#   Missing: borrower's FICO score is unobserved
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


# The csv file used below must be in the working directory.
# If you an error message, make sure that the file is
# located in your working directory.
# Also make sure that the name has not changed.

# Load library for computing the AUROC.
# You will have to install this package the first time.
# Press the "Install" button or run the following (without the #):
# install.packages('pROC')
library(pROC)


##################################################
# Loading the Data and Conducting Initial Assessment
##################################################

credit_data <- read.csv('LOANS.csv')

# Inspect the contents.
summary(credit_data)
# Make sure there are no problems with the data.

# Inspect the dependent variable.
hist(credit_data[, 'default'])

#------------------------------------------------------------
# Numeric Variables
#------------------------------------------------------------

# Inspect the correlations between numeric explanatory variables.
cor(credit_data[, 1:5])
# Be aware of any explanatory variables that are highly correlated
# (both positively and negatively) with each other.




# Compare the distributions of variables
# for classes of the dependent variable.
summary(credit_data[credit_data[, 'default'] == 0,
                    c('bmaxrate', 'amount', 'close', 'bankcardutil')])
summary(credit_data[credit_data[, 'default'] == 1,
                    c('bmaxrate', 'amount', 'close', 'bankcardutil')])


#------------------------------------------------------------
# Categorical (factor) Variables
#------------------------------------------------------------

# Use table() to tabulate categorical variables (AKA factor variables).
table(credit_data[, 'rating'], useNA = 'ifany')

# Set the order of the factor labels.
credit_data[, 'rating'] <- factor(credit_data[, 'rating'],
                                  levels = c('Missing', 'AA', 'A', 'B', 'C', 'D'))

table(credit_data[, 'rating'], useNA = 'ifany')


# Now inspect the tables by default status.
table(credit_data[credit_data[, 'default'] == 0, 'rating'], useNA = 'ifany')
table(credit_data[credit_data[, 'default'] == 1, 'rating'], useNA = 'ifany')

# Are the proportions different?


##################################################
# Generating New Variables
##################################################

# You can create new variables by adding commands
# within this program that add new columns to the
# credit_data data frame.




##################################################
# Estimating a Regression Model
# Model 1: Linear model for default probability
# Start with a full model that includes all variables.
##################################################

# Estimate a regression model.
lm_model_1 <- lm(data = credit_data,
                 formula = default ~ bmaxrate + amount +
                   close + bankcardutil + rating)

# Output the results to screen.
summary(lm_model_1)

# Calculate the predictions of this model.
credit_data[, 'default_prob_lm_1'] <- predict(lm_model_1)

summary(credit_data[, 'default_prob_lm_1'])
# Does anything look unusual?


# Measure model performance with AUROC.
roc(response = credit_data[, 'default'],
    predictor = credit_data[, 'default_prob_lm_1'])


##################################################
# Estimating a Regression Model
# Model 2: Linear model for default probability
# Trim down the model by omitting variables.
##################################################

# Estimate a regression model.
lm_model_2 <- lm(data = credit_data,
                 formula = default ~
                   # Remove some variables from here:
                   rating +
                   bmaxrate +
                   amount +
                   bankcardutil +
                   close)
# Note that the '+' signs allow for the formula to continue
# to the next line.

# Output the results to screen.
summary(lm_model_2)

# Calculate the predictions of this model.
credit_data[, 'default_prob_lm_2'] <- predict(lm_model_2)

summary(credit_data[, 'default_prob_lm_2'])
# Does anything look unusual?


# Measure model performance with AUROC.
roc(response = credit_data[, 'default'],
    predictor = credit_data[, 'default_prob_lm_2'])




##################################################
# Estimating a Regression Model
# Model 3: Logistic model for default probability
# Start with a full model that includes all variables.
##################################################

# Estimate a logistic regression model.
logit_model_1 <- glm(data = credit_data,
                     formula = default ~ bmaxrate + amount +
                       close + bankcardutil + rating,
                     family = 'binomial')

# Output the results to screen.
summary(logit_model_1)


# Calculate the predictions of this model.
credit_data[, 'default_prob_logit_1'] <- predict(logit_model_1, type = 'response')

summary(credit_data[, 'default_prob_logit_1'])
# Does this look better?


# Measure model performance with AUROC.
roc(response = credit_data[, 'default'],
    predictor = credit_data[, 'default_prob_logit_1'])



##################################################
# Estimating a Regression Model
# Model 4: Logistic model for default probability
# Trim down the model by omitting variables.
##################################################

# Estimate a logistic regression model.
logit_model_2 <- glm(data = credit_data,
                     formula = default ~
                       # Remove some variables from here:
                       rating +
                       bmaxrate +
                       amount +
                       bankcardutil +
                       close,
                     family = 'binomial')

# Output the results to screen.
summary(logit_model_2)

# Calculate the predictions of this model.
credit_data[, 'default_prob_logit_2'] <- predict(logit_model_2, type = 'response')

summary(credit_data[, 'default_prob_logit_2'])


# Measure model performance with AUROC.
roc(response = credit_data[, 'default'],
    predictor = credit_data[, 'default_prob_logit_2'])

# Which model has the higher AUROC value:
# The linear model or the logistic model?




##################################################
# Models with careful accounting of the
# timing of information.
##################################################

# The previous models were built using only after
# the loans were made.
# Now build a model using the information that
# a lender would have known at the time
# the loan was requested.


# Estimate a logistic regression model.
logit_model_3 <- glm(data = credit_data,
                     formula = default ~
                       # Remove some variables from here:
                       rating +
                       bmaxrate +
                       amount +
                       bankcardutil +
                       close,
                     family = 'binomial')

# Output the results to screen.
summary(logit_model_3)

# How did the model change as a result of this restriction?



# Calculate the predictions of this model.
credit_data[, 'default_prob_logit_3'] <- predict(logit_model_3, type = 'response')

summary(credit_data[, 'default_prob_logit_3'])


# Measure model performance with AUROC.
roc(response = credit_data[, 'default'],
    predictor = credit_data[, 'default_prob_logit_3'])

# What was the change in the AUROC value?
# Did any of the remaining variables change significance?


##################################################
# End
##################################################
