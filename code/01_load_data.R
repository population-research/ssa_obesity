# Place holder script to allow renv setup

library(tidyverse)
library(here)
library(haven)
library(vtable)

rename_to_lower_snake <- function(df) {
  df %>%
    rename_with( ~gsub("([a-z])([A-Z])", "\\1_\\2", .x) ) %>%  # Adds _ to camel case var names
    rename_with( ~tolower(gsub("[ ]+", "_", .x)) )  # Converts to lower and substitutes _ for spaces
}


## look at phase 6 of the data --> tsetir63fl.dta

## onedrive/stormborn/data/dhs_ir/ ]--> with all individual recoding.

unzip(here("data", "tz_phase6"),
      exdir = here("data", "tz_phase6"),
      junkpaths = TRUE,
      unzip = "unzip"
)


## direct load of the tz dataset
t_dataset <- read_dta("/Users/krypton/obesity/data/tz_phase6/TZIR63DT/TZIR63FL.DTA") %>%
  select(where(is.numeric))


# select necessary variables
# 1. drop birth variables as bidx,bord, b's.
# 2. drop contraceptive: c300's and v30a-z family planning sources.
# 3. kept maternity information midx..m's, in case of need of vaccination information and related diseases.
# 4. kept maternity and feedinf information v400's. constains bmi and related information like smoking habits etc
# 5. kept hidx's and h's: information about health card and vaccination records on it etc
# 6. kept hw's: height weight and haemoglobin information
# 7. kept v's: marital status
# 8. kept mm's: contain maternal mortality -pshych
# 9. drop malaria information ml's
# 10. kept d1s: domestic violence: ex. can lead to stress
# 11. drop g's: female genital cutting
################ 12. S's NOT IN MANUAL

# mutate(bmi = v444a) %>%

# Identify columns to remove
columns_to_remove <- grep("^b|^ml|^g|^c30|^v30", names(t_dataset))

# Remove columns from the dataset
new_df <- t_dataset[, -columns_to_remove]  %>%
  mutate(bmi = v444a)

# Define the values to be removed since they are invalid responses.
values_to_remove <- c(9999, 999, 99, 9, 9998, 998, 98, 8, 9997, 997, 97, 7) # careful here

# Loop through each column and remove the values
for (col in names(new_df)) {
  new_df[[col]][new_df[[col]] %in% values_to_remove] <- NA
}


# Print the column names with NA values
print(na_cols)

## fill na with median
# Find NA values in each column and replace with median value
filled_df <- new_df %>%
  mutate(across(everything(), ~ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Identify columns with NA values
na_cols <- colnames(filled_df)[colSums(is.na(filled_df)) > 0]

## removing columns with no data
final_data <- filled_df[, colSums(is.na(filled_df)) == 0]



## Identify and turn categorical variables into factors

# Identify categorical variables
categorical_vars <- sapply(final_data, is.character)

# Convert categorical variables to factors
final_data[categorical_vars] <- lapply(final_data[categorical_vars], factor)


# Identify factor variables
factor_vars <- sapply(final_data, is.factor)

# Subset the dataset with factor variables only
factor_dataset <- final_data[, factor_vars]

# Create contingency table for factor variables
contingency_table <- table(factor_dataset)

# Print the contingency table
print(contingency_table)

## divide bmi by 100 at one point

# find correlation
correlations <- as.data.frame(cor(final_data)) %>%
  select("v444a") %>%
  sort()

print(correlations[10])

#### DATA MINING PROCEDURES

# 1. Recursive Feature Elimination (RFE): RFE is a backward feature selection method that recursively removes less important features from the dataset.
# It starts with all variables, trains a model, evaluates feature importance, and eliminates the least significant features.
# The process is repeated until the desired number of features remains.

library(caret)

# Define the target variable 'x'
target_variable <- "v444a"

# Separate the features and the target variable
features <- final_data[, !(names(final_data) %in% target_variable)]
target <- final_data[, target_variable]

# Perform Recursive Feature Elimination
control <- rfeControl(functions = rfFuncs, method = "cv", number = 5)
rfe_result <- rfe(features, target, sizes = c(1:(ncol(features)-1)), rfeControl = control)

# Print the selected features
print(rfe_result$optVariables)

# 2. Lasso (Least Absolute Shrinkage and Selection Operator): Lasso is a regularization technique that performs both feature selection and model fitting.
# It adds a penalty term to the model's cost function, encouraging the coefficients of irrelevant variables to shrink to zero.
# As a result, the most relevant variables are selected.
#
# 3. Random Forests: Random Forests use an ensemble of decision trees to evaluate feature importance.
# The algorithm measures the decrease in prediction accuracy when a particular variable is randomly permuted,
# providing an estimate of its importance. Variables with higher importance scores are considered more relevant.

library(randomForest)

# Build the random forest model
rf_model <- randomForest(features, target, ntree = 100, importance = TRUE)

# Get the variable importance measures
var_importance <- importance(rf_model)

# Sort the variable importance in descending order
sorted_importance <- var_importance[order(-var_importance[, 1]), ]

# Print the variable importance
print(sorted_importance)

#
# 4. Information Gain or Gain Ratio: These measures are commonly used in decision tree-based algorithms to evaluate the relevance of features.
# Information Gain calculates the reduction in entropy or impurity when a feature is included in the split, while Gain Ratio adjusts the Information Gain
# by taking into account the intrinsic information of the feature itself.
#

library(InformationValue) ## --> unable to find information to install the library package.



# 5. Chi-square Test or Mutual Information: These statistical methods can be used to measure the dependence or association between two variables.
# They are often applied in feature selection for categorical or discrete variables.
# Chi-square test evaluates the independence between variables, while Mutual Information measures the amount of
# information shared by the variables.

# Load necessary libraries
library(readr)

# Perform a chi-square test for each feature in the dataset
for (feature in names(final_data)) {
  if (feature != target_variable) {
    # Create a contingency table between the feature and variable x
    contingency_table <- table(target_variable, final_data[[feature]])

    # Perform chi-square test
    chi_square_result <- chisq.test(contingency_table)

    # Print the results
    cat("Feature:", feature, "\n")
    cat("Chi-square statistic:", chi_square_result$statistic, "\n")
    cat("Degrees of freedom:", chi_square_result$parameter, "\n")
    cat("p-value:", chi_square_result$p.value, "\n")
    cat("--------------------------------------------------\n")
  }
}



#
# phase6_tz_data1 <- na.omit(phase6_tz_data)
# # Split data into training and testing sets
# set.seed(123)
# training_indices <- createDataPartition(bmi, p = 0.7, list = FALSE)
# training <- data[phase6_tz_data, ]
# testing <- data[-phase6_tz_data, ]
# # Train a linear regression model to predict BMI
# model <- train(BMI ~ ., data = training, method = "lm")
#
# # Print the importance of each predictor variable
# varImp(model)

# preprocess

#- drop birth
# turn string variables to factors or drop
# most with b to drop
