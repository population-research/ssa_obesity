library(tidyverse)
library(here)
library(haven)
library(vtable)
library(janitor)

# Function to rename variables to lower_snake_case
rename_to_lower_snake <- function(df) {
  df %>%
    rename_with(~ gsub("([a-z])([A-Z])", "\\1_\\2", .x)) %>%
    rename_with(~ tolower(gsub("[ ]+", "_", .x)))
}

# Unzip the dataset
unzip(here("data", "tz_phase6"), exdir = here("data", "tz_phase6"), junkpaths = TRUE, unzip = "unzip")

# Directly load the tz dataset
t_dataset <- read_dta(here("data", "tz_phase6", "TZIR63DT", "TZIR63FL.DTA"))

# Define columns to remove
columns_to_remove <- grep("^b|^ml|^g|^c30|^v30", names(t_dataset))

# Remove columns and create new dataframe
new_df <- t_dataset[, -columns_to_remove] %>%
  mutate(bmi = v444a)

# Define values to remove
values_to_remove <- c(9999, 999, 99, 9, 9998, 998, 98, 8, 9997, 997, 97, 7)

# Replace invalid values with NA
new_df <- new_df %>%
  mutate(across(everything(), ~ ifelse(. %in% values_to_remove, NA, .)))

# Fill NA values with median
filled_df <- new_df %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Remove columns with no data
final_data <- filled_df[, colSums(is.na(filled_df)) == 0]

# Convert categorical variables to factors
categorical_vars <- sapply(final_data, is.character)
final_data[categorical_vars] <- lapply(final_data[categorical_vars], as.factor)

# Subset the dataset with factor variables only
factor_vars <- sapply(final_data, is.factor)
factor_dataset <- final_data[, factor_vars]


# Identify factor variables with more than one level
valid_factors <- sapply(final_data, function(x) is.factor(x) && length(levels(x)) > 1)

# Subset the dataset to include only valid factor variables
factor_data <- final_data[, valid_factors]

# Convert factor variables to dummy variables
dummy_vars <- model.matrix(~.-1, data = factor_data)

# Combine "bmi" variable and dummy variables with the original numeric variables
features <- cbind(final_data[,"bmi"], dummy_vars)

# subset data for less memory use
subset_data <- features[1:100, 1:50]


# Perform PCA on the features
pca_result <- prcomp(subset_data, center = TRUE, scale. = TRUE)

# Access the principal components
principal_components <- pca_result$x

# Access the standard deviations of each principal component
standard_deviations <- pca_result$sdev

# Access the proportion of variance explained by each principal component
variance_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Access the cumulative proportion of variance explained
cumulative_variance_explained <- cumsum(variance_explained)

# Plot the scree plot
plot(1:length(variance_explained), cumulative_variance_explained, type = "b", xlab = "Principal Component", ylab = "Cumulative Variance Explained")

# Plot the biplot (if desired)
biplot(pca_result)
