#Importing the dataset
library(readr)
Crop_recommendation <- read_csv("data/Crop_recommendation.csv")
View(Crop_recommendation)


######## Issue 1: Descriptive Statistics

# Load necessary libraries
library(dplyr)

# Measures of Frequency
frequency_table <- table(Crop_recommendation$label)
print("Measures of Frequency:")
print(frequency_table)

# Measures of Central Tendency
central_tendency <- summary(Crop_recommendation[, c("N", "P", "K", "temperature", "humidity", "ph", "rainfall")])
print("Measures of Central Tendency:")
print(central_tendency)

# Measures of Distribution
distribution <- sapply(Crop_recommendation[, c("N", "P", "K", "temperature", "humidity", "ph", "rainfall")], sd)
print("Measures of Distribution:")
print(distribution)

# Measures of Relationship
correlation_matrix <- cor(Crop_recommendation[, c("N", "P", "K", "temperature", "humidity", "ph", "rainfall")])
print("Measures of Relationship (Correlation Matrix):")
print(correlation_matrix)

######## Issue 2: Inferential Statistics - ANOVA

# Example: One-way ANOVA to test if there are any significant differences in the 'N' values among different crops
anova_result <- aov(N ~ label, data = Crop_recommendation)
print("ANOVA Results:")
print(summary(anova_result))

# Note: This can be repeated for other variables (P, K, temperature, humidity, ph, rainfall) by changing the formula accordingly.



######## Issue 3: Basic Visualization - Univariate Plots

# Load necessary libraries
library(ggplot2)

# Univariate Plot for 'N' values
ggplot(Crop_recommendation, aes(x = label, y = N)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Boxplot of N values for each crop",
       x = "Crop",
       y = "N Value")

# The above code can be repeated for other variables (P, K, temperature, humidity, ph, rainfall) to create univariate plots for each.

######## Issue 3: Basic Visualization - Multivariate Plots

# Multivariate Plot for 'N' and 'P' values
ggplot(Crop_recommendation, aes(x = N, y = P, color = label)) +
  geom_point() +
  labs(title = "Scatter plot of N vs P for each crop",
       x = "N Value",
       y = "P Value")

# The above code can be repeated for other combinations of variables to create multivariate plots.



######## Issue 4: Preprocessing and Data Transformation

# Load necessary libraries
library(tidyr)

# Confirmation of the presence of missing values
missing_values <- sum(is.na(Crop_recommendation))
print(paste("Number of Missing Values:", missing_values))

# Data imputation (not applicable but did for practic)
# For simplicity, let's impute missing values with the mean of each column
Crop_recommendation_imputed <- Crop_recommendation %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .))

# Confirm that missing values are imputed
missing_values_after_imputation <- sum(is.na(Crop_recommendation_imputed))
print(paste("Number of Missing Values After Imputation:", missing_values_after_imputation))

# Data transformation (not applicable but did for practice)
# For example, you can log-transform the 'rainfall' variable
Crop_recommendation_transformed <- Crop_recommendation_imputed %>%
  mutate(rainfall_log = log1p(rainfall))

# Display the head of the transformed dataset
print("Head of the Transformed Dataset:")
print(head(Crop_recommendation_transformed))



######## Issue 5 for Milestone 3: Training the Model

# Load necessary libraries
library(caret)

# Data Splitting
set.seed(123)  # For reproducibility
index <- createDataPartition(Crop_recommendation$N, p = 0.8, list = FALSE)
train_data <- Crop_recommendation[index, ]
test_data <- Crop_recommendation[-index, ]

# Bootstrapping (Example for 'N' variable)
bootstrap_sample <- createDataPartition(train_data$N, p = 0.8, list = FALSE)
bootstrapped_data <- train_data[bootstrap_sample, ]

# Cross-validation (Example for 'N' variable)
control <- trainControl(method = "cv", number = 5)
lm_model <- train(N ~ ., data = train_data, method = "lm", trControl = control)

# Model Training (Regression - Example for 'N' variable)
rf_model <- train(N ~ ., data = train_data, method = "rf", trControl = control)

# Model Performance Comparison using resamples
models_list <- list(LinearModel = lm_model, RandomForest = rf_model)
resamples <- resamples(models_list)

# Print summary
summary(resamples)




######## Issue 6 for Milestone 4: Hyper-Parameter Tuning and Ensembles

# Load necessary libraries
library(randomForest)

# Hyperparameter Tuning using Random Search (Example for 'N' variable)
set.seed(123)  # For reproducibility
index <- createDataPartition(Crop_recommendation$N, p = 0.8, list = FALSE)
train_data <- Crop_recommendation[index, ]
test_data <- Crop_recommendation[-index, ]

# Define the hyperparameter grid
hyperparameter_grid <- expand.grid(mtry = seq(1, 7, by = 1))

# Random Search
tune_control <- trainControl(method = "cv", number = 5)
tuned_model <- train(N ~ ., data = train_data, method = "rf", trControl = tune_control, tuneGrid = hyperparameter_grid)

# Print the best tuning parameters
print("Best Tuning Parameters:")
print(tuned_model$bestTune)

# Ensemble (Random Forest)
# Train multiple models with different random seeds
ensemble_models <- lapply(1:5, function(seed) {
  set.seed(seed)
  train(N ~ ., data = train_data, method = "rf", trControl = tune_control, tuneGrid = hyperparameter_grid)
})

# Combine models into an ensemble manually
ensemble_predict <- function(models, newdata) {
  predictions <- lapply(models, function(model) predict(model, newdata))
  ensemble_prediction <- rowMeans(do.call(cbind, predictions))
  return(ensemble_prediction)
}

# Apply ensemble_predict function to your models
ensemble_predictions <- ensemble_predict(ensemble_models, test_data)

# Print ensemble predictions
print("Ensemble Predictions:")
print(ensemble_predictions)


######## Issue 7 for Milestone 5: Consolidation


