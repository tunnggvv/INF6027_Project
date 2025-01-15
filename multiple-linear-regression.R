getwd()
setwd('/Users/a88698/Desktop/INF6027 Introduction to data science/report')

dataset <- read.csv("thefinaldataset.csv")

# Perform chi-square test for categorical variables
categorical_variables <- c("speech_category","explicit", 
                           "track_genre","mood_category")
table(dataset$mood_category, dataset$popularity)

for (var in categorical_variables) {
  chi_test <- chisq.test(table(dataset[[var]], dataset$popularity))
  print(paste("Chi-square test for", var, "and popularity"))
  print(chi_test)
}
chisq.test(table(dataset$mood_category, dataset$popularity), simulate.p.value = TRUE)

# Load required libraries
library(tidyverse)
library(caTools)  # For splitting data

# Ensure that all numeric variables are properly formatted
dataset <- dataset %>%
  mutate(across(where(is.character), as.factor))

# Scale numeric independent variables
numeric_columns <- c("danceability", "tempo", "duration_ms", "acousticness")
dataset[numeric_columns] <- scale(dataset[numeric_columns])

# Split dataset into training and testing sets (70-30 split)
set.seed(123)  # For reproducibility
split <- sample.split(dataset$popularity, SplitRatio = 0.7)
train_data <- subset(dataset, split == TRUE)
test_data <- subset(dataset, split == FALSE)

# Fit a multiple linear regression model on the training set
linear_model <- lm(popularity ~ danceability + tempo + duration_ms + acousticness + 
                     speech_category + explicit + mood_category + track_genre, 
                   data = train_data)

# Summary of the model
summary(linear_model)

# Make predictions on the test data
predictions <- predict(linear_model, newdata = test_data)

# Compare predictions to actual values
results <- data.frame(Actual = test_data$popularity, Predicted = predictions)
print(head(results))

# Evaluate the model's performance (e.g., RMSE)
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
cat("Root Mean Squared Error (RMSE):", rmse, "\n")

