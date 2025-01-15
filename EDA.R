getwd()
setwd('/Users/a88698/Desktop/INF6027 Introduction to data science/report')

# Load required libraries
library(dplyr)
library(ggplot2)
library(corrplot)
library(reshape2)
library(GGally)
library(tidyr)
# Load the dataset
dataset <- read.csv("thefinaldataset.csv")
dataset$explicit <- as.factor(dataset$explicit)
dataset$track_genre <- factor(dataset$track_genre, levels = c("chill", "emo", "k-pop", "mandopop", "r-n-b", "world-music"))
dataset$speech_category <- factor(dataset$speech_category, levels = c("music", "both", "words"))
dataset$mood_category <- as.factor(dataset$mood_category)
# View the structure of the dataset
str(dataset)
# View the first few rows
head(dataset)
# Select only numeric variables
numeric_data <- dataset %>%
  select_if(is.numeric)
# Reshape data for combined histogram using pivot_longer
melted_data <- numeric_data %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Ensure 'popularity' is numeric
dataset$popularity <- as.numeric(dataset$popularity)

# Select numeric variables
numeric_data <- dataset %>%
  select(popularity, danceability, tempo, duration_ms, acousticness)
# Load necessary libraries
library(reshape2)
library(ggplot2)

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
# Melt the correlation matrix
cor_long <- melt(cor_matrix)

# Create the correlation heatmap with annotations
ggplot(cor_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Add white gridlines
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +  # Add correlation values
  scale_fill_gradient2(low = "purple", high = "pink", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Correlation") +
  theme_minimal() +  # Use minimal theme for clarity
  labs(title = "Correlation Matrix for Popularity",
       x = NULL, y = NULL) +  # Remove axis labels for a cleaner look
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10)) +
  coord_fixed()  # Ensure cells are square

# Combine all numeric variables into one long-format data frame
numeric_columns <- dataset %>%
  select(where(is.numeric))

# Exclude 'popularity' when creating melted_data
melted_data <- dataset %>%
  select(where(is.numeric), -popularity) %>% # Exclude 'popularity'
  pivot_longer(
    cols = everything(),
    names_to = "Variable", # New column for variable names
    values_to = "Value"    # New column for variable values
  )

# Create boxplots
ggplot(melted_data, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(color = "black") +
  labs(
    title = "Boxplots of Numeric Variables",
    x = "Variables",
    y = "Values"
  ) +
  scale_fill_manual(
    values = c("acousticness" = "orange", 
               "danceability" = "lightblue", 
               "duration_ms" = "lightgreen", 
               "tempo" = "pink")  # Customize each variable's color
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",                      # Remove legend
    axis.text.x = element_text(angle = 45, hjust = 1) # Rotate x-axis labels
  )

