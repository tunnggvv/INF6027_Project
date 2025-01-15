getwd()
setwd('/Users/a88698/Desktop/INF6027 Introduction to data science/report')

dataset <- read.csv("thefinaldataset.csv")
dataset$explicit <- as.factor(dataset$explicit)
dataset$track_genre <- as.factor(dataset$track_genre)
dataset$speech_category <- factor(dataset$speech_category, levels = c("music", "both", "words"))
dataset$mood_category <- as.factor(dataset$mood_category)

#PCA
numeric_data <- dataset[, c("danceability", "tempo", "duration_ms", "acousticness")]
numeric_data_scaled <- scale(numeric_data)

install.packages("car", type = "binary")
install.packages("leaps", type = "binary")
install.packages('FactoMineR')
library(FactoMineR)
pca_result <- PCA(numeric_data_scaled, graph = FALSE)
summary(pca_result)

install.packages("factoextra")
library(factoextra)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))
fviz_pca_var(pca_result, col.var = "contrib", 
             gradient.cols = c("steelblue", "orange", "darkgreen"),
             repel = TRUE)# Avoid overlapping labels

#random forest for feature importance
set.seed(123)
library(caTools)
split <- sample.split(dataset$popularity, SplitRatio = 0.7)
train <- subset(dataset, split == TRUE)
test <- subset(dataset, split == FALSE)

install.packages("randomForest", type = "binary")
library(randomForest)
str(train$popularity)

# Fit the model
rf_model <- randomForest(popularity ~ ., data = train, importance = TRUE)

# View feature importance
importance <- importance(rf_model)
print(importance)
varImpPlot(rf_model)

