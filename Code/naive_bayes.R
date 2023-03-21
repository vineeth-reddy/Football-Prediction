library(tidyverse)
setwd("/Users/vineethreddy/Documents/projects")

df <- read.csv("matches.csv")
df <- df[,c("home_team_fifa_rank","away_team_fifa_rank","home_team_total_fifa_points","away_team_total_fifa_points","home_team_score","away_team_score","home_team_result")]

# Splitting dataset into training and testing sets
library(caret)
set.seed(321)
trainIndex <- createDataPartition(df$home_team_result, p = 0.7, list = FALSE)
training <- df[trainIndex,]
testing <- df[-trainIndex,]

training$home_team_result <- factor(training$home_team_result)
testing$home_team_result <- factor(testing$home_team_result)
# Fitting Naive Bayes Model
library(e1071)
#model <- naiveBayes(home_team_result ~ ., data = training)

# laplace smoothing
model <- naiveBayes(home_team_result ~ ., data = training,laplace=1)
# Making predictions on testing set
predictions <- predict(model, newdata = testing)

confusionMatrix(predictions, testing$home_team_result)

nb_cm<- confusionMatrix(predictions, testing$home_team_result)


library(ggplot2)
ggplot(data = as.data.frame(nb_cm$table), aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = Freq), size = 15, color = "black") +
  labs(title = "Naive Bayes Confusion Matrix",
       x = "Actual", y = "Predicted")
