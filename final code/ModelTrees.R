library(caret)
library(dplyr)
library(parallel)
library(doParallel)
library(gbm)
library(earth)
library(fastDummies)
library(mlbench)
library(MLmetrics)
library(tidyverse)
library(rpart.plot)

install.packages("rpart.plot")


#install.packages("dplyr")

num_cores <- 4
cluster <- makePSOCKcluster(num_cores)
registerDoParallel(cluster)

getwd()

LifeExpectancy <- read_csv('/Users/wikto/OneDrive/Desktop/FYP/FYP/Life Expectancy Data Only.csv')

set.seed(42)
train_index <- createDataPartition(
  LifeExpectancy$`Life expectancy`, 
  p = 0.75,
  list = FALSE
)


#expectancy_train <- LifeExpectancy[train_index,]
#expectancy_test <- LifeExpectancy[-train_index,]

expectancy_train <- LifeExpectancy[LifeExpectancy$Year <= 2009,]
expectancy_test <- LifeExpectancy[LifeExpectancy$Year > 2009,]


## Model training of the Model Trees

repeats = 5
folds = 10

fit_control <- trainControl(
  method = 'repeatedcv',
  number = folds,
  repeats = repeats,
  search = 'random'
)

ptm <- proc.time()

model_tree_fit <- train(
  `Life expectancy` ~ ., 
  data = expectancy_train, 
  method = 'cubist',
  metric = 'MAE',
  tuneLength = 40,
  allowParallel = TRUE,
  trControl = fit_control
)

model_tree_time <- proc.time() - ptm

model_tree_time

model_tree_fit

model_tree_predictions <- predict(model_tree_fit, newdata = expectancy_test)
model_tree_mae <- MAE(model_tree_predictions, expectancy_test$`Life expectancy`)
model_tree_mape <- MAPE(model_tree_predictions, expectancy_test$`Life expectancy`)


predictions <- predict(model_tree_fit, newdata = expectancy_train)
MAE(predictions, expectancy_train$`Life expectancy`)

max(expectancy_test$`Life expectancy`) - min(expectancy_test$`Life expectancy`)

plot(model_tree_fit)


model_tree_fit$bestTune


model_tree_mae


model_tree_predictions

model_tree_mape*100


view(LifeExpectancy)


plot(expectancy_test$`Life expectancy`, model_tree_predictions)




plot(expectancy_test$`Life expectancy`, expectancy_test$`Life expectancy` - model_tree_predictions)

plot(model_tree_predictions, expectancy_test$`Life expectancy`)


#actuals vs residuals plot
plot(expectancy_test$`Life expectancy`, model_tree_predictions)


####Comparing SVR to model trees

svr_mape*100

model_tree_mape*100

svr_mae

model_tree_mae





                         