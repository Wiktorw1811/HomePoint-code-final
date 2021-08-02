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

expectancy_train <- LifeExpectancy[train_index,]
expectancy_test <- LifeExpectancy[-train_index,]

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

svr_fit <- train(
  `Life expectancy` ~ ., 
  data = expectancy_train, 
  method = 'svmRadialCost',
  metric = 'MAE',
  tuneLength = 100,
  allowParallel = TRUE,
  trControl = fit_control
)

svr_time <- proc.time() - ptm

summary(svr_fit)


svr_predictions <- predict(svr_fit, newdata = expectancy_test)
svr_mae <- MAE(svr_predictions, expectancy_test$`Life expectancy`)
svr_mape <- MAPE(svr_predictions, expectancy_test$`Life expectancy`)

svr_mae
svr_mape*100
svr_predictions

svr_fit



