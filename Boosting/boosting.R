library(adabag)
library(mlbench)
library(rpart)
library(tidyverse)
library(types)
library(dplyr)
library(pryr)
library(kknn)
library(tree)
library(maptree)
library(caret)

# 1
data(Vehicle)
n_samples <- nrow(Vehicle)
train_indices <- sample(1:n_samples, .7 * n_samples)
test_indices <- -train_indices
maxdepth <- 5

adaboost_errors <- data.frame(n_trees = numeric(0), error_train = numeric(0), error_test = numeric(0))
for (max_iter in seq(from = 1, to = 301, by = 10)) {
  print(max_iter)
  Vehicle.adaboost <- boosting(Class ~ ., data = Vehicle[train_indices,], mfinal = max_iter, maxdepth = maxdepth)
  pred_test <- predict.boosting(Vehicle.adaboost, newdata = Vehicle[test_indices,])
  pred_train <- predict.boosting(Vehicle.adaboost, newdata = Vehicle[train_indices,])
  adaboost_errors[nrow(adaboost_errors) + 1,] <- list(max_iter, pred_train$error, pred_test$error)
}
ggplot(data = adaboost_errors) +
  geom_line(mapping = aes(x = n_trees, y = error_train, color = 'red')) +
  geom_line(mapping = aes(x = n_trees, y = error_test, color = 'blue')) +
  scale_color_discrete(name = "error", labels = c("Train", "Test")) +
  theme_bw()


# 2
data(Glass)
n_samples <- nrow(Glass)
train_indices <- sample(1:n_samples, .7 * n_samples)
test_indices <- -train_indices
maxdepth <- 5

bagging_errors <- data.frame(n_trees = numeric(0), error_train = numeric(0), error_test = numeric(0))
for (max_iter in seq(from = 1, to = 201, by = 10)) {
  print(max_iter)
  Glass.bagging <- bagging(Type ~ ., data = Glass[train_indices,], mfinal = max_iter, maxdepth = maxdepth)
  pred_test <- predict.bagging(Glass.bagging, newdata = Glass[test_indices,])
  pred_train <- predict.bagging(Glass.bagging, newdata = Glass[train_indices,])
  bagging_errors[nrow(bagging_errors) + 1,] <- list(max_iter, pred_train$error, pred_test$error)
}

ggplot(data = bagging_errors) +
  geom_line(mapping = aes(x = n_trees, y = error_train, color = "blue")) +
  geom_line(mapping = aes(x = n_trees, y = error_test, color = "red")) +
  scale_color_discrete(name = "error", labels = c("train", "test")) +
  theme_bw()

# 3

knn_w <- function(target, train, k, w)
  return(list(target = target, train = train, levels = levels(train[, target]), k = k, w = w))

knn_w_predicted <- function(clfier, testdata) {
  n = nrow(testdata)
  pred = rep(NA_character_, n)
  trainlabels = clfier$train[, clfier$target]
  
  train <- clfier$train[, !(names(clfier$train) %in% clfier$target)]
  test <- testdata[, !(names(testdata) %in% clfier$target)]
  
  for (i in 1:n) {
    n_number = order(apply(train, 1, function(x)
      sum((test[i,] - x)^2)))[1:clfier$k]
    
    myfreq <- data.frame(names = clfier$levels,
                         freq = rep(0, length(clfier$levels)))
    for (t in n_number) {
      myfreq[myfreq$names == trainlabels[t], ][2] <- myfreq[myfreq$names == trainlabels[t],][2] + clfier$w[t]
    }
    most_frequent = clfier$levels[myfreq$freq ==
                                    max(myfreq$freq)]
    pred[i] = sample(most_frequent, 1)
  }
  
  factor(pred, levels = levels(trainlabels))
}

knn_boosting <- function(target, data, k = 11, mfinal = 1, ...) {
  n <- nrow(data)
  w <- rep(1/n, each = n)
  
  classifiers <- list()
  
  alphas <- vector()
  for (t in 1:mfinal) {
    clfier <- knn_w(target, train = data, k = k, w)
    knn_predicted <- knn_w_predicted(clfier, data)
    error <- vector()
    for (i in 1:n) {
      if (data[[target]][i] != knn_predicted[i]) 
        error <- append(error, w[i])
    }
    
    if (sum(error) >= 0.5) {
      break()
    }
    
    classifiers[[t]] <- clfier
    alphas[[t]] <- log((1 - sum(error)) / sum(error)) / 2
    for (i in 1:n) {
      if (knn_predicted[i] != data[[target]][i]) {
        w[i] <- w[i]*exp(alphas[[t]])
      } else{
        w[i] <- w[i]*exp(-alphas[[t]])
      }
    }
  }
  
  result <- list()
  
  result$classifiers <- classifiers
  result$alphas <- alphas
  result$levels <- levels(data[, target])
  return(result)
}

boosting_pred <- function(clfier, testdata) {
  n <- nrow(testdata)
  pred = rep(NA_character_, n)
  
  for (i in 1:n) {
    myfreq <- data.frame(names = clfier$levels,
                         freq = rep(0, length(clfier$levels)))
    
    for (j in 1:length(clfier$classifiers)) {
      prediction <- knn_w_predicted(clfier$classifiers[[j]], testdata[i, ])
      myfreq[myfreq$names == prediction, ][2] <- myfreq[myfreq$names == prediction, ][2] +
        clfier$alphas[j]
    }
    
    most_frequent = clfier$levels[myfreq$freq == max(myfreq$freq)]
    pred[i] <- sample(most_frequent, 1)
  }
  factor(pred, levels = clfier$levels)
}

n_samples <- nrow(Glass)
train_indices <- sample(1:n_samples, .7 * n_samples)
test_indices <- -train_indices

boosting <- knn_boosting('Type', Glass[train_indices,], mfinal = 1)
pred <- boosting_pred(boosting, Glass[test_indices, ])
tb <- table(Glass[test_indices, ]$Type, pred)
print(tb)
error <- 1 - sum(diag(tbl_knn)) / sum(tbl_knn)
print(error)

Glass_rpart <- rpart(Type~.,data=Glass[train_indices,])
Glass_rpart_pred <-  predict(Glass_rpart, newdata=Glass[test_indices, ],type="class")
tb <- table(Glass_rpart_pred, Glass$Type[test_indices])
print(tb)
error_rpart <- 1-(sum(diag(tb))/sum(tb))
print(error_rpart)


n_samples <- nrow(Vehicle)
train_indices <- sample(1:n_samples, .7 * n_samples)
test_indices <- -train_indices

boosting <- knn_boosting('Class', Vehicle[train_indices,], mfinal = 1)
pred <- boosting_pred(boosting, Vehicle[test_indices, ])
tb <- table(Vehicle[test_indices, ]$Type, pred)
print(tb)
error <- 1 - sum(diag(tbl_knn)) / sum(tbl_knn)
print(error)

Vehicle_rpart <- rpart(Class~.,data=Vehicle[train_indices,])
Vehicle_rpart_pred <-  predict(Vehicle_rpart, newdata=Vehicle[test_indices, ],type="class")
tb <- table(Vehicle_rpart_pred, Vehicle$Class[test_indices])
print(tb)
error_rpart <- 1-(sum(diag(tb))/sum(tb))
print(error_rpart)
