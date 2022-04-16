library(readr)
library(e1071)
library(M3C)
library(plotly)
library(adabag)
library(cluster)
library(fpc)
library(factoextra)
library(tsne)
library(glmnet)


indian_data <- read.csv("Indian Liver Patient Dataset (ILPD).csv", stringsAsFactors = TRUE)
indian_data$Gender <- as.numeric(as.factor(indian_data$Gender))
indian_data <- unique(indian_data)
indian_data[] <- lapply(indian_data_train, factor)
indian_data
indian_data_nrow <- nrow(indian_data)
indian_data_ind <- sample(seq_len(indian_data_nrow), size = round(indian_data_nrow * .8))
indian_data_train <- indian_data[indian_data_ind, ]
indian_data_test <- indian_data[-indian_data_ind, ]

f <- subset(indian_data, select = -c(Is_pacient))
tsne <- data.frame(tsne(na.omit(f), initial_dims = 2))
pdb <- cbind(tsne, na.omit(indian_data)$Is_pacient)
fig <- plot_ly(data = pdb, x = ~X1, y = ~X2, type = 'scatter', mode = 'markers', split = na.omit(indian_data)$Is_pacient)
fig

# Naive bayes

model_naive_bayes <- naiveBayes(Is_pacient ~ ., data = indian_data_train)
model_naive_bayes_predict <- predict(model_naive_bayes, indian_data_test)

naive_bayes_t <- table(model_naive_bayes_predict, indian_data_test$Is_pacient)
naive_bayes_error <- 1 - (sum(diag(naive_bayes_t))/sum(naive_bayes_t))
print(naive_bayes_t)
print(naive_bayes_error, digits = 4)


# Bagging 
indian_data_test$Is_pacient <- as.factor(indian_data_test$Is_pacient)
indian_data_train$Is_pacient <- as.factor(indian_data_train$Is_pacient)

df_bagging <- data.frame()
mfinal <- seq(1, 30, 1)
for (i in mfinal) {
  print(i)
  model_bagging <- bagging(Is_pacient ~., data = indian_data_train, mfinal = i, maxdepth = 5)
  pred_bagging_test <- predict.bagging(model_bagging, newdata = indian_data_test)
  df_bagging <- rbind(df_bagging, list(i, pred_bagging_test$error))
}
colnames(df_bagging) <- c('mfinal', 'test')

fig_bagging <- plot_ly(df_bagging, x = ~mfinal)
fig_bagging <- fig_bagging %>% add_trace(y=~test, mode = 'lines+markers', name = 'test')
fig_bagging <- fig_bagging %>% layout(xaxis = list(title = "Number of trees"), yaxis = list(title = "Error"))
fig_bagging

best_number_tree <- mfinal[unlist(which.min(df_bagging$test))]
print(best_number_tree)

best_model_bagging <- bagging(Is_pacient ~., data = indian_data_train, mfinal = best_number_tree, maxdepth = 5)
best_pred_bagging <- predict.bagging(best_model_bagging, newdata = indian_data_test)
print(best_pred_bagging$confusion)
print(best_pred_bagging$error, digits = 4)


# Boosting

df_boosting <- data.frame()
mfinal <- seq(1, 30, 1)
for (i in mfinal) {
  print(i)
  model_boosting <- boosting(Is_pacient ~., data = indian_data_train, mfinal = i, 
                             maxdepth = 5)
  pred_boosting_test <- predict.boosting(model_boosting, newdata = indian_data_test)
  df_boosting <- rbind(df_boosting, list(i, pred_boosting_test$error))
}
colnames(df_boosting) <- c('mfinal', 'test')

fig_bbosting <- plot_ly(df_boosting, x = ~mfinal)
fig_bbosting <- fig_bbosting %>% add_trace(y=~test, mode = 'lines+markers', name = 'test')
fig_bbosting <- fig_bbosting %>% layout(xaxis = list(title = "Number of trees"), 
                                        yaxis = list(title = "Error"))
fig_bbosting

best_number_tree <- mfinal[unlist(which.min(df_boosting$test))]

best_model_boosting <- boosting(Is_pacient ~., data = indian_data_train, mfinal = best_number_tree, maxdepth = 5)
best_pred_boosting <- predict.boosting(best_model_boosting, newdata = indian_data_test)
print(best_pred_boosting$confusion)
print(best_pred_boosting$error, digits = 4)


# K-means
indian_new <- subset(indian_data, select = -c(Is_pacient))
indian_new[is.na(indian_new)] = 0

indian_kmeans <- kmeans(indian_new, 2)

t <- table(indian_kmeans$cluster, indian_data$Is_pacient)
error <- 1 - sum(diag(t)) / sum(t)
print(t)
print(error, digits = 4)

indian_data[is.na(indian_data)] = 0
indian_kmeans <- kmeans(indian_data, 2)
indian_kmeans$cluster
t <- table(indian_kmeans$cluster, indian_data$Is_pacient)
error <- 1 - sum(diag(t)) / sum(t)
print(t)
print(error, digits = 4)


# lasso
x <- as.matrix(indian_data[,-11])
indian_glm <- cv.glmnet(x,indian_data[, 11] , family = "binomial", alpha = 1)
min(indian_glm$lambda)
coef(indian_glm, s = min(indian_glm$lambda))


# pca 
new_data <- as.matrix(indian_data[,-11])
pca <- prcomp(na.omit(new_data))

pca_data <- pca$x[, c(4, 8, 9, 10)]
pca_data <- data.frame(pca_data, Is_pacient = indian_data[,11])
pca_data_nrow <- nrow(pca_data)
pca_data_ind <- sample(seq_len(pca_data_nrow), size = round(pca_data_nrow * .8))
pca_data_train <- indian_data[pca_data_ind, ]
pca_data_test <- indian_data[-pca_data_ind, ]


model_naive_bayes <- naiveBayes(Is_pacient ~ ., data = pca_data_train)
model_naive_bayes_predict <- predict(model_naive_bayes, pca_data_test)

naive_bayes_t <- table(model_naive_bayes_predict, pca_data_test$Is_pacient)
naive_bayes_error <- 1 - (sum(diag(naive_bayes_t))/sum(naive_bayes_t))
print(naive_bayes_t)
print(naive_bayes_error, digits = 4)


pca_data_train$Is_pacient <- as.factor(pca_data_train$Is_pacient)
pca_data_test$Is_pacient <- as.factor(pca_data_test$Is_pacient)

best_model_bagging <- bagging(Is_pacient ~., data = pca_data_train, mfinal = 3, maxdepth = 5)
best_pred_bagging <- predict.bagging(best_model_bagging, newdata = pca_data_test)
print(best_pred_bagging$confusion)
print(best_pred_bagging$error, digits = 4)


best_model_boosting <- boosting(Is_pacient ~., data = pca_data_train, mfinal = 27, maxdepth = 5)
best_pred_boosting <- predict.boosting(best_model_boosting, newdata = pca_data_test)
print(best_pred_boosting$confusion)
print(best_pred_boosting$error, digits = 4)
