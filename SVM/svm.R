library(e1071)
library(tidyverse)


area.pallete = function(n = 2) {
  cols = rainbow(n)
  cols[1:2] = c("PaleGreen", "Pink")
  return(cols)
}
symbols.pallete = c("White", "Red")



# 1

svm1_train <- read.table("svmdata1.txt", stringsAsFactors = TRUE)
svm1_test <- read.table("svmdata1test.txt", stringsAsFactors = TRUE)

svm1_model <- svm(Color~., data = svm1_train, type = "C-classification", cost = 1, kernel = "linear")
svm1_model$tot.nSV

plot(svm1_model, svm1_train, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)

svm1_predict <- predict(svm1_model, svm1_test)

table(svm1_model$fitted, svm1_train$Color)
table(svm1_predict, svm1_test$Color)


# 2
svm2_train <- read.table("svmdata2.txt", stringsAsFactors = TRUE)
svm2_test <- read.table("svmdata2test.txt", stringsAsFactors = TRUE)

for (C in c(1, 50, 100, 200)) {
  svm2_model_train <- svm(Colors~., data = svm2_train, type = "C-classification", cost = C, kernel = "linear")
  t <- table(svm2_model_train$fitted, svm2_train$Colors)
  print(C)
  print(t)
}

for (C in c(1, 50, 100, 200)) {
  svm2_model_test <- svm(Colors~., data = svm2_train, type = "C-classification", cost = C, kernel = "linear")
  table(predict(svm2_model_test, svm2_test), svm2_test$Colors)  
  print(C)
  print(t)
}

svm2_model <- svm(Colors~., data = svm2_train, type = "C-classification", cost = 200, kernel = "linear")
plot(svm2_model, svm2_train, grid = 250, symbolPalette = symbols.pallete, color.palette = area.pallete)

# 3  
svm3 <- read.table("svmdata3.txt", stringsAsFactors = TRUE)

n3 <- nrow(svm3)
indicates3 <- sample(1:n3, .7 * n3)
svm3_train <- svm3[indicates3, ]
svm3_test <- svm3[-indicates3, ]

for (d in c(1, 2, 3, 4, 5, 6)) {
  svm3_model_poly <- svm(Colors~., data = svm3_train, kernel = "polynomial", degree = d)
  svm3_predict_poly <- predict(svm3_model_poly, svm3_test)
  print('polynomial')
  print(d)
  print(table(svm3_predict_poly, svm3_test$Colors))
}

svm3_model_rad <- svm(Colors~., data = svm3_train, kernel = "radial")
svm3_predict_rad <- predict(svm3_model_rad, svm3_test)
print("radial")
print(table(svm3_predict_poly, svm3_test$Colors))

svm3_model_sig <- svm(Colors~., data = svm3_train, kernel = "sigmoid")
svm3_predict_sig <- predict(svm3_model_sig, svm3_test)
print("sigmoid")
print(table(svm3_predict_sig, svm3_test$Colors))

svm3_model_poly2 <- svm(Colors~., data = svm3_train, kernel = "polynomial", degree = 2)
plot(svm3_model_poly2, svm3_train, symbolPalette = symbols.pallete, color.palette = area.pallete)

# 4
svm4_train <- read.table("svmdata4.txt", stringsAsFactors = TRUE)
svm4_test <- read.table("svmdata4test.txt", stringsAsFactors = TRUE)

svm4_model_poly1 <- svm(Colors~., data = svm4_train, kernel = "polynomial", degree = 1)
svm4_predict_poly1 <- predict(svm4_model_poly1, svm4_test)
print("polynomial")
print(1)
print(table(svm4_predict_poly1, svm4_test$Colors))

svm4_model_poly2 <- svm(Colors~., data = svm4_train, kernel = "polynomial", degree = 2)
svm4_predict_poly2 <- predict(svm4_model_poly2, svm4_test)
print("polynomial")
print(2)
print(table(svm4_predict_poly2, svm4_test$Colors))

svm4_model_rad <- svm(Colors~., data = svm4_train, kernel = "radial")
svm4_predict_rad <- predict(svm4_model_rad, svm4_test)
print("radial")
print(table(svm4_predict_rad, svm4_test$Colors))

svm4_model_sig <- svm(Colors~., data = svm4_train, kernel = "sigmoid")
svm4_predict_sig <- predict(svm4_model_sig, svm4_test)
print("sigmoid")
print(table(svm4_predict_sig, svm4_test$Colors))

plot(svm4_model_rad, svm4_train, symbolPalette = symbols.pallete, color.palette = area.pallete)


# 5
svm5_train <- read.table("svmdata5.txt", stringsAsFactors = TRUE)
svm5_test <- read.table("svmdata5test.txt", stringsAsFactors = TRUE)

svm5_params <- expand.grid(kernel = c("polynomial", "radial", "sigmoid"), gamma = c(0.01, 0.1, 1, 10, 100))
mapply(function(kernel, gamma) {
  svm5_model <- svm(Colors ~., data = svm5_train, kernel = kernel, gamma = gamma, degree = 2)
  print(plot(svm5_model, svm5_train, symbolPalette = symbols.pallete, color.palette = area.pallete, main = 'scsc'))
  svm5_predict <- predict(svm5_model, svm5_test)
  t <- table(svm5_predict, svm5_test$Colors)
  print(kernel)
  print(gamma)
  print(t)
}, svm5_params$kernel, svm5_params$gamma)



# 6
svm6 <- read.table("svmdata6.txt", stringsAsFactors = TRUE)
plot(svm6$X, svm6$Y)
svm6_model <- svm(Y ~ X, data = svm6, type = "eps-regression", kernel = "radial")
points(svm6$X, svm6$Y, col = 'red')
predictions = predict(svm6_model, svm6)
lines(svm6$X, predictions, col="dodgerblue", lwd = 2)
lines(svm6$X, predictions + svm6_model$epsilon, col = "cyan")
lines(svm6$X, predictions - svm6_model$epsilon, col = "cyan")


svm6_stats <- data.frame(epsilon = .01 * 1:20)
svm6_stats$sd <- svm6_stats$epsilon %>% lapply(function(epsilon) {
  svm6_predicted <- svm(Y ~ X, data = svm6, type = "eps-regression", kernel = "radial", epsilon = epsilon)$fitted
  sd(svm6_predicted - svm6$Y)
})

plot(svm6_stats$epsilon, svm6_stats$sd, type = "o", xlab = 'epsilon', ylab = '—редн€€ ошибка')
