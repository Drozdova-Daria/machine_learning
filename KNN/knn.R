library(e1071)
library(kernlab)
library(mlbench)
library(kknn)


tic_tac_toe <- function(data_rand, n, percent) {
  
  nt <- as.integer(n*percent)
  A_train <- data_rand[1:nt, ]
  A_test <- data_rand[(nt+1):n, ]
  
  prop.table(table(A_train$V10))
  prop.table(table(A_test$V10))
  
  A_classifier <- naiveBayes(V10 ~ ., data = A_train)
  A_predicted <- predict(A_classifier, A_test)
  t <- table(A_predicted, A_test$V10)
  
  return ((t[2] + t[3]) / (sum(t)))
}

spam_data <- function(spam, percent) {
  count <- as.integer(dim(spam)[1] * percent)
  idx <- sample(1:dim(spam)[1], count)
  spamtrain <- spam[-idx, ]
  spamtest <- spam[idx, ]
  model <- naiveBayes(type ~ ., data = spamtrain)
  predict(model, spamtest)
  t <- table(predict(model, spamtest), spamtest$type)
  
  return ((t[2] + t[3]) / (sum(t)))
}

# 1 
tic_tac <- read.table("Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
n <- dim(tic_tac)[1]
percent <- seq(0.1, 0.9, by = .1)
set.seed(12345)
A_rand <- tic_tac[ order(runif(n)), ]
y = list()
for (x in percent) {
  t <- tic_tac_toe(A_rand, n, x)
  y[length(y) + 1] <- t
}
plot(percent, y, type = "o", xlab='Доля обучающих данных', ylab='Доля ошибочно классифицированных данных', col="blue")
grid(10, 5)

data(spam)
y = list()
for (x in percent) {
  t <- spam_data(spam, x)
  y[length(y) + 1] <- t
}
plot(percent, y, type = "o", xlab='Доля обучающих данных', ylab='Доля ошибочно классифицированных данных', col="blue")
grid(10, 5)


# 2

data(Glass)
dim(Glass)
Glass[0,]
glass <- Glass[,-1]

glass.learn <- glass[1:200,]
glass.valid <- glass[-c(1:200),]
fit.kknn <- kknn(Type ~ ., glass.learn, glass.valid)

fit.train1 <- train.kknn(Type ~ ., glass.learn, kmax = 15,kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 1)
fit.train2 <- train.kknn(Type ~ ., glass.learn, kmax = 15, kernel = c("triangular", "rectangular", "epanechnikov", "optimal"), distance = 2)

summary(fit.train1)
summary(fit.train2)

plot(fit.train1)
plot(fit.train2)

example <- data.frame(Na = c(11.7), 
                      Mg = c(1.01), 
                      Al = c(1.19), 
                      Si = c(72.59), 
                      K = c(0.43), 
                      Ca = c(11.44), 
                      Ba = c(0.02), 
                      Fe = c(0.1) )
kknn.example <- kknn(Type ~ ., glass.learn, example, distance = 1, kernel = "triangular")
summary(kknn.example)


# 3

svm_data <- read.table("svmdata4.txt", stringsAsFactors = TRUE)
svm_test <- read.table("svmdata4test.txt", stringsAsFactors = TRUE)
plot(svm_data$X1, svm_data$X2, pch=21, bg=c("red","blue") [unclass(svm_data$Colors)],  main="My train data")
train.kknn(formula = Colors ~ ., data = svm_data, kmax = 15, distance = 1, kernel = c("triangular", "rectangular", "epanechnikov", "optimal"))

# 4

titanic.train <- read.csv(file = "train.csv", header = TRUE)
titanic.test <- read.csv(file = "test.csv", header = TRUE)
fit.kknn <- kknn(Sex ~ ., titanic.train, kmax = 15, distance = 1, kernel = c("triangular", "rectangular", "epanechnikov", "optimal"))
summary(fit.kknn)