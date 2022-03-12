library(e1071)
library(dplyr)
library(kernlab)



tic_tac_toe <- function(data_rand, n, percent) {
  
  nt <- as.integer(n*percent)
  A_train <- data_rand[1:nt, ]
  A_test <- data_rand[(nt+1):n, ]
  
  prop.table(table(A_train$V10))
  prop.table(table(A_test$V10))
  
  A_classifier <- naiveBayes(V10 ~ ., data = A_train)
  A_predicted <- predict(A_classifier, A_test)
  table(A_predicted, A_test$V10)
}

spam_data <- function(spam, count) {
  idx <- sample(1:dim(spam)[1], count)
  spamtrain <- spam[-idx, ]
  print(dim(spamtrain))
  spamtest <- spam[idx, ]
  model <- naiveBayes(type ~ ., data = spamtrain)
  predict(model, spamtest)
  table(predict(model, spamtest), spamtest$type)
}

# 1 
tic_tac <- read.table("Tic_tac_toe.txt", sep = ",", stringsAsFactors = TRUE)
n <- dim(tic_tac)[1]
set.seed(12345)
A_rand <- tic_tac[ order(runif(n)), ]
tic_tac_toe(A_rand, n, 0.8)
tic_tac_toe(A_rand, n, 0.5)
tic_tac_toe(A_rand, n, 0.2)

data(spam)
print(dim(spam))
spam_data(spam, 1000)
spam_data(spam, 2300)
spam_data(spam, 3000)

# 2

x <- seq(10.1, 15, by = .1)
length(x)
X1_ <- dnorm(x, mean = 10, sd = 4)
X2_ <- dnorm(x, mean = 14, sd = 4)
X1 <- dnorm(x, mean = 20, sd = 3)
X2 <- dnorm(x, mean = 18, sd = 3)


df1 <- data.frame('X1' = X1_, 'X2'= X2_, class=2)
df2 <- data.frame('X1' = X1, 'X2'= X2, class=1)
df <- rbind(df1, df2)

plot(df$X1, df$X2, pch=21, bg=c("red","blue") [unclass(df$class)])

n <- dim(df)[1]
set.seed(12345)
df_rand <- df[order(runif(n)), ]
nt <- as.integer(n * 0.5)
df_train <- df_rand[1:nt, ]
df_test <- df_rand[(nt+1):n, ]
prop.table(table(df_train$class))
prop.table(table(df_test$class))
df_classifier <- naiveBayes(class ~ ., data = df_train)
df_predicted <- predict(df_classifier, df_test)
table(df_predicted, df_test$class)


# 3

titanic.train <- read.csv(file = "train.csv", header = TRUE)
test <- read.csv(file = "test.csv", header = TRUE)
survived <- read.csv(file = 'gender_submission.csv', header = TRUE)
titanic.test <-merge(survived, test)

prop.table(table(titanic.train$Survived))
prop.table(table(titanic.test$Survived))
titanic_classifier <- naiveBayes(Survived ~ ., data = titanic.train)
titanic_predicted <- predict(titanic_classifier, titanic.test)
table(titanic_predicted, titanic.test$Survived)
