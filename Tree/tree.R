library(mlbench)
library(tree)
library(maptree)
library(DAAG)
library(rpart)
library(rpart.plot)


# 1
data(Glass)
gl.tree <- tree(Type ~., Glass)
draw.tree(gl.tree, cex=0.7)
gl.tree1 <- snip.tree(gl.tree)
draw.tree(gl.tree1)
plot(gl.tree1)
text(gl.tree1)

# 2
data(spam7)
sp.tree <- tree(yesno ~., spam7)
draw.tree(sp.tree, cex=0.7)
sp.prune <- cv.tree(sp.tree,, prune.tree)
plot(sp.prune)
k <- sp.prune$k[-1]
for (i in 1:length(k))
  t <- prune.tree(sp.tree, k = k[i])
  draw.tree(t, cex=0.7)
t <- prune.tree(sp.tree, k = 67.724)
draw.tree(t, cex=0.7)
t <- prune.tree(sp.tree, k = 69.74901)
draw.tree(t, cex=0.7)
t <- prune.tree(sp.tree, k = 79.50991)
draw.tree(t, cex=0.7)
t <- prune.tree(sp.tree, k = 158.29894)
draw.tree(t, cex=0.7)
t <- prune.tree(sp.tree, k = 214.40057)
draw.tree(t, cex=0.7)
t <- prune.tree(sp.tree, k = 746.81961)
draw.tree(t, cex=0.7)
t <- prune.tree(sp.tree, k = 1565.46721)
draw.tree(t, cex=0.7)


# 3
data(nsw74psid1)
sp.tree <- tree(re78 ~., nsw74psid1)
draw.tree(sp.tree, cex=0.7)

nsw.rpart <- rpart(re78 ~ ., data = nsw74psid1, cp = 0.001)
plotcp(nsw.rpart)
nsw.rpart <- prune(nsw.rpart, cp = 0.002)
printcp(nsw.rpart)


# 4

lenses <- read.table('Lenses.txt', header=FALSE)
lenses <- lenses[, -1]
names(lenses) <- c('age', 'vision', 'astigmatism', 'tear', 'lense')
lenses$age[lenses$age == 1] = 'young'
lenses$age[lenses$age == 2] = 'pre-aging'
lenses$age[lenses$age == 3] = 'senile'
lenses$vision[lenses$vision == 1] = 'myopia'
lenses$vision[lenses$vision == 2] = 'farsightedness'
lenses$astigmatism[lenses$astigmatism == 1] = 'no'
lenses$astigmatism[lenses$astigmatism == 2] = 'yes'
lenses$tear[lenses$tear == 1] = 'abbreviated'
lenses$tear[lenses$tear == 2] = 'normal'
lenses$lense[lenses$lense == 1] = 'hard'
lenses$lense[lenses$lense == 2] = 'soft'
lenses$lense[lenses$lense == 3] = 'no'
lenses$age = as.factor(lenses$age)
lenses$vision = as.factor(lenses$vision)
lenses$astigmatism = as.factor(lenses$astigmatism)
lenses$tear = as.factor(lenses$tear)
lenses$lense = as.factor(lenses$lense)
lenses
lenses.tree <- tree(lense ~ ., lenses)
draw.tree(lenses.tree)

subset(lenses, age == 'pre-aging')
subset(lenses, vision == 'myopia')
subset(lenses, astigmatism == 'yes')
subset(lenses, tear == 'abbreviated')


# 5

svm_data <- read.table("svmdata4.txt", stringsAsFactors = TRUE)
svm_test <- read.table("svmdata4test.txt", stringsAsFactors = TRUE)
svm_fit <- rpart(Colors ~ ., data = svm_data, method = 'class')
rpart.plot(svm_fit)
svm_predict <- predict(svm_fit, svm_test, type = 'class')
table(svm_test$Colors, svm_predict)

# 6
data(ptitanic)
create_train_test <- function(data, size = 0.8, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- 1: total_row
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}
data_train <- create_train_test(ptitanic, 0.8, train = TRUE)
data_test <- create_train_test(ptitanic, 0.8, train = FALSE)
titanic_fit <- rpart(survived ~ ., data = data_train, method = 'class')
rpart.plot(titanic_fit)
titanic_predict <- predict(titanic_fit, data_test, type = 'class')
table(data_test$survived, titanic_predict)