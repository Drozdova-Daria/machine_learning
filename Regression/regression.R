library(MASS)
library(plotly)
library(tidyr)
library(dplyr)
library(datasets)
library(TSstudio)
library(timetk)
# 1

reg <- read.table("reglab1.txt", stringsAsFactors = TRUE, header = TRUE)

f_qr = lm(z ~., data = reg, method = 'qr')
f_qr
summary(f_qr)

f_frame = lm(z ~., data = reg, method = 'model.frame')
f_frame
summary(f_frame)

# 2

reg2 <- read.table("reglab2.txt", stringsAsFactors = TRUE, header = TRUE)
f <- lm(y ~ x1, reg2)
s <- summary(f)
sum(unlist(lapply(s$residuals, function(x) x^2)))

for (i in 1:4) {
  comb <- combn(c('x1', 'x2', 'x3', 'x4'), i)
  for (j in 1:length(comb[1,])) {
    assign(comb[,j], x1)
    f <- lm(y ~ comb[,j], reg2)
    
  }
}

reg2.features <- colnames(reg2)[2:ncol(reg2)]

devnull <- seq_along(reg2.features) %>% lapply(function(m = ? integer) {
  combn(reg2.features, m) %>% apply(2, function(features_subset = ? character) {
    formula <- paste("y ~", paste(features_subset, collapse = " + "))
    reg2.lm <- lm(formula = formula, reg2)
    reg2.rss <- deviance(reg2.lm)
    cat("formula:", formula, "rss:", round(reg2.rss, 2), "\n")
  })
})

reg2.lm <- lm(y ~ x1 + x2 + x3 + x4, reg2)
summary(reg2.lm)

# 3

cygage <- read.table("cygage.txt", stringsAsFactors = TRUE, header = TRUE)
c <- lm(calAge ~ Depth, cygage)  
c
summary(c)

# 4
data(longley)
long <- lm(Employed ~., longley)
long
summary(long)

new.longley <- longley[,-5]
n <- dim(new.longley)[1]
set.seed(123)
A_rand <- new.longley[ order(runif(n)), ]
nt <- as.integer(n*0.2)
longley_train <- A_rand[1:nt, ]
longley_test <- A_rand[(nt+1):n, ]

l <- sapply(0:25, function(x) 10^(-3 + 0.2 * x))
longley.reg_train <- lm.ridge(GNP.deflator ~., longley_train, lambda = l)
summary(longley.reg_train)
plot(longley.reg_train)
longley.reg_test <- lm.ridge(GNP.deflator ~., longley_test, lambda = l)
summary(longley.reg_test)
plot(longley.reg_test)

# 5
data('EuStockMarkets')
stocks <- as.data.frame(EuStockMarkets) %>%
  gather(index, price) %>%
  mutate(time = rep(time(EuStockMarkets), 4))
plot_ly(stocks, x = ~time, y = ~price, color = ~index, mode = "lines")

stocks.CAC <- stocks[stocks$index == 'CAC', ]
CAC.reg = lm(time ~ price, data = stocks.CAC)
CAC.reg
summary(CAC.reg)

stocks.DAX <- stocks[stocks$index == 'DAX', ]
DAX.reg = lm(time ~ price, data = stocks.DAX)
DAX.reg
summary(DAX.reg)

stocks.FTSE <- stocks[stocks$index == 'FTSE', ]
FTSE.reg = lm(time ~ price, data = stocks.FTSE)
FTSE.reg
summary(FTSE.reg)

stocks.SMI <- stocks[stocks$index == 'SMI', ]
SMI.reg = lm(time ~ price, data = stocks.SMI)
SMI.reg
summary(SMI.reg)

# 6

data(JohnsonJohnson)
johnson <- timetk::tk_tbl(JohnsonJohnson) %>%
  rename(profit = value) %>%
  separate(index, c("year", "quarter")) %>%
  mutate(year = as.integer(year))
plot_ly(johnson, x = ~year, y = ~profit, color = ~quarter, mode = "lines")

johnson.Q1 <- johnson[johnson$quarter == 'Q1', ]
reg.Q1 <- lm(profit ~ year, johnson.Q1)
johnson.Q2 <- johnson[johnson$quarter == 'Q2', ]
reg.Q2 <- lm(profit ~ year, johnson.Q2)
johnson.Q3 <- johnson[johnson$quarter == 'Q3', ]
reg.Q3 <- lm(profit ~ year, johnson.Q3)
johnson.Q4 <- johnson[johnson$quarter == 'Q4', ]
reg.Q4 <- lm(profit ~ year, johnson.Q4)
johnson.reg <- lm(profit ~ year, johnson)

predict(reg.Q1, data.frame(year = 2016))
predict(reg.Q2, data.frame(year = 2016))
predict(reg.Q3, data.frame(year = 2016))
predict(reg.Q4, data.frame(year = 2016))
predict(johnson.reg, data.frame(year = 2016))


# 7

data(sunspot.year)
sunspot <- timetk::tk_tbl(sunspot.year) %>% rename(year = index)
plot_ly(sunspot, x = ~year, y = ~value, mode = "lines")

sun.reg <- lm(year~., data=sunspot)
summary(sun.reg)

# 8

ukgas <- read.csv(file = "UKgas.csv", header = TRUE) %>%
  transmute(year = floor(time), quarter = time %% 1, consumption = UKgas)
ukgas$quarter <- ukgas$quarter %>%
  recode(`0.00` = "Q1", `0.25` = "Q2", `0.50` = "Q3", `0.75` = "Q4") %>%
  as.factor
plot_ly(ukgas, x = ~year, y = ~consumption, color = ~quarter, mode = "lines")

ukgas.Q1 <- ukgas[ukgas$quarter == 'Q1', ]
reg.Q1 <- lm(consumption ~ year, ukgas.Q1)
ukgas.Q2 <- ukgas[ukgas$quarter == 'Q2', ]
reg.Q2 <- lm(consumption ~ year, ukgas.Q2)
ukgas.Q3 <- ukgas[ukgas$quarter == 'Q3', ]
reg.Q3 <- lm(consumption ~ year, ukgas.Q3)
ukgas.Q4 <- ukgas[ukgas$quarter == 'Q4', ]
reg.Q4 <- lm(consumption ~ year, ukgas.Q4)
ukgas.reg <- lm(consumption ~ year, ukgas)

predict(reg.Q1, data.frame(year = 2016))
predict(reg.Q2, data.frame(year = 2016))
predict(reg.Q3, data.frame(year = 2016))
predict(reg.Q4, data.frame(year = 2016))
predict(ukgas.reg, data.frame(year = 2016))

# 9

data(cars)
cars_df <- cars %>% data.frame

cars.reg <- lm(dist ~ speed, cars_df)
predict(cars.reg, data.frame(speed=40))
