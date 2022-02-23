library(cluster)

# 1

data(pluton)
print(pluton)
pluton10 <- kmeans(pluton, 3, iter.max = 10)
pluton100 <- kmeans(pluton, 3, iter.max = 100)
pluton500 <- kmeans(pluton, 3, iter.max = 500)

print(pluton10)
print(pluton100)
print(pluton500)

print(pluton10$iter)
print(pluton100$iter)
print(pluton500$iter)

# 2

n <- 1000
x <- rbind(cbind(rnorm(n, mean = 0, sd = 10), rnorm(n, mean = 10, sd = 0.1)),
           cbind(rnorm(n, mean = 0,sd = 20),rnorm(n, mean = 20, sd = 0.1)), 
           cbind(rnorm(n, mean = 15, sd = 0.2), rnorm(n,mean = 8, sd = 10)))

euclidean_t <- clara(x, 3, stand=TRUE, metric = "euclidean")
print(sum(euclidean_t$clusinfo[,4]))

euclidean_f <- clara(x, 3, stand=FALSE, metric = "euclidean")
print(sum(euclidean_f$clusinfo[,4]))

manhattan_t <- clara(x, 3, stand=TRUE, metric = "manhattan")
print(sum(manhattan_t$clusinfo[,4]))

manhattan_f <- clara(x, 3, stand=FALSE, metric = "manhattan")
print(sum(manhattan_f$clusinfo[,4]))

# 3

plot(agnes(votes.repub))

# 4

data(animals)
plot(agnes(animals))

war_a <- subset(animals, war ==2)
print(war_a)
not_fly_a <- subset(war_a, fly == 1)
print(not_fly_a)

war_a <- subset(animals, war == 1)
print(war_a)
not_fly_a <- subset(war_a, fly == 1)
print(not_fly_a)

# 5

seeds <- read.table('seeds_dataset.txt', header=FALSE)
names(seeds) <- list('Area', 'P', 'C', 'Len_s', 'Width', 'Asym', 'Len_e', 'Sort')

seeds_means <- kmeans(seeds, 3)
print(seeds_means$centers)

seeds_clara <- clara(seeds, 3, stand=TRUE, metric = "manhattan")
print(seeds_clara$medoids)