#rm(list = ls(all = TRUE))
setwd("~/Desktop/AidData/liberia")

library(graphics)
library(stats)

old.par <- par(mfrow=c(3, 2))
citypops <- read.csv("citypopdata.csv")
lib_citpops <- data.frame(citypops)
lib_citpops <- lib_citpops[order(lib_citpops$Total, decreasing = TRUE),]

#save(df,file="~/Desktop/AidData/liberia/lib_citpops.Rdata")

poprank <- data.frame(x = 1:973, y1 = lib_citpops$Total)
## Zipf's Comparison ##
y1 <- 10210762/poprank$x
y2 <- poprank$y
popr_rankden0 <- data.frame(x = poprank$x, y1 = 10210762/poprank$x)
plot(popr_rankden0$x, y1, col = "red", main = "Cities Plotted in Rank-Order Compared to Zipf's Law", xlab = "City Rank", ylab = "Population")
points(popr_rankden0$x, y2, col = "green")

## Plot w/o Montserrado, Monrovia ##
poprank2 <- poprank[2:973,]
poprank2$x <- seq(1:972)
#plot(poprank2)
## Zipf's Comparison ##
z10 <- 50245/poprank2$x
y20 <- poprank2$y
popr_rankden <- data.frame(x = poprank2$x, z10 = 50245/poprank2$x)
plot(popr_rankden$x, z10, col = "red", main = "Cities Compared to Zipf's Law, Excluding Montserrado", xlab = "City Rank", ylab = "Population")
points(popr_rankden$x, y20, col = "green")

## Plot w/o Pops less than 750 ##
poprank750 <- subset(poprank2, y >= 750)
poprank750$x <- 1:514
#plot(poprank750)
## Zipf's Comparison ##
y11 <- 50245/poprank750$x
y21 <- poprank750$y
popr_rankden2 <- data.frame(x = poprank750$x, y11 = 50245/poprank750$x)
plot(popr_rankden2$x, y11, col = "red", main = "Cities With Populations of 750+ (514)", xlab = "City Rank", ylab = "Population")
points(popr_rankden2$x, y21, col = "green")

## Plot w/o Pops less than 1000 ##
poprank1000 <- subset(poprank2, y >= 1000)
poprank1000$x <- 1:311
#plot(poprank1000)
## Zipf's Comparison ##
y12 <- 50245/poprank1000$x
y22 <- poprank1000$y
popr_rankden3 <- data.frame(x = poprank1000$x, y12 = 50245/poprank1000$x)
plot(popr_rankden3$x, y12, col = "red", main = "Cities With Populations of 1000+ (311)", xlab = "City Rank", ylab = "Population")
points(popr_rankden3$x, y22, col = "green")

## Plot w/o Pops Less than 2500 ##
poprank2500 <- subset(poprank2, y >= 2500)
poprank2500$x <- 1:61
poprank2500 <- data.frame(x = poprank2500$x, y1 = poprank2500$y, y2 = lib_citpops$Town.Village[2:62])
#plot(poprank2500)
## Zipf's Comparison ##
y13 <- 50245/poprank2500$x
y23 <- poprank2500$y1
popr_rankden4 <- data.frame(x = poprank2500$x, y13 = 50245/poprank2500$x)
plot(popr_rankden4$x, y13, col = "red", main = "Cities With Populations of 2500+ (61)", xlab = "City Rank", ylab = "Population")
points(popr_rankden4$x, y23, col = "green")

## Plot w/o Pops less than 3000 ##
poprank3000 <- subset(poprank2, y >= 3000)
poprank3000$x <- 1:49
#plot(poprank3000)
## Zipf's Comparison ##
y14 <- 50245/poprank3000$x
y24 <- poprank3000$y
popr_rankden5 <- data.frame(x = poprank3000$x, y14 = 50245/poprank3000$x)
plot(popr_rankden5$x, y14, col = "red", main = "Cities With Populations of 3000+ (49)", xlab = "City Rank", ylab = "Population")
plot(popr_rankden5$x, y14, col = "red", main = "Cities With Populations of 3000+ (49)", xlab = "City Rank", ylab = "Population")

points(popr_rankden5$x, y24, col = "green")

par(old.par)

## Differences Between Plots ##
plot.new()
poprank_diff <- y20 - y10
popr_rankden$diffs <- poprank_diff
popr_rankden$ro <- y20
plot(x = popr_rankden$x, y = popr_rankden$diffs)

plot.new()
## Log Plot ## 
## Plot w/o Montserrado, Monrovia ##
poprank2 <- poprank[2:973,]
poprank2$x <- seq(1:972)
#plot(poprank2)
## Zipf's Comparison ##
# y10 <- 50245/poprank2$x
# y20 <- poprank2$y
y30 <- poprank2$y/50245
# popr_rankden <- data.frame(x = poprank2$x, y10 = 50245/poprank2$x)
# plot(popr_rankden$x, y10, col = "red", main = "Cities Compared to Zipf's Law, Excluding Montserrado", xlab = "City Rank", ylab = "Population")
# points(popr_rankden$x, y20, col = "green")
plot(popr_rankden$x, y30, col = "blue", ylog = TRUE, ylim = c(0.00001, 1))
logrank <- data.frame(x = popr_rankden$x, y = logrank2)

## Log Zipf's Comparison - All Cities ##
plot.new()
logpoprank <- data.frame(x = log10(popr_rankden0$x), y = log10(popr_rankden0$y1))
logcitpops <- data.frame(x = log10(poprank$x), y = log10(lib_citpops$Total))
xl <- 10210762/logpoprank$x
yl <- logcitpops$y
plot(logcitpops, col = "black", main = "Cities Plotted in Rank-Order Compared to Zipf's Law", xlab = "City Rank", ylab = "Population", points(logpoprank, col = "green"))
points(logpoprank, col = "green")

## Log Zipf's Comparison - w/o Montserrado ##
plot.new()
logranky <- log10(poprank2$y)
logrankx <- log10(poprank2$x)
logrankxy <- data.frame(x = logrankx, y = logranky)
plot(logrankxy, main = "City Populations in Rank Order (Log Scales)", xlab = "Log Rank", ylab = "Log Population", xaxp= c(0, 3, 10))
z <- abline(4.4431, -0.5793, col = "red")
lines(z, type = "o", col = "red")
#zlogrankx <- log(50245/poprank2$y)
points(log10(popr_rankden$x), log10(popr_rankden$z10), col = "green")
points(0, 4.701093, col = "blue")
## Log Differences Between Plots ##
#logpoprank_diff <- logranky - (log10(popr_rankden$z10))
#points(log10(popr_rankden$x), (5 * logpoprank_diff), col = "red")

## Log Zipf's Comparison - w/o Montserrado, Greater than 2500 ##
plot.new()
logrank25y <- log10(poprank2500$y1)
logrank25x <- log10(poprank2500$x)
logrank25xy <- data.frame(x = logrank25x, y = logrank25y)
plot(logrank25xy, main = "City Populations in Rank Order (Log Scales)", xlab = "Log Rank", ylab = "Log Population", 
        #xaxp= c(0, 3, 10), yaxp = c(2, 5, 10))
        usr = c(0, 2, 2, 5))
#z25 <- abline(line(logrank25xy$y), col = "red")
#lines(z25, type = "o", col = "red")
#line(logrank25xy$y)
#zlogrankx <- log(50245/poprank2$y)
points(log10(popr_rankden4$x), log10(popr_rankden4$y13), col = "green")
points(0, 4.701093, col = "blue", usr = c(0, 2, 2, 5))
