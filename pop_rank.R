#rm(list = ls(all = TRUE))
setwd("~/Desktop/AidData/liberia")

citypops <- read.csv("citypopdata.csv")
lib_citpops <- data.frame(citypops)
lib_citpops <- lib_citpops[order(lib_citpops$Total, decreasing = TRUE),]

poprank <- data.frame(x = 1:973, y = lib_citpops$Total)

## Plot w/o Montserrado, Monrovia ##
poprank2 <- poprank[2:973,]

## Plot w/o Pops Less than 2500 ##
poprank2500 <- subset(poprank2, y >= 2500)
poprank2500$x <- 1:61
plot(poprank2500)

## Zipf's Comparison ##
y1 <- 50245/poprank2500$x
y2 <- poprank2500$y
popr_rankden <- data.frame(x = poprank2500$x, y1 = 50245/poprank2500$x)
plot(popr_rankden$x, y1, col = "red")
points(popr_rankden$x, y2, col = "green")

