rm(list = ls(all = TRUE))
setwd("~/Desktop/AidData/R_Labs/boundaries")

# install.packages("plotrix")
# install.packages("spatstat")
# library(plotrix)
# library(ggplot2)
# 10 Random points within a 1x1 square
# plot.new()
# ranx <- (runif(10, min=0, max=1))
# rany <- (runif(10, min=0, max=1))
# x <- c(0, 1, 1, 0)
# y <- c(0, 0, 1, 1)
# polygon(x, y, density = NULL, angle = 45, border = NULL, col = NA, lty = par("lty"))
# xy.coords( ranx, rany), xlab = NULL, ylab = NULL, log = NULL, recycle = FALSE)
# plot( ranx, rany )
# box(lty = 'solid', col = 'red')


# # Random points within a circle
# ranx_rad <- (runif(10, min=-1, max=1))
# rany_rad <- (runif(10, min=0, max=6.28))
# radial.plot(ranx_rad, radial.pos = rany_rad,start=pi/2, clockwise=TRUE,
#             rp.type="s", main="Test Symbols (clockwise)",
#             point.symbols=16, point.col="green", show.centroid=TRUE, show.radial.grid = FALSE, show.grid = TRUE)

# Random points within a political boundary
# install.packages("devtools")
# devtools::install_github("dkahle/ggmap")
# devtools::install_version("ggplot2", version = "2.1.0", repos = "http://cran.us.r-project.org")

# install.packages("mapdata")
# library(devtools)
library(ggmap)
library(ggplot2)
library(maps)
library(mapdata)
library(spatstat)
# dev_mode(on=T)
plot.new()
old.par <- par(mfrow=c(2, 3))

# ghana <- get_map(source = "stamen", location = c(lon = -1.02, lat = 7.85), zoom = 7, maptype = "toner")
# ghana <- ggmap(ghana)
# ghana
# 
# mapx <- (runif(50, min=-2, max=0))
# mapy <- (runif(50, min=5.8, max=11))
# map_df <- data.frame(mapx, mapy)
# map_df
# ghana <- ghana + geom_point(aes(mapx, mapy), data = map_df, size = 1, col = "red")
# ghana

#plot(density(cbind(mapx, mapy)), 0.5)

ranx <- (runif(50, min=0.2, max=0.86))
rany <- (runif(50, min=0, max=0.9))
P <- ppp(ranx, rany, poly = list(x=c(0.2, 0.86, 0.82, 0.82, 0.77, 0.8, 0.2, 0.2, 0.16, 0.2), y=c(0, 0.22, 0.3, 0.8, 0.85, 0.9, 0.9, 0.5, 0.2, 0)))
plot(P)
summary(P)
#x1 <- c(0.2, 0.16, 0.2, 0.2, 0.8, 0.77, 0.82, 0.82, 0.86, 0.2) 
#y1 <- c(0, 0.2, 0.5, 0.9, 0.9, 0.85, 0.8, 0.3, 0.22, 0)
#c(0.2, 0.86, 0.82, 0.82, 0.77, 0.8, 0.2, 0.2, 0.16, 0.2)
#c(0, 0.22, 0.3, 0.8, 0.85, 0.9, 0.9, 0.5, 0.2, 0)
#plot(x1, y1)

k <- Kest(P)
plot(k)

plot(density(P, .05))
contour(density(P, .05, axes = FALSE))



# X <- swedishpines
# plot(density(X, 10))
# summary(X)


par(old.par)

warnings()










