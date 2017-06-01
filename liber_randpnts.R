rm(list = ls(all = TRUE))
setwd("~/Desktop/AidData/R_Labs/liberia")

#install.packages("spatstat")

library(rgdal)
library(dplyr)
library(rgeos)
library(ggmap)
library(raster)
library(spatstat)
library(maptools)
library(sp)
library(foreign)
library(knitr)

plot.new()

load("cwiq10.RData")

country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(country)
country <- spTransform(country, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
win <- as(country,"owin")

ranx <- (runif(500, min=223700, max=680781))
rany <- (runif(500, min=481399, max=945462))
country <- ppp(x = ranx, y = rany, window = win)
plot(country)
plot(density(country, 10000))



