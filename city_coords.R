rm(list = ls(all = TRUE))
setwd("~/Desktop/AidData/Lib_Repo")

library(ggmap)
library(ggplot2)
library(sp)

ganta <- get_map("Zwedru Liberia", maptype = "roadmap")
ggmap(ganta)
geocode(ganta, output = c("latlon"), source = "dsk")


coordpull <- function(u) {
  
  w <- paste(u, "Liberia")
  lib_citpops$citytitle <- w
  u <- get_googlemap((dQuote(y)), maptype = "roadmap")
  #paste(x)"1" <- geocode(x)

coordbb <- attr(u, "bb") }

coordpull("Zwedru")
ganta$ll.lat
attr(ganta, "bb")

 

for (val in lib_citpops$Town.Village)

# w <- paste(lib_citpops$Town.Village, "Liberia")
# citytitle <- data.frame(x = w, y = lib_citpops$Total)
# citytitle$x <- as.character(citytitle$x)
  citytitle2<-citytitle[1:5,]
citytitle$x <- as.character(citytitle$x)
  bbs<- data.frame(matrix(nrow=973,ncol=5))
  for (i in 121:nrow(citytitle)) {
u <- get_googlemap(dQuote(citytitle[i,]$x), maptype = "roadmap", source = c("google", "osm", "stamen"))
bbs[i,1] <- citytitle[i,]$x
bbs[i,2:5] <- attr(u, "bb")}
  
  (if(simpleError("Error in data.frame(ll.lat = ll[1], ll.lon = ll[2], ur.lat = ur[1], ur.lon = ur[2]) : 
  arguments imply differing number of rows: 0, 1", call = next))) 
# clat <- (bbs$ll.lat + bbs$ur.lat)/2
# clon <- (bbs$ll.lon + bbs$ur.lon)/2
# citytitle$lat <- clat
# citytitle$long <-clon


### 7. Brewerville City
### 16. Saclepea Liberia
### 21. Tappita City Liberia
### 24. Karnplay Liberia
### 29. Kanweaken Liberia
### 31. Wayzohn Community Liberia
### 35. Barkedu Liberia
### 47. Borkeza Liberia
### 50. Kai Liberia
### 56. Juluken Liberia
### 57. Lugbye Liberia
### 58. Gbarnga Siaquelleh Liberia
### 59. Weajue Liberia
### 62. Gbeleyee Liberia
### 71. Dinplay Liberia
### 72. Gbakliken Liberia
### 76. Kronoken City Liberia
### 77. Marlay Liberia
### 78. Foequelleh Liberia
### 79. Gbawiliken Cavalla Liberia
### 81. Graie Township Liberia
### 82. Fissibu Liberia
### 83. River Gbeh Township Liberia
### 84. Nyor Diaplay Liberia
### 87. Zowieta Liberia
### 89. Weasua Liberia
### 92. Blemieplay Liberia
### 97. Gbloulay Liberia
### 99. Ziggida Liberia
### 101. Lofa Bridge Liberia
### 106. Gahnmue Liberia
### 109. New Yourpea Liberia
### 111.
### 116. Gbono Paye Liberia
### 120. Beo-Yoolar Liberia

  cities$X1 <- citytitle$x
  cities$X2 <- citytitle$y
  cities$X3 <- attr(u, "bb")

p <- get_googlemap("Kronoken City Liberia", maptype = "roadmap")
bbr <- attr(p, "bb")
bbr
#cities <- data.frame(matrix(nrow = 973, ncol = 6))

clat <- (bbs$ll.lat + bbs$ur.lat)/2
clon <- (bbs$ll.lon + bbs$ur.lon)/2
citytitle$lat <- clat
citytitle$long <-clon
zwed <- get_map(location = c(lon = centdf$y2, lat = centdf$y1))
ggmap(zwed)
citytitle <- data.frame(x = lib_citpops$Town.Village, y = lib_citpops$Total)

world <- read.table(file = "worldcitiespop.txt",sep = ",",header = TRUE,fill=TRUE)
save(world, file = "world_city_data.RData")

liberiaCities <- read.csv("liberia_city_names.csv")
comNames <- merge(liberiaCities, lib_citpops, by.x = "AccentCity", by.y = "Town.Village")
    
