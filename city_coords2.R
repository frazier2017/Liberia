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
  
  #(if(simpleError("Error in data.frame(ll.lat = ll[1], ll.lon = ll[2], ur.lat = ur[1], ur.lon = ur[2]) : 
 # arguments imply differing number of rows: 0, 1", call = next))) 
# clat <- (bbs$ll.lat + bbs$ur.lat)/2
# clon <- (bbs$ll.lon + bbs$ur.lon)/2
# citytitle$lat <- clat
# citytitle$long <-clon

  for (i in 59:nrow(bbs25)) {
    u <- get_googlemap(dQuote(bbs25[i,]$City), maptype = "roadmap", source = c("google", "osm", "stamen"))
    bbs25[i,2:5] <- attr(u, "bb")
    bbs25[i, 6] <- ((bbs25[i,]$ur.lat - bbs25[i,]$ll.lat)/2) + bbs25[i,]$ll.lat
    bbs25[i, 7] <- ((bbs25[i,]$ur.lon - bbs25[i,]$ll.lon)/2) + bbs25[i,]$ll.lon
  }
  
  LibCities2500 <- merge(poprank2500, bbs25, by.x = "citytitle", by.y = "City")
  View(LibCities2500)
  colnames(LibCities2500)[1:4] <- c("City 'Liberia'", "Rank", "Population", "City")
  LibCities2500 <- LibCities2500[order(LibCities2500$Rank),]
  LibCities2500 <- LibCities2500[order(LibCities2500$Population),]
  LibCities2500 <- LibCities2500[order(LibCities2500$Population, decreasing = TRUE),]

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

  # cities$X1 <- citytitle$x
  # cities$X2 <- citytitle$y
  # cities$X3 <- attr(u, "bb")

p <- get_googlemap("Kronoken City Liberia", maptype = "roadmap"))
bbr <- attr(p, "bb")

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
    
for (i in 59:nrow(bbs25)) {
u <- get_googlemap(dQuote(bbs25[i,]$City), maptype = "roadmap", source = c("google", "osm", "stamen"))
bbs25[i,2:5] <- attr(u, "bb")
bbs25[i, 6] <- ((bbs25[i,]$ur.lat - bbs25[i,]$ll.lat)/2) + bbs25[i,]$ll.lat
bbs25[i, 7] <- ((bbs25[i,]$ur.lon - bbs25[i,]$ll.lon)/2) + bbs25[i,]$ll.lon
}
LibCities2500 <- merge(poprank2500, bbs25, by.x = "citytitle", by.y = "City")
View(LibCities2500)
colnames(LibCities2500)[1:4] <- c("City 'Liberia'", "Rank", "Population", "City")
LibCities2500 <- LibCities2500[order(LibCities2500$Rank),]
LibCities2500 <- LibCities2500[order(LibCities2500$Population),]
LibCities2500 <- LibCities2500[order(LibCities2500$Population, decreasing = TRUE),]
LibCities2500[10, 9] <- "6.4233333"
LibCities2500[10, 10] <- "-10.7838889"
LibCities2500[10, 9] <- "4.588590"
LibCities2500[10, 10] <- "-7.672413"
LibCities2500[7, 9] <- "6.4233333"
LibCities2500[7, 10] <- "-10.7838889"
LibCities2500[16, 9] <- "7.1094444"
LibCities2500[19, 10] <- "-8.8405556"
LibCities2500[16, 9] <- "7.1094444"
LibCities2500[16, 10] <- "-8.8405556"
LibCities2500[16, 9] <- "6.274929"
LibCities2500[16, 10] <- "-10.347261"
LibCities2500[19, 10] <- "-8.708892"
LibCities2500[17, 9] <- "7.1094444"
LibCities2500[17, 10] <- "-8.8405556"
LibCities2500[48, 9] <- "7.8727778"
LibCities2500[48, 10] <- "-9.4838889"
LibCities2500[51, 9] <- "6.3166667"
LibCities2500[51, 10] <- "-9.75"
LibCities2500[58, 9] <- "7.6088889"
LibCities2500[58, 10] <- "-8.6225"
LibCities2500[60, 9] <- "7.1608333"
LibCities2500[60, 10] <- "-10.9175"

save(df, file = "~/Desktop/AidData/Lib_Repo/LibCities2500.RData")

load("LibCities2500.csv")

