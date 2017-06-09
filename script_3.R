rm(list=ls())
setwd("~/GitHub/Liberia")


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
library(gridExtra)
library(rasterVis)
library(colorspace)
library(mapdata)
library(rgl)
library(scales)
library(plot3Drgl)
library(akima)

##

#### DATA ####


## Import Shapefiles ##
country<-readOGR(dsn="shapefiles", layer="liberia_revised",stringsAsFactors=FALSE, verbose=FALSE)
cnty<-readOGR(dsn = "shapefiles",layer="counties",stringsAsFactors = FALSE,verbose=FALSE)
dist<-readOGR(dsn="shapefiles",layer="districts", stringsAsFactors = FALSE,verbose = FALSE)
clan<-readOGR(dsn="shapefiles",layer="clans", stringsAsFactors = FALSE,verbose = FALSE)

country<-spTransform(country, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
cnty<-spTransform(cnty, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
dist<-spTransform(dist, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
clan<-spTransform(clan, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))


### Fortify shapefiles ###
country_f<-fortify(country)
cnty_f<-fortify(cnty)
dist_f<-fortify(dist)
clan_f<-fortify(clan)


## Import GPW4 data ##
gpw4_2000<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2000.tif")
gpw4_2000<-crop(gpw4_2000,extent(country))
gpw4_2000<-mask(gpw4_2000,country)

gpw4_2005<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2005.tif")
gpw4_2005<-crop(gpw4_2005,extent(country))
gpw4_2005<-mask(gpw4_2005,country)

gpw4_2010<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2010.tif")
gpw4_2010<-crop(gpw4_2010,extent(country))
gpw4_2010<-mask(gpw4_2010,country)

gpw4_2015<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2015.tif")
gpw4_2015<-crop(gpw4_2015,extent(country))
gpw4_2015<-mask(gpw4_2015,country)


## Calculate growth rate ##
growth_00_05<-overlay(gpw4_2000,gpw4_2005,fun=function(r1,r2){return((r2-r1)/r1)})
growth_05_10<-overlay(gpw4_2005,gpw4_2010,fun=function(r1,r2){return((r2-r1)/r1)})
growth_10_15<-overlay(gpw4_2010,gpw4_2015,fun=function(r1,r2){return((r2-r1)/r1)})


## Load final data ##
load("lbr_data_final.RData")


#### Test Plots #####

plus_10_data<-subset(lbr_data_final,lbr_data_final$pop2010 >= 10)
plus_20_data<-subset(lbr_data_final,lbr_data_final$pop2010 >= 20)
plus_30_data<-subset(lbr_data_final,lbr_data_final$pop2010 >= 30)

win<-as(country,"owin")

ppp0<-ppp(lbr_data_final$long,lbr_data_final$lat,marks=lbr_data_final$pop2010,window=win)
ppp1<-ppp(plus_10_data$long,plus_10_data$lat,marks=plus_10_data$pop2010,window=win)
ppp2<-ppp(plus_20_data$long,plus_20_data$lat,marks=plus_20_data$pop2010,window=win)
ppp3<-ppp(plus_30_data$long,plus_30_data$lat,marks=plus_30_data$pop2010,window=win)

plot(density(ppp0,.000001))
contour(density(ppp0))
plot(density(ppp1,.1))
plot(density(ppp2,.1))
plot(density(ppp3,.1))
plot(Smooth.ppp(ppp1))

plot(ppp0,use.marks= FALSE, chars=".")

test<-ppp(lbr_data_final$long,lbr_data_final$lat,marks=lbr_data_final$pop2010,c(-12,-7),c(4,9))
A<-colourmap(rainbow(128),range=range(marks(test)))
plot(test,pch=21,cex=.01,bg=blueblue)


ppp2<-as.ppp(data_final,W=win)
plot.ppp(ppp,cols=ppp$marks)
plot(density(ppp,.1))



gplot(gpw4_2000)+geom_map(data=country_f,map=country_f,aes(x=long,y=lat,map_id=id))+geom_tile(aes(fill = value))
gplot(growth_00_05)+geom_map(data=country_f,map=country_f,aes(x=long,y=lat,map_id=id))+geom_tile(aes(fill = value))+geom_point(data=city_points2,aes(x=Longitude,y=Latitude),cex=.01)

lambda<-function(x,y){100*(y^2+x^4)}
X<-rpoispp(lambda,nsim=10)
X
plot(X)
plot(density(X))
