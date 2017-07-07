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
library(SpaDES)
library(rmapshaper)

source("~/Documents/R/functions.R")

#### DATA ####


## Import Shapefiles ##

# countries
ben<-readOGR(dsn="shapefiles/BEN_shp", layer="BEN_adm0",stringsAsFactors=FALSE, verbose=FALSE)
bfa<-readOGR(dsn="shapefiles/BFA_shp", layer="BFA_adm0",stringsAsFactors=FALSE, verbose=FALSE)
civ<-readOGR(dsn="shapefiles/CIV_shp", layer="CIV_adm0",stringsAsFactors=FALSE, verbose=FALSE)
cpv<-readOGR(dsn="shapefiles/CPV_shp", layer="CPV_adm0",stringsAsFactors=FALSE, verbose=FALSE)
gha<-readOGR(dsn="shapefiles/GHA_shp", layer="GHA_adm0",stringsAsFactors=FALSE, verbose=FALSE)
gin<-readOGR(dsn="shapefiles/GIN_shp", layer="GIN_adm0",stringsAsFactors=FALSE, verbose=FALSE)
gmb<-readOGR(dsn="shapefiles/GMB_shp", layer="GMB_adm0",stringsAsFactors=FALSE, verbose=FALSE)
gnb<-readOGR(dsn="shapefiles/GNB_shp", layer="GNB_adm0",stringsAsFactors=FALSE, verbose=FALSE)
lbr<-readOGR(dsn="shapefiles/LBR_shp", layer="LBR_adm0",stringsAsFactors=FALSE, verbose=FALSE)
mli<-readOGR(dsn="shapefiles/MLI_shp", layer="MLI_adm0",stringsAsFactors=FALSE, verbose=FALSE)
mrt<-readOGR(dsn="shapefiles/MRT_shp", layer="MRT_adm0",stringsAsFactors=FALSE, verbose=FALSE)
ner<-readOGR(dsn="shapefiles/NER_shp", layer="NER_adm0",stringsAsFactors=FALSE, verbose=FALSE)
nga<-readOGR(dsn="shapefiles/NGA_shp", layer="NGA_adm0",stringsAsFactors=FALSE, verbose=FALSE)
sen<-readOGR(dsn="shapefiles/SEN_shp", layer="SEN_adm0",stringsAsFactors=FALSE, verbose=FALSE)
#shn<-readOGR(dsn="shapefiles/SHN_shp", layer="SHN_adm0",stringsAsFactors=FALSE, verbose=FALSE)
sle<-readOGR(dsn="shapefiles/SLE_shp", layer="SLE_adm0",stringsAsFactors=FALSE, verbose=FALSE)
#stp<-readOGR(dsn="shapefiles/STP_shp", layer="STP_adm0",stringsAsFactors=FALSE, verbose=FALSE)
tgo<-readOGR(dsn="shapefiles/TGO_shp", layer="TGO_adm0",stringsAsFactors=FALSE, verbose=FALSE)

load("shapefiles/WAF_shp/WAF_adm0.RData")


lbr2<-readOGR(dsn = "shapefiles/tyler_LBR",layer="liberia_revised",stringsAsFactors = FALSE,verbose=FALSE)
lbr_cnty<-readOGR(dsn = "shapefiles/tyler_LBR",layer="counties",stringsAsFactors = FALSE,verbose=FALSE)
lbr_dist<-readOGR(dsn="shapefiles/tyler_LBR",layer="districts", stringsAsFactors = FALSE,verbose = FALSE)
lbr_clan<-readOGR(dsn="shapefiles/tyler_LBR",layer="clans", stringsAsFactors = FALSE,verbose = FALSE)

lbr2<-spTransform(lbr2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lbr_cnty<-spTransform(lbr_cnty, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lbr_dist<-spTransform(lbr_dist, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lbr_clan<-spTransform(lbr_clan, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

lbr_f<-fortify(lbr)
lbr2_f<-fortify(lbr2)
lbr_cnty_f<-fortify(lbr_cnty)
lbr_dist_f<-fortify(lbr_dist)
lbr_clan_f<-fortify(lbr_clan)



## Import WorldPop ##
wp_lbr<-raster("~/GoogleDrive/LiberiaProject/LBR-WP/LBR10adjv3.tif")
proj4string(wp_lbr)

## Import GPW4 data ##
gpw4_2010<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2010.tif")
gpw4_2010<-crop(gpw4_2010,extent(lbr))
gpw4_2010<-mask(gpw4_2010,lbr)

## IMPORT OSM DATA ##
dsn<-"/Users/chriselsner/GoogleDrive/LiberiaProject/LBR-OSM"
bldgs<-readOGR(dsn = "/Users/chriselsner/GoogleDrive/LiberiaProject/LBR-OSM", layer = "gis.osm_buildings_a_free_1", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(bldgs)


## Load final data ##
load("RData/lbr_gpw4_data.RData")


## Load pixel boundaries data frame ##
load("RData/lbr_gpw4_pixels.RData")


## complete pop and spatial data set ##
load("RData/lbr_gpw4_complete.RData")


## load city points data ##
load("RData/city_points.RData")


# TESTING STUFF -----

quartz()
blueblue<-colorRampPalette(brewer.pal(9,"Blues"))(250)

bain<-lbr_clan[which(lbr_clan@data$CLNAME=="Bain"),]
plot(bain)
wp_bain<-crop(wp_lbr,bain)
wp_bain<-mask(wp_bain, bain)

pop<-sum(wp_bain@data@values,na.rm = T)

wp_bain_im <- as.im(wp_bain)
plot(wp_bain_im)
class(wp_bain_im)
class(wp_lbr)

bain_win<- as(bain,"owin")
bain_ppp <- rpoint(pop, f=wp_bain_im,bain_win)
bain_ppp_im<-as.im(bain_ppp)
plot(bain_ppp_im)
bain_dens<-density(bain_ppp,sigma=bw.ppl)
plot(bain_dens)
summary(bain_ppp_im)
summary(bain_dens)

bain_cont<-contour(wp_bain_im,add=T)

# Clustering Funnction -----
contourclust<-function(pobj,){ # pobj = point pobject (raster, im, ppp)
  if (class(pobj) == "raster"){
    pobj<-as.im(pobj)
  }else if (class(pobj) == "im"){
    
  }
}
Dsg <- as(wp_bain_im, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
Dcl <- contourLines(Dim, nlevels = 100)  # create contour object - change 8 for more/fewer levels
SLDF <- ContourLines2SLDF(Dcl) # convert to SpatialLinesDataFrame
plot(SLDF, col = terrain.colors(100))
as(SpatialPoints(SLDF), "ppp")
xxx <- SLDF[-c(1,3,4), ]
class(xxx@lines[[1]])
class(xxx)
Polyclust <- gPolygonize(xxx)
plot(Polyclust)
gas <- gArea(Polyclust, byid = T)
Polyclust <- SpatialPolygonsDataFrame(Polyclust, data = data.frame(gas), match.ID = F)
plot(Polyclust)
# Workaround 
pxy <- cbind(p$x,p$y)
pxy <- as.data.frame(pxy)

pxy$observation <- 1:nrow(pxy) 
pxy$observation <- as.data.frame(pxy$observation)

dfxy <- as.data.frame(cbind(pxy$V1,pxy$V2))

chocho <- SpatialPointsDataFrame(dfxy,pxy$observation)
cAg <- aggregate(chocho, by = Polyclust, FUN = length)


#Add cex= to change point size 
plot(p, main = "Pixel Image with Function based on Density of Raster Map",cex=.03)
rp<-raster(as.im(p))
open3d()
plot3D(rp, col = blueblue)

