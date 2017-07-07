rm(list=ls())
setwd("~/GitHub/Liberia")

#install.packages("SpaDES",dependencies = T)
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

quartz()
par(mfrow=c(1,2))
plot(lbr)
plot(lbr2)
# 

## Import GPW4 data ##
gpw4_2000<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2000.tif")
gpw4_2000<-crop(gpw4_2000,extent(lbr))
#gpw4_2000<-mask(gpw4_2000,lbr)

gpw4_2005<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2005.tif")
gpw4_2005<-crop(gpw4_2005,extent(lbr))
#gpw4_2005<-mask(gpw4_2005,lbr)

gpw4_2010<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2010.tif")
gpw4_2010<-crop(gpw4_2010,extent(lbr))
gpw4_2010<-mask(gpw4_2010,lbr)

gpw4_2015<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2015.tif")
gpw4_2015<-crop(gpw4_2015,extent(lbr))
#gpw4_2015<-mask(gpw4_2015,lbr)


## Calculate growth rate ##
growth_00_05<-overlay(gpw4_2000,gpw4_2005,fun=function(r1,r2){return((r2-r1)/r1)})
growth_05_10<-overlay(gpw4_2005,gpw4_2010,fun=function(r1,r2){return((r2-r1)/r1)})
growth_10_15<-overlay(gpw4_2010,gpw4_2015,fun=function(r1,r2){return((r2-r1)/r1)})


## Load final data ##
load("lbr_data_final.RData")


## Load pixel boundaries data frame ##
load("gpw4_lbr_spdf.RData")


## complete pop and spatial data set ##
load("complete_data.RData")


## load population samples ##
load("points_df_10percent.RData")

## load city points data ##
load("city_points.RData")

####


# CREATE WEST AFRICA SHAPEFILE -----------------------------------

shp_list<-list(ben,bfa,civ,cpv,gha,gin,gmb,gnb,lbr,mli,mrt,ner,nga,sen,sle,tgo)
#shp_list<-lapply(1:length(shp_list), function(i) gSimplify(shp_list[[i]],tol=.005))
shp_list<-lapply(1:length(shp_list),function(i) {SpatialPolygons(shp_list[[i]]@polygons)})

waf<-union(shp_list[[1]],shp_list[[2]])
for (i in 3:length(shp_list)){
  waf<-union(waf,shp_list[[i]])
}

area <- lapply(waf@polygons, function(x) sapply(x@Polygons, function(y) y@area))
mainPolys <- lapply(area, function(x) which(x > 0.001))
waf@plotOrder <- 1:length(waf@polygons)

for(i in 1:length(mainPolys)){
  waf@polygons[[i]]@Polygons <- waf@polygons[[i]]@Polygons[mainPolys[[i]]]
  waf@polygons[[i]]@plotOrder <- 1:length(waf@polygons[[i]]@Polygons)
}

waf@proj4string<-CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" )


for(i in 1:length(waf@polygons)){
  for(j in 1:length(waf@polygons[[i]]@Polygons)){
    temp <- as.data.frame(waf@polygons[[i]]@Polygons[[j]]@coords)
    names(temp) <- c("x", "y")
    temp2 <- dp(temp, 0.01)
    waf@polygons[[i]]@Polygons[[j]]@coords <- as.matrix(cbind(temp2$x, temp2$y))
  }
}


iso<-c("CPV","CIV","NER","NGA","GMB","GNB","MRT","MLI","SEN","LBR","GIN","SLE","GHA","BFA","BEN","TGO")
names<-c("Cape Verde","Ivory Coast","Niger","Nigeria","Gambia","Guinea Bissau","Mauritania","Mali","Senegal","Liberia","Guinea","Sierra Leone","Ghana","Burkina Faso","Benin","Togo")
id<-seq(1,16)
df<-data.frame(iso,names,id)
df[,1]<-as.character(df[,1])
df[,2]<-as.character(df[,2])
waf<-SpatialPolygonsDataFrame(waf,df)

plot(waf)

save(waf,file = "WAF_adm0.RData")
####
# TEST WITH WOLRDPOP DATA ----------------------------------------

quant<-quantile(getValues(wp_lbr),probs = seq(0, 1, .015625),na.rm=T)
quant


quartz()
wp_lbr<-raster("~/Documents/wm/year1/monroe_project/LBR-POP/LBR10adjv3.tif")
wp_lbr_region<-crop(wp_lbr,c(-9,-8.6,5.6,6))
sum(wp_lbr_region@data@values)
region_shp<-rasterToPolygons(wp_lbr_region)
region_f<-fortify(region_shp)
plot(wp_lbr2)

blueblue<-colorRampPalette(brewer.pal(9,"Blues"))(250)

bain<-lbr_clan[which(lbr_clan@data$CLNAME=="Bain"),]
plot(bain)
wp_bain<-crop(wp_lbr,bain)
wp_bain<-mask(wp_bain, bain)
pop<-sum(wp_bain@data@values,na.rm = T)
wp_bain_im <- as.im(wp_bain)
win<- as(bain,"owin")

floor(cellStats(wp_bain,'sum'))

# Clustering Funnction -----
contourclust<-function(points,){
  
}
#####

p <- rpoint(pop, f=wp_bain_im,win)
p2<-rpoint(pop, f=wp_bain_im)
plot(wp_bain_im)
x<-contour(wp_bain_im,add=T)
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

plot(p2, main = "Pixel Image with Function based on Density of Raster Map",cex=.03)
rp2<-raster(as.im(p2))
open3d()
plot3D(rp2, col = blueblue)


diggle<-bw.diggle(p)
plot(diggle)
ppl<-bw.ppl(p)
scott<-bw.scott(p)
frac<-bw.frac(p)
#pcf<-bw.pcf(p)
stoyan<-bw.stoyan(p)

quartz()
rho<-rhohat(p,wp_bain_im)
plot(rho)

quartz()
par(mfrow=c(2,3))
dens<-plot(density(p),main="default")
dens_diggle<-plot(density(p,sigma=diggle),main="diggle")
dens_ppl<-plot(density(p,sigma=ppl),main="ppl")
dens_scott<-plot(density(p,sigma=scott),main="scott")
dens_frac<-plot(density(p,sigma=frac),main="frac")
dens_stoyan<-plot(density(p,sigma=stoyan),main="stoyan")


## GPW4 test
gpw4_2010<-crop(gpw4_2010,waf)
gpw4_2010<-mask(gpw4_2010,waf)
quartz()
plot(gpw4_2010)
gpw4_2010@data@values
pop<-sum(gpw4_2010@data@values,na.rm = T)
gpw4_2010_im<-as.im(gpw4_2010)
win<-as(waf,"owin")


p <- rpoint(pop/1000, f=gpw4_2010_im,win)
p2<-rpoint(pop, f=gpw4_2010_im)
plot(gpw4_2010_im)

#Add cex= to change point size 
plot(p, main = "Pixel Image with Function based on Density of Raster Map",cex=.03)
rp<-raster(as.im(p))
p_im<-as.im(p)
open3d()
plot3D(rp, col = blueblue)

sig<-bw.diggle(p)
scott<-bw.scott(p)

# Lovelace: http://robinlovelace.net/r/2014/03/21/clustering-points-R.html
# P=9956 persons plot from code above in world pop
Dens <- density(p, sigma=sig)  # create density object
class(Dens)  
plot(Dens)

con <- contour(Dens, nlevels=100) # plot as contours - this is where we're heading
Dsg <- as(Dens, "SpatialGridDataFrame")  # convert to spatial grid class
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



con2<-contour(gpw4_2010_im,nlevels=100)
Dsg2<-as(gpw4_2010_im,"SpatialGridDataFrame")
Dim2 <- as.image.SpatialGridDataFrame(Dsg2)  # convert again to an image
Dcl2 <- contourLines(Dim2, nlevels = 200)  # create contour object - change 8 for more/fewer levels
SLDF2 <- ContourLines2SLDF(Dcl2) # convert to SpatialLinesDataFrame
#plot(SLDF2, col = terrain.colors(100))
as(SpatialPoints(SLDF2), "ppp")
xxx2 <- SLDF2[-c(1,3,4), ]
class(xxx2@lines[[1]])
class(xxx2)
Polyclust2 <- gPolygonize(xxx2)
#plot(Polyclust2)
gas2 <- gArea(Polyclust2, byid = T)
Polyclust2 <- SpatialPolygonsDataFrame(Polyclust2, data = data.frame(gas2), match.ID = F)
#plot(Polyclust2)




plot.new()
plot(Dens, main = "West Africa (Lovelace)")
# plot(lnd, border = "grey", lwd = 2, add = T)
plot(SLDF, col = terrain.colors(8), add = T)

quartz()
plot(log(gpw4_2010_im), main = "")
# plot(lnd, border = "grey", lwd = 2, add = T)
plot(SLDF2, col = terrain.colors(8), add = T)
points(point)


list<-lapply(region_shp@polygons,function(i) {i@Polygons[[1]]@coords})
m<-matrix(ncol=2)
m<-rbind(m,list[[3]])
df<-as.data.frame(list)

x<-coordinates(region_shp)

for(i in 1:length(list)){
  m<-rbind(m,list[[i]])
}


m2<-m[-1,]
save(m2,file="m2.RData")
cent<-gCentroid(region_shp,byid = T)


coords<-cent@coords

x<-rep(1:230400,each=5)
y<-rep(1:230400)

m1<-cbind(coords,y)
colnames(m1)<-c("x","y","z")
df1<-as.data.frame(m1)

m2<-cbind(m2,x)
colnames(m2)<-c("x","y","z")
df2<-as.data.frame(m2)

df3<-left_join(df2,df1,by = c("z"="z"))
colnames(df3)<-c("x","y","id","ct_x","ct_y")
df4<-raster2df(wp_lbr_region,marks=T)

df5<-left_join(df3,df4,by=c(c("ct_x"="x"),c("ct_y"="y")))
df5<-left_join(df3,df4,by=c("id"="cell"))
sum(df5[,8])
df5[,8]<-round(df5[,8],0)
sum(df5[,8])


for (i in 1:230400){
  m2[(1+(5*(i-1))):(5*i),3]<-i
}

m_final<-matrix(nrow=0,ncol=3)
for (i in 1:29340){
  if (bong_sub_pops[i,4] > 0){
    sub<-[(1+(5*(i-1))):(5*i),]
    m1<-matrix(nrow=0,ncol=3)
    while(nrow(m1) < bong_sub_pops[i,4]) {
      m2<-matrix(nrow=100,ncol=3)
      m2[,1]<-runif(100,max=(max(sub$long,na.rm=T)+.00000001),min = (min(sub$long,na.rm=T)-.00000001))
      m2[,2]<-runif(100,max=(max(sub$lat,na.rm=T)+.00000001),min = (min(sub$lat,na.rm=T)-.00000001))
      m2[,3]<-point.in.polygon(m2[,1],m2[,2],bong_sub@polygons[[1]]@Polygons[[1]]@coords[,1],bong_sub@polygons[[1]]@Polygons[[1]]@coords[,2])
      m2<-subset(m2, m2[,3]>0)
      m1<-rbind(m1,m2)
      m1<-unique(m1)
    }
    m1<-m1[1:bong_sub_pops[i,4],]
    m_final<-rbind(m_final,m1)
  }
}





df3<-as.data.frame(wp_lbr_region)

df2<-raster2df(wp_lbr_region,c(-8.7,-8.6,5.6,5.7),marks=T)
ppp<-raster2ppp(wp_lbr_region,marks=T)
plot(ppp)

blueblue<-colorRampPalette(brewer.pal(9,"Blues"))(250)
open3d()
plot3D(wp_lbr2, col = blueblue)

open3d()
plot3D(wp_lbr3, col = blueblue)


win <- owin( c(-9, -8.6), c(5.6, 6))
runpt <- runifpoint(9956, win = win)
runpt0 <- as.ppp(runpt)
plot(runpt0, main = "Random Points Distributed within same bbox")

# Pixel image 9956 persons
wp_lbr2_im <- as.im(wp_lbr2)
p <- rpoint(8416, f=wp_lbr2_im)
plot(wp_lbr2_im)
#Add cex= to change point size 
plot(p, main = "Pixel Image with Function based on Density of Raster Map")
rp<-raster(as.im(p))
open3d()
plot3D(rp, col = blueblue)

quartz()
p2 <- rpoint(8416, f=wp_lbr2_im)
plot(p2, main = "Pixel Image with Function based on Density of Raster Map")
rp2<-raster(as.im(p2))
open3d()
plot3D(rp2, col = blueblue)


x<-mask(wp_lbr2,Polyclust)
sum(x@data@values,na.rm=T)
y<-mask(rp,Polyclust)
sum(y@data@values,na.rm=T)
z<-mask(rp2,Polyclust)
sum(z@data@values,na.rm=T)


sum(wp_lbr2@data@values,na.rm=T)








# Lovelace: http://robinlovelace.net/r/2014/03/21/clustering-points-R.html
# P=9956 persons plot from code above in world pop
Dens <- density(p, adjust = 0.2)  # create density object
class(Dens)  
plot(Dens)  
con <- contour(density(p, adjust = 0.2), nlevels=4)  # plot as contours - this is where we're heading
Dsg <- as(Dens, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
Dcl <- contourLines(Dim, nlevels = 4)  # create contour object - change 8 for more/fewer levels
SLDF <- ContourLines2SLDF(Dcl) # convert to SpatialLinesDataFrame
plot(SLDF, col = terrain.colors(8))
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
plot.new()
plot(Dens, main = "")
# plot(lnd, border = "grey", lwd = 2, add = T)
plot(SLDF, col = terrain.colors(8), add = T)
plot(cAg, col = "red", border = "white", add = T)


# Lovelace: http://robinlovelace.net/r/2014/03/21/clustering-points-R.html
# P=9956 persons plot from code above in world pop
quartz()
Dens <- density(wp_lbr3,weights=wp_lbr3$marks)  # create density object
class(Dens)  
plot(Dens)  
con <- contour(density(wp_lbr3, adjust = 0.2), nlevels=4)  # plot as contours - this is where we're heading
Dsg <- as(Dens, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
Dcl <- contourLines(Dim, nlevels = 4)  # create contour object - change 8 for more/fewer levels
SLDF <- ContourLines2SLDF(Dcl) # convert to SpatialLinesDataFrame
plot(SLDF, col = terrain.colors(8))
xxx <- SLDF[-c(1,3,4), ]
class(xxx@lines[[1]])
class(xxx)
Polyclust <- gPolygonize(xxx)
plot(Polyclust)
gas <- gArea(Polyclust, byid = F)
Polyclust <- SpatialPolygonsDataFrame(Polyclust, data = data.frame(gas), match.ID = F)
plot(Polyclust)
# Workaround 
pxy <- cbind(wp_lbr3$x,wp_lbr3$y)
pxy <- as.data.frame(pxy)

pxy$observation <- 1:nrow(pxy) 
pxy$observation <- as.data.frame(pxy$observation)

dfxy <- as.data.frame(cbind(pxy$V1,pxy$V2))

chocho <- SpatialPointsDataFrame(dfxy,pxy$observation)
cAg <- aggregate(chocho, by = Polyclust, FUN = length)
plot.new()
plot(Dens, main = "")
# plot(lnd, border = "grey", lwd = 2, add = T)
plot(SLDF, col = terrain.colors(8), add = T)
plot(cAg, col = "red", border = "white", add = T)


# Lovelace: http://robinlovelace.net/r/2014/03/21/clustering-points-R.html
# P=9956 persons plot from code above in world pop
quartz()
Dens <- density(p2, adjust = 0.2)  # create density object
class(Dens)  
plot(Dens)  
con <- contour(density(p2, adjust = 0.2), nlevels=4)  # plot as contours - this is where we're heading
Dsg <- as(Dens, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
Dcl <- contourLines(Dim, nlevels = 4)  # create contour object - change 8 for more/fewer levels
SLDF <- ContourLines2SLDF(Dcl) # convert to SpatialLinesDataFrame
plot(SLDF, col = terrain.colors(8))
as(SpatialPoints(SLDF), "ppp")
xxx <- SLDF[-c(1,3,4), ]
Polyclust <- gPolygonize(xxx)
plot(Polyclust)
gas <- gArea(Polyclust, byid = T)
Polyclust <- SpatialPolygonsDataFrame(Polyclust, data = data.frame(gas), match.ID = F)
plot(Polyclust)
# Workaround 
pxy <- cbind(p2$x,p2$y)
pxy <- as.data.frame(p2xy)

p2xy$observation <- 1:nrow(p2xy) 
p2xy$observation <- as.data.frame(p2xy$observation)

dfxy <- as.data.frame(cbind(p2xy$V1,p2xy$V2))

chocho <- SpatialPointsDataFrame(dfxy,p2xy$observation)
cAg <- aggregate(chocho, by = Polyclust, FUN = length)
plot.new()
plot(Dens, main = "")
# plot(lnd, border = "grey", lwd = 2, add = T)
plot(SLDF, col = terrain.colors(8), add = T)
plot(cAg, col = "red", border = "white", add = T)

quartz()
wp_lbr2<-wp_lbr>0
plot(wp_lbr2)
x<-matrix(getValues(wp_lbr))
plot(log(wp_lbr))
gpw4_2010

gplot(wp_lbr)+geom_tile(aes(fill=value))+geom_map(data=lbr2_f,map=lbr2_f,aes(x=long,y=lat,map_id=id),alpha=0,col="black",cex=.1)


proj4string(test)
proj4string(lbr)

y<-cellsFromExtent(test,lbr)
unique(x)
na.rm(x)



high_low_lbr2<-data.frame(cell=cellsFromExtent(test,lbr2), pop=getValues(test))
high_low_lbr2$long<-xFromCell(test,high_low_lbr2$cell)
high_low_lbr2$lat<-yFromCell(test,high_low_lbr2$cell)
high_low_lbr2$pop2<-high_low_lbr2$pop
high_low_lbr2$pop2[is.na(high_low_lbr2$pop2)]<-0


# Calculate Using Means
adj<-sapply(1:24988401, function(i) {adj(test,high_low_lbr2$cell[i],directions=4,pairs=F,include=F)})
adj_mean<-lapply(1:24988401, function(i) {mean(sapply(1:length(adj[[i]]), function(j) {high_low_lbr2$pop[adj[[i]][j]]}),na.rm=T)})

high_low_lbr2$adj<-adj
high_low_lbr2$adj_mean<-adj_mean

mean<-mean(high_low_lbr2$pop,na.rm=T)

quart_1<-mean(high_low_lbr2$pop[which(high_low_lbr2$pop <= mean)],na.rm=T)
quart_3<-mean(high_low_lbr2$pop[which(high_low_lbr2$pop >= mean)],na.rm=T)

oct_1<-mean(high_low_lbr2$pop[which(high_low_lbr2$pop <= quart_1)],na.rm=T)
oct_2<-mean(high_low_lbr2$pop[which(high_low_lbr2$pop >= quart_1 & high_low_lbr2$pop <= mean)],na.rm=T)
oct_3<-mean(high_low_lbr2$pop[which(high_low_lbr2$pop >= mean & high_low_lbr2$pop <= quart_3)],na.rm=T)
oct_4<-mean(high_low_lbr2$pop[which(high_low_lbr2$pop >= quart_3)],na.rm=T)

high_low_lbr2$cellscore<-ifelse(high_low_lbr2$pop >= oct_4, 8,
                                ifelse(high_low_lbr2$pop >= quart_3, 7,
                                       ifelse(high_low_lbr2$pop >= oct_3, 6,
                                              ifelse(high_low_lbr2$pop >= mean, 5,
                                                     ifelse(high_low_lbr2$pop >= oct_2, 4,
                                                            ifelse(high_low_lbr2$pop >= quart_1, 3,
                                                                   ifelse(high_low_lbr2$pop >= oct_1, 2,
                                                                          ifelse(high_low_lbr2$pop < oct_1, 1, NA))))))))

high_low_lbr2$adj_score<-ifelse(high_low_lbr2$adj_mean >= oct_4, 8,
                                ifelse(high_low_lbr2$adj_mean >= quart_3, 7,
                                       ifelse(high_low_lbr2$adj_mean >= oct_3, 6,
                                              ifelse(high_low_lbr2$adj_mean >= mean, 5,
                                                     ifelse(high_low_lbr2$adj_mean >= oct_2, 4,
                                                            ifelse(high_low_lbr2$adj_mean >= quart_1, 3,
                                                                   ifelse(high_low_lbr2$adj_mean >= oct_1, 2,
                                                                          ifelse(high_low_lbr2$adj_mean < oct_1, 1, NA))))))))

high_low_lbr2$score<-high_low_lbr2$cellscore * 10 + high_low_lbr2$adj_score

save(high_low_lbr2,file = "high_low_lbr2.RData")

###
# NIGHT LIGHTS DATA ---------------------------------------

quartz()
lights<-raster("~/Downloads/F182010.v4/F182010.v4d_web.stable_lights.avg_vis.tif")
lights2<-raster("~/Downloads/F182010.v4/F182010.v4d_web.cf_cvg.tif")
lights3<-raster("~/Downloads/F182010.v4/F182010.v4d_web.avg_vis.tif")


lbr_lights<-crop(lights,extent(lbr))
plot(lbr_lights)
lights0<-lbr_lights>0
gplot(lights0)+geom_tile(aes(fill=factor(value)))+
  geom_map(data=lbr_cnty_f,map=lbr_cnty_f,aes(x=long,y=lat,map_id=id),alpha=0,col="black",cex=.1)
x<-getValues(lbr_lights)
x<-x[which(x>0)]


lbr_lights2<-crop(lights2,extent(lbr))
plot(lbr_lights2)
gplot(lbr_lights2)+geom_tile(aes(fill=value))+
  geom_map(data=lbr_cnty_f,map=lbr_cnty_f,aes(x=long,y=lat,map_id=id),alpha=0,col="black",cex=.1)
y<-getValues(lbr_lights2)
y<-y[which(y>0)]


lbr_lights3<-crop(lights3,extent(lbr))
plot(lbr_lights3)
gplot(lbr_lights3)+geom_tile(aes(fill=factor(value)))+
  geom_map(data=lbr_cnty_f,map=lbr_cnty_f,aes(x=long,y=lat,map_id=id),alpha=0,col="black",cex=.1)
z<-getValues(lbr_lights3)
z<-z[which(z>0)]

####

# HIGH LOW CLUSTERING METHOD -------------------------------------


# Data
gpw4_2010<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2010.tif")
gpw4_2010<-crop(gpw4_2010,extent(lbr))
gpw4_2010<-mask(gpw4_2010,lbr)

load("high_low_lbr.RData")
load("high_low_lbr_med.RData")


# Quick Plotting

  # means
  quartz()
  ggplot()+geom_tile(data=high_low_lbr,aes(x=long,y=lat,fill=score),cex=.8)+
    geom_map(data=lbr_dist_f,map=lbr_dist_f,aes(x=long,y=lat,map_id=id),alpha=0,col="black",cex=.1)+
    ggtitle("High Low with Means")
  
  quartz()
  raster1<-rasterFromXYZ(high_low_lbr[,c(3,4,9)])
  gplot(raster1)+geom_tile(aes(fill=factor(value)))  
  
  # medians
  quartz()
  ggplot()+geom_tile(data=high_low_lbr_med,aes(x=long,y=lat,fill=score),cex=.8)+
    geom_map(data=lbr_dist_f,map=lbr_dist_f,aes(x=long,y=lat,map_id=id),alpha=0,col="black",cex=.1)+
    ggtitle("High Low with Medians")


# Test Writing Function
x<-seq(0,100)
y<-list(x)
means<-vector()
for (i in 1:2){
  m<-sapply(1:length(y),function(j) mean(y[[j]]))
  means<-c(means,m)
  y<-sapply(1:length(m), function(h) {list(y[[h]][which((y[[h]]>=m[[h]]))], y[[h]][which((y[[h]]<=m[[h]]))])})
}
means<-sort(means)
means<-c(min(x),means,max(x))
means

score<-sapply(1:length(x),function(i) {sapply(1:(length(means)-1),function(j) if (x[i]>means[j] & x[i]<=means[j+1]) {j})})
x<-sapply(1:(length(means)-1),function(j) if (x[i]>means[j] & x[i]<=means[j+1]) {j})


# Initialize Data Frame
high_low_lbr<-data.frame(cell=cellsFromExtent(gpw4_2010,lbr), pop=getValues(gpw4_2010))
high_low_lbr$long<-xFromCell(gpw4_2010,high_low_lbr$cell)
high_low_lbr$lat<-yFromCell(gpw4_2010,high_low_lbr$cell)
high_low_lbr$pop2<-high_low_lbr$pop
high_low_lbr$pop2[is.na(high_low_lbr$pop2)]<-0


# Calculate Using Means
adj<-sapply(1:249984, function(i) {adj(gpw4_2010,high_low_lbr$cell[i],directions=4,pairs=F,include=F)})
adj_mean<-lapply(1:249984, function(i) {mean(sapply(1:length(adj[[i]]), function(j) {high_low_lbr$pop[adj[[i]][j]]}),na.rm=T)})

high_low_lbr$adj<-adj
high_low_lbr$adj_mean<-adj_mean

mean<-mean(high_low_lbr$pop,na.rm=T)

quart_1<-mean(high_low_lbr$pop[which(high_low_lbr$pop <= mean)],na.rm=T)
quart_3<-mean(high_low_lbr$pop[which(high_low_lbr$pop >= mean)],na.rm=T)

oct_1<-mean(high_low_lbr$pop[which(high_low_lbr$pop <= quart_1)],na.rm=T)
oct_2<-mean(high_low_lbr$pop[which(high_low_lbr$pop >= quart_1 & high_low_lbr$pop <= mean)],na.rm=T)
oct_3<-mean(high_low_lbr$pop[which(high_low_lbr$pop >= mean & high_low_lbr$pop <= quart_3)],na.rm=T)
oct_4<-mean(high_low_lbr$pop[which(high_low_lbr$pop >= quart_3)],na.rm=T)

high_low_lbr$cellscore<-ifelse(high_low_lbr$pop >= oct_4, 8,
                        ifelse(high_low_lbr$pop >= quart_3, 7,
                        ifelse(high_low_lbr$pop >= oct_3, 6,
                        ifelse(high_low_lbr$pop >= mean, 5,
                        ifelse(high_low_lbr$pop >= oct_2, 4,
                        ifelse(high_low_lbr$pop >= quart_1, 3,
                        ifelse(high_low_lbr$pop >= oct_1, 2,
                        ifelse(high_low_lbr$pop < oct_1, 1, NA))))))))

high_low_lbr$adj_score<-ifelse(high_low_lbr$adj_mean >= oct_4, 8,
                        ifelse(high_low_lbr$adj_mean >= quart_3, 7,
                        ifelse(high_low_lbr$adj_mean >= oct_3, 6,
                        ifelse(high_low_lbr$adj_mean >= mean, 5,
                        ifelse(high_low_lbr$adj_mean >= oct_2, 4,
                        ifelse(high_low_lbr$adj_mean >= quart_1, 3,
                        ifelse(high_low_lbr$adj_mean >= oct_1, 2,
                        ifelse(high_low_lbr$adj_mean < oct_1, 1, NA))))))))

high_low_lbr$score<-high_low_lbr$cellscore * 10 + high_low_lbr$adj_score

save(high_low_lbr,file = "high_low_lbr.RData")


# Calculate Using Medians
high_low_lbr_med<-high_low_lbr

quant<-quantile(high_low_lbr_med$pop,probs=seq(0,1,.125) ,na.rm=T)
quant

high_low_lbr_med$cellscore<-ifelse(high_low_lbr_med$pop >= quant[[8]], 8, 
                        ifelse(high_low_lbr_med$pop >= quant[[7]], 7,
                        ifelse(high_low_lbr_med$pop >= quant[[6]], 6,
                        ifelse(high_low_lbr_med$pop >= quant[[5]], 5,
                        ifelse(high_low_lbr_med$pop >= quant[[4]], 4,
                        ifelse(high_low_lbr_med$pop >= quant[[3]], 3,
                        ifelse(high_low_lbr_med$pop >= quant[[2]], 2,
                        ifelse(high_low_lbr_med$pop >= quant[[1]], 1, NA))))))))

high_low_lbr_med$adj_score<-ifelse(high_low_lbr_med$adj_mean >= quant[[8]], 8, 
                        ifelse(high_low_lbr_med$adj_mean >= quant[[7]], 7,
                        ifelse(high_low_lbr_med$adj_mean >= quant[[6]], 6,
                        ifelse(high_low_lbr_med$adj_mean >= quant[[5]], 5,
                        ifelse(high_low_lbr_med$adj_mean >= quant[[4]], 4,
                        ifelse(high_low_lbr_med$adj_mean >= quant[[3]], 3,
                        ifelse(high_low_lbr_med$adj_mean >= quant[[2]], 2,
                        ifelse(high_low_lbr_med$adj_mean >= quant[[1]], 1, NA))))))))

high_low_lbr_med$score<-high_low_lbr_med$cellscore * 10 + high_low_lbr_med$adj_score

save(high_low_lbr_med,file="high_low_lbr_med.RData")
####

#### SAMPLE GABARGNA ####

point<-data.frame(long=-9.471618,lat=7.001112)

gabargna<-subset(good_good2, ((good_good2$long>=-9.971618 & good_good2$long<=-8.971618) & 
                              (good_good2$lat>=6.501112 & good_good2$lat<=7.501112)))
ids<-unique(gabargna$id)

gabargna<-good_good2[good_good2$id %in% ids,]
save(gabargna,file="gabargna.RData")
#quartz()
ggplot()+geom_map(data=gabargna,map=gabargna,aes(x=long,y=lat,map_id=id))+geom_point(data = point,aes(x=long,y=lat))+geom_map(data=clan_f,map=clan_f,aes(x=long,y=lat,map_id=id))

g_pops<-gabargna[,c(6,10:16)]
g_pops[,c(2:8)]<-g_pops[,c(2:8)]/5
g_pops<-aggregate(.~id,g_pops,FUN=sum)
g_pops$id<-as.numeric(g_pops$id)
g_pops<-g_pops[order(g_pops$id),]
row.names(g_pops)<-g_pops$id
g_pops[,c(2:8)]<-round(g_pops[,c(2:8)]/10,0)
g_pop_list<-g_pops[,4]
sum(g_pop_list)
sum(g_pops[,4])

m_final<-matrix(nrow=0,ncol=3)
for (i in 1:13402){
  if (g_pops[i,4] > 0){
    sub<-gabargna[(1+(5*(i-1))):(5*i),]
    m1<-matrix(nrow=0,ncol=3)
    while(nrow(m1) < g_pops[i,4]) {
      m2<-matrix(nrow=100,ncol=3)
      m2[,1]<-runif(100,max=(max(sub$long,na.rm=T)+.00000001),min = (min(sub$long,na.rm=T)-.00000001))
      m2[,2]<-runif(100,max=(max(sub$lat,na.rm=T)+.00000001),min = (min(sub$lat,na.rm=T)-.00000001))
      m2[,3]<-point.in.polygon(m2[,1],m2[,2],sub$long,sub$lat)
      m2<-subset(m2, m2[,3]>0)
      m1<-rbind(m1,m2)
      m1<-unique(m1)
    }
    m1<-m1[1:g_pops[i,4],]
    m_final<-rbind(m_final,m1)
  }
}

gabargna_points_df<-m_final
save(gabargna_points_df,file="gabargna_points_df.RData")
load("ppp_points_df.RData")

win<- owin(xrange=c(-9.975,-8.966667),yrange=c(6.5,7.508333))
ppp_gabargna<-ppp(m_final[,1],m_final[,2],window=win)
summary(ppp_gabargna)
plot(ppp_gabargna,chars=".")

dens_gabargna<-density(ppp_gabargna,eps=.008333)
summary(dens_gabargna)
plot(dens_gabargna)

ppm_gabargna<-ppm(ppp_gabargna,~dens,covariates = list(dens=dens_gabargna))
sim_gabargna<-simulate(ppm_gabargna,nsim = 10)
plot(sim_gabargna,chars=".")
plot(density(sim_gabargna))

quadratcount(ppp_gabargna,nx=2,ny=2)

intens_gabargna<-intensity.ppp(ppp_gabargna)
smooth_gabargna<-smooth(ppp_gabargna)


### Gbargna High Low ###
gbargna<-crop(gpw4_2010,extent(c(-9.75,-9.15,6.48,7.25)))
high_low_gb<-data.frame(cell=cellsFromExtent(gbargna,extent(c(-9.75,-9.15,6.48,7.25))), pop=getValues(gbargna))
high_low_gb$long<-xFromCell(gbargna,high_low_gb$cell)
high_low_gb$lat<-yFromCell(gbargna,high_low_gb$cell)
high_low_gb$pop2<-high_low_gb$pop
high_low_gb$pop2[is.na(high_low_gb$pop2)]<-0
mean<-mean(high_low_gb$pop,na.rm=T)

#above_mean<-sapply(1:249984,function(i) {high_low_gb$pop2[i] >= mean})
adj<-sapply(1:6624, function(i) {adj(gbargna,high_low_gb$cell[i],directions=4,pairs=F,include=F)})
adj_mean<-lapply(1:6624, function(i) {mean(sapply(1:length(adj[[i]]), function(j) {high_low_gb$pop[adj[[i]][j]]}),na.rm=T)})

#high_low_gb$abv_mean<-above_mean
high_low_gb$adj<-adj
high_low_gb$adj_mean<-adj_mean

quart_1<-mean(high_low_gb$pop[which(high_low_gb$pop <= mean)],na.rm=T)
quart_3<-mean(high_low_gb$pop[which(high_low_gb$pop >= mean)],na.rm=T)

oct_1<-mean(high_low_gb$pop[which(high_low_gb$pop <= quart_1)],na.rm=T)
oct_2<-mean(high_low_gb$pop[which(high_low_gb$pop >= quart_1 & high_low_gb$pop <= mean)],na.rm=T)
oct_3<-mean(high_low_gb$pop[which(high_low_gb$pop >= mean & high_low_gb$pop <= quart_3)],na.rm=T)
oct_4<-mean(high_low_gb$pop[which(high_low_gb$pop >= quart_3)],na.rm=T)

high_low_gb$cellscore<-ifelse(high_low_gb$pop >= oct_4, 8,
                               ifelse(high_low_gb$pop >= quart_3, 7,
                                      ifelse(high_low_gb$pop >= oct_3, 6,
                                             ifelse(high_low_gb$pop >= mean, 5,
                                                    ifelse(high_low_gb$pop >= oct_2, 4,
                                                           ifelse(high_low_gb$pop >= quart_1, 3,
                                                                  ifelse(high_low_gb$pop >= oct_1, 2,
                                                                         ifelse(high_low_gb$pop < oct_1, 1, NA))))))))

high_low_gb$adj_score<-ifelse(high_low_gb$adj_mean >= oct_4, 8,
                               ifelse(high_low_gb$adj_mean >= quart_3, 7,
                                      ifelse(high_low_gb$adj_mean >= oct_3, 6,
                                             ifelse(high_low_gb$adj_mean >= mean, 5,
                                                    ifelse(high_low_gb$adj_mean >= oct_2, 4,
                                                           ifelse(high_low_gb$adj_mean >= quart_1, 3,
                                                                  ifelse(high_low_gb$adj_mean >= oct_1, 2,
                                                                         ifelse(high_low_gb$adj_mean < oct_1, 1, NA))))))))

high_low_gb$score<-high_low_gb$cellscore * 10 + high_low_gb$adj_score

ggplot()+geom_tile(data=high_low_gb,aes(x=long,y=lat,fill=factor(score)),cex=.8)+
  #geom_map(data=lbr_dist_f,map=lbr_dist_f,aes(x=long,y=lat,map_id=id),alpha=0,col="black",cex=.1)+
  ggtitle("High Low with Means")

sum(high_low_gb$pop[which(high_low_gb$score >= 66)],na.rm = T)

###
#### SAMPLE JORQUELLEH DIST ####

jorquelleh<-lbr_clan[which(lbr_clan@data$FIRST_DNAM=="Jorquelleh"),]
plot(jorquelleh)
jorquelleh@data$ID<-rep(1,5)
jorquelleh  <- unionSpatialPolygons(jorquelleh,jorquelleh@data$ID)
plot(jorquelleh)
jorquelleh_f<-point.in.polygon(good_good2$long,good_good2$lat,jorquelleh@polygons[[1]]@Polygons[[1]]@coords[,1],jorquelleh@polygons[[1]]@Polygons[[1]]@coords[,2])
jorquelleh_f<-cbind(good_good2,jorquelleh_f)
jorquelleh_f<-subset(jorquelleh_f,jorquelleh_f>0)
ggplot()+geom_map(data=jorquelleh_f,map=jorquelleh_f,aes(x=long,y=lat,map_id=id))+geom_point(data=point,aes(x=long,y=lat))


j_pops<-jorquelleh_f[,c(6,10:16)]
ids<-unique(j_pops$id)
j_pops[,c(2:8)]<-j_pops[,c(2:8)]/5
sum(j_pops$pop2010)
j_pops<-aggregate(.~id,j_pops,FUN=sum)
j_pops$id<-as.numeric(j_pops$id)
j_pops<-j_pops[order(j_pops$id),]
row.names(j_pops)<-j_pops$id
j_pops[,c(2:8)]<-round(j_pops[,c(2:8)],0)
sum(j_pops[,4])
j_sub<-good_good2[good_good2$id %in% ids,]


m_final<-matrix(nrow=0,ncol=3)
for (i in 1:1645){
  if (j_pops[i,4] > 0){
    sub<-j_sub[(1+(5*(i-1))):(5*i),]
    m1<-matrix(nrow=0,ncol=3)
    while(nrow(m1) < j_pops[i,4]) {
      m2<-matrix(nrow=100,ncol=3)
      m2[,1]<-runif(100,max=(max(sub$long,na.rm=T)+.00000001),min = (min(sub$long,na.rm=T)-.00000001))
      m2[,2]<-runif(100,max=(max(sub$lat,na.rm=T)+.00000001),min = (min(sub$lat,na.rm=T)-.00000001))
      m2[,3]<-point.in.polygon(m2[,1],m2[,2],jorquelleh@polygons[[1]]@Polygons[[1]]@coords[,1],jorquelleh@polygons[[1]]@Polygons[[1]]@coords[,2])
      m2<-subset(m2, m2[,3]>0)
      m1<-rbind(m1,m2)
      m1<-unique(m1)
    }
    m1<-m1[1:j_pops[i,4],]
    m_final<-rbind(m_final,m1)
  }
}

jorquelleh_f_points_df<-m_final
save(jorquelleh_f_points_df,file="jorquelleh_f_points_df.RData")


win<- as(jorquelleh,"owin")
m_final[,3]<-as.numeric(m_final[,3])
ppp_jorquelleh_f<-ppp(m_final[,1],m_final[,2],window=win)
summary(ppp_jorquelleh_f)
plot(ppp_jorquelleh_f,chars=".")
x<-pixellate(ppp_jorquelleh_f,eps=.008333)
summary(x)
plot(x)
points(point)

dens_jorquelleh_f<-density(ppp_jorquelleh_f,eps=.008333)
summary(dens_jorquelleh_f)
plot(dens_jorquelleh_f)

ppm_jorquelleh_f<-ppm(ppp_jorquelleh_f,~pixel,covariates = list(pixel=x))
ppm_jorquelleh_f2<-ppm(ppp_jorquelleh_f,~1)
sim_jorquelleh_f<-simulate(ppm_jorquelleh_f,nsim = 10)
sim_jorquelleh_f2<-simulate(ppm_jorquelleh_f2,nsim = 10)
#plot(sim_jorquelleh_f,chars=".")
#plot(sim_jorquelleh_f2,chars=".")
plot(density(sim_jorquelleh_f))
plot(density(sim_jorquelleh_f2))


plot(cdf.test(ppm_jorquelleh_f,x,test="ks"))
plot(cdf.test(ppm_jorquelleh_f2,x,test="ks"))

plot(berman.test(ppm_jorquelleh_f,x))
plot(berman.test(ppm_jorquelleh_f2,x))


####

#### SMALL AREA SAMPLE ####

bong_sub<-clan[clan@data$FIRST_DNAM %in% c("Jorquelleh","Panta","Zota","Kpaai","Kokoyah","Suakoko","Tukpahblee","Boinsen"),]
plot(bong_sub)
bong_sub@data$ID<-rep(1,24)
bong_sub  <- unionSpatialPolygons(bong_sub,bong_sub@data$ID)
plot(bong_sub)
bong_sub_f<-point.in.polygon(good_good2$long,good_good2$lat,bong_sub@polygons[[1]]@Polygons[[1]]@coords[,1],bong_sub@polygons[[1]]@Polygons[[1]]@coords[,2])
bong_sub_f<-cbind(good_good2,bong_sub_f)
bong_sub_f<-subset(bong_sub_f,bong_sub_f>0)
ggplot()+geom_map(data=bong_sub_f,map=bong_sub_f,aes(x=long,y=lat,map_id=id))+geom_point(data=point,aes(x=long,y=lat))


bong_sub_pops<-bong_sub_f[,c(6,10:16)]
ids<-unique(bong_sub_pops$id)
bong_sub_pops[,c(2:8)]<-bong_sub_pops[,c(2:8)]/5
sum(bong_sub_pops$pop2010)
bong_sub_pops<-aggregate(.~id,bong_sub_pops,FUN=sum)
bong_sub_pops$id<-as.numeric(bong_sub_pops$id)
bong_sub_pops<-bong_sub_pops[order(bong_sub_pops$id),]
row.names(bong_sub_pops)<-bong_sub_pops$id
bong_sub_pops[,c(2:8)]<-round(bong_sub_pops[,c(2:8)],0)
sum(bong_sub_pops[,4])
b_sub<-good_good2[good_good2$id %in% ids,]


m_final<-matrix(nrow=0,ncol=3)
for (i in 1:29340){
  if (bong_sub_pops[i,4] > 0){
    sub<-b_sub[(1+(5*(i-1))):(5*i),]
    m1<-matrix(nrow=0,ncol=3)
    while(nrow(m1) < bong_sub_pops[i,4]) {
      m2<-matrix(nrow=100,ncol=3)
      m2[,1]<-runif(100,max=(max(sub$long,na.rm=T)+.00000001),min = (min(sub$long,na.rm=T)-.00000001))
      m2[,2]<-runif(100,max=(max(sub$lat,na.rm=T)+.00000001),min = (min(sub$lat,na.rm=T)-.00000001))
      m2[,3]<-point.in.polygon(m2[,1],m2[,2],bong_sub@polygons[[1]]@Polygons[[1]]@coords[,1],bong_sub@polygons[[1]]@Polygons[[1]]@coords[,2])
      m2<-subset(m2, m2[,3]>0)
      m1<-rbind(m1,m2)
      m1<-unique(m1)
    }
    m1<-m1[1:bong_sub_pops[i,4],]
    m_final<-rbind(m_final,m1)
  }
}

bong_sub_f_points_df<-m_final
save(bong_sub_f_points_df,file="bong_sub_f_points_df.RData")


win<- as(bong_sub,"owin")
ppp_bong_sub_f<-ppp(m_final[,1],m_final[,2],window=win)
summary(ppp_bong_sub_f)
plot(ppp_bong_sub_f,chars=".")

dens_bong_sub_f<-density(ppp_bong_sub_f,eps=.008333)
summary(dens_bong_sub_f)
plot(dens_bong_sub_f)
points(point)

ppm_bong_sub_f<-ppm(ppp_bong_sub_f,~dens,covariates = list(dens=dens_bong_sub_f))
sim_bong_sub_f<-simulate(ppm_bong_sub_f,nsim = 10)
plot(sim_bong_sub_f,chars=".")
plot(density(sim_bong_sub_f))

plot(quadratcount(ppp_bong_sub_f,nx=10,ny=10))

intens_bong_sub_f<-intensity.ppp(ppp_bong_sub_f)
smooth_bong_sub_f<-smooth(ppp_bong_sub_f)


#### SPATSTAT EXPERIMENT ####

quartz()
plot.new()
par(mfrow=c(1,3))
rand_num<-sample(1:100,36,replace=T)
win_list<-list()
for (i  in 0:5 ) {
  for (j in 0:5){
    win<-owin(xrange=c(i,i+1),yrange=c(j,j+1))
    win_list<-c(win_list,list(win))
  }
}
ppp_list<-lapply(1:36,function(i) rpoint(rand_num[[i]],win=win_list[[i]]))
ppp_list<-as.solist(ppp_list)
ppp_total<-superimpose(ppp_list,W=tess(tiles=win_list))
x<-pixellate(ppp_total,eps=1)
plot(x)

#plot(ppp_total)
dens1<-density(ppp_total,eps=.01)
summary(dens1)
dens1[ppp_total]
plot(dens1)
plot(quadratcount(ppp_total,nx=6,ny=6))
intensity(ppp_total)
intens<-intensity(quadratcount(ppp_total,nx=6,ny=6),image=T)
plot(intens)

summary(ppp_total)

####
#### 10% SAMPLE ####

# good_good<-left_join(y,data3,by="id")
# colnames(good_good)<-c("long","lat","order","hole","piece","id","group","ct_long","ct_lat","pop2010")
# save(good_good,file="good_good.RData")
# good_good2<-left_join(good_good,lbr_data_final,by=c(c("ct_long"="long"),c("ct_lat"="lat")))
# good_good2<-good_good2[,c(1:9,11:17)]
# colnames(good_good2)[6]<-"id"
# colnames(good_good2)[12]<-"pop2010"
# save(good_good2,file="complete_data.RData")

pops<-good_good2[,c(6,10:16)]
pops[,c(2:8)]<-pops[,c(2:8)]/5
pops<-aggregate(.~id,pops,FUN=sum)
pops$id<-as.numeric(pops$id)
pops<-pops[order(pops$id),]
row.names(pops)<-pops$id
pops[,c(2:8)]<-round(pops[,c(2:8)]/10,0)
pop_list<-pops[,4]
sum(pop_list)
sum(pops[,4])

m_final<-matrix(nrow=0,ncol=3)
for (i in 1:112830){
  if (pop_list[i] > 0){
    sub<-good_good2[(1+(5*(i-1))):(5*i),]
    m1<-matrix(nrow=0,ncol=3)
    while(nrow(m1) < pop_list[[i]]) {
      m2<-matrix(nrow=100,ncol=3)
      m2[,1]<-runif(100,max=(max(sub$long,na.rm=T)+.00000001),min = (min(sub$long,na.rm=T)-.00000001))
      m2[,2]<-runif(100,max=(max(sub$lat,na.rm=T)+.00000001),min = (min(sub$lat,na.rm=T)-.00000001))
      m2[,3]<-point.in.polygon(m2[,1],m2[,2],sub$long,sub$lat)
      m2<-subset(m2, m2[,3]>0)
      m1<-rbind(m1,m2)
      m1<-unique(m1)
    }
    m1<-m1[1:pop_list[[i]],]
    m_final<-rbind(m_final,m1)
  }
}


save(points_df_10percent,file="points_df_10percent.RData")
load("ppp_points_df.RData")

win<-as(lbr,"owin")
ppp_10percent<-ppp(points_df_10percent[,1],points_df_10percent[,2],window=win)
summary(ppp_10percent)
plot(ppp_10percent,chars=".")

plot(pixellate(ppp_10percent))

dens_10percent<-density(ppp_10percent,eps=.008333)
summary(dens_10percent)
plot(dens_10percent)
contour(dens_10percent)

ppm_10percent<-ppm(ppp_10percent,~density,covariates = list(density=dens_10percent))
sim_10percent<-simulate(ppm_10percent,nsim = 10)
plot(sim_10percent)
plot(density(sim_10percent))

plot(quadratcount(ppp_10percent,nx=100,ny=100))

intens_10percent<-intensity.ppp(ppp_10percent)
smooth_10percent<-smooth(ppp_10percent)

495.84063362534505

503.839313572543

####
#### 2% SAMPLE ####

pops<-good_good2[,c(6,10:16)]
pops[,c(2:8)]<-pops[,c(2:8)]/5
pops2<-aggregate(.~id,pops,FUN=sum)
pops2$id<-as.numeric(pops2$id)
pops2<-pops2[order(pops2$id),]
row.names(pops2)<-pops2$id
pops2[,c(2:8)]<-round(pops2[,c(2:8)]/50,0)

test_pop<-subset(pops2,pops2$pop2010<=0)
test_grouping<-pops2[pops2$id %in% test_pop$id,]
quartz()
ggplot(country_f)+geom_map(data=test_grouping,map = test_grouping,aes(x=long,y=lat,map_id=id))
sum(test_grouping$pop2010)
test<-good_good2[,c(1:9)]
test$id<-as.numeric(test$id)
test_grouping<-right_join(test,test_grouping,by = c("id"="id"))
x<-chull(test_grouping$long,test_grouping$lat)
poly<-test_grouping[x,]
ggplot()+geom_polygon(data=poly,aes(x=long,y=lat))

pop_list<-pops[,4]
sum(pop_list)
sum(pops[,4])
####
#### PPM TEST (Clan level) ####
gpw4_2000_clan<-extract(gpw4_2000,clan,df=TRUE)
gpw4_2000_clan<-aggregate(.~ID,gpw4_2000_clan,FUN=sum)
gpw4_2000_clan$ID<-as.character(seq(0,815))
clan_f_sum<-clan_f
clan_f_sum$x<-1
clan_f_sum<-aggregate(x~id,clan_f_sum,FUN=sum)
gpw4_2000_clan2<-gpw4_2000_clan
gpw4_2000_clan2$gpw.v4.population.count_2000<-gpw4_2000_clan2$gpw.v4.population.count_2000/clan_f_sum$x
clan_df<-left_join(clan_f,gpw4_2000_clan,by=c("id"="ID"))
clan_df<-left_join(clan_df,gpw4_2000_clan2,by=c("id"="ID"))
clan_f_sum$id<-as.numeric(clan_f_sum$id)

sp_list_2<-list()
sp_list_2<- lapply(1:length(clan), function(i) clan[i,]@polygons[[1]])
pop_list_2<-gpw4_2000_clan$gpw.v4.population.count_2000
pop_list_2<-pop_list_2/100
pop_list_2<-round(pop_list_2,0)

sp_list_2[[1]]
pop_list_2[[1]]


id<-1:nrow(data3)
data3<-cbind(data3,id)
data3<-as.data.frame(data3)
data3$id<-as.character(data3$id)


p<-point.in.polygon(coords[[1]][,1],coords[[1]][,2],x[[1]],y[[1]])

m_final<-matrix(nrow=0,ncol=3)
for (i in 1:length(coords)){
  m1<-matrix(nrow=0,ncol=3)
  while(nrow(m1) < pop_list_2[[i]]) {
    m2<-matrix(nrow=100,ncol=3)
    m2[,1]<-runif(100,max=(max(coords[[i]][,1],na.rm=T)+.00000001),min = (min(coords[[i]][,1],na.rm=T)-.00000001))
    m2[,2]<-runif(100,max=(max(coords[[i]][,2],na.rm=T)+.00000001),min = (min(coords[[i]][,2],na.rm=T)-.00000001))
    m2[,3]<-point.in.polygon(m2[,1],m2[,2],coords[[i]][,1],coords[[i]][,2])
    m2<-subset(m2, m2[,3]>0)
    m1<-rbind(m1,m2)
    m1<-unique(m1)
  }
  m1<-m1[1:pop_list_2[[i]],]
  m_final<-rbind(m_final,m1)
}

win<-as(country,"owin")
hope<-ppp(m_final[,1],m_final[,2],window=win)
plot(hope)
dens<-density(hope,.5)
plot(dens)
summary(hope)
plot(density(hope))

####
#### BASIC PPP PLOTS (Liberia) #####
plus_10_data<-subset(lbr_data_final,lbr_data_final$pop2010 >= 10)
plus_20_data<-subset(lbr_data_final,lbr_data_final$pop2010 >= 20)
plus_30_data<-subset(lbr_data_final,lbr_data_final$pop2010 >= 30)

win<-as(country,"owin")

ppp0<-ppp(lbr_data_final$long,lbr_data_final$lat,marks=lbr_data_final$pop2010,window=win)
summary(ppp0)
ppp0_1<-ppp(lbr_data_final$long,lbr_data_final$lat,window=win)
summary(ppp0)

identify(ppp0)

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

####
#### CHANGE COORDINATES (NE) ####
lbr_data_final_NE<-lbr_data_final
coordinates(lbr_data_final_NE)<-c("long","lat")
proj4string(lbr_data_final_NE)<-CRS("+proj=longlat +datum=WGS84")
lbr_data_final_NE<- spTransform(lbr_data_final_NE, CRS("+proj=utm +zone=29 ellps=WGS84"))
lbr_data_final_NE<- as.data.frame(lbr_data_final_NE)
colnames(lbr_data_final_NE)[9:10]<-c("east","north")

country_NE<-spTransform(country, CRS("+proj=utm +zone=29 ellps=WGS84"))
win_NE<-as(country_NE,"owin")
ppp_test<-ppp(lbr_data_final_NE$east,lbr_data_final_NE$north,marks=lbr_data_final_NE$pop2010,window = win_NE)
ppp_test<-rescale.ppp(ppp_test,1000)
unitname(ppp_test)<-"kilometers"
plot(ppp_test)
summary(ppp_test)
dens<-density(ppp_test)
summary(dens)
plot(dens)

dimyx=c(ny, nx)

mean(lbr_data_final_NE$pop2010)

####
#### MAPS FROM SHAPEFILES/DATA ####

### CORRECT DATA ###
cnty@data$SUM_HH[1]<-20508
dist@data[which(dist@data$FIRST_CCNA=="Bomi"),]$HHOLDS<-dist@data[which(dist@data$FIRST_CCNA=="Bomi"),]$TOTAL/(cnty@data$SUM_TOTAL[1]/cnty@data$SUM_HH[1])
clan@data[which(clan@data$FIRST_CCNA=="Bomi"),]$SUM_HH<-clan@data[which(clan@data$FIRST_CCNA=="Bomi"),]$SUM_TOTAL/(cnty@data$SUM_TOTAL[1]/cnty@data$SUM_HH[1])
clan@data[which(clan@data$FIRST_CCNA=="River Gee"),]$SUM_HH[29]<-clan@data[which(clan@data$FIRST_CCNA=="River Gee"),]$SUM_TOTAL[29]/mean(clan@data[which(clan@data$FIRST_CCNA=="River Gee"),]$SUM_TOTAL[c(1:28,30:45)]/clan@data[which(clan@data$FIRST_CCNA=="River Gee"),]$SUM_HH[c(1:28,30:45)])


### MERGE DATA ###
cnty@data$ID<-as.character(c(0:14))
cnty_shp_f<-left_join(x=cnty_f,y=cnty@data,by=c("id"="ID"))
dist@data$ID<-as.character(c(0:135))
dist_shp_f<-left_join(x=dist_f,y=dist@data,by=c("id"="ID"))
clan@data$ID<-as.character(c(0:815))
clan_shp_f<-left_join(x=clan_f,y=clan@data,by=c("id"="ID"))

# ### ADJUST LABELS ###
# ct_points<-gCentroid(counties,byid=TRUE)
# county_labels<-cbind.data.frame(counties@data$CCNAME,ct_points@coords[,1],ct_points@coords[,2])
# colnames(county_labels)<-c("name","x","y")
# county_labels[9,2]<--10.2
# county_labels[9,3]<-6.63
# ct_points_2<-gCentroid(districts,byid=TRUE)
# district_labels<-cbind.data.frame(districts@data$DNAME,ct_points_2@coords[,1],ct_points_2@coords[,2])
# colnames(district_labels)<-c("name","x","y")
# ct_points_3<-gCentroid(clans,byid=TRUE)
# clan_labels<-cbind.data.frame(clans@data$CLNAME,clans@data$FIRST_DNAM,ct_points_3@coords[,1],ct_points_3@coords[,2])
# colnames(clan_labels)<-c("name","district","x","y")
# clan_labels2<-clan_labels
# n<-nrow(clan_labels[which(clan_labels$district=="Greater Monrovia"),])
# remove<-vector("list",length=n)
# clan_labels[which(clan_labels$dist == "Greater Monrovia"),]$name <- remove
# m<-nrow(clan_labels[which(clan_labels$dist=="Commonwealth"),])
# remove2<-vector("list",m)
# clan_labels[which(clan_labels$dist == "Commonwealth"),]$name <- remove2



### MAKE MAPS ###
map4<-ggplot()+
  geom_map(data=cnty_shp_f,map=cnty_shp_f,aes(x=long,y=lat,map_id=id,fill=log(SUM_TOTAL)))+
  scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
  geom_map(data=cnty_shp_f,map=cnty_shp_f,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.2)+
  theme(text = element_text(color = "white"),
        rect = element_rect(fill = "grey35", color = "grey35"),
        plot.background = element_rect(fill = "grey35", color = "grey35"),
        panel.background = element_rect(fill = "grey35", color = "grey35"),
        plot.title = element_text(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.1,.2))+
  ggtitle("County Pop. (Shapefile)")


map5<-ggplot()+
  geom_map(data=dist_shp_f,map=dist_shp_f,aes(x=long,y=lat,map_id=id,fill=log(TOTAL)))+
  scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
  geom_map(data=cnty_shp_f,map=cnty_shp_f,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
  theme(text = element_text(color = "white"),
        rect = element_rect(fill = "grey35", color = "grey35"),
        plot.background = element_rect(fill = "grey35", color = "grey35"),
        panel.background = element_rect(fill = "grey35", color = "grey35"),
        plot.title = element_text(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.1,.2))+
  ggtitle("District Pop. (Shapefile))")


map6<-ggplot()+
  geom_map(data=clan_shp_f,map=clan_shp_f,aes(x=long,y=lat,map_id=id,fill=log(SUM_TOTAL)))+
  scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
  geom_map(data=cnty_shp_f,map=cnty_shp_f,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
  theme(text = element_text(color = "white"),
        rect = element_rect(fill = "grey35", color = "grey35"),
        plot.background = element_rect(fill = "grey35", color = "grey35"),
        panel.background = element_rect(fill = "grey35", color = "grey35"),
        plot.title = element_text(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.1,.2))+
  ggtitle("Clan Pop. (Shapefile)")

####
#### MAPS FROM GPW4 ####

### JOIN DATA ###

#country_gpw4<-extract(gpw4,country,df=TRUE)
#save(country_gpw4,file = "country_gpw4.RData")
load("country_gpw4.RData")
country_gpw4_totals<-aggregate(.~ID,country_gpw4,sum)
country_gpw4_totals$ID<-as.character((country_gpw4_totals$ID-1))
names(country_gpw4_totals)[2]<-"pop"
country_gpw4_f<-left_join(x=country_f,y=country_gpw4_totals,by=c("id"="ID"))


#cnty_gpw4<-extract(gpw4,cnty,df=TRUE)
#save(cnty_gpw4,file = "cnty_gpw4.RData")
load("cnty_gpw4.RData")
cnty_gpw4_totals<-aggregate(.~ID,cnty_gpw4,sum)
cnty_gpw4_totals$ID<-as.character((cnty_gpw4_totals$ID-1))
names(cnty_gpw4_totals)[2]<-"pop"
cnty_gpw4_f<-left_join(x=cnty_f,y=cnty_gpw4_totals,by=c("id"="ID"))


#dist_gpw4<-extract(gpw4,dist,df=TRUE)
#save(dist_gpw4,file = "dist_gpw4.RData")
load("dist_gpw4.RData")
dist_gpw4_totals<-aggregate(.~ID,dist_gpw4,sum)
dist_gpw4_totals$ID<-as.character((dist_gpw4_totals$ID-1))
names(dist_gpw4_totals)[2]<-"pop"
dist_gpw4_f<-left_join(x=dist_f,y=dist_gpw4_totals,by=c("id"="ID"))


#clan_gpw4<-extract(gpw4,clan,df=TRUE)
#save(clan_gpw4,file = "clan_gpw4.RData")
load("clan_gpw4.RData")
clan_gpw4_totals<-aggregate(.~ID,clan_gpw4,sum)
clan_gpw4_totals$ID<-as.character((clan_gpw4_totals$ID-1))
names(clan_gpw4_totals)[2]<-"pop"
clan_gpw4_f<-left_join(x=clan_f,y=clan_gpw4_totals,by=c("id"="ID"))


### MAKE MAPS ###
# map0<-ggplot(country_gpw4_f, aes(x=long, y = lat))+
#   geom_map(data = country_gpw4_f, map=country_gpw4_f,aes(x=long, y=lat,map_id=id,fill=pop))+
#   scale_fill_gradient(low="yellow",high="red",space="Lab")
# map0

# map1<-ggplot(cnty_gpw4_f, aes(x=long, y = lat))+
#   geom_map(data = cnty_gpw4_f, map=cnty_gpw4_f,aes(x=long, y=lat,map_id=id,fill=pop))+
#   scale_fill_gradient(low="yellow",high="red",space="Lab")
# map1

# map2<-ggplot(dist_gpw4_f, aes(x=long, y = lat))+
#   geom_map(data = dist_gpw4_f, map=dist_gpw4_f,aes(x=long, y=lat,map_id=id,fill=pop))+
#   scale_fill_gradient(low="yellow",high="red",space="Lab")
# map2

# map3<-ggplot(clan_gpw4_f, aes(x=long, y = lat))+
#   geom_map(data = clan_gpw4_f, map=clan_gpw4_f,aes(x=long, y=lat,map_id=id,fill=pop))+
#   scale_fill_gradient(low="yellow",high="red",space="Lab")
# map3

map1<-ggplot()+
  geom_map(data=cnty_gpw4_f,map=cnty_gpw4_f,aes(x=long,y=lat,map_id=id,fill=log(pop)))+
  scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
  geom_map(data=cnty_gpw4_f,map=cnty_gpw4_f,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
  theme(text = element_text(color = "white"),
        rect = element_rect(fill = "grey35", color = "grey35"),
        plot.background = element_rect(fill = "grey35", color = "grey35"),
        panel.background = element_rect(fill = "grey35", color = "grey35"),
        plot.title = element_text(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.1,.2))+
  ggtitle("County Pop. (gpw4)")

map2<-ggplot()+
  geom_map(data=dist_gpw4_f,map=dist_gpw4_f,aes(x=long,y=lat,map_id=id,fill=log(pop)))+
  scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
  geom_map(data=cnty_gpw4_f,map=cnty_gpw4_f,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
  theme(text = element_text(color = "white"),
        rect = element_rect(fill = "grey35", color = "grey35"),
        plot.background = element_rect(fill = "grey35", color = "grey35"),
        panel.background = element_rect(fill = "grey35", color = "grey35"),
        plot.title = element_text(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.1,.2))+
  ggtitle("District Pop. (gpw4)")

map3<-ggplot()+
  geom_map(data=clan_gpw4_f,map=clan_gpw4_f,aes(x=long,y=lat,map_id=id,fill=log(pop)))+
  scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
  geom_map(data=cnty_gpw4_f,map=cnty_gpw4_f,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
  theme(text = element_text(color = "white"),
        rect = element_rect(fill = "grey35", color = "grey35"),
        plot.background = element_rect(fill = "grey35", color = "grey35"),
        panel.background = element_rect(fill = "grey35", color = "grey35"),
        plot.title = element_text(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(.1,.2))+
  ggtitle("Clan Pop. (gpw4)")

comparison<-arrangeGrob(map1,map4,map2,map5,map3,map6,nrow=3,ncol=2)
ggsave("comparison.png",comparison,width = 20, height = 18, dpi = 150)

####

#### SURVEY DATA (for later) ####
load("cwiq10.RData")

cnty_survey_cpts <- cbind.data.frame(county_id = counties@data$FIRST_CCOD, county_labs, stringsAsFactors = FALSE)
dist_survey_cpts <- cbind.data.frame(county_id = districts@data$FIRST_CCOD, district_id = districts@data$FIRST_DCOD, district_labs)
clan_survey_cpts <- cbind.data.frame(county_id = clans@data$FIRST_CCOD, district_id = clans@data$FIRST_DCOD, clan_id = clans@data$FIRST_CLCO, clan_labs)

# Counties
# table(cnty_survey_cpts$county_id)
# table(cwiq10$county)
cwiq10$county <- substr(cwiq10$hid_mungai,1,2)
cwiq10$county[cwiq10$county == "01"] <- "30"
county_cwiq10 <- left_join(x = cnty_survey_cpts, y = cwiq10, by = c("county_id" = "county"))

# Districts
# length(table(dist_survey_cpts$district_code))
# length(table(cwiq10$district_code))
cwiq10$district <- substr(cwiq10$hid_mungai,3,4)
dist_survey_cpts$district_code  <- paste(dist_survey_cpts$county_id, dist_survey_cpts$district_id, sep = "")
cwiq10$district_code <- paste(cwiq10$county, cwiq10$district, sep= "")
district_cwiq10 <- left_join(x = dist_survey_cpts, y = cwiq10, by = c("district_code" = "district_code"))
# anti_join(x = dist_survey_cpts, y = cwiq10, by = c("district_code" = "district_code"))
# anti_join(x = cwiq10, y = dist_survey_cpts, by = c("district_code" = "district_code"))

# Clans
cwiq10$clan_town <- substr(cwiq10$hid_mungai,5,7)
clan_survey_cpts$clan_code  <- paste(clan_survey_cpts$county_id, clan_survey_cpts$district_id, clan_survey_cpts$clan_id, sep = "")
cwiq10$clan_town_code <- paste(cwiq10$county, cwiq10$district, cwiq10$clan_town, sep= "")
clan_cwiq10 <- left_join(x = clan_survey_cpts, y = cwiq10, by = c("clan_code" = "clan_town_code"))
# anti_join(x = clan_survey_cpts, y = cwiq10, by = c("clan_code" = "clan_town_code"))
# anti_join(x = cwiq10, y = clan_survey_cpts, by = c("clan_town_code" = "clan_code"))

# Being Analyzing Point Pattern

#Add National boundary for Liberia

options(scipen=999)

country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(country)
country <- spTransform(country, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

win <- as(country,"owin")

cnty_weights <- county_cwiq10$wta_hh
dist_weights <- district_cwiq10$wta_hh
clan_weights <- clan_cwiq10$wta_hh

# Total Population

lbr_cnty <- ppp(x = county_cwiq10$x, y = county_cwiq10$y, window = win, marks = cnty_weights)
lbr_dist <- ppp(x = district_cwiq10$x, y = district_cwiq10$y, window = win, marks = dist_weights)
lbr_clan <- ppp(x = clan_cwiq10$x, y = clan_cwiq10$y, window = win, marks = clan_weights)


par(mar=c(0,0,1,0))

png("latex/pop_county.png")
plot(density(lbr_cnty), main = "County Weights")
dev.off()

png("latex/pop_district.png")
plot(density(lbr_dist), main = "District Weights")
dev.off()

png("latex/pop_clan.png")
plot(density(lbr_clan), main = "Clan Weights")
dev.off()


# Population by Gender

cnty_gender <- as.factor(county_cwiq10$n1)
dist_gender <- as.factor(district_cwiq10$n1)
clan_gender <- as.factor(clan_cwiq10$n1)

levels(cnty_gender) <- c("male", "female")
levels(dist_gender) <- c("male", "female")
levels(clan_gender) <- c("male", "female")

lbr_cnty_gender <- ppp(x = county_cwiq10$x, y = county_cwiq10$y, window = win, marks = cnty_gender)
lbr_dist_gender <- ppp(x = district_cwiq10$x, y = district_cwiq10$y, window = win, marks = dist_gender)
lbr_clan_gender <- ppp(x = clan_cwiq10$x, y = clan_cwiq10$y, window = win, marks = clan_gender)

par(oma=c(0,0,0,0))
par(mar=c(0,0,0,0))

pdf("latex/gender_county.pdf", width = 10, height = 5)
plot(density(split(lbr_cnty_gender, weights = cnty_weights)), main = NA)
dev.off()

pdf("latex/gender_district.pdf", width = 10, height = 5)
plot(density(split(lbr_dist_gender, weights = dist_weights)), main = NA)
dev.off()

pdf("latex/gender_clan.pdf", width = 10, height = 5)
plot(density(split(lbr_clan_gender, weights = clan_weights)), main = NA)
dev.off()

#Female Population

cnty_gender_female <- subset(county_cwiq10, n1 == 2)
dist_gender_female <- subset(district_cwiq10, n1 == 2)
clan_gender_female <- subset(clan_cwiq10, n1 == 2)

cnty_female_ppp <- ppp(x = cnty_gender_female$x, y = cnty_gender_female$y, window = win, marks = cnty_gender_female$wta_hh)
dist_female_ppp <- ppp(x = dist_gender_female$x, y = dist_gender_female$y, window = win, marks = dist_gender_female$wta_hh)
clan_female_ppp <- ppp(x = clan_gender_female$x, y = clan_gender_female$y, window = win, marks = clan_gender_female$wta_hh)

pdf("latex/female_county.pdf")
plot(density(cnty_female_ppp), main = "County Weights")
dev.off()

pdf("latex/female_dist.pdf")
plot(density(dist_female_ppp), main = "District Weights")
dev.off()

pdf("latex/female_clan.pdf")
plot(density(clan_female_ppp), main = "Clan Weights")
dev.off()

#Male Population

cnty_gender_male <- subset(county_cwiq10, n1 == 1)
dist_gender_male <- subset(district_cwiq10, n1 == 1)
clan_gender_male <- subset(clan_cwiq10, n1 == 1)

cnty_male_ppp <- ppp(x = cnty_gender_male$x, y = cnty_gender_male$y, window = win, marks = cnty_gender_male$wta_hh)
dist_male_ppp <- ppp(x = dist_gender_male$x, y = dist_gender_male$y, window = win, marks = dist_gender_male$wta_hh)
clan_male_ppp <- ppp(x = clan_gender_male$x, y = clan_gender_male$y, window = win, marks = clan_gender_male$wta_hh)

pdf("latex/male_county.pdf")
plot(density(cnty_male_ppp), main = "County Weights")
dev.off()

pdf("latex/male_dist.pdf")
plot(density(dist_male_ppp), main = "District Weights")
dev.off()

pdf("latex/male_clan.pdf")
plot(density(clan_male_ppp), main = "Clan Weights")
dev.off()

# Ethnicity in Montserrado County

#Base map Montserrado
coastal_lbr <- get_map(location = c(-11.00, 6.10, -10.25, 6.85), maptype = "watercolor")
coastal_lbr <- ggmap(coastal_lbr)

coastal_lbr <- coastal_lbr + geom_map(data=clans_f,map=clans_f,aes(x=long, y=lat, map_id=id,fill = log10(SUM_TOTAL)))
coastal_lbr <- coastal_lbr + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
coastal_lbr <- coastal_lbr + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
coastal_lbr <- coastal_lbr + scale_fill_gradient(low="yellow", high="red", space="Lab")
coastal_lbr <- coastal_lbr + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
coastal_lbr <- coastal_lbr + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
coastal_lbr <- coastal_lbr + ggtitle("Clans")
ggsave("latex/clans_coastal_lbr.pdf", coastal_lbr, width = 10, height = 10, dpi = 300)

par(mar=c(0,0,1,1))

clan_ethnic_bassa <- subset(clan_cwiq10, n11 == 1)
clan_ethnic_kpelle <- subset(clan_cwiq10, n11 == 8)
clan_ethnic_kissi <- subset(clan_cwiq10, n11 == 9)
clan_ethnic_lorma <- subset(clan_cwiq10, n11 == 12)

montserrado <- subset(clans, FIRST_CCOD == 30 )

m_win <- as(montserrado,"owin")

clan_monts_bassa <- ppp(x = clan_ethnic_bassa$x, y = clan_ethnic_bassa$y, window = m_win, marks = clan_ethnic_bassa$wta_hh)
clan_monts_kpelle <- ppp(x = clan_ethnic_kpelle$x, y = clan_ethnic_kpelle$y, window = m_win, marks = clan_ethnic_kpelle$wta_hh)
clan_monts_kissi <- ppp(x = clan_ethnic_kissi$x, y = clan_ethnic_kissi$y, window = m_win, marks = clan_ethnic_kissi$wta_hh)
clan_monts_lorma <- ppp(x = clan_ethnic_lorma$x, y = clan_ethnic_lorma$y, window = m_win, marks = clan_ethnic_lorma$wta_hh)

pdf("latex/clan_monts_bassa.pdf")
plot(density(clan_monts_bassa), main = "Bassa")
dev.off()

pdf("latex/clan_monts_kpelle.pdf")
plot(density(clan_monts_kpelle), main = "Kpelle")
dev.off()

pdf("latex/clan_monts_kissi.pdf")
plot(density(clan_monts_kissi), main = "Kissi")
dev.off()

pdf("latex/clan_monts_lorma.pdf")
plot(density(clan_monts_lorma), main = "Lorma")
dev.off()

# Compare Religion and Education across Liberia

clan_religion_christian <- subset(clan_cwiq10, n12 == 1)
clan_religion_muslim <- subset(clan_cwiq10, n12 == 2)
clan_edu_none <- subset(clan_cwiq10, o3 == 0)
clan_edu_p6 <- subset(clan_cwiq10, o3 == 16)
clan_edu_sh12 <- subset(clan_cwiq10, o3 == 26)
clan_edu_uni <- subset(clan_cwiq10, o3 == 31)


clan_cnty_christian <- ppp(x = clan_religion_christian$x, y = clan_religion_christian$y, window = win, marks = clan_religion_christian$wta_hh)
clan_cnty_muslim <- ppp(x = clan_religion_muslim$x, y = clan_religion_muslim$y, window = win, marks = clan_religion_muslim$wta_hh)
clan_cnty_none <- ppp(x = clan_edu_none$x, y = clan_edu_none$y, window = win, marks = clan_edu_none$wta_hh)
clan_cnty_p6 <- ppp(x = clan_edu_p6$x, y = clan_edu_p6$y, window = win, marks = clan_edu_p6$wta_hh)
clan_cnty_sh12 <- ppp(x = clan_edu_sh12$x, y = clan_edu_sh12$y, window = win, marks = clan_edu_sh12$wta_hh)
clan_cnty_uni <- ppp(x = clan_edu_uni$x, y = clan_edu_uni$y, window = win, marks = clan_edu_uni$wta_hh)

par(mar=c(0,0,1,1))

pdf("latex/clan_cnty_christian.pdf")
plot(density(clan_cnty_christian), main = "Christian")
dev.off()

pdf("latex/clan_cnty_muslim.pdf")
plot(density(clan_cnty_muslim), main = "Muslim")
dev.off()

pdf("latex/clan_cnty_none.pdf")
plot(density(clan_cnty_none), main = "No Education")
dev.off()

pdf("latex/clan_cnty_p6.pdf")
plot(density(clan_cnty_p6), main = "Grade School")
dev.off()

pdf("latex/clan_cnty_sh12.pdf")
plot(density(clan_cnty_sh12), main = "High School")
dev.off()

pdf("latex/clan_cnty_uni.pdf")
plot(density(clan_cnty_uni), main = "University")
dev.off()

# Compare Religion and Education in Montserrado

clan_mont_christian <- ppp(x = clan_religion_christian$x, y = clan_religion_christian$y, window = m_win, marks = clan_religion_christian$wta_hh)
clan_mont_muslim <- ppp(x = clan_religion_muslim$x, y = clan_religion_muslim$y, window = m_win, marks = clan_religion_muslim$wta_hh)
clan_mont_none <- ppp(x = clan_edu_none$x, y = clan_edu_none$y, window = m_win, marks = clan_edu_none$wta_hh)
clan_mont_p6 <- ppp(x = clan_edu_p6$x, y = clan_edu_p6$y, window = m_win, marks = clan_edu_p6$wta_hh)
clan_mont_sh12 <- ppp(x = clan_edu_sh12$x, y = clan_edu_sh12$y, window = m_win, marks = clan_edu_sh12$wta_hh)
clan_mont_uni <- ppp(x = clan_edu_uni$x, y = clan_edu_uni$y, window = m_win, marks = clan_edu_uni$wta_hh)

par(mar=c(0,0,1,1))

pdf("latex/clan_mont_christian.pdf")
plot(density(clan_mont_christian), main = "Christian")
dev.off()

pdf("latex/clan_mont_muslim.pdf")
plot(density(clan_mont_muslim), main = "Muslim")
dev.off()

pdf("latex/clan_mont_none.pdf")
plot(density(clan_mont_none), main = "No Education")
dev.off()

pdf("latex/clan_mont_p6.pdf")
plot(density(clan_mont_p6), main = "Grade School")
dev.off()

pdf("latex/clan_mont_sh12.pdf")
plot(density(clan_mont_sh12), main = "High School")
dev.off()

pdf("latex/clan_mont_uni.pdf")
plot(density(clan_mont_uni), main = "University")
dev.off()



# Population by Age

cnty_age <- cut(county_cwiq10$n4, 4)
dist_age <- cut(district_cwiq10$n4, 4)
clan_age <- cut(clan_cwiq10$n4, 4)

lbr_cnty_age <- ppp(x = county_cwiq10$x, y = county_cwiq10$y, window = win, marks = cnty_age)
lbr_dist_age <- ppp(x = district_cwiq10$x, y = district_cwiq10$y, window = win, marks = dist_age)
lbr_clan_age <- ppp(x = clan_cwiq10$x, y = clan_cwiq10$y, window = win, marks = clan_age)

plot(density(split(lbr_cnty_age, weights = cnty_weights)))
plot(density(split(lbr_dist_age, weights = dist_weights)))
plot(density(split(lbr_clan_age, weights = clan_weights)))

# Ethnicity

cnty_ethnic <- as.factor(county_cwiq10$n11)
dist_ethnic <- as.factor(district_cwiq10$n11)
clan_ethnic <- as.factor(clan_cwiq10$n11)

levels(cnty_ethnic) <- c("bassa", "belle", "dey", "gbandi", "gio", "gola", "grebo", "kpelle", "kissi", "krahn", "kru", "lorma", "mandingo", "mano", "mende", "sapo", "vai", "naturalized", "congo/american")
levels(dist_ethnic) <- c("bassa", "belle", "dey", "gbandi", "gio", "gola", "grebo", "kpelle", "kissi", "krahn", "kru", "lorma", "mandingo", "mano", "mende", "sapo", "vai", "naturalized", "congo/american")
levels(clan_ethnic) <- c("bassa", "belle", "dey", "gbandi", "gio", "gola", "grebo", "kpelle", "kissi", "krahn", "kru", "lorma", "mandingo", "mano", "mende", "sapo", "vai", "naturalized", "congo/american")

lbr_cnty_ethnic <- ppp(x = county_cwiq10$x, y = county_cwiq10$y, window = win, marks = cnty_ethnic)
lbr_dist_ethnic <- ppp(x = district_cwiq10$x, y = district_cwiq10$y, window = win, marks = dist_ethnic)
lbr_clan_ethnic <- ppp(x = clan_cwiq10$x, y = clan_cwiq10$y, window = win, marks = clan_ethnic)

plot(density(split(lbr_cnty_ethnic, weights = cnty_weights)))
plot(density(split(lbr_dist_ethnic, weights = dist_weights)))
plot(density(split(lbr_clan_ethnic, weights = clan_weights)))

# Religion

cnty_religion <- as.factor(county_cwiq10$n12)
dist_religion <- as.factor(district_cwiq10$n12)
clan_religion <- as.factor(clan_cwiq10$n12)

levels(cnty_religion) <- c("christian", "muslim", "traditional", "other", "none")
levels(dist_religion) <- c("christian", "muslim", "traditional", "other", "none")
levels(clan_religion) <- c("christian", "muslim", "traditional", "other", "none")

lbr_cnty_religion <- ppp(x = county_cwiq10$x, y = county_cwiq10$y, window = win, marks = cnty_religion)
lbr_dist_religion <- ppp(x = district_cwiq10$x, y = district_cwiq10$y, window = win, marks = dist_religion)
lbr_clan_religion <- ppp(x = clan_cwiq10$x, y = clan_cwiq10$y, window = win, marks = clan_religion)

plot(density(split(lbr_cnty_religion, weights = cnty_weights)))
plot(density(split(lbr_dist_religion, weights = dist_weights)))
plot(density(split(lbr_clan_religion, weights = clan_weights)))

# Education

cnty_edu <- as.factor(county_cwiq10$o3)
dist_edu <- as.factor(district_cwiq10$o3)
clan_edu <- as.factor(clan_cwiq10$o3)

levels(cnty_edu) <- c("none", "pre-school", "p1", "p2", "p3", "p4", "p5", "p6", "s7", "s8", "s9", "sh10", "sh11", "sh12", "university", "vocational", "tech-training", "technical")
levels(dist_edu) <- c("none", "pre-school", "p1", "p2", "p3", "p4", "p5", "p6", "s7", "s8", "s9", "sh10", "sh11", "sh12", "university", "vocational", "tech-training", "technical")
levels(clan_edu) <- c("none", "pre-school", "p1", "p2", "p3", "p4", "p5", "p6", "s7", "s8", "s9", "sh10", "sh11", "sh12", "university", "vocational", "tech-training", "technical")

lbr_cnty_edu <- ppp(x = county_cwiq10$x, y = county_cwiq10$y, window = win, marks = cnty_edu)
lbr_dist_edu <- ppp(x = district_cwiq10$x, y = district_cwiq10$y, window = win, marks = dist_edu)
lbr_clan_edu <- ppp(x = clan_cwiq10$x, y = clan_cwiq10$y, window = win, marks = clan_edu)

plot(density(split(lbr_cnty_edu, weights = cnty_weights)), main = "Education - County Weights")
plot(density(split(lbr_dist_edu, weights = dist_weights)), main = "Education - District Weights")
plot(density(split(lbr_clan_edu, weights = clan_weights)), main = "Education - Clan Weights")







#contour(density(liberia_gender, weights = weights))

lbr_dist_gender <- ppp(x = district_cwiq10$x, y = district_cwiq10$y, window = win, marks = gender)
plot(density(lbr_cnty_gender, weights = weights))
plot(density(split(lbr_cnty_gender, weights = weights)))


### LABELING ###

# country_names <- africa@data$COUNTRY
# country_cpts <- gCentroid(africa, byid=TRUE)
# country_labs <- cbind.data.frame(country_names, country_cpts@coords[,1], country_cpts@coords[,2])
# names(country_labs) <- c("country","x","y")
# country_labs[1,2] <- -7.75 #modify Cote d'Ivore location
# country_labs[1,3] <- 6.75
# country_labs[2,2] <- -9.00 #modify Guinea location
# country_labs[2,3] <- 8.4
# country_labs[3,2] <- -11.15 #modify Guinea location
# country_labs[3,3] <- 8.00

lbr_1_names <- lbr_1@data$CCNAME
lbr_1_cpts <- gCentroid(lbr_1, byid=TRUE)
lbr_1_labs <- cbind.data.frame(lbr_1_names, lbr_1_cpts@coords[,1], lbr_1_cpts@coords[,2])
names(lbr_1_labs) <- c("lbr_1","x","y")
lbr_1_labs[9,3] <- 6.65 # Margibi lbr_1 location
lbr_1_labs[9,2] <- -10.25

# counties_d <- counties@data[,c(2,4:7)]
# counties_d$id <- row.names(counties)
# counties_f <- left_join(x = counties_f, y = counties_d, by = c("id" = "id"))

district_names <- districts@data$DNAME
district_cpts <- gCentroid(districts, byid=TRUE)
district_labs <- cbind.data.frame(district_names, district_cpts@coords[,1], district_cpts@coords[,2])
names(district_labs) <- c("district","x","y")

# districts_d <- districts@data[,c(1,7:10)]
# districts_d$id <- row.names(districts)
# districts_f <- left_join(x = districts_f, y = districts_d, by = c("id" = "id"))

lbr_3_names <- lbr_3@data$CLNAME
lbr_3_cpts <- gCentroid(lbr_3, byid=TRUE)
lbr_3_labs <- cbind.data.frame(lbr_3_names, lbr_3_cpts@coords[,1], lbr_3_cpts@coords[,2])
names(lbr_3_labs) <- c("clans","x","y")

# clans_d <- clans@data[,c(1,5:8)]
# clans_d$id <- row.names(clans)
# clans_f <- left_join(x = clans_f, y = clans_d, by = c("id" = "id"))


# LOAD CWIQ10 DATA AND JOIN TO GOV SUBDIVISION CENTER POINTS
load("cwiq10.RData")

cnty_survey_cpts <- cbind.data.frame(county_id = counties@data$FIRST_CCOD, county_labs, stringsAsFactors = FALSE)
dist_survey_cpts <- cbind.data.frame(county_id = districts@data$FIRST_CCOD, district_id = districts@data$FIRST_DCOD, district_labs)


lbr_3_survey_cpts <- cbind.data.frame(county_id = lbr_3@data$FIRST_CCOD, district_id = lbr_3@data$FIRST_DCOD, clan_id = lbr_3@data$FIRST_CLCO, lbr_3_labs)
cwiq10$clan_town <- substr(cwiq10$hid_mungai,5,7)
lbr_3_survey_cpts$clan_code  <- paste(lbr_3_survey_cpts$county_id, lbr_3_survey_cpts$district_id, lbr_3_survey_cpts$clan_id, sep = "")
cwiq10$clan_town_code <- paste(cwiq10$county, cwiq10$district, cwiq10$clan_town, sep= "")
lbr_3_cwiq10 <- left_join(x = lbr_3_survey_cpts, y = cwiq10, by = c("clan_code" = "clan_town_code"))

# Counties
# table(cnty_survey_cpts$county_id)
# table(cwiq10$county)
cwiq10$county <- substr(cwiq10$hid_mungai,1,2)
cwiq10$county[cwiq10$county == "01"] <- "30"
county_cwiq10 <- left_join(x = cnty_survey_cpts, y = cwiq10, by = c("county_id" = "county"))

# Districts
# length(table(dist_survey_cpts$district_code))
# length(table(cwiq10$district_code))
cwiq10$district <- substr(cwiq10$hid_mungai,3,4)
dist_survey_cpts$district_code  <- paste(dist_survey_cpts$county_id, dist_survey_cpts$district_id, sep = "")
cwiq10$district_code <- paste(cwiq10$county, cwiq10$district, sep= "")
district_cwiq10 <- left_join(x = dist_survey_cpts, y = cwiq10, by = c("district_code" = "district_code"))
# anti_join(x = dist_survey_cpts, y = cwiq10, by = c("district_code" = "district_code"))
# anti_join(x = cwiq10, y = dist_survey_cpts, by = c("district_code" = "district_code"))

# Clans
cwiq10$clan_town <- substr(cwiq10$hid_mungai,5,7)
lbr_3_survey_cpts$clan_code  <- paste(lbr_3_survey_cpts$county_id, lbr_3_survey_cpts$district_id, lbr_3_survey_cpts$clan_id, sep = "")
cwiq10$clan_town_code <- paste(cwiq10$county, cwiq10$district, cwiq10$clan_town, sep= "")
lbr_3_cwiq10 <- left_join(x = lbr_3_survey_cpts, y = cwiq10, by = c("clan_code" = "clan_town_code"))
# anti_join(x = clan_survey_cpts, y = cwiq10, by = c("clan_code" = "clan_town_code"))
# anti_join(x = cwiq10, y = clan_survey_cpts, by = c("clan_town_code" = "clan_code"))

x<-sum(lbr_3_cwiq10$wta_hh,na.rm=T)

y<-unique(lbr_3@data$FIRST_CLCO)
z<-unique(cwiq10$clan_town)
a<-unique(lbr_3_cwiq10$clans)
b<-subset(a, !(a %in% lbr_3@data$CLNAME))
lbr_3@data$CLNAME
lbr_3@polygons[319]

unique(lbr_3$CLNAME)
unique(lbr_3_cwiq10$district_id)

lbr_3_cwiq10$dist_code<-paste(lbr_3_cwiq10$county_id,lbr_3_cwiq10$district_id)

test2<-subset(lbr_3_cwiq10, is.na(lbr_3_cwiq10$wta_hh))

test<-aggregate(wta_hh ~ county_id, lbr_3_cwiq10, sum)

unique(lbr_3$FIRST_CCOD)

win<-as(lbr_1@polygons[[1]]@Polygons[[1]]@coords,"owin")

lbr_1@polygons[1]

# Being Analyzing Point Pattern

#Add National boundary for Liberia

options(scipen=999)

country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(country)
country <- spTransform(country, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

win <- as(country,"owin")



cnty_weights <- county_cwiq10$wta_hh
dist_weights <- district_cwiq10$wta_hh
clan_weights <- clan_cwiq10$wta_hh

# Total Population

lbr_cnty <- ppp(x = county_cwiq10$x, y = county_cwiq10$y, window = win, marks = cnty_weights)
lbr_dist <- ppp(x = district_cwiq10$x, y = district_cwiq10$y, window = win, marks = dist_weights)
lbr_clan <- ppp(x = clan_cwiq10$x, y = clan_cwiq10$y, window = win, marks = clan_weights)

plot(lbr_cnty)

par(mar=c(0,0,1,0))


plot.new()
x<-density(lbr_cnty)
class(x)
y<-as.data.frame.im(x)
ggplot(counties_f, aes(x=long,y=lat))+
  geom_map(data=counties_f,map = counties_f,aes(x=long, y=lat,map_id=id))#+
#geom_point(data=y,aes(x=x,y=y,colour=value),cex=1)+
#scale_fill_gradient(low="red",high="orange",guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")

points(clans_f$long,clans_f$lat,cex=.001)


summary(x)
png("latex/pop_district.png")
plot(density(lbr_dist), main = "District Weights")
dev.off()

png("latex/pop_clan.png")
plot(density(lbr_clan), main = "Clan Weights")
dev.off()

####
#### GPW4 EXTRACTION ####

## Extract location and values ##
data<-cbind(xyFromCell(gpw4_2000,1:ncell(gpw4_2000)),getValues(gpw4_2000))
data<-na.omit(data)
colnames(data)<-c("long","lat","pop2000")

data2<-cbind(xyFromCell(gpw4_2005,1:ncell(gpw4_2005)),getValues(gpw4_2005))
data2<-na.omit(data2)
colnames(data2)<-c("long","lat","pop2005")

data3<-cbind(xyFromCell(gpw4_2010,1:ncell(gpw4_2010)),getValues(gpw4_2010))
data3<-na.omit(data3)
colnames(data3)<-c("long","lat","pop2010")

data4<-cbind(xyFromCell(gpw4_2015,1:ncell(gpw4_2015)),getValues(gpw4_2015))
data4<-na.omit(data4)
colnames(data4)<-c("long","lat","pop2015")

data5<-cbind(xyFromCell(growth_00_05,1:ncell(growth_00_05)),getValues(growth_00_05))
data5<-na.omit(data5)
colnames(data5)<-c("long","lat","growth05")

data6<-cbind(xyFromCell(growth_05_10,1:ncell(growth_05_10)),getValues(growth_05_10))
data6<-na.omit(data6)
colnames(data6)<-c("long","lat","growth10")

data7<-cbind(xyFromCell(growth_10_15,1:ncell(growth_10_15)),getValues(growth_10_15))
data7<-na.omit(data7)
colnames(data7)<-c("long","lat","growth15")

lbr_data_final<-merge(data,data2,by=c("long","lat"))
lbr_data_final<-merge(lbr_data_final,data3,by=c("long","lat"))
lbr_data_final<-merge(lbr_data_final,data4,by=c("long","lat"))
lbr_data_final<-merge(lbr_data_final,data5,by=c("long","lat"))
lbr_data_final<-merge(lbr_data_final,data6,by=c("long","lat"))
lbr_data_final<-merge(lbr_data_final,data7,by=c("long","lat"))
lbr_data_final[,10]<-1:nrow(lbr_data_final)
colnames(lbr_data_final)[10]<-"id"

save(lbr_data_final,file = "lbr_data_final.RData")
load("lbr_data_final.RData")

####


#### GET CITY POINTS ####

city_points<-read.csv("liberia_city_names.csv")
city_points<-distinct(city_points,Latitude,Longitude,.keep_all = T)
save(city_points,file="city_points.RData")

####

