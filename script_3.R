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

#### RANDOMLY DISTRIBUTE POINTS ####
gpw4_2000<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2000.tif")
gpw4_2000<-crop(gpw4_2000,extent(country))
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

test_shp<-disaggregate(clan)

sp_list<-list()
sp_list<- lapply(1:length(clan), function(i) clan[i,])
pop_list<-gpw4_2000_clan$gpw.v4.population.count_2000
pop_list<-pop_list/100
pop_list<-round(pop_list,0)

sp_list[[1]]
pop_list[[1]]

### LOOP to get all random points ###
df_final<-data.frame(matrix(nrow=0,ncol=4))
for (i in length(sp_list)){
  sp_list_f<-fortify(sp_list[[i]])
  colnames(sp_list_f)[1]<-"x"
  colnames(sp_list_f)[2]<-"y"
  df<-data.frame(matrix(nrow=0,ncol=4))
  colnames(df)<-c("x","y","id","contains")
  while(nrow(df) < 100) {
    df.1<-data.frame(matrix(nrow=100,ncol=3))
    colnames(df.1)<-c("x","y","id")
    df.1$x<-round(runif(100,max=(max(sp_list_f$x,na.rm=T)+.00000001),min = (min(sp_list_f$x,na.rm=T)-.00000001)),digits=5)
    df.1$y<-round(runif(100,max=(max(sp_list_f$y,na.rm=T)+.00000001),min = (min(sp_list_f$y,na.rm=T)-.00000001)),digits=5)
    df.1$id<-row.names(df.1)
    df.2<-data.frame(t(sapply(1:100,function(i) list(id=df.1[i,3], gContains(sp_list[[i]],SpatialPoints(df.1[i,1:2],proj4string=CRS(proj4string(sp_list[[i]]))))))))
    colnames(df.2)<-c("id","contains")
    df.2$id<-as.character(df.2$id)
    df.2$contains<-as.character(df.2$contains)
    df.3<-left_join(df.1,df.2,by="id")
    df.3<-subset(df.3, df.3[,4]=="TRUE")
    df<-rbind(df,df.3)
    df<-unique(df)
  }
  df<-df[1:pop_list[[i]],]
  df_final<-rbind(df_final,df)
}


## TEST ##
df_final<-data.frame(matrix(nrow=0,ncol=4))

sp_list_f<-fortify(sp_list[[3]])
colnames(sp_list_f)[1]<-"x"
colnames(sp_list_f)[2]<-"y"
df<-data.frame(matrix(nrow=0,ncol=4))
colnames(df)<-c("x","y","id","contains")
n<-pop_list[[3]]
df.1<-data.frame(matrix(nrow=1000,ncol=3))
colnames(df.1)<-c("x","y","id")
df.1$x<-round(runif(1000,max=(max(sp_list_f$x,na.rm=T)+.00000001),min = (min(sp_list_f$x,na.rm=T)-.00000001)),digits=5)
df.1$y<-round(runif(1000,max=(max(sp_list_f$y,na.rm=T)+.00000001),min = (min(sp_list_f$y,na.rm=T)-.00000001)),digits=5)
df.1$id<-row.names(df.1)
df.2<-data.frame(t(sapply(1:1000,function(i) list(id=df.1[i,3], gContains(sp_list[[3]],SpatialPoints(df.1[i,1:2],proj4string=CRS(proj4string(sp_list[[3]]))))))))
colnames(df.2)<-c("id","contains")
df.2$id<-as.character(df.2$id)
df.2$contains<-as.character(df.2$contains)
df.3<-left_join(df.1,df.2,by="id")
df.3<-subset(df.3, df.3[,4]=="TRUE")
df<-rbind(df,df.3)
df<-unique(df)
df<-df[1:n,]
df_final<-rbind(df_final,df)

pop_list


if (class(shp)=="SpatialPolygonsDataFrame"){
  shp_f<-fortify(shp)
  colnames(shp_f)[1]<-"x"
  colnames(shp_f)[2]<-"y"
  df<-data.frame(matrix(nrow=0,ncol=4))
  colnames(df)<-c("x","y","id","contains")
  while(nrow(df) < n) {
    df.1<-data.frame(matrix(nrow=10,ncol=3))
    colnames(df.1)<-c("x","y","id")
    df.1$x<-round(runif(10,max=(max(shp_f$x,na.rm=T)+.00000001),min = (min(shp_f$x,na.rm=T)-.00000001)),digits=5)
    df.1$y<-round(runif(10,max=(max(shp_f$y,na.rm=T)+.00000001),min = (min(shp_f$y,na.rm=T)-.00000001)),digits=5)
    df.1$id<-row.names(df.1)
    df.2<-data.frame(t(sapply(1:10,function(i) list(id=df.1[i,3], gContains(shp,SpatialPoints(df.1[i,1:2],proj4string=CRS(proj4string(shp))))))))
    colnames(df.2)<-c("id","contains")
    df.2$id<-as.character(df.2$id)
    df.2$contains<-as.character(df.2$contains)
    df.3<-left_join(df.1,df.2,by="id")
    df.3<-subset(df.3, df.3[,4]=="TRUE")
    df<-rbind(df,df.3)
    #df<-unique(df)
  }
  df<-df[1:n,]
  a<-ggplot(shp_f,aes(x=x,y=y))+
    geom_map(data=shp_f,map=shp_f,aes(x=x, y=y,map_id=id))+
    geom_point(data=df,aes(x=x,y=y),size=.2,color="red")

for (i in 1:816){
  poly<-clan[i,]@polygons[[]]
}



test_poly<-clan[1,]@polygons[[1]]
class(test_poly)
plot(test_poly)
x<-fortify(test_poly)
test_poly<-Polygons(test_poly)

list_owin<-list()
for (id in clan_df$id){
  sub<-subset(clan_df,clan_df$id==as.character(id))
  sub2<-sub[,1:2]
  sub2<-unique(sub2)
  win<-owin(poly=data.frame(x=rev(sub2$long),y=rev(sub2$lat)))
  list_owin<-c(list_owin,list(win))
}

tess2<-tess(tiles=list_owin)
plot(tess2)

sub<-subset(clan_df,clan_df$id==as.character(0))
sub2<-sub[,1:2]
sub2<-unique(sub2)
win<-owin(poly=data.frame(x=rev(sub2$long),y=rev(sub2$lat)))
list_owin<-c(list_owin,list(win))

sub<-subset(clan_df,clan_df$id==as.character(1))
sub2<-sub[,1:2]
sub2<-unique(sub2)
win2<-owin(poly=data.frame(x=rev(sub2$long),y=rev(sub2$lat)))
list_owin<-c(list_owin,list(win2))
class(list_owin[1])

tess1<-tess(tiles=list_owin)
plot(tess1)

x<-list(win,win2)


list_owin[1]

load("gpw4_lbr_spdf.RData")
win1<-owin(xrange=c(-9.775000,-9.766667),yrange=c(8.541667,8.550000))
win1<-runifpoint()


#### CHANGE COORDINATES ####
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

#### PPM TEST ####
win<-as(country,"owin")

ppp0<-ppp(lbr_data_final$long,lbr_data_final$lat,marks=lbr_data_final$pop2010,window=win)
test0<-ppm(ppp0)

ppp1<-ppp(lbr_data_final$long,lbr_data_final$lat,window=win)
text1<-ppm(ppp1)
plot(density(simulate(text1)))
plot(envelope(text1,Kest,nsim=39))

#### Test Plots #####

x<-runif(10,0,1) 
y<-runif(10,0,1)
xy<-ppp(x,y,c(0,1),c(0,1))
summary(xy)
dens<-density(xy)
summary(dens)
plot(xy)
plot(density(xy))
plot.new()
identify.ppp(xy)

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



gplot(gpw4_2000)+geom_map(data=country_f,map=country_f,aes(x=long,y=lat,map_id=id))+geom_tile(aes(fill = value))
gplot(growth_00_05)+geom_map(data=country_f,map=country_f,aes(x=long,y=lat,map_id=id))+geom_tile(aes(fill = value))+geom_point(data=city_points2,aes(x=Longitude,y=Latitude),cex=.01)

lambda<-function(x,y){100*(y^2+x^4)}
X<-rpoispp(lambda,nsim=10)
X
plot(X)
plot(density(X))
