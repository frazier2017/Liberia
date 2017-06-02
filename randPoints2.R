## This is a function to plot random points
## within a window (it can be a ploygon or a
## shapefile).  It is useful to also have the
## shape.Polygon function for this as well.


rm(list=ls())
setwd("~/Documents/W_M/Year_1/2017_Spring/Monroe Project")

#### FUNCTION ####
c(max(liberia_f$long),min(liberia_f$long))

randPoints2<-function(shp,n){ ## shp=SpatialPolygonsDataFrame; n=number of points 
  library(rgdal)
  library(raster)
  library(rgeos)
  library(ggplot2)
  library(sp)
  library(dplyr)
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
  } else if (class(shp)=="data.frame"){
    colnames(shp)[1]<-"x"
    colnames(shp)[2]<-"y"
    df<-data.frame(matrix(nrow=0,ncol=3))
    while(nrow(df) < n) {
      df.1<-data.frame(matrix(nrow=n,ncol=3))
      df.1[,1]<-round(runif(n,max=(max(shp$x,na.rm=T)+.00000001),min = (min(shp$x,na.rm=T)-.00000001)),digits=5)
      df.1[,2]<-round(runif(n,max=(max(shp$y,na.rm=T)+.00000001),min = (min(shp$y,na.rm=T)-.00000001)),digits=5)
      df.1[,3]<-point.in.polygon(df.1[,1],df.1[,2],shp$x,shp$y)
      df.1<-subset(df.1, df.1[,3]>0)
      df<-rbind(df,df.1)
      #df<-unique(df)
    }
    colnames(df)<-c("x","y","z")
    df<-df[1:n,]
    df
    a<-ggplot(shp,aes(x=x,y=y))+
       geom_path()+
       geom_point(data=df,aes(x=x,y=y),size=.2,color="red")
  }
}
####




#### TEST ####
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)
library(ggmap)
library(sp)

## CASE 1
shp1<-shape.Polygon(4,1,point=c(0,0),type="reg")
test<-randPoints2(shp1,100)
test



liberia<-readRDS("LBR_adm0.rds")
system.time(liberia_f<-fortify(liberia))
test1<-randPoints2(liberia,100)
test1.1<-randPoints2(liberia,500)
ggsave("liberia1.png",test1)
ggsave("liberia1.1.png",test1.1)

cuba<-readRDS("CUB_adm0.rds")
cuba_f<-fortify(cuba)
system.time(fortify(cuba))
test2<-randPoints2(cuba,10000)
ggsave("cuba1.png",test2)



states<-readOGR(dsn="shapefiles",layer="USA_adm0",stringsAsFactors = FALSE, verbose = FALSE)
states<-spTransform(states,CRS("+proj=longlat +datum=WGS84"))

gClip<-function(shp,bb){
  if(class(bb)=="matrix")
    b_poly<-as(extent(as.vector(t(bb))),"SpatialPolygons")
  else
    b_poly<-as(extent(bb),"SpatialPolygons")
  proj4string(b_poly)<-proj4string(shp)
  gIntersection(shp,b_poly,byid=T)
}

#states<-gClip(states,matrix(c(-124.848974, 24.396308,-66.885444, 49.384358),ncol = 2))
states_f<-fortify(states)

ggplot(states_f,aes(x=long,y=lat))+geom_map(data=states_f,map=states_f,aes(x=long, y=lat,map_id=id))
ggplot(states_f2,aes(x=long,y=lat))+geom_map(data=states_f2,map=states_f2,aes(x=long, y=lat,map_id=id))


nigeria<-readOGR(dsn="shapefiles",layer="NGA_adm1",stringsAsFactors = FALSE,verbose = FALSE)
liberia<-readOGR(dsn="shapefiles", layer="liberia_revised",stringsAsFactors=FALSE, verbose=FALSE)




