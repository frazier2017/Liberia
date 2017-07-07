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
#gpw4_2000<-mask(gpw4_2000,country)

gpw4_2005<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2005.tif")
gpw4_2005<-crop(gpw4_2005,extent(country))
#gpw4_2005<-mask(gpw4_2005,country)

gpw4_2010<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2010.tif")
gpw4_2010<-crop(gpw4_2010,extent(country))
#gpw4_2010<-mask(gpw4_2010,country)

gpw4_2015<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2015.tif")
gpw4_2015<-crop(gpw4_2015,extent(country))
#gpw4_2015<-mask(gpw4_2015,country)


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

## set up country window ##
country_win<-as(country,"owin")


#### SPATSTAT EXPERIMENT ####

win1<-owin(xrange = c(0,1),yrange = c(0,1))
ppp1<-rpoint(14,win=win1)
win2<-owin(c(-1,0),c(0,1))
ppp2<-rpoint(45,win=win2)
win3<-owin(xrange = c(-1,0),yrange = c(-1,0))
ppp3<-rpoint(25,win=win3)
win4<-owin(c(0,1),c(-1,0))
ppp4<-rpoint(10,win=win4)
win_total<-owin(c(-1,1),c(-1,1))
ppp_total<-superimpose(ppp1,ppp2,ppp3,ppp4,W=win_total)
plot(ppp_total)
dens<-density(ppp_total,eps=.01)
plot(dens)
summary(dens)
ppm_test<-ppm(ppp_total,~dens,covariates=list(dens=dens))
y<-predict(ppm_test)
plot(y)
x<-simulate(ppm_test,nsim = 10)
plot(density(x))

k<-Kest(ppp_total)
plot(k)


windows<-list(win1,win2,win3,win4)
tess1<-tess(tiles=windows)
plot(tess1)
plot(win1)
plot(win2)

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


ppp_10percent<-ppp(m_final[,1],m_final[,2],window=win)
summary(ppp_10percent)
plot(ppp_10percent,chars=".")

dens_10percent<-density(ppp_10percent,eps=.008333)
summary(dens_10percent)
plot(dens_10percent)

ppm_10percent<-ppm(ppp_10percent,~density,covariates = list(density=dens_10percent))
sim_10percent<-simulate(ppm_10percent,nsim = 10)
plot(sim_10percent)
plot(density(sim_10percent))

plot(quadratcount(ppp_10percent,nx=100,ny=100))

intens_10percent<-intensity.ppp(ppp_10percent)
smooth_10percent<-smooth(ppp_10percent)

495.84063362534505

503.839313572543

#####
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
