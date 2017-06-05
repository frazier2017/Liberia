#Raster: Crop, Raster, Stack...Look into Raster Functions KDE in a different package; also look up set up github in RStudio
#https://oscarperpinan.github.io/rastervis/FAQ.html#breaks see after lunch for shp to ras
#1.Make 3-D visualization (Perspective plot) 2. overlay the shapefile of Liberia
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
##

#### IMPORT SHAPEFILES ####
lbr_0<-readOGR(dsn="shapefiles", layer="liberia_revised",stringsAsFactors=FALSE, verbose=FALSE)
lbr_1<-readOGR(dsn = "shapefiles",layer="counties",stringsAsFactors = FALSE,verbose=FALSE)
lbr_2<-readOGR(dsn="shapefiles",layer="districts", stringsAsFactors = FALSE,verbose = FALSE)
lbr_3<-readOGR(dsn="shapefiles",layer="clans", stringsAsFactors = FALSE,verbose = FALSE)

proj4string(lbr_0)
proj4string(lbr_1)
proj4string(lbr_2)
proj4string(lbr_3)

lbr_0<-spTransform(lbr_0, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lbr_1<-spTransform(lbr_1, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lbr_2<-spTransform(lbr_2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
lbr_3<-spTransform(lbr_3, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

### FORTIFY ###
lbr_0_f<-fortify(lbr_0)
lbr_1_f<-fortify(lbr_1)
lbr_2_f<-fortify(lbr_2)
lbr_3_f<-fortify(lbr_3)

##


#### V1. SHAPEFILES DATA ####
  
  ### CORRECT DATA ###
  lbr_1@data$SUM_HH[1]<-20508
  lbr_2@data[which(lbr_2@data$FIRST_CCNA=="Bomi"),]$HHOLDS<-lbr_2@data[which(lbr_2@data$FIRST_CCNA=="Bomi"),]$TOTAL/(lbr_1@data$SUM_TOTAL[1]/lbr_1@data$SUM_HH[1])
  lbr_3@data[which(lbr_3@data$FIRST_CCNA=="Bomi"),]$SUM_HH<-lbr_3@data[which(lbr_3@data$FIRST_CCNA=="Bomi"),]$SUM_TOTAL/(lbr_1@data$SUM_TOTAL[1]/lbr_1@data$SUM_HH[1])
  lbr_3@data[which(lbr_3@data$FIRST_CCNA=="River Gee"),]$SUM_HH[29]<-lbr_3@data[which(lbr_3@data$FIRST_CCNA=="River Gee"),]$SUM_TOTAL[29]/mean(lbr_3@data[which(lbr_3@data$FIRST_CCNA=="River Gee"),]$SUM_TOTAL[c(1:28,30:45)]/lbr_3@data[which(lbr_3@data$FIRST_CCNA=="River Gee"),]$SUM_HH[c(1:28,30:45)])
  
  
  ### MERGE DATA ###
  lbr_1@data$ID<-as.character(c(0:14))
  lbr_1_f_shp<-left_join(x=lbr_1_f,y=lbr_1@data,by=c("id"="ID"))
  lbr_2@data$ID<-as.character(c(0:135))
  lbr_2_f_shp<-left_join(x=lbr_2_f,y=lbr_2@data,by=c("id"="ID"))
  lbr_3@data$ID<-as.character(c(0:815))
  lbr_3_f_shp<-left_join(x=lbr_3_f,y=lbr_3@data,by=c("id"="ID"))
  
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
    geom_map(data=lbr_1_f_shp,map=lbr_1_f_shp,aes(x=long,y=lat,map_id=id,fill=log(SUM_TOTAL)))+
    scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
    geom_map(data=lbr_1_f_shp,map=lbr_1_f_shp,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.2)+
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
    geom_map(data=lbr_2_f_shp,map=lbr_2_f_shp,aes(x=long,y=lat,map_id=id,fill=log(TOTAL)))+
    scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
    geom_map(data=lbr_1_f_shp,map=lbr_1_f_shp,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
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
    geom_map(data=lbr_3_f_shp,map=lbr_3_f_shp,aes(x=long,y=lat,map_id=id,fill=log(SUM_TOTAL)))+
    scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
    geom_map(data=lbr_1_f_shp,map=lbr_1_f_shp,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
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

  
##
  
  
#### V2. GPW4 ####
  
  ### LOAD GWP4 DATA ###
  gpw4<-raster("gpw-v4-population-count_2010.tif")
  #gpw4
  #plot(gpw4)
  
  ### JOIN DATA ###
  
  #lbr_0_gpw4<-extract(gpw4,lbr_0,df=TRUE)
  #save(lbr_0_gpw4,file = "lbr_0_gpw4.RData")
  load("lbr_0_gpw4.RData")
  lbr_0_gpw4_totals<-aggregate(.~ID,lbr_0_gpw4,sum)
  lbr_0_gpw4_totals$ID<-as.character((lbr_0_gpw4_totals$ID-1))
  names(lbr_0_gpw4_totals)[2]<-"pop"
  lbr_0_f_gpw4<-left_join(x=lbr_0_f,y=lbr_0_gpw4_totals,by=c("id"="ID"))
  
  
  #lbr_1_gpw4<-extract(gpw4,lbr_1,df=TRUE)
  #save(lbr_1_gpw4,file = "lbr_1_gpw4.RData")
  load("lbr_1_gpw4.RData")
  lbr_1_gpw4_totals<-aggregate(.~ID,lbr_1_gpw4,sum)
  lbr_1_gpw4_totals$ID<-as.character((lbr_1_gpw4_totals$ID-1))
  names(lbr_1_gpw4_totals)[2]<-"pop"
  lbr_1_f_gpw4<-left_join(x=lbr_1_f,y=lbr_1_gpw4_totals,by=c("id"="ID"))
  
  
  #lbr_2_gpw4<-extract(gpw4,lbr_2,df=TRUE)
  #save(lbr_2_gpw4,file = "lbr_2_gpw4.RData")
  load("lbr_2_gpw4.RData")
  lbr_2_gpw4_totals<-aggregate(.~ID,lbr_2_gpw4,sum)
  lbr_2_gpw4_totals$ID<-as.character((lbr_2_gpw4_totals$ID-1))
  names(lbr_2_gpw4_totals)[2]<-"pop"
  lbr_2_f_gpw4<-left_join(x=lbr_2_f,y=lbr_2_gpw4_totals,by=c("id"="ID"))
  
  
  #lbr_3_gpw4<-extract(gpw4,lbr_3,df=TRUE)
  #save(lbr_3_gpw4,file = "lbr_3_gpw4.RData")
  load("lbr_3_gpw4.RData")
  lbr_3_gpw4_totals<-aggregate(.~ID,lbr_3_gpw4,sum)
  lbr_3_gpw4_totals$ID<-as.character((lbr_3_gpw4_totals$ID-1))
  names(lbr_3_gpw4_totals)[2]<-"pop"
  lbr_3_f_gpw4<-left_join(x=lbr_3_f,y=lbr_3_gpw4_totals,by=c("id"="ID"))
  
  
  ### MAKE MAPS ###
  map0<-ggplot(lbr_0_f_gpw4, aes(x=long, y = lat))+
             geom_map(data = lbr_0_f_gpw4, map=lbr_0_f_gpw4,aes(x=long, y=lat,map_id=id,fill=pop))+
             scale_fill_gradient(low="yellow",high="red",space="Lab")

  
  # map1<-ggplot(lbr_1_f_gpw4, aes(x=long, y = lat))+
  #   geom_map(data = lbr_1_f_gpw4, map=lbr_1_f_gpw4,aes(x=long, y=lat,map_id=id,fill=pop))+
  #   scale_fill_gradient(low="yellow",high="red",space="Lab")
  # map1
  
  map1<-ggplot()+
    geom_map(data=lbr_1_f_gpw4,map=lbr_1_f_gpw4,aes(x=long,y=lat,map_id=id,fill=log(pop)))+
    scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
    geom_map(data=lbr_1_f_gpw4,map=lbr_1_f_gpw4,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
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
  
  # map2<-ggplot(lbr_2_f_gpw4, aes(x=long, y = lat))+
  #   geom_map(data = lbr_2_f_gpw4, map=lbr_2_f_gpw4,aes(x=long, y=lat,map_id=id,fill=pop))+
  #   scale_fill_gradient(low="yellow",high="red",space="Lab")
  # map2
  
  map2<-ggplot()+
    geom_map(data=lbr_2_f_gpw4,map=lbr_2_f_gpw4,aes(x=long,y=lat,map_id=id,fill=log(pop)))+
    scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
    geom_map(data=lbr_1_f_gpw4,map=lbr_1_f_gpw4,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
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
  
  # map3<-ggplot(lbr_3_f_gpw4, aes(x=long, y = lat))+
  #   geom_map(data = lbr_3_f_gpw4, map=lbr_3_f_gpw4,aes(x=long, y=lat,map_id=id,fill=pop))+
  #   scale_fill_gradient(low="yellow",high="red",space="Lab")
  # map3
  
  map3<-ggplot()+
    geom_map(data=lbr_3_f_gpw4,map=lbr_3_f_gpw4,aes(x=long,y=lat,map_id=id,fill=log(pop)))+
    scale_fill_gradientn(colours = c("grey100","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
    geom_map(data=lbr_1_f_gpw4,map=lbr_1_f_gpw4,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
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
  map3  

##
#ggsave("lbr_county.png", arrangeGrob(lbr_county_pop, lbr_county_density,lbr_county_perfem,lbr_county_house, nrow = 2,ncol = 2), width = 14, height = 12, dpi = 150)
comparison<-arrangeGrob(map1,map4,map2,map5,map3,map6,nrow=3,ncol=2)
ggsave("comparison.png",comparison,width = 20, height = 18, dpi = 150)
##

#### V3. SURVEY DATA ####

#### LABELING ####

#COUNTRIES
# create labels & join data to data frame

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

lbr_1_d <- lbr_1@data[,c(2,4:7)]
lbr_1_d$id <- row.names(lbr_1)
lbr_1_f_sub <- left_join(x = lbr_1_f, y = lbr_1_d, by = c("id" = "id"))

lbr_2_names <- lbr_2@data$DNAME
lbr_2_cpts <- gCentroid(lbr_2, byid=TRUE)
lbr_2_labs <- cbind.data.frame(lbr_2_names, lbr_2_cpts@coords[,1], lbr_2_cpts@coords[,2])
names(lbr_2_labs) <- c("lbr_2","x","y")

lbr_2_d <- lbr_2@data[,c(1,7:10)]
lbr_2_d$id <- row.names(lbr_2)
lbr_2_f_sub <- left_join(x = lbr_2_f, y = lbr_2_d, by = c("id" = "id"))

lbr_3_names <- lbr_3@data$CLNAME
lbr_3_cpts <- gCentroid(lbr_3, byid=TRUE)
lbr_3_labs <- cbind.data.frame(lbr_3_names, lbr_3_cpts@coords[,1], lbr_3_cpts@coords[,2])
names(lbr_3_labs) <- c("lbr_3","x","y")

lbr_3_d <- lbr_3@data[,c(1,5:8)]
lbr_3_d$id <- row.names(lbr_3)
lbr_3_f_sub <- left_join(x = lbr_3_f, y = lbr_3_d, by = c("id" = "id"))

# LOAD CWIQ10 DATA AND JOIN TO GOV SUBDIVISION CENTER POINTS
load("cwiq10.RData")

cnty_survey_cpts <- cbind.data.frame(county_id = lbr_1@data$FIRST_CCOD, lbr_1_labs, stringsAsFactors = FALSE)
dist_survey_cpts <- cbind.data.frame(county_id = lbr_2@data$FIRST_CCOD, lbr_2_id = lbr_2@data$FIRST_DCOD, lbr_2_labs)
clan_survey_cpts <- cbind.data.frame(county_id = lbr_3@data$FIRST_CCOD, lbr_3_id = lbr_3@data$FIRST_DCOD, clan_id = lbr_3@data$FIRST_CLCO, lbr_3_labs)

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
dist_survey_cpts$district_code  <- paste(dist_survey_cpts$county_id, dist_survey_cpts$lbr_2_id, sep = "")
cwiq10$district_code <- paste(cwiq10$county, cwiq10$district, sep= "")
district_cwiq10 <- left_join(x = dist_survey_cpts, y = cwiq10, by = c("district_code" = "district_code"))

# anti_join(x = dist_survey_cpts, y = cwiq10, by = c("district_code" = "district_code"))
# anti_join(x = cwiq10, y = dist_survey_cpts, by = c("district_code" = "district_code"))

# Clans
cwiq10$clan_town <- substr(cwiq10$hid_mungai,5,7)
clan_survey_cpts$clan_code  <- paste(clan_survey_cpts$county_id, clan_survey_cpts$lbr_3_id, clan_survey_cpts$clan_id, sep = "")
cwiq10$clan_town_code <- paste(cwiq10$county, cwiq10$district, cwiq10$clan_town, sep= "")
clan_cwiq10 <- left_join(x = clan_survey_cpts, y = cwiq10, by = c("clan_code" = "clan_town_code"))
# anti_join(x = clan_survey_cpts, y = cwiq10, by = c("clan_code" = "clan_town_code"))
# anti_join(x = cwiq10, y = clan_survey_cpts, by = c("clan_town_code" = "clan_code"))

x<-sum(clan_cwiq10$wta_hh,na.rm=T)




unique(lbr_3$CLNAME)






clan_cwiq10$district_code<-paste(clan_cwiq10$county_id, clan_cwiq10$lbr_3_id)

y <- aggregate(clan_cwiq10$wta_hh ~ district_code, clan_cwiq10, sum) 

z <- aggregate(clan_cwiq10$wta_hh ~ county_id, clan_cwiq10, sum) 


#### BASE MAPPING RANDOM POINTS WITHIN LIBERIA COUNTIES
counties <- readOGR(dsn="shapefiles", layer="counties", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(counties)
country <- spTransform(counties, CRS("+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))

win <- as(counties,"owin")


# ranx <- (runif(1000, min=223700, max=680781))
# rany <- (runif(1000, min=481399, max=945462))
# country <- ppp(x = ranx, y = rany, window = win)
# plot(country)
# plot(density(country, 0.1))

county1 <- runifpoint(1000, win)
plot(county1)

counties <- county[counties$]

win2 <- as(counties@polygons,"owin")

county2 <- runifpoint(1000, win2)
plot(county2)



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


##


#### EXPERIMENT ####
  
  africa <- readOGR(dsn="shapefiles", layer="africa", stringsAsFactors=FALSE, verbose=FALSE)
  country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
  counties <- readOGR(dsn="shapefiles", layer="counties", stringsAsFactors=FALSE, verbose=FALSE)
  districts <- readOGR(dsn="shapefiles", layer="districts", stringsAsFactors=FALSE, verbose=FALSE)
  clans <- readOGR(dsn="shapefiles", layer="clans", stringsAsFactors=FALSE, verbose=FALSE)
  
  proj4string(counties)
  
  country <- spTransform(country, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  counties <- spTransform(counties, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  districts <- spTransform(districts, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  clans <- spTransform(clans, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  #modify africa data frame
  africa <- subset(africa, Land_Type == "Primary land")
  #africa <- subset(africa, COUNTRY == "Sierra Leone" | COUNTRY == "Guinea" | COUNTRY == "Côte d'Ivoire")
  
  #correct Number of Households in Bomi County
  #bomi <- subset(districts@data, FIRST_CCNA == "Bomi")
  #counties@data[1,7] <- sum(bomi$HHOLDS)
  
  ###correct data
  counties@data$SUM_HH[1]<-20508
  districts@data[which(districts@data$FIRST_CCNA=="Bomi"),]$HHOLDS<-districts@data[which(districts@data$FIRST_CCNA=="Bomi"),]$TOTAL/(counties@data$SUM_TOTAL[1]/counties@data$SUM_HH[1])
  clans@data[which(clans@data$FIRST_CCNA=="Bomi"),]$SUM_HH<-clans@data[which(clans@data$FIRST_CCNA=="Bomi"),]$SUM_TOTAL/(counties@data$SUM_TOTAL[1]/counties@data$SUM_HH[1])
  clans@data[which(clans@data$FIRST_CCNA=="River Gee"),]$SUM_HH[29]<-clans@data[which(clans@data$FIRST_CCNA=="River Gee"),]$SUM_TOTAL[29]/mean(clans@data[which(clans@data$FIRST_CCNA=="River Gee"),]$SUM_TOTAL[c(1:28,30:45)]/clans@data[which(clans@data$FIRST_CCNA=="River Gee"),]$SUM_HH[c(1:28,30:45)])
  
  
  africa_f <- fortify(africa)
  country_f <- fortify(country)
  counties_f <- fortify(counties)
  districts_f <- fortify(districts)
  clans_f <- fortify(clans)
  
  win <- as(country,"owin")
  points<-runifpoint(100,win)
  points_quad<-quadratcount(points,nx=10,ny=10)
  plot(points_quad)
  plot(points,cex=.1)
  plot(density(points,.2))
  x<-density(points,.2)
  x<-as.data.frame.im(x)
  
  
  win2<-as(clans,"owin")
  points2<-runifpoint(100,win2)
  plot(points2,cex=.1)
  plot(density(points2)+points2)
  
  ggplot(counties_f, aes(x=long,y=lat))+
    geom_map(data=counties_f,map = counties_f,aes(x=long, y=lat,map_id=id))+
    geom_polygon(data=x,aes(x=x,y=y,fill=value))#+
  scale_fill_gradient(low="red",high="orange",guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")
  
  ##
  
#### EXPERIMENTING RASTER GPW4 ####
  
  ###RASTER GPW4###
  
  
  ### LOAD GWP4 DATA ###
  gpw4<-raster("gpw-v4-population-count_2010.tif")
  #gpw4
  #plot(gpw4)
  
  ###TESTING###
    # x <- raster(ncol=36, nrow=18, xmn=-11.5, xmx=4.25, ymn=-7.25, ymx=8.6)
    # res(x)
    # ncol(x)
  
    ##Dots exist with below function, but doesn't zoom in##
      
      # projection(gpw4) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
      # plot(gpw4)
      
    ##Setting xmin through y max changes viewing window; display is still the world and not cropped##
    
      # xmin(gpw4) <- (-11.65)
      # xmax(gpw4) <- (4.25)
      # ymin(gpw4) <- (-7.25)
      # ymax(gpw4) <- (8.6)
      # 
      # gpw4
      # plot(gpw4)
  
    ##Commands in Raster##
      # hasValues(gpw4)
      # res(gpw4)
      # dim(gpw4)
      # inMemory(gpw4)
      # nlayers(gpw4)
    
      # gpw4test1 <- crop(gpw4, extent(-11.65, 4.25, -7.25, 8.6))
      # plot(gpw4test1)
    
    ##xmin, xmax, ymin, ymax (-14, -5, 4.2, 13) Fit in Liberia##
      # gpw4test <- crop(gpw4, extent(-14, -5, 4.2, 13)) -----> Updated bbox ((-11.55, -7.3, 4.2, 8.6))
      plot(gpw4test, axes = F, box = F, col=topo.colors((10000)))
      
      #Scale in Grey#
        plot(gpw4test, axes = F, box = F, col=grey(1:100/100))
    
    ##Changing the Resolution by Aggregating## 
      gpw4test_aggre <- aggregate(gpw4test, fact = 15)
      plot(gpw4test_aggre, col=topo.colors(10))
      
    ##Plot based on values
      plot(gpw4test, col=ifelse(value < 0, topo.colors(10)))
    
      
####Testing 2.0#####
      gpw4<-raster("gpw-v4-population-count_2010.tif")
      #gpw4test <- crop(gpw4, extent(-14, -5, 4.2, 13)) ---> updated bbox -11.55, -7.3, 4.2, 8.6
      
      levelplot(gpw4test) + layer(sp.polygons(lbr_1))
      
      piplup <- con(raster!=0, gpw4test)
      
      
      ##RasterVis## 
      myTheme <- rasterTheme(region=sequential_hcl(10, power=2.2))
      lbrbluelvl <- levelplot(gpw4test, par.settings = myTheme, contour = TRUE)
      lbrbluelvl
      
      lbrgreylvl <- levelplot(gpw4test,col.regions = grey(0:100/100), layers=1)      
      lbrgreylvl
      
      gpw4test@data@min
      gpw4test@data@min <- 0
      
      ##Scale setting, by = breaks in scale)
        piplup <- seq(.01, 100, by = 5)
        m1 <- levelplot(gpw4test, at=piplup, main = "Scale 0.01-100 breaks of 5")
        
        turtwig <- seq(0.01, 250, by = 5)
        m2 <- levelplot(gpw4test, at = turtwig, main = "Scale 0.01-250 breaks of 5")
        
        chimchar <- seq(0.01, 500, by = 5)
        m3 <- levelplot(gpw4test, at = chimchar, main = "Scale 0.01-500 breaks of 5")
        
        bulbasaur <- seq(0.01, 100, by = 10)
        m4 <- levelplot(gpw4test, at = bulbasaur, main = "Scale 0.01-100 breaks of 10")
        
        charmander <- seq(0.01, 250, by = 10)
        m5 <- levelplot(gpw4test, at = charmander, main = "Scale 0.01-250 breaks of 10")
        
        squirtle <- seq(0.01, 500, by = 10)
        m6 <- levelplot(gpw4test, at = squirtle, main = "Scale 0.01-500 breaks of 10")
        
        comparison<-arrangeGrob(m1,m4,m2,m5,m3,m6,nrow=3,ncol=2)
        ggsave("lbr_com.png",comparison,width = 20, height = 18, dpi = 150)
        
        mew <- seq(1, 20, by = 1)
        m7 <- levelplot(gpw4test, at = mew, main = "Liberia Scale 1-20 breaks of 1")
        m7
        
        
      ##Working Overlaying Shapefiles: Must load in the shapefile using readOGR then use spTransform then you can plot
        m8 <- levelplot(gpw4test, at = mew) + layer(sp.polygons(lbr_0))
        m8
        
        m9 <- levelplot(gpw4test, at = piplup) + layer(sp.polygons(lbr_0))
        m9
      ##Adjusting bounding box to fit Liberia##
        gpw4<-raster("gpw-v4-population-count_2010.tif")
        gpw4test <- crop(gpw4, extent(-11.55, -7.3, 4.2, 8.6)) #xmin, xmax, ymin, ymax 
        
        m10 <- levelplot(gpw4test, at = piplup, main = "Scale 0.01-100 breaks of 5 \n with Liberia border overlay") + layer(sp.polygons(lbr_0))
        m10
        
        
####LBR Overlay SHP on LEVEL.PLOT####
  #This section will have some code from other sections so that anyone can just copy paste just this section out
    
    ###READING SHAPEFILES###
    lbr_0<-readOGR(dsn="shapefiles", layer="liberia_revised",stringsAsFactors=FALSE, verbose=FALSE)
    lbr_1<-readOGR(dsn = "shapefiles",layer="counties",stringsAsFactors = FALSE,verbose=FALSE)
    lbr_2<-readOGR(dsn="shapefiles",layer="districts", stringsAsFactors = FALSE,verbose = FALSE)
    lbr_3<-readOGR(dsn="shapefiles",layer="clans", stringsAsFactors = FALSE,verbose = FALSE)
        
    proj4string(lbr_0)
    proj4string(lbr_1)
    proj4string(lbr_2)
    proj4string(lbr_3)
        
    lbr_0<-spTransform(lbr_0, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    lbr_1<-spTransform(lbr_1, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    lbr_2<-spTransform(lbr_2, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    lbr_3<-spTransform(lbr_3, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
    ###READING GPW AND CROP###    
    gpw4<-raster("gpw-v4-population-count_2010.tif")
    gpw4 <- crop(gpw4, extent(-14, -5, 4.2, 13))
        
    ###PLOTTING###
    
    ##County##
      piplup <- seq(.01, 100, by = 5)
      m1 <- levelplot(gpw4test, at=piplup, main = "Lbr County Lvl Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_1, col = "gray80"))
      
      turtwig <- seq(0.01, 250, by = 5)
      m2 <- levelplot(gpw4test, at = turtwig, main = "Lbr County Lvl Scale 0.01-250 breaks of 5") + layer(sp.polygons(lbr_1, col = "gray80"))
      
      chimchar <- seq(0.01, 500, by = 5)
      m3 <- levelplot(gpw4test, at = chimchar, main = "Lbr County Lvl Scale 0.01-500 breaks of 5") + layer(sp.polygons(lbr_1, col = "gray80"))
      
      bulbasaur <- seq(0.01, 100, by = 10)
      m4 <- levelplot(gpw4test, at = bulbasaur, main = "Lbr County Lvl Scale 0.01-100 breaks of 10") + layer(sp.polygons(lbr_1, col = "gray80"))
      
      charmander <- seq(0.01, 250, by = 10)
      m5 <- levelplot(gpw4test, at = charmander, main = "Lbr County Lvl Scale 0.01-250 breaks of 10") + layer(sp.polygons(lbr_1, col = "gray80"))
      
      squirtle <- seq(0.01, 500, by = 10)
      m6 <- levelplot(gpw4test, at = squirtle, main = "Lbr County Lvl Scale 0.01-500 breaks of 10") + layer(sp.polygons(lbr_1, col = "gray80"))
      
      comparison<-arrangeGrob(m1,m4,m2,m5,m3,m6,nrow=3,ncol=2)
      ggsave("lbr_com_shp_cnty.png",comparison,width = 22, height = 20, dpi = 150)
    
    ##Districts##
      piplup <- seq(.01, 100, by = 5)
      m7 <- levelplot(gpw4test, at=piplup, main = "Lbr District Lvl Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_2, col = "gray80"))
      
      turtwig <- seq(0.01, 250, by = 5)
      m8 <- levelplot(gpw4test, at = turtwig, main = "Lbr District Lvl Scale 0.01-250 breaks of 5") + layer(sp.polygons(lbr_2, col = "gray80"))
      
      chimchar <- seq(0.01, 500, by = 5)
      m9 <- levelplot(gpw4test, at = chimchar, main = "Lbr District Lvl Scale 0.01-500 breaks of 5") + layer(sp.polygons(lbr_2, col = "gray80"))
      
      bulbasaur <- seq(0.01, 100, by = 10)
      m10 <- levelplot(gpw4test, at = bulbasaur, main = "Lbr District Lvl Scale 0.01-100 breaks of 10") + layer(sp.polygons(lbr_2, col = "gray80"))
      
      charmander <- seq(0.01, 250, by = 10)
      m11 <- levelplot(gpw4test, at = charmander, main = "Lbr District Lvl Scale 0.01-250 breaks of 10") + layer(sp.polygons(lbr_2, col = "gray80"))
      
      squirtle <- seq(0.01, 500, by = 10)
      m12 <- levelplot(gpw4test, at = squirtle, main = "Lbr District Lvl Scale 0.01-500 breaks of 10") + layer(sp.polygons(lbr_2, col = "gray80"))
      
      comparison<-arrangeGrob(m7,m10,m11,m5,m9,m12,nrow=3,ncol=2)
      ggsave("lbr_com_shp_dist.png",comparison,width = 22, height = 20, dpi = 150)
      
    ##Clan##
      piplup <- seq(.01, 100, by = 5)
      m13 <- levelplot(gpw4test, at=piplup, main = "Lbr Clan Lvl Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_3, col = "gray80"))
      
      turtwig <- seq(0.01, 250, by = 5)
      m14 <- levelplot(gpw4test, at = turtwig, main = "Lbr Clan Lvl Scale 0.01-250 breaks of 5") + layer(sp.polygons(lbr_3, col = "gray80"))
      
      chimchar <- seq(0.01, 500, by = 5)
      m15 <- levelplot(gpw4test, at = chimchar, main = "Lbr Clan Lvl Scale 0.01-500 breaks of 5") + layer(sp.polygons(lbr_3, col = "gray80"))
      
      bulbasaur <- seq(0.01, 100, by = 10)
      m16 <- levelplot(gpw4test, at = bulbasaur, main = "Lbr Clan Lvl Scale 0.01-100 breaks of 10") + layer(sp.polygons(lbr_3, col = "gray80"))
      
      charmander <- seq(0.01, 250, by = 10)
      m17 <- levelplot(gpw4test, at = charmander, main = "Lbr Clan Lvl Scale 0.01-250 breaks of 10") + layer(sp.polygons(lbr_3, col = "gray80"))
      
      squirtle <- seq(0.01, 500, by = 10)
      m18 <- levelplot(gpw4test, at = squirtle, main = "Lbr Clan Lvl Scale 0.01-500 breaks of 10") + layer(sp.polygons(lbr_3, col = "gray80"))
      
      comparison<-arrangeGrob(m13,m16,m14,m17,m15,m18,nrow=3,ncol=2)
      ggsave("lbr_com_shp_clan.png",comparison,width = 22, height = 20, dpi = 150)
      
      
      
    
    
    
    
      # e <- extract(gpw4test2, lbr_1, fun=mean)
      # plot(e)
      # Could be useful for Shapefiles to Raster/Raster plotting: 
      #https://amywhiteheadresearch.wordpress.com/2014/05/01/shp2raster/
      #https://rpubs.com/alobo/vectorOnraster
      #http://neondataskills.org/R/Plot-Rasters-In-R/
      #

      
####3D Visualization Testing####      


    
    ###Plot3D in rgl package with RasterVis and Raster###
    gpw4test1<-raster("gpw-v4-population-count_2010.tif")
    gpw4test1 <- crop(gpw4, extent(-11.55, -7.3, 4.2, 8.6)) #xmin, xmax, ymin, ymax 
    plot3D(gpw4test1, useLegend = T, at = piplup, col = terrain.colors(20)) + layer(sp.polygons(lbr_1, col = "gray80"))
    ##3D Liberia Model /n Scale 0.01-100, breaks every 5 (from piplup)##
    
    
    gpw4<-raster("gpw-v4-population-count_2010.tif") 
    gpw4test2 <- crop(gpw4, extent (-8.8,-7.2,6.5,7.6))
    plot3D(gpw4test2, useLegend = T, at = piplup, col = terrain.colors(20))
    
    
    pikachu <- seq(0.01, 1000, by = 5)
    
    
    gpw4test3 <- crop(gpw4, extent (-9,-6.5,6,8))
    plot3D(gpw4test3, useLegend = T, at = pikachu, col = terrain.colors(20))
    
    gpw4test4 <- crop(gpw4, extent (-9,-8.9,6,6.5))
    plot3D(gpw4test4, useLegend = T, at = pikachu, col = terrain.colors(20))
    