#To do: 1. Recheck the Liberia plots for growth rate for the one white spot
#2. Create 3D and 2D maps to anaylze the population overtime in those regions encompasses not only Liberia (2d levelplots to get scales and 3d for visiualization)
#3. If time is availible screenshot and add scales to 3D maps from 2D
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
library(plot3Drgl)
library(plotly)
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
    gpw4test<- crop(gpw4, extent(-11.55, -7.3, 4.2, 8.6)) #-11.55, -7.3, 4.2, 8.6
        
    gpw42010<-raster("gpw-v4-population-count_2010.tif")
    gpw42005<-raster("gpw-v4-population-count_2005.tif")
    gpw42000<-raster("gpw-v4-population-count_2000.tif")
    gpw42015<-raster("gpw-v4-population-count_2015.tif")
    
    ###PLOTTING###
    ##2000-2015##
    gpw42000<- crop(gpw42000, extent(-11.55, -7.3, 4.2, 8.6))
    m2000 <- levelplot(gpw42000,at=piplup, main = "2000", col.regions=terrain.colors(25)) + layer(sp.polygons(lbr_1, col = "black"))
    
    
    gpw42005<- crop(gpw42005, extent(-11.55, -7.3, 4.2, 8.6))
    m2005 <- levelplot(gpw42005,at=piplup, main = "2005", col.regions=terrain.colors(25)) + layer(sp.polygons(lbr_1, col = "black"))
    
    
    gpw42010<- crop(gpw42010, extent(-11.55, -7.3, 4.2, 8.6))
    m2010 <- levelplot(gpw42010,at=piplup, main = "2010", col.regions=terrain.colors(25)) + layer(sp.polygons(lbr_1, col = "black"))
    
    
    gpw42015<- crop(gpw42015, extent(-11.55, -7.3, 4.2, 8.6))
    m2015 <- levelplot(gpw42015,at=piplup, main = "2015", col.regions=terrain.colors(25)) + layer(sp.polygons(lbr_1, col = "black"))
    
    yrcom00_15_.01_to_100_by_5 <- arrangeGrob(m2000,m2005,m2010,m2015,nrow=2,ncol=2)
    ggsave("lbr_com_shp_rcom00_15_.01_to_100_by_5.png",yrcom00_15_.01_to_100_by_5,width = 26, height = 24, dpi = 150)
    
    ##Growth Rate##
    extent(country)
    gpw42000<-crop(gpw42000,extent(country))
    gpw42000<-mask(gpw42000,country)
    gpw42005<-crop(gpw42005,extent(country))
    gpw42005<-mask(gpw42005,country)
    gpw42010<-crop(gpw42010,extent(country))
    gpw42010<-mask(gpw42010,country)
    gpw42015<-crop(gpw42015,extent(country))
    gpw42015<-mask(gpw42015,country)
    
    r00_15 <- (gpw42015-gpw42000)/(gpw42000)
    r00_15m <- levelplot(r00_15, col.regions=heat.colors(20), main = "Lbr Growth Rate 2000-2015") + layer(sp.polygons(lbr_0, col = "black"))
    
    r00_05 <- (gpw42005-gpw42000)/(gpw42000)
    r00_05m <- levelplot(r00_05, col.regions=heat.colors(20), main = "Lbr Growth Rate 2000-2005") + layer(sp.polygons(lbr_0, col = "black"))
    
    
    r05_10 <- (gpw42010-gpw42005)/(gpw42005)
    r05_10m <- levelplot(r05_10, col.regions=heat.colors(20), main = "Lbr Growth Rate 2005-2010") + layer(sp.polygons(lbr_0, col = "black"))
    
    
    r10_15 <- (gpw42015-gpw42010)/(gpw42010)
    r10_15m <- levelplot(r10_15, col.regions=heat.colors(20), main = "Lbr Growth Rate 2010-2015") + layer(sp.polygons(lbr_0, col = "black"))
    
    comparison_gr<-arrangeGrob(r00_15m,r00_05m,r05_10m,r10_15m, nrow = 2, ncol = 2)
    ggsave("lbr_com_GR_00_15.png",comparison_gr,width = 26, height = 24, dpi = 150)
      
    ##County##
      piplup <- seq(0.01, 100, by = 5)
      m1 <- levelplot(gpw4test, at=piplup, col.regions=terrain.colors(20), main = "Lbr County Lvl Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_1, col = "black"))
      m1
      turtwig <- seq(0.01, 250, by = 5)
      m2 <- levelplot(gpw4test, at = turtwig, col.regions=terrain.colors(20), main = "Lbr County Lvl Scale 0.01-250 breaks of 5") + layer(sp.polygons(lbr_1, col = "black"))
      
      chimchar <- seq(0.01, 500, by = 5)
      m3 <- levelplot(gpw4test, at = chimchar, col.regions=terrain.colors(20), main = "Lbr County Lvl Scale 0.01-500 breaks of 5") + layer(sp.polygons(lbr_1, col = "black"))
      
      bulbasaur <- seq(0.01, 100, by = 10)
      m4 <- levelplot(gpw4test, at = bulbasaur, col.regions=terrain.colors(20), main = "Lbr County Lvl Scale 0.01-100 breaks of 10") + layer(sp.polygons(lbr_1, col = "black"))
      
      charmander <- seq(0.01, 250, by = 10)
      m5 <- levelplot(gpw4test, at = charmander, col.regions=terrain.colors(20), main = "Lbr County Lvl Scale 0.01-250 breaks of 10") + layer(sp.polygons(lbr_1, col = "black"))
      
      squirtle <- seq(0.01, 500, by = 10)
      m6 <- levelplot(gpw4test, at = squirtle, col.regions=terrain.colors(20), main = "Lbr County Lvl Scale 0.01-500 breaks of 10") + layer(sp.polygons(lbr_1, col = "gray80"))
      
      comparison<-arrangeGrob(m1,m4,m2,m5,m3,m6,nrow=3,ncol=2)
      ggsave("lbr_com_shp_cntyv2.png",comparison,width = 22, height = 20, dpi = 150)
    
    ##Districts##
      piplup <- seq(.01, 100, by = 5)
      m7 <- levelplot(gpw4test, at=piplup, col.regions=terrain.colors(20), main = "Lbr District Lvl Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_2, col = "black"))
      
      turtwig <- seq(0.01, 250, by = 5)
      m8 <- levelplot(gpw4test, at = turtwig, col.regions=terrain.colors(20), main = "Lbr District Lvl Scale 0.01-250 breaks of 5") + layer(sp.polygons(lbr_2, col = "black"))
      
      chimchar <- seq(0.01, 500, by = 5)
      m9 <- levelplot(gpw4test, at = chimchar, col.regions=terrain.colors(20), main = "Lbr District Lvl Scale 0.01-500 breaks of 5") + layer(sp.polygons(lbr_2, col = "black"))
      
      bulbasaur <- seq(0.01, 100, by = 10)
      m10 <- levelplot(gpw4test, at = bulbasaur, col.regions=terrain.colors(20), main = "Lbr District Lvl Scale 0.01-100 breaks of 10") + layer(sp.polygons(lbr_2, col = "black"))
      
      charmander <- seq(0.01, 250, by = 10)
      m11 <- levelplot(gpw4test, at = charmander, col.regions=terrain.colors(20), main = "Lbr District Lvl Scale 0.01-250 breaks of 10") + layer(sp.polygons(lbr_2, col = "black"))
      
      squirtle <- seq(0.01, 500, by = 10)
      m12 <- levelplot(gpw4test, at = squirtle, col.regions=terrain.colors(20), main = "Lbr District Lvl Scale 0.01-500 breaks of 10") + layer(sp.polygons(lbr_2, col = "black"))
      
       comparison<-arrangeGrob(m7,m10,m11,m5,m9,m12,nrow=3,ncol=2)
       ggsave("lbr_com_shp_dist.png",comparison,width = 22, height = 20, dpi = 150)
       
    ##Clan##
      piplup <- seq(.01, 100, by = 5)
      m13 <- levelplot(gpw4test, at=piplup, col.regions=terrain.colors(20), main = "Lbr Clan Lvl Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_3))
      
      turtwig <- seq(0.01, 250, by = 5)
      m14 <- levelplot(gpw4test, at = turtwig, col.regions=terrain.colors(20), main = "Lbr Clan Lvl Scale 0.01-250 breaks of 5") + layer(sp.polygons(lbr_3, col = "black"))
      
      chimchar <- seq(0.01, 500, by = 5)
      m15 <- levelplot(gpw4test, at = chimchar, col.regions=terrain.colors(20), main = "Lbr Clan Lvl Scale 0.01-500 breaks of 5") + layer(sp.polygons(lbr_3, col = "black"))
      
      bulbasaur <- seq(0.01, 100, by = 10)
      m16 <- levelplot(gpw4test, at = bulbasaur, col.regions=terrain.colors(20), main = "Lbr Clan Lvl Scale 0.01-100 breaks of 10") + layer(sp.polygons(lbr_3, col = "black"))
      
      charmander <- seq(0.01, 250, by = 10)
      m17 <- levelplot(gpw4test, at = charmander, col.regions=terrain.colors(20), main = "Lbr Clan Lvl Scale 0.01-250 breaks of 10") + layer(sp.polygons(lbr_3, col = "black"))
      
      squirtle <- seq(0.01, 500, by = 10)
      m18 <- levelplot(gpw4test, at = squirtle, col.regions=terrain.colors(20), main = "Lbr Clan Lvl Scale 0.01-500 breaks of 10") + layer(sp.polygons(lbr_3, col = "black"))
      
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
        gpw4<-raster("gpw-v4-population-count_2010.tif")
           country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
           counties <- readOGR(dsn="shapefiles", layer="counties", stringsAsFactors=FALSE, verbose=FALSE)
        gpw4test1 <- crop(gpw4, extent(-11.55, -7.3, 4.2, 8.6)) #xmin, xmax, ymin, ymax 
        gpw4test1 <- crop(gpw4, extent(country)) #xmin, xmax, ymin, ymax 
        
    #RasterVis
        charizard <- seq(1,50, by=1)
        plot3D(gpw4test1, decorate3d = T, useLegend = TRUE, at = piplup, adjust=T, col=rainbow(20))
        
    #Example Below for plot3D
        # open3d()
        # x <- sort(rnorm(1000))
        # y <- rnorm(1000)
        # z <- rnorm(1000) + atan2(x, y)
        # plot3d(x, y, z, col = rainbow(1000))
    
    #Raster
        persp(gpw4test1, col=cols)
        example(persp3D)
        persp3D(z=gpw4test1, lighting = T, lphi = 90)
        # Attempted Funtion to assign color values to above function
        # f <- function(x,y) { r <- sqrt(x^2+y^2); 10 * sin(r)/r }
        #       z <- outer(x, y, f)
        #       z[is.na(z)] <- 1
        #       op <- par(bg = "white")
        #       zz <- (z[-1,-1] + z[-1,-ncol(z)] + z[-nrow(z),-1] + 
        #                               z[-nrow(z),-ncol(z)])/4
        #       cols <- topo.colors(length(zz))
        #       cols[order(zz)] <- cols
        
        
    ##3D Liberia Model /n Scale 0.01-100, breaks every 5 (from piplup)##
    
    
        gpw4<-raster("gpw-v4-population-count_2010.tif") 
        gpw4test2 <- crop(gpw4, extent (-11.55, -7.3, 4.2, 8.6))
        plot3D(gpw4test2, useLegend = T, at = piplup, col = terrain.colors(20))
        
        
        pikachu <- seq(0.01, 1000, by = 5)
        
        
        gpw4test3 <- crop(gpw4, extent (-9,-6.5,6,8))
        plot3D(gpw4test3, useLegend = T, at = pikachu, col = terrain.colors(20))
        
        gpw4test4 <- crop(gpw4, extent (-9,-8.9,6,6.5))
        plot3D(gpw4test4, useLegend = T, at = pikachu, col = terrain.colors(20))
        
    ##Plotly testing: Needs to be in the form of numbers/dataframe and not a raster so plotly will not work at the moment 
        p <- plot_ly(
          x = rnorm( 1000 ), y = rnorm( 1000 ),
          mode = 'markers' )
        p
        
        t<-plot_ly(type='scattergeo', lon=c(42, 39), lat=c(12,22), text=c('Lbr'), mode='markers')
        t
        
        plot_geo()
        
        t1 <- load("gpw4_lbr_spdf.RData")
        
    ###Plan 3: 3D plots of 2000, 2005, 2010, and 2015 data. Compare, then find an area that experienced growth then zoom in and replot the same years
    country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
    counties <- readOGR(dsn="shapefiles", layer="counties", stringsAsFactors=FALSE, verbose=FALSE)
    
    country <- spTransform(country, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    counties <- spTransform(counties, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
    gpw42000<-raster("gpw-v4-population-count_2000.tif")
    gpw42005<-raster("gpw-v4-population-count_2005.tif")
    gpw42010<-raster("gpw-v4-population-count_2010.tif")
    gpw42015<-raster("gpw-v4-population-count_2015.tif")
    piplup <- seq(0.01, 100, by = 5)
    
  #Cropping/Masking 2000-2015
    gpw42000 <-crop(gpw42000,extent(country))
    gpw42000<-mask(gpw42000,country)
    
    gpw42005 <-crop(gpw42005,extent(country))
    gpw42005<-mask(gpw42005,country)
    
    gpw42010 <-crop(gpw42010,extent(country))
    gpw42010<-mask(gpw42010,country)
    
    gpw42015 <-crop(gpw42010,extent(country))
    gpw42015 <-mask(gpw42010,country)
    
    #Tip: Ploting all of these at once overlays them on top of each other a bit from the original plot: Don't do that
    m2000D<- plot3D(gpw42000, at = piplup, adjust=T, col=terrain.colors(20))
    m2005D<- plot3D(gpw42005, at = piplup, adjust=T, col=terrain.colors(20))
    m2010D<- plot3D(gpw42010, at = piplup, adjust=T, col=terrain.colors(20))
    m2015D<- plot3D(gpw42015, at = piplup, adjust=T, col=terrain.colors(20)) 
    
    #Plan 3a Plot a small section
    gpw2000c <- crop(gpw42000, extent(-9.9, -9.3, 6.5, 7.4))
    gpw2005c <- crop(gpw42005, extent(-9.9, -9.3, 6.5, 7.4))
    gpw2010c <- crop(gpw42010, extent(-9.9, -9.3, 6.5, 7.4))
    gpw2015c <- crop(gpw42015, extent(-9.9, -9.3, 6.5, 7.4))
    
    
    #Just showing the three 2000,2005,2010 plots there is a slight increase in elevation as the years
    #go up. Also note the color change in the plateau gets a stratified appearence as time goes on
    plot3D(gpw2000c, at=dr, col=terrain.colors)
    plot3D(gpw2005c, col=terrain.colors)
    plot3D(gpw2010c, col=terrain.colors)
    plot3D(gpw2015c, col=terrain.colors)
    dr <- seq(1,100, by =1 )
    #Plant 3b plot where is a river based on lat long coordinates and observe 3D map
    gpw2000river <- crop(gpw42000, extent(-10, -9.8, 7, 8))
    plot3D(gpw2000river, col=terrain.colors(20))
    
    #Coordinated above shoudl show that individuals are not living on the intersection
    #of St. Paul River on the border of Gbarpolu and Bong, over the 3D map serves to show
    #no change in population based on the natural barrier. Thus, 3Dplot mapping may not be
    #suitible to tell where people may not live due to not account for geographic barriers
    
    circ <- drawPoly(sp=T, col='red', lwd = 2)
    
    m2010D<- plot3D(gpw42010, at = piplup, adjust=T, col=terrain.colors(20))
    
    
    x <- c(-11,-11,-10,-10)
    y <- c(5,6,6,5)
    z <- C(10,10,10,10)
    m2010E <- plot3D(gpw42010, at = piplup, adjust=T, col=terrain.colors(20)) + layer(polygon3d(x, y, z, col = 'red'))
    polygon3d(x, y, z=rep(120, 4), col = 'blue')
    polygon3d(x, y, z=rep(140, 4), col = 'black')
    
    
    
####Extracting GPW4 Data GGPLOT2 - Chris####
    gpw4test<-raster("gpw-v4-population-count_2010.tif")
    # plot1<-gplot(gpw4test)+geom_tile(aes(fill = value))
    # plot1
    
    country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
    counties <- readOGR(dsn="shapefiles", layer="counties", stringsAsFactors=FALSE, verbose=FALSE)
    
    proj4string(country)
    proj4string(counties)
    
    country <- spTransform(country, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    counties <- spTransform(counties, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
    country_f <- fortify(country)
    counties_f <- fortify(counties, region = "CCNAME")
    
    extent(country)
    gpw4_lbr<-crop(gpw4,extent(country))
    gpw4_lbr<-mask(gpw4_lbr,country)
    
    # x<-rasterToPolygons(gpw4_lbr) 
    # system.time(y<-fortify(x)) Takes forever (900 seconds)
    save(y,file = "gpw4_lbr_spdf.RData")
    load("gpw4_lbr_spdf.RData")
    data<-x@data
    data[,2]<-row.names(data)
    colnames(data)<-c("pop","id")
    z<-left_join(y,data,by="id")
    ggplot()+geom_map(data=country_f,map = country_f,aes(x=long,y=lat,map_id=id))+geom_map(data=z,map=z,aes(x=long,y=lat,map_id=id,fill=pop))
    
    gpw4_lbr
    
    
    ###PLOT###
    plot2<-gplot(gpw4_lbr)+
      geom_tile(aes(fill = log(value)))+
      scale_fill_gradientn(colours = c("grey100","coral","red2"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")+
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
      ggtitle("Plot (gpw4)")
    plot2
    colors()
    
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
    map1
    
    
    
####
####GPW4 MAPS OVERTIME 2000, 2005, 2010, 2015####
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
    
    ###READING GPW AND CROP### #-11.55, -7.3, 4.2, 8.6 ## 
    gpw42000 <- raster("gpw-v4-population-count_2000.tif")
    gpw42000c<- crop(gpw42000, extent(-11.55, -7.3, 4.2, 8.6))
    
    gpw42005 <- raster("gpw-v4-population-count_2005.tif")
    gpw42005c<- crop(gpw42005, extent(-11.55, -7.3, 4.2, 8.6))
    
    gpw42010<-raster("gpw-v4-population-count_2010.tif")
    gpw42010c<- crop(gpw42010, extent(-11.55, -7.3, 4.2, 8.6)) 
    
    gpw42015 <- raster("gpw-v4-population-count_2015.tif")
    gpw42015c <- crop(gpw42015, extent(-11.55, -7.3, 4.2, 8.6))
    
   
    ###PLOTS###
    piplup <- seq(.01, 100, by = 5)
    gpw42000m <- levelplot(gpw42000c, at=piplup, col.regions=terrain.colors(20), main = "Lbr 2000 Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_0, col = "black"))

    gpw42005m <- levelplot(gpw42005c, at = piplup, col.regions=terrain.colors(20), main = "Lbr 2005 Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_0, col = "black"))
    
    gpw42010m <- levelplot(gpw42010c, at = piplup, col.regions=terrain.colors(20), main = "Lbr 2010 Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_0, col = "black"))
    
    gpw42015m <- levelplot(gpw42015c, at = piplup, col.regions=terrain.colors(20), main = "Lbr 2015 Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_0, col = "black"))
    

    comparison<-arrangeGrob(gpw42000m,gpw42005m,gpw42010m,gpw42015m,nrow=2,ncol=2)
    ggsave("lbr_com_2000_thr_2015.png",comparison,width = 28, height = 26, dpi = 150)
    
####IGNORE Varience Between Populations in LBR####
    ###READING SHP###
    gpw4_lbr<-raster("gpw-v4-population-count_2010.tif")
    gpw42010<-raster("gpw-v4-population-count_2010.tif")
    gpw42005<-raster("gpw-v4-population-count_2005.tif")
    gpw42000<-raster("gpw-v4-population-count_2000.tif")
    gpw42015<-raster("gpw-v4-population-count_2015.tif")
    
    country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
    counties <- readOGR(dsn="shapefiles", layer="counties", stringsAsFactors=FALSE, verbose=FALSE)
    
    proj4string(country)
    proj4string(counties)
    
    country <- spTransform(country, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    counties <- spTransform(counties, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    
    country_f <- fortify(country)
    counties_f <- fortify(counties, region = "CCNAME")
    
    
    ###TEST###
    r1 <- (gpw42015-gpw42000)
    r1m <- levelplot(gpw4test, at=piplup, col.regions=terrain.colors(20), main = "Lbr County Lvl Scale 0.01-100 breaks of 5") + layer(sp.polygons(lbr_0, col = "black"))
    
    
    
    
    extent(country)
    gpw4_lbr<-crop(gpw4,extent(country))
    gpw4_lbr<-mask(gpw4_lbr,country)
    
    gpw42005c <- crop(gpw42005, extent(country))
    gpw42005c <- mask(gpw42005, country)
    
    ###2010-2005###
    x<-rasterToPolygons(gpw4_lbr) 
    # system.time(y<-fortify(x)) #Takes forever (900 seconds)
    #save(y,file = "gpw42010_lbr_spdf.RData")
    load("gpw42010_lbr_spdf.RData")
    data<-x@data
    data[,2]<-row.names(data)
    colnames(data)<-c("pop","id")
    z10<-left_join(y,data,by="id")
    
    ###2005###
    # gpw42005_r <- rasterToPolygons(gpw42005c)
    # system.time(gpw42005_f <- fortify(gpw42005_r))
    # save(gpw42005_f, file = "gpw42005_f.RData")
    load("gpw42005_f.RData")
    data2005<-gpw42005_r@data
    data2005[,2]<-row.names(data2005)
    colnames(data2005)<-c("pop","id")
    # z<-left_join(gpw42005_f,data2005,by="id")
    
    ###Testing###
    subdata1 <- merge(x = z10, y = data2005, by = "id", all = T)
    subdata2 <- merge(z, data2005)
    
    ###2005-2010###
    subdata1$C <- ((subdata1$pop.x - subdata1$pop.y)/(subdata1$pop.y))
   
    z10$pop <- subdata1$C
    
    lbr_grth_05_10<-ggplot()+
      geom_map(data=z10,map=z10,aes(x=long,y=lat,map_id=id,fill=pop))+
      scale_fill_gradientn(colours = c("grey100","red2","blue"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")
      geom_map(data=z10,map=z10,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
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
      ggtitle("Liberia Growth Rate 2005 to 2010")
      lbr_grth_05_10
      
    ###2000 to 2005###
      extent(country)
      gpw4_lbr2000<-crop(gpw42000,extent(country))
      gpw4_lbr2000<-mask(gpw4_lbr2000,country)
      
      x2000<-rasterToPolygons(gpw4_lbr2000) 
      # system.time(y2000<-fortify(x2000)) #Takes forever (900 seconds)
      # save(y2000,file = "gpw42000_lbr_spdf.RData")
      load("gpw42000_lbr_spdf.RData")
      data2000<-x2000@data
      data2000[,2]<-row.names(data)
      colnames(data2000)<-c("pop","id")
      z2000<-left_join(y2000,data2000,by="id")
      
      #data=2005
      
      z1<-left_join(data,data2000,by="id")
      
      ###
      z1$C <- ((z1$pop.x - z1$pop.y)/(z1$pop.y))
      z00_05 <- left_join(y2000,z1, by="id")
      
      
      
      lbr_grth_00_05<-ggplot()+
        geom_map(data=z00_05,map=z00_05,aes(x=long,y=lat,map_id=id,fill=C))+
        scale_fill_gradientn(colours = c("grey100","red2","blue"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")
        geom_map(data=z00_05,map=z00_05,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
        theme(text = element_text(color = "white"),
              rect = element_rect(fill = "grey35", color = "grey35"),
              plot.background = element_rect(fill = "grey35", color = "grey35"),
              panel.background = element_rect(fill = "grey35", color = "grey35"),
              plot.title = element_text("2000-2005"),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.position = c(.1,.2))+
        ggtitle("Liberia Growth Rate 2000 to 2005")
      lbr_grth_00_05
    
    ###2000 to 2010
      #2010
      load("gpw42010_lbr_spdf.RData")
      data<-x@data
      data[,2]<-row.names(data)
      colnames(data)<-c("pop","id")
      z10<-left_join(y,data,by="id")
      
      z_00_10 <- left_join(z10, data2000, by="id") 
      
      z_00_10$C <- ((z_00_10$pop.x - z_00_10$pop.y)/(z_00_10$pop.y))
      z00_10 <- left_join(y2000,z1, by="id")
      
      lbr_grth_00_10<-ggplot()+
        geom_map(data=z00_10,map=z00_10,aes(x=long,y=lat,map_id=id,fill=C))+
        scale_fill_gradientn(colours = c("grey100","red2","blue"),guide=guide_colourbar(title=NULL),space="Lab",na.value = "grey60")
      geom_map(data=z00_10,map=z00_10,aes(x=long,y=lat,map_id=id),color="grey45",alpha=0,size=.1)+
        theme(text = element_text(color = "white"),
              rect = element_rect(fill = "grey35", color = "grey35"),
              plot.background = element_rect(fill = "grey35", color = "grey35"),
              panel.background = element_rect(fill = "grey35", color = "grey35"),
              plot.title = element_text("2000-2005"),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              legend.position = c(.1,.2))+
        ggtitle("Liberia Growth Rate 2000 to 2005")
      lbr_grth_00_10
      
    ###2000 to 2015
      #2015
      x2015<-rasterToPolygons(gpw42015) 
      # system.time(y2000<-fortify(x2000)) #Takes forever (900 seconds)
      # save(y2000,file = "gpw42000_lbr_spdf.RData")
      load("gpw42015_lbr_spdf.RData")
      data2015<-x2015@data
      data2015[,2]<-row.names(data)
      colnames(data2015)<-c("pop","id")
      z2015<-left_join(y2015,data2015,by="id")
      
      #data=2005
      
      z00_15<-left_join(z2015,data2000,by="id")
      
      ###
      z00_15$C <- ((z1$pop.x - z1$pop.y)/(z1$pop.y))
      z00_15 <- left_join(y2000,z1, by="id")
      
      
      
      
      
#####MAPS 06/07/17####
      country <- readOGR(dsn="shapefiles", layer="liberia_revised", stringsAsFactors=FALSE, verbose=FALSE)
      counties <- readOGR(dsn="shapefiles", layer="counties", stringsAsFactors=FALSE, verbose=FALSE)
      
      country <- spTransform(country, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      counties <- spTransform(counties, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      
      gpw42000<-raster("gpw-v4-population-count_2000.tif")
      gpw42005<-raster("gpw-v4-population-count_2005.tif")
      gpw42010<-raster("gpw-v4-population-count_2010.tif")
      gpw42015<-raster("gpw-v4-population-count_2015.tif")
      piplup <- seq(1, 250, by = 1)
      
      blueblue<-colorRampPalette(brewer.pal(9,"Blues"))(250)
      gpw42010<- crop(gpw42010, extent(-10, -9, 7, 8))
      m2010 <- levelplot(gpw42010,at=piplup, main = "2010", col.regions=blueblue) + layer(sp.polygons(lbr_1, col = "black"))
      m2010
      
      
    