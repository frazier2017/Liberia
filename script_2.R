#Set the rectangle box to be within only one color blob 
rm(list=ls())
setwd("~/GitHub/Liberia")

#install.packages("mapdata")
#install.packages("rgl",dependencies = T)
#install.packages("XQuartz")

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
library(plotly)

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
gpw4_2000<-raster("gpw-v4-population-count_2000.tif")
gpw4_2000<-crop(gpw4_2000,extent(country))
#gpw4_2000<-mask(gpw4_2000,country)

gpw4_2005<-raster("gpw-v4-population-count_2005.tif")
gpw4_2005<-crop(gpw4_2005,extent(country))
#gpw4_2005<-mask(gpw4_2005,country)

gpw4_2010<-raster("gpw-v4-population-count_2010.tif")
gpw4_2010<-crop(gpw4_2010,extent(country))
#gpw4_2010<-mask(gpw4_2010,country)

gpw4_2015<-raster("gpw-v4-population-count_2015.tif")
gpw4_2015<-crop(gpw4_2015,extent(country))
#gpw4_2015<-mask(gpw4_2015,country)


##


#### V1. SHAPEFILES DATA ####

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


##


#### V2. GPW4 ####

### LOAD GWP4 DATA ###
gpw4<-raster("gpw-v4-population-count_2010.tif")
#gpw4
#plot(gpw4)

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
#           geom_map(data = country_gpw4_f, map=country_gpw4_f,aes(x=long, y=lat,map_id=id,fill=pop))+
#           scale_fill_gradient(low="yellow",high="red",space="Lab")


# map1<-ggplot(cnty_gpw4_f, aes(x=long, y = lat))+
#   geom_map(data = cnty_gpw4_f, map=cnty_gpw4_f,aes(x=long, y=lat,map_id=id,fill=pop))+
#   scale_fill_gradient(low="yellow",high="red",space="Lab")
# map1

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

# map2<-ggplot(dist_gpw4_f, aes(x=long, y = lat))+
#   geom_map(data = dist_gpw4_f, map=dist_gpw4_f,aes(x=long, y=lat,map_id=id,fill=pop))+
#   scale_fill_gradient(low="yellow",high="red",space="Lab")
# map2

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

# map3<-ggplot(clan_gpw4_f, aes(x=long, y = lat))+
#   geom_map(data = clan_gpw4_f, map=clan_gpw4_f,aes(x=long, y=lat,map_id=id,fill=pop))+
#   scale_fill_gradient(low="yellow",high="red",space="Lab")
map3

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

##
comparison<-arrangeGrob(map1,map4,map2,map5,map3,map6,nrow=3,ncol=2)
ggsave("comparison.png",comparison,width = 20, height = 18, dpi = 150)
##


#### V3. SURVEY DATA ####
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



#### GPW4 EXTRACTION ####

## Calculate growth rate ##
growth_00_05<-overlay(gpw4_2000,gpw4_2005,fun=function(r1,r2){return((r2-r1)/r1)})
growth_05_10<-overlay(gpw4_2005,gpw4_2010,fun=function(r1,r2){return((r2-r1)/r1)})
growth_10_15<-overlay(gpw4_2010,gpw4_2015,fun=function(r1,r2){return((r2-r1)/r1)})

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

data_final<-merge(data,data2,by=c("long","lat"))
data_final<-merge(data_final,data3,by=c("long","lat"))
data_final<-merge(data_final,data4,by=c("long","lat"))
data_final<-merge(data_final,data5,by=c("long","lat"))
data_final<-merge(data_final,data6,by=c("long","lat"))
data_final<-merge(data_final,data7,by=c("long","lat"))
data_final[,10]<-1:nrow(data_final)
colnames(data_final)[10]<-"id"

## Get city points ##
city_points<-read.csv("liberia_city_names.csv")
city_points<-distinct(city_points,Latitude,Longitude,.keep_all = T)

load("comNames.RData")


## Test Plots ##
#TESTING PLOTS FOR PPP#

win<-as(country,"owin")
ppp<-ppp(data_final$long,data_final$lat,marks=data_final$pop2010,window=win)

ppp2<-as.ppp(data_final,W=win)
plot.ppp(ppp,cols=ppp$marks)
plot(density(ppp,10000))



gplot(gpw4_2000)+geom_map(data=country_f,map=country_f,aes(x=long,y=lat,map_id=id))+geom_tile(aes(fill = value))
gplot(growth_00_05)+geom_map(data=country_f,map=country_f,aes(x=long,y=lat,map_id=id))+geom_tile(aes(fill = value))+geom_point(data=city_points2,aes(x=Longitude,y=Latitude),cex=.01)




#### LBR Overlay SHP on LEVEL.PLOT ####

###PLOTTING###
##2000-2015##
gpw42000<-raster("gpw-v4-population-count_2000.tif")
gpw42005<-raster("gpw-v4-population-count_2005.tif")
gpw42010<-raster("gpw-v4-population-count_2010.tif")
gpw42015<-raster("gpw-v4-population-count_2015.tif")

piplup <- seq(0.01, 100, by = 5)
gpw42000<- crop(gpw42000, extent(-11.55, -7.3, 4.2, 8.6))
m2000 <- levelplot(gpw42000,at=piplup, main = "2000", col.regions=terrain.colors(25)) + layer(sp.polygons(lbr_1, col = "black"))


gpw42005<- crop(gpw42005, extent(-11.55, -7.3, 4.2, 8.6))
m2005 <- levelplot(gpw42005,at=piplup, main = "2005", col.regions=terrain.colors(25)) + layer(sp.polygons(lbr_1, col = "black"))


gpw42010<- crop(gpw42010, extent(-11.55, -7.3, 4.2, 8.6))
m2010 <- levelplot(gpw42010,at=piplup, main = "2010", col.regions=blueblue) + layer(sp.polygons(country, col = "black"))
m2010

chris1 <- persp(gpw42010)
chrisistrying <- arrangeGrob(chris1, m2010)
ggsave("tst1.png", chrisistrying, width = 20, height = 18, dpi = 150)
gpw42015<- crop(gpw42015, extent(-11.55, -7.3, 4.2, 8.6))

m2015 <- levelplot(gpw42015,at=piplup, main = "2015", col.regions=terrain.colors(25)) + layer(sp.polygons(lbr_1, col = "black"))

yrcom00_15_.01_to_100_by_5 <- arrangeGrob(m2000,m2005,m2010,m2015,nrow=2,ncol=2)
ggsave("lbr_com_shp_rcom00_15_.01_to_100_by_5.png",yrcom00_15_.01_to_100_by_5,width = 26, height = 24, dpi = 150)

##Growth Rate##

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
##

#### 3D Visualization Testing ####      

## Plot3D in rgl package with RasterVis and Raster ##
gpw4test1 <- crop(gpw4, extent(-11.55, -7.3, 4.2, 8.6)) #xmin, xmax, ymin, ymax 
gpw4test1 <- crop(gpw4, extent(country)) #xmin, xmax, ymin, ymax
# gpw4test1 <-mask(gpw4, country)
gpw4test2 <- crop(gpw4, extent (-11.55, -7.3, 4.2, 8.6))


entei <- seq(1,3000, by = 1)

plot3D(gpw4test2, useLegend = T, at = piplup,col = blueblue)

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
##


#### MAPS 06/07/17 and on####
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
m2010 <- levelplot(gpw42010,at=piplup, main = "2010", col.regions=blueblue) + layer(sp.polygons(counties, col = "black")) 
m2010
##

###Makes a Triangle on the Map with the layer function for polys, values can be extracted, but need to attribute them to some data###
  cds1 <- rbind(c(-9.6,7.2), c(-9.4,7.2), c(-9.3, 7.2), c(-9.5,7.1), c(-9.6,7.2))
  
  cds2 <- rbind(c(-9.6,7.2), c(-9.4,7.2), c(-9.3, 7.2), c(-9.5,7.1), c(-9.6,7.2))
  
  polys <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                                                         Polygons(list(Polygon(cds2)), 2))),data.frame(ID=c(1,2)))
  #Combines the polygon with the raster layer to gain the data# 
  ( v <- extract(gpw42010, polys, cellnumbers=T, weights=T) )
  
  #Method: Add NA to absent class values if they exist (not used for this data set)#
  v.counts <- lapply(v,table)
  (v.pct <- lapply(v.counts, FUN=function(x){ x / sum(x) } ) )
  class.df <- as.data.frame(t(sapply(v.pct,'[',1:length(unique(gpw42010)))))  
  class.df[is.na(class.df)] <- 0   
  names(class.df) <- paste("class", names(class.df),sep="")
  polys@data <- data.frame(polys@data, class.df)
  head(polys@data)
  
  #After Extract use this function to determine the number of pixels# 
  lengths(v)
  #Sum the values within the polygon
  sum(v[[1]])
  #Test
  id.cell <- extract(gpw42010, polys, cellnumbers=T)[1]
  rowColFromCell(gpw42010, id.cell)
  #Map#
  mpoly2010 <- levelplot(gpw42010,at=piplup, main = "2010", col.regions=blueblue) + layer(sp.polygons(counties, col = "black")) + layer(sp.polygons(polys, col = "black"))
  mpoly2010

###Making a Rectangle 
  cds1 <- rbind(c(-9.6,7.2), c(-9.3,7.2), c(-9.3,7.1), c(-9.6,7.1), c(-9.6, 7.2))
  cds2 <- rbind(c(-9.6,7.2), c(-9.3,7.2), c(-9.3,7.1), c(-9.6,7.1), c(-9.6, 7.2))
  
  polys <- SpatialPolygonsDataFrame(SpatialPolygons(list(Polygons(list(Polygon(cds1)), 1), 
                                                         Polygons(list(Polygon(cds2)), 2))),data.frame(ID=c(1,2)))
  #Combines the polygon with the raster layer to gain the data# 
  ( v <- extract(gpw42010, polys) )
 
  lengths(v)
  sum(v[[1]])
  m2poly2010 <- levelplot(gpw42010,at=piplup, main = "2010", col.regions=blueblue) + layer(sp.polygons(counties, col = "black")) + layer(sp.polygons(polys, col = "black"))
  m2poly2010

  ###Attempting to put city points down
      lbr_cty <- read.csv("LibCities2500.csv")
      gpw42010<-raster("gpw-v4-population-count_2010.tif")
      
      subcity <- subset(lbr_cty, select=c("Lat","Long"))
      
      p2 <- spplot(subcity, pch=20) 
      
      
      gpw42010<- crop(gpw42010, extent(-11.55, -7.3, 4.2, 8.6))
      m2010 <- levelplot(gpw42010,at=piplup, main = "2010", col.regions=blueblue) + layer(sp.polygons(cnty, col = "black")) + layer(sp.polygons(subcity, col=))
      m2010
      
      persp(gpw42010, col = blueblue)
      