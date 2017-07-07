rm(list=ls())

setwd("/Users/tylerfrazier/Tresors/teaching/INTR100_Fall16/labs/lab_7_final")

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

#CREATE SPATIAL POLYGON DATAFRAMES, CORRECT DATA AND FORTIFY
#counties <- getData("GADM", country="LBR", level=1)

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
africa <- subset(africa, COUNTRY == "Sierra Leone" | COUNTRY == "Guinea" | COUNTRY == "Côte d'Ivoire")

#correct Number of Households in Bomi County
bomi <- subset(districts@data, FIRST_CCNA == "Bomi")
counties@data[1,7] <- sum(bomi$HHOLDS)

africa_f <- fortify(africa)
country_f <- fortify(country)
counties_f <- fortify(counties)
districts_f <- fortify(districts)
clans_f <- fortify(clans)

#COUNTRIES
#create labels & join data to data frame
country_names <- africa@data$COUNTRY
country_cpts <- gCentroid(africa, byid=TRUE)
country_labs <- cbind.data.frame(country_names, country_cpts@coords[,1], country_cpts@coords[,2])
names(country_labs) <- c("country","x","y")
country_labs[1,2] <- -7.75 #modify Cote d'Ivore location
country_labs[1,3] <- 6.75 
country_labs[2,2] <- -9.00 #modify Guinea location
country_labs[2,3] <- 8.4 
country_labs[3,2] <- -11.15 #modify Guinea location
country_labs[3,3] <- 8.00 

#COUNTIES
#create labels & join data to data frame
county_names <- counties@data$CCNAME
county_cpts <- gCentroid(counties, byid=TRUE)
county_labs <- cbind.data.frame(county_names, county_cpts@coords[,1], county_cpts@coords[,2])
names(county_labs) <- c("county","x","y")
county_labs[9,3] <- 6.65 # Margibi county location
county_labs[9,2] <- -10.25

counties_d <- counties@data[,c(2,4:7)]
counties_d$id <- row.names(counties)
counties_f <- left_join(x = counties_f, y = counties_d, by = c("id" = "id"))

#DISTRICTS
#create labels & join data to data frame
district_names <- districts@data$DNAME
district_cpts <- gCentroid(districts, byid=TRUE)
district_labs <- cbind.data.frame(district_names, district_cpts@coords[,1], district_cpts@coords[,2])
names(district_labs) <- c("district","x","y")

districts_d <- districts@data[,c(1,7:10)]
districts_d$id <- row.names(districts)
districts_f <- left_join(x = districts_f, y = districts_d, by = c("id" = "id"))

#CLANS
#create labels & join data to data frame
clan_names <- clans@data$CLNAME
clan_cpts <- gCentroid(clans, byid=TRUE)
clan_labs <- cbind.data.frame(clan_names, clan_cpts@coords[,1], clan_cpts@coords[,2])
names(clan_labs) <- c("clan","x","y")

clans_d <- clans@data[,c(1,5:8)]
clans_d$id <- row.names(clans)
clans_f <- left_join(x = clans_f, y = clans_d, by = c("id" = "id"))

# PLOT TOTAL POPULATION, MALE, FEMALE AND HOUSEHOLDS
#Base map
liberia <- get_map(location = c(-11.65, 4.25, -7.25, 8.6), maptype = "watercolor")
liberia <- ggmap(liberia)

# Population
# Counties
lbr_counties <- liberia + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id,fill =log10(SUM_TOTAL)))
lbr_counties <- lbr_counties + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .5)
lbr_counties <- lbr_counties + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_counties <- lbr_counties + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_counties <- lbr_counties + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_counties <- lbr_counties + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_counties <- lbr_counties + ggtitle("Counties")
ggsave("latex/lbr_counties.pdf", lbr_counties, width = 10, height = 10, dpi = 300)

# Districts
lbr_districts <- liberia + geom_map(data=districts_f,map=districts_f,aes(x=long, y=lat, map_id=id,fill =log10(TOTAL)))
lbr_districts <- lbr_districts + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_districts <- lbr_districts + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_districts <- lbr_districts + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_districts <- lbr_districts + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_districts <- lbr_districts + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_districts <- lbr_districts + ggtitle("Districts")
ggsave("latex/lbr_districts.pdf", lbr_districts, width = 10, height = 10, dpi = 300)

# Clans
lbr_clans <- liberia + geom_map(data=clans_f,map=clans_f,aes(x=long, y=lat, map_id=id,fill = log10(SUM_TOTAL)))
#lbr_clans <- lbr_clans + geom_map(data=districts_f,map=districts_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .15)
lbr_clans <- lbr_clans + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_clans <- lbr_clans + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_clans <- lbr_clans + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_clans <- lbr_clans + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_clans <- lbr_clans + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_clans <- lbr_clans + ggtitle("Clans")
ggsave("latex/lbr_clans.pdf", lbr_clans, width = 10, height = 10, dpi = 300)

# Male
# Counties
lbr_counties <- liberia + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id,fill =log10(SUM_MALE)))
lbr_counties <- lbr_counties + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .5)
lbr_counties <- lbr_counties + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_counties <- lbr_counties + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_counties <- lbr_counties + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_counties <- lbr_counties + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_counties <- lbr_counties + ggtitle("Counties")
ggsave("latex/male_counties.pdf", lbr_counties, width = 10, height = 10, dpi = 300)

# Districts
lbr_districts <- liberia + geom_map(data=districts_f,map=districts_f,aes(x=long, y=lat, map_id=id,fill =log10(MALE)))
lbr_districts <- lbr_districts + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_districts <- lbr_districts + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_districts <- lbr_districts + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_districts <- lbr_districts + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_districts <- lbr_districts + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_districts <- lbr_districts + ggtitle("Districts")
ggsave("latex/male_districts.pdf", lbr_districts, width = 10, height = 10, dpi = 300)

# Clans
lbr_clans <- liberia + geom_map(data=clans_f,map=clans_f,aes(x=long, y=lat, map_id=id,fill = log10(SUM_MALE)))
#lbr_clans <- lbr_clans + geom_map(data=districts_f,map=districts_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .15)
lbr_clans <- lbr_clans + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_clans <- lbr_clans + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_clans <- lbr_clans + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_clans <- lbr_clans + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_clans <- lbr_clans + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_clans <- lbr_clans + ggtitle("Clans")
ggsave("latex/male_clans.pdf", lbr_clans, width = 10, height = 10, dpi = 300)

# Female
# Counties
lbr_counties <- liberia + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id,fill =log10(SUM_FEMALE)))
lbr_counties <- lbr_counties + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .5)
lbr_counties <- lbr_counties + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_counties <- lbr_counties + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_counties <- lbr_counties + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_counties <- lbr_counties + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_counties <- lbr_counties + ggtitle("Counties")
ggsave("latex/female_counties.pdf", lbr_counties, width = 10, height = 10, dpi = 300)

# Districts
lbr_districts <- liberia + geom_map(data=districts_f,map=districts_f,aes(x=long, y=lat, map_id=id,fill =log10(FEMALE)))
lbr_districts <- lbr_districts + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_districts <- lbr_districts + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_districts <- lbr_districts + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_districts <- lbr_districts + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_districts <- lbr_districts + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_districts <- lbr_districts + ggtitle("Districts")
ggsave("latex/female_districts.pdf", lbr_districts, width = 10, height = 10, dpi = 300)

# Clans
lbr_clans <- liberia + geom_map(data=clans_f,map=clans_f,aes(x=long, y=lat, map_id=id,fill = log10(SUM_FEMALE)))
#lbr_clans <- lbr_clans + geom_map(data=districts_f,map=districts_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = .15)
lbr_clans <- lbr_clans + geom_map(data=counties_f,map=counties_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_clans <- lbr_clans + geom_map(data=africa_f,map=africa_f,aes(x=long, y=lat, map_id=id), color ="white", alpha = 0, size = 1.5)
lbr_clans <- lbr_clans + scale_fill_gradient(low="yellow", high="red", space="Lab")
lbr_clans <- lbr_clans + annotate('text', x = county_labs$x, y = county_labs$y, label = county_labs$county, size = 4)
lbr_clans <- lbr_clans + annotate('text', x = country_labs$x, y = country_labs$y, label = country_labs$country, size = 8)
lbr_clans <- lbr_clans + ggtitle("Clans")
ggsave("latex/female_clans.pdf", lbr_clans, width = 10, height = 10, dpi = 300)



# DATA PREPARATION FOR LAB
# Import CWIQ10 from SPSS and save as .RData

# cwiq10 <- read.spss("LBR_CWIQ_FINAL.sav", to.data.frame = TRUE, use.value.labels = FALSE) #add foreign::
# which( colnames(cwiq10)=="wta_hh" ) #give which column number
# import only select variables
# cwiq10 <- read.spss("LBR_CWIQ_FINAL.sav", to.data.frame = TRUE, use.value.labels = FALSE)[,c(1,7,8,9,10,11,12,15,18,19,24:26,33,35,253)]
# cwiq10$hid_mungai <- as.character(cwiq10$hid_mungai)
# save(cwiq10, file = "cwiq10.RData")
# rm(cwiq10)

# LOAD CWIQ10 DATA AND JOIN TO GOV SUBDIVISION CENTER POINTS
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
     
     







###### Setting Points ##### Not Needed ##############

# convert 01 to 30

cpts03 <- subset(cnty_survey_cpts,county_id == "03")
cpts06 <- subset(cnty_survey_cpts,county_id == "06")
cpts09 <- subset(cnty_survey_cpts,county_id == "09")
cpts12 <- subset(cnty_survey_cpts,county_id == "12")
cpts15 <- subset(cnty_survey_cpts,county_id == "15")
cpts18 <- subset(cnty_survey_cpts,county_id == "18")
cpts21 <- subset(cnty_survey_cpts,county_id == "21")
cpts24 <- subset(cnty_survey_cpts,county_id == "24")
cpts27 <- subset(cnty_survey_cpts,county_id == "27")
cpts30 <- subset(cnty_survey_cpts,county_id == "30")
cpts33 <- subset(cnty_survey_cpts,county_id == "33")
cpts36 <- subset(cnty_survey_cpts,county_id == "36")
cpts39 <- subset(cnty_survey_cpts,county_id == "39")
cpts42 <- subset(cnty_survey_cpts,county_id == "42")
cpts45 <- subset(cnty_survey_cpts,county_id == "45")

# simplify

cwiq10_cpts <- rbind.data.frame(
  
  cpts03[rep(1, each = nrow(subset(cwiq10, county == 3))),],
  cpts06[rep(1, each = nrow(subset(cwiq10, county == 6))),],
  cpts09[rep(1, each = nrow(subset(cwiq10, county == 9))),],
  cpts12[rep(1, each = nrow(subset(cwiq10, county == 12))),],
  cpts15[rep(1, each = nrow(subset(cwiq10, county == 15))),],
  cpts18[rep(1, each = nrow(subset(cwiq10, county == 18))),],
  cpts21[rep(1, each = nrow(subset(cwiq10, county == 21))),],
  cpts24[rep(1, each = nrow(subset(cwiq10, county == 24))),],
  cpts27[rep(1, each = nrow(subset(cwiq10, county == 27))),],
  cpts30[rep(1, each = nrow(subset(cwiq10, county == 1 | county == 30))),],
  cpts33[rep(1, each = nrow(subset(cwiq10, county == 33))),],
  cpts36[rep(1, each = nrow(subset(cwiq10, county == 36))),],
  cpts39[rep(1, each = nrow(subset(cwiq10, county == 39))),],
  cpts42[rep(1, each = nrow(subset(cwiq10, county == 42))),],
  cpts45[rep(1, each = nrow(subset(cwiq10, county == 45))),]
  
)



###### Setting Points ##### Not Needed ##############





#win <- as(counties,"owin")

win1 <- slot(counties, "polygons")[1]
win2 <- slot(counties, "polygons")[2]
win3 <- slot(counties, "polygons")[3]
win4 <- slot(counties, "polygons")[4]
win5 <- slot(counties, "polygons")[5]
win6 <- slot(counties, "polygons")[6]
win7 <- slot(counties, "polygons")[7]
win8 <- slot(counties, "polygons")[8]
win9 <- slot(counties, "polygons")[9]
win10 <- slot(counties, "polygons")[10]
win11 <- slot(counties, "polygons")[11]
win12 <- slot(counties, "polygons")[12]
win13 <- slot(counties, "polygons")[13]
win14 <- slot(counties, "polygons")[14]
win15 <- slot(counties, "polygons")[15]

#win1 <- counties@polygons[1]

win1 <- SpatialPolygons(win1)
win2 <- SpatialPolygons(win2)
win3 <- SpatialPolygons(win3)
win4 <- SpatialPolygons(win4)
win5 <- SpatialPolygons(win5)
win6 <- SpatialPolygons(win6)
win7 <- SpatialPolygons(win7)
win8 <- SpatialPolygons(win8)
win9 <- SpatialPolygons(win9)
win10 <- SpatialPolygons(win10)
win11 <- SpatialPolygons(win11)
win12 <- SpatialPolygons(win12)
win13 <- SpatialPolygons(win13)
win14 <- SpatialPolygons(win14)
win15 <- SpatialPolygons(win15)

#win2_3 <- lapply(c(win2,win3), function(x){SpatialPolygons(list(x))})

win1 <- as(win1,"owin")
win2 <- as(win2,"owin")
win3 <- as(win3,"owin")
win4 <- as(win4,"owin")
win5 <- as(win5,"owin")
win6 <- as(win6,"owin")
win7 <- as(win7,"owin")
win8 <- as(win8,"owin")
win9 <- as(win9,"owin")
win10 <- as(win10,"owin")
win11 <- as(win11,"owin")
win12 <- as(win12,"owin")
win13 <- as(win13,"owin")
win14 <- as(win14,"owin")
win15 <- as(win15,"owin")

# win10 <- rpoint(5, win = win1)
# win10 <- dirichlet(win10)
# 
# win10a <- as.owin(win10[1])
# win10b <- as.owin(win10[2])
# win10c <- as.owin(win10[3])
# win10d <- as.owin(win10[4])
# win10e <- as.owin(win10[5])

#win2_3 <- solapply(win2_3, as.owin)

# wins <- solist(win1,win2,win3,win4,
#                win5,win6,win7,win8,win9,
#                win10a,win10b,win10c,win10d,win10e,
#                win11,win12,win13,win14,win15)

#w/o dirichelet polygons

wins <- solist(win1,win2,win3,win4,
               win5,win6,win7,win8,win9,
               win10,win11,win12,win13,win14,
               win15)

te <- tess(tiles=wins)

plot(te)


govs <- as(te, "SpatialPolygons")

proj4string(govs) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#govs <- spTransform(govs, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

plot(govs)

govs <- fortify(govs)

liberia <- get_map(location = c(-13, 3, -6, 9), maptype = "watercolor")

liberia <- ggmap(liberia)

liberia <- liberia + geom_map(data=govs,
                              map=govs,
                              aes(x=long, y=lat, map_id=id),
                              color ="white",
                              fill ="orangered4",
                              alpha = .4,
                              size = .2)





win1 <- as(counties[counties@data$OBJECTID == 1,],"owin")
win2 <- as(counties[counties@data$OBJECTID == 2,],"owin")
win3 <- as(counties[counties@data$OBJECTID == 3,],"owin")
win4 <- as(counties[counties@data$OBJECTID == 4,],"owin")
win5 <- as(counties[counties@data$OBJECTID == 5,],"owin")

win1 <- rpoint(10, win = win1)

win1 <- dirichlet(win1)

win1_2 <- solapply(c(win1,win2), as.owin)



plot(win1)





plot(counties)
points(spsample(counties[counties@data$OBJECTID == 2,], n = 10, "random"), pch = 1, cex = .75)

