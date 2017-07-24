
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
library(tmap)
library(sf)
library(igraph)

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
wp_lbr_10<-raster("~/GoogleDrive/LiberiaProject/LBR-WP/LBR10adjv3.tif")
proj4string(wp_lbr_10)
wp_lbr_14 <- raster("~/GoogleDrive/LiberiaProject/LBR-WP/LBR14adjv1.tif")
proj4string(wp_lbr_14)

## Import GPW4 data ##
gpw4_2010<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2010.tif")
gpw4_2010<-crop(gpw4_2010,extent(lbr))
gpw4_2010<-mask(gpw4_2010,lbr)

## IMPORT OSM DATA ##
dsn<-"/Users/chriselsner/GoogleDrive/LiberiaProject/LBR-OSM"
bldgs<-readOGR(dsn = "/Users/chriselsner/GoogleDrive/LiberiaProject/LBR-OSM", layer = "gis.osm_buildings_a_free_1", stringsAsFactors=FALSE, verbose=FALSE)
proj4string(bldgs)

#####
## Load final data ##
load("RData/lbr_gpw4_data.RData")


## Load pixel boundaries data frame ##
load("RData/lbr_gpw4_pixels.RData")


## complete pop and spatial data set ##
load("RData/lbr_gpw4_complete.RData")


## load city points data ##
load("RData/city_points.RData")


# TYLERS CODE -------

# Import
prj_lbr <- "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#wp_lbr_10_utm<-projectRaster(wp_lbr_10,crs=prj_lbr)
wp_lbr_14_utm <- projectRaster(wp_lbr_14, crs = prj_lbr)
lbr_clan_utm <- spTransform(lbr_clan, CRS = prj_lbr)
lbr2_utm<-spTransform(lbr2, CRS = prj_lbr)

# bain singular -----
quartz()
plot(wp_lbr_14_utm_utm)
plot(lbr_clan_utm, add = TRUE)

bain_utm<- subset(lbr_clan_utm, CLNAME == "Bain")
plot(bain_utm)

#wpop_bain10 <- crop(wpop_lbr10, bain)
#wp_bain_10 <- mask(wp_bain_10, bain)

wp_bain_14_utm <- crop(wp_lbr_14_utm, bain_utm)
wp_bain_14_utm <- mask(wp_bain_14_utm, bain_utm)


# Find total pop

#cellStats(wpop_lbr10, 'sum')
#cellStats(wp_bain_10, 'sum')

cellStats(wp_lbr_14_utm, 'sum')
cellStats(wp_bain_14_utm, 'sum')

lbr_clan_utm@data$SUM_TOTAL[which(lbr_clan_utm@data$CLNAME == "Bain")]


# Analysis
wp_bain_14_utm_im<-as.im(wp_bain_14_utm)


pop <- floor(cellStats(wp_bain_14_utm, 'sum'))
pop2 <- floor(cellStats(wp_bain_14_utm, 'sum')/100)
win <- as(bain_utm,"owin")

set.seed(1)
bain_utm_ppp <- rpoint(pop, f = wp_bain_14_utm_im, win = win)
bain_utm_ppp2 <- rpoint(pop2, f = wp_bain_14_utm_im, win = win)
quartz()
plot(bain_utm_ppp, cex=.03)
plot(bain_utm_ppp2,cex=.03)


ppl <- bw.ppl(bain_utm_ppp)
#diggle <- bw.diggle(bain_ppp)

bain_utm_dens<-density(bain_utm_ppp,sigma=ppl)

Dsg <- as(bain_utm_dens, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image

if (is.null(lvl)){
  Dcl <- contourLines(Dim)  # create contour object
}else{
  Dcl <- contourLines(Dim,levels=.0006)  # create contour object
}

SLDF <- ContourLines2SLDF(Dcl, CRS(prj_lbr))
min_lvl<-min(as.numeric(as.character(SLDF@data$level)))
plot(SLDF)
SLDF<-SLDF[which(SLDF@data$level==min_lvl),]
plot(SLDF)
PS1 <- SpatialLines2PolySet(SLDF)
SP1 <- PolySet2SpatialPolygons(PS1, close_polys=TRUE)
plot(SP1)
PS2 <- SpatialPolygons2PolySet(SP1)
SL1<-PolySet2SpatialLines(PS2)
poly<-gPolygonize(SL1)
ids<-1:length(poly)
gas<-gArea(poly,byid = T)
head(sort(gas))
SPDF<-SpatialPolygonsDataFrame(poly,data = data.frame(ids,gas))
vals<-extract(wp_bain_14_utm, SPDF)
SPDF@data["totals"] <- unlist(lapply(vals, 'sum'))
print(paste("lvl: ",min_lvl))
SPDF

x<-sapply(SPDF@polygons,function(i) length(i@Polygons))
x

y<-SPDF@polygons[[52]]
y<-SpatialPolygons(list(y))
y<-gUnaryUnion(y)
SPDF@polygons[[52]]<-gUnaryUnion(SPDF@polygons[[52]]@Polygons)
plot(SPDF)



qtm(shp = SPDF, fill = "totals", fill.palette = "-Blues", format = "World_wide", fill.title="Bain \n Lowest Level of 8: 0.0005")



bain_dens<-density(bain_ppp,sigma=ppl)
LR<-scanLRTS(bain_ppp,r=2*ppl)
ST<-scan.test(bain_ppp,r=2*ppl)
quartz()
plot(bain_dens)
quartz()
plot(LR)
quartz()
plot(ST)

nne<-nnclean(bain_ppp,k=10)
quartz()
plot(nne,size=.03)
plot.ppp(nne)


# Bain plus ------
bain_plus<-readOGR(dsn = "shapefiles",layer="bain_surroundings",stringsAsFactors = FALSE,verbose=FALSE)
prj_lbr <- "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
wp_lbr_14_utm <- projectRaster(wp_lbr_14, crs = prj_lbr)
bain_plus_utm<-spTransform(bain_plus, CRS = prj_lbr)


wp_bain_plus_14_utm <- crop(wp_lbr_14_utm, bain_plus)
wp_bain_plus_14_utm <- mask(wp_bain_plus_14_utm, bain_plus)


# Find total pop

#cellStats(wpop_lbr10, 'sum')
#cellStats(wp_bain_10, 'sum')

#cellStats(wp_lbr_14_utm, 'sum')
cellStats(wp_bain_plus_14_utm, 'sum')

#lbr_clan_utm@data$SUM_TOTAL[which(lbr_clan_utm@data$CLNAME == "Bain")]



# Analysis
wp_bain_plus_14_utm_im<-as.im(wp_bain_plus_14_utm)
quartz()
plot(wp_bain_plus_14_utm)

pop <- floor(cellStats(wp_bain_plus_14_utm, 'sum'))
pop2<-floor(cellStats(wp_bain_plus_14_utm, 'sum')/10)
win <- as(bain_plus_utm,"owin")

set.seed(1)
wp_bain_plus_14_utm_ppp <- rpoint(pop, f = wp_bain_plus_14_utm_im, win = win)
wp_bain_plus_14_utm_ppp2 <- rpoint(pop2, f = wp_bain_plus_14_utm_im, win = win)
quartz()
plot(wp_bain_plus_14_utm_ppp, cex=.03)
plot(wp_bain_plus_14_utm_ppp2, cex=.03)


ppl <- bw.ppl(wp_bain_plus_14_utm_ppp)
ppl
#diggle <- bw.diggle(bain_plus_ppp)

# Quick stats checking ----
wp_bain_plus_14_utm_dens<-density(wp_bain_plus_14_utm_ppp,sigma=ppl)
LR<-scanLRTS(wp_bain_plus_14_utm_ppp,r=2*ppl)
ST<-scan.test(wp_bain_plus_14_utm_ppp,r=ppl)
quartz()
plot(wp_bain_plus_14_utm_dens)
quartz()
plot(LR)
quartz()
plot(ST,main="Scan Statistic \n r = 288.6996 ")

ST_im<-as.im(ST)
contour(ST_im,levels=50)

nne<-nnclean(wp_bain_plus_14_utm_ppp,k=10)
quartz()
plot(nne,cols=c("red","blue"),size=.03,bg="black")

plot.ppp(nne)
feat<-nne[which(nne$marks=="feature")]
feat<-unmark(feat)
plot(feat,size=.03)
feat_dens<-density(feat,sigma=ppl)
plot(feat_dens)
x<-contour(feat_dens,levels=.001)
feat_im<-as.im(feat)
plot(feat_im)
contour(feat_im,levels=20)
plot(x)
ll_feat_dens<-lovelace(feat_dens)
# SOLLVING ISSUE OF THE INNACURATE POLYGONS -----

ll_bain_plus_im<-lovelace(wp_bain_plus_14_utm)

Dsg <- as(feat_dens, "SpatialGridDataFrame")  # convert to spatial grid class
Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image

if (is.null(lvl)){
  Dcl <- contourLines(Dim)  # create contour object
}else{
  Dcl <- contourLines(Dim,levels=.0002)  # create contour object
}

SLDF <- ContourLines2SLDF(Dcl, CRS(prj_lbr))
min_lvl<-min(as.numeric(as.character(SLDF@data$level)))
plot(SLDF)
SLDF<-SLDF[which(SLDF@data$level==min_lvl),]
plot(SLDF)
PS1 <- SpatialLines2PolySet(SLDF)
SP1 <- PolySet2SpatialPolygons(PS1, close_polys=TRUE)
plot(SP1)
PS2 <- SpatialPolygons2PolySet(SP1)
SL1<-PolySet2SpatialLines(PS2)
poly<-gPolygonize(SL1)
ids<-1:length(poly)
gas<-gArea(poly,byid = T)
head(sort(gas))
SPDF<-SpatialPolygonsDataFrame(poly,data = data.frame(ids,gas))
vals<-extract(wp_bain_plus_14_utm, SPDF)
SPDF@data["totals"] <- unlist(lapply(vals, 'sum'))
print(paste("lvl: ",min_lvl))
SPDF
head(sort(ll_bain_plus_im@data$totals),n=20)



ll_bain_plus_ppp<-lovelace(wp_bain_plus_14_utm,use_ppp = T,win=bain_plus_utm)
sort(ll_bain_plus_ppp@data$totals)
sum(ll_bain_plus_ppp$totals)

quartz()
plot(wp_bain_plus_14_utm,main="contours from raster")
plot(ll_bain_plus_im,add=T)
sum(ll_bain_plus_im@data$totals, na.rm = TRUE) / sum(wp_bain_plus_14_utm@data@values,na.rm=T)

quartz()
plot(wp_bain_plus_14_utm,main="contours from ppp")
plot(ll_bain_plus_ppp,add=T)
sum(ll_bain_plus_ppp@data$totals, na.rm = TRUE) / sum(wp_bain_plus_14_utm@data@values,na.rm=T)


df<-raster2df(wp_bain_plus_14_utm)
plot(ecdf(df$vals))
###
# Modified ECDF method ------

load("wp_lbr_10_utm_msk.RData")
wp_lbr_10_utm_msk<-mask(wp_lbr_10_utm,lbr2_utm)
save(wp_lbr_10_utm_msk,file="wp_lbr_10_utm_msk.RData")
plot(wp_lbr_10_utm_msk)

y<-floor(cellStats(wp_lbr_10_utm_msk,"sum"))
x<-cellStats(wp_lbr_10_utm_msk,"max")
plot(x=1,xlim=c(0,x),ylim=c(0,y))

vals<-getValues(wp_lbr_10_utm_msk)
vals<-na.omit(vals)
vals<-as.vector(vals)
unq<-unique(vals)
z<-sapply(1:100,function(i) unlist(list(vals[i],sum(vals[which(vals<=vals[i])]))))
z<-t(z)
a<-sapply(1:5000,function(i) unlist(list(unq[i],sum(vals[which(vals<=unq[i])]))))
a<-t(a)
quartz()
plot(x=z[,1],y=z[,2],xlim=c(0,x),ylim=c(0,y))
plot(x=a[,1],y=a[,2],xlim=c(0,x),ylim=c(0,y))#,add=T)

plot(ecdf(wp_lbr_10_utm_msk))

##


# Network Analyisis Clustering ------

nodeclust <- function (df, current_point, r){
  
  circle <- shape.circle (c (current_point[,1], current_point[,2]), r = r)
  last_point <- current_point
  df <- df[which (df[,3] != current_point[,3]),]
  point_set <- df[which (point.in.polygon (df[,1], df[,2], circle$x, circle$y) > 0),]
  
  if (nrow (point_set) == 0){
    return (current_point)
    
  }else{
    df <- subset(df, !(df[,3] %in% point_set[,3]))
    branch <- unique(bind_rows(lapply (1:nrow (point_set), function (i) {nodeclust (df, point_set[i,], r)})))
    cluster<-rbind (last_point, branch)
    return (cluster)
  }
}

ncluster <- function(df, r){
  clusts <- list()
  i <- 1
  while (i <= nrow (df)){
    clust <- nodeclust (df, df[1,], r)
    df <- subset(df, !(df[,3] %in% clust[,3]))
    clusts <- c(clusts, list(clust))
    i <- 1
  }
  return (clusts)
}

install.packages("progress")
library(progress)

win<-owin(c(0,10),c(0,10))
f<-function(x,y){x+y}
set.seed(5)
box<-rpoint(1000,f=f,win=win)
quartz()
plot(box)
df<-as.data.frame(box)
df$id<-seq(1,nrow(df))
plot(df$x,df$y)

clusts <- ncluster(df,.3)

bounds <- lapply (1:length(clusts), function(i) {lapply (1:nrow (clusts[[i]]), function(j) {shape.circle (c (clusts[[i]][j,1], clusts[[i]][j,2]), .1)})})

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,17), col=sample(col_vector, 17))
cols<-sample(col_vector, 17)

plot(df$x,df$y)
for (i in 1:length(bounds)){
  for (j in 1:length(bounds[[i]])){
    lines(bounds[[i]][[j]],col = cols[i])
  }
}
#
plot(wp_bain_plus_14_utm_ppp,cex=.03)
bain_plus_df<-as.data.frame(wp_bain_plus_14_utm_ppp)
bain_plus_df$id<-seq(1,nrow(bain_plus_df))
plot(bain_plus_df,cex=.03)
bain_plus_clust<-ncluster(bain_plus_df,10)

bain_utm_ppp<-rpoint(floor((cellStats(wp_bain_14_utm, 'sum'))), f = wp_bain_14_utm_im, win=win)
plot(bain_utm_ppp,cex=.03)
bain_df<-as.data.frame(bain_utm_ppp)
bain_df$id<-seq(1,nrow(bain_df))
plot(bain_df$x,bain_df$y,cex=.03)
bain_clust<-ncluster(bain_df,70)

adj_m<-find_neighbors(bain_df,20)
graph <- graph_from_adjacency_matrix(adj_m, mode = "undirected")
edges <- get.edgelist(graph)
links <- data.frame (id = unique (unlist (list (edges[,1],edges[,2]))), group = clusters (graph)$membership)
links <- split(links$id,links$group)
graph2 <- graph_from_adjacency_matrix(adj_m, mode = "undirected", diag = FALSE)
l <- as.matrix(bain_df[,1:2])
plot(graph2, vertex.size = 1, vertex.color = "black", vertex.label= NA, edge.arrow.size = .1, 
     axes = TRUE, layout = l, xlim = c(floor (min (l[,1] - 1)), floor (max (l[,1] + 1))), ylim = c(floor (min (l[,2] - 1)), floor (max (l[,2] + 1))), rescale = FALSE, 
     mark.groups = links, mark.shape = 1)

bain_gclust<-graphclust(bain_df, r=20)

len<-sapply(1:length(links), function(i) length(links[[i]]))
max(len)

bain_gclust2<-bain_df[which(bain_df$id %in% bain_gclust[[2]]),]
M<-as.matrix(bain_gclust2)
points(M)
shell<-convex_hull(M[,1:2])
shell_df <- df[which(df$id %in% shell),]
points(shell$rescoords,col="purple")
polygon(shell$rescoords)

install.packages("alphahull")
library(alphahull)

ahull_shell<-ahull(M,alpha=400)
plot(ahull_shell, col = c(6, rep(1, 5)), xlab = "x-coordinate", ylab = "y-coordinate")

n<-300
theta<-runif(n,0,2*pi)
r<-sqrt(runif(n,0.25^2,0.5^2))
x<-cbind(0.5+r*cos(theta),0.5+r*sin(theta))
alpha <- 0.15
alphahull <- ahull(x, alpha = alpha)
plot(alphahull, col = c(6, rep(1, 5)), xlab = "x-coordinate", ylab = "y-coordinate", main = expression(paste(alpha, "-hull")))

# Try #2: Network Clustering ----
win<-owin(c(0,10),c(0,10))
f<-function(x,y){sqrt(x) + x^2 + y}
set.seed(5)
box<-rpoint(1000,f=f,win=win)
quartz()
plot(box)
df<-as.data.frame(box)
df$id<-seq(1,nrow(df))
plot(df$x,df$y,cex=.1)
for (i in 1:100){
  lines(circles[[i]],col="red")
}


df<-as.data.frame(rpoint(10000))
system.time (circles <- lapply (1:nrow (df), function(i) {shape.circle (c (df$x[i], df$y[i]), r = .1)}))

x<-df$x
y<-df$y
len<-nrow(df)
system.time(circles <- lapply (1:len, function(i) {shape.circle (c (x[i], y[i]), r =.1)}))

system.time(point_sets <- lapply (1:len, function(i) {point.in.polygon (x, y, circles[[i]]$x, circles[[i]]$y, mode.checked = TRUE)}))

system.time(adj_m <- matrix(unlist (point_sets), ncol = len, byrow = TRUE))

find_neighbors<-function(df, r){
  x<-df$x
  y<-df$y
  len<-nrow(df)
  circles <- lapply (1:len, function(i) {shape.circle (c (x[i], y[i]), r = r)})
  point_sets <- lapply (1:len, function(i) {point.in.polygon (x, y, circles[[i]]$x, circles[[i]]$y, mode.checked = TRUE)})
  adj_m <- matrix(unlist (point_sets), ncol = len, byrow = TRUE)
  #diag(adj_m) <- 0
  adj_m
}

x<-find_neighbors(df,.1)

graphclust <- function (df, r){
  adj_m <- find_neighbors (df, r)
  graph <- graph_from_adjacency_matrix(adj_m, mode = "undirected")
  edges <- get.edgelist(graph)
  links <- data.frame (id = unique (unlist (list (edges[,1],edges[,2]))), group = clusters (graph)$membership)
  links <- split(links$id,links$group)
  #graph2 <- graph_from_adjacency_matrix(adj_m, mode = "undirected", diag = FALSE)
  l <- as.matrix(df[,c("x","y")])
  #plot(graph2, vertex.size = 1, vertex.color = "black", vertex.label= NA, edge.arrow.size = .1, 
  #     axes = TRUE, layout = l, xlim = c(floor (min (l[,1] - 1)), floor (max (l[,1] + 1))), ylim = c(floor (min (l[,2] - 1)), floor (max (l[,2] + 1))), rescale = FALSE, 
  #     mark.groups = links, mark.shape = 1)
  return (links)
}

test <- graphclust (df, .5)

test2<-df[which(df$id %in% test[[1]]),]
M<-as.matrix(test2)
points(M)
shell<-convex_hull(M[,1:2])
shell_df <- df[which(df$id %in% shell),]
points(shell$rescoords,col="purple")
polygon(shell$rescoords)

install.packages("alphahull")
library(alphahull)

ahul

rm()


