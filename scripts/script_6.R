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
library(alphahull)

source("~/Documents/R/functions.R")

# CLUSTERING FUNCTIONS ------
  
  # SUB FUNCTIONS
    shape.circle <- function(center = c(a,b),r){
      tt <- seq(0,2*pi,length.out = 100)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      shp<-data.frame (x = xx,y = yy)
    }

  # LOVELACE 
    lovelace<-function(rast,lvl=NULL,use_ppp=F,...){
      
      im<-as.im(rast)
      
      if (use_ppp==T){
        if (hasArg(win)!=T){
          win=NULL
        }
        if (hasArg(exact)!=T){
          exact=F
        }
        ppp<-raster2ppp(rast,win,exact)
        bw<-bw.ppl(ppp)
        im <- density.ppp(ppp, sigma = bw)
      }
      
      Dsg <- as(im, "SpatialGridDataFrame")  # convert to spatial grid class
      Dim <- as.image.SpatialGridDataFrame(Dsg)  # convert again to an image
      
      if (is.null(lvl)){
        Dcl <- contourLines(Dim)  # create contour object
      }else{
        Dcl <- contourLines(Dim,levels=lvl)  # create contour object
      }
      
      SLDF <- ContourLines2SLDF(Dcl, CRS(prj))
      min_lvl<-min(as.numeric(as.character(SLDF@data$level)))
      SLDF<-SLDF[which(SLDF@data$level==min_lvl),]
      PS1 <- SpatialLines2PolySet(SLDF)
      SP1 <- PolySet2SpatialPolygons(PS1, close_polys=TRUE)
      PS2 <- SpatialPolygons2PolySet(SP1)
      SL1<-PolySet2SpatialLines(PS2)
      poly<-gPolygonize(SL1)
      ids<-1:length(poly)
      SPDF<-SpatialPolygonsDataFrame(poly,data = data.frame(ids))
      vals<-extract(rast, SPDF)
      SPDF@data["totals"] <- unlist(lapply(vals, 'sum'))
      print(paste("lvl: ",min_lvl))
      SPDF
    }
  
  
  # NODES
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
  
  
  # GRAPH
    find_neighbors<-function(df, r){
      circles <- lapply (1:nrow (df), function(i) {shape.circle (c (df$x[i], df$y[i]), r = r)})
      point_sets <- lapply (1:length (circles), function(i) {point.in.polygon (df$x, df$y, circles[[i]]$x, circles[[i]]$y)})
      adj_m <- matrix(unlist (point_sets), ncol = nrow (df), byrow = TRUE)
      adj_m
    }
    
    graphclust <- function (df, r, plot = FALSE){
      adj_m <- find_neighbors (df, r)
      graph <- graph_from_adjacency_matrix(adj_m, mode = "undirected")
      edges <- get.edgelist(graph)
      links <- data.frame (id = unique (unlist (list (edges[,1],edges[,2]))), group = clusters (graph)$membership)
      links <- split(links$id,links$group)
      if (plot == TRUE){
        graph2 <- graph_from_adjacency_matrix(adj_m, mode = "undirected", diag = FALSE)
        l <- as.matrix(df[,c("x","y")])
        plot(graph2, vertex.size = 1, vertex.color = "black", vertex.label= NA, edge.arrow.size = .1, 
             axes = TRUE, layout = l, xlim = c(floor (min (l[,1])), floor (max (l[,1] + 1))), ylim = c(floor (min (l[,2])), floor (max (l[,2] + 1))), rescale = FALSE, 
             mark.groups = links, mark.shape = 1)
      }
      return (links)
    }
  
  
##
# DATA ------
  
  # COUNTRY SHAPEFILES
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
    
    
  # WORLDPOP
    wp_lbr_10<-raster("~/GoogleDrive/LiberiaProject/LBR-WP/LBR10adjv3.tif")
    proj4string(wp_lbr_10)
    wp_lbr_14 <- raster("~/GoogleDrive/LiberiaProject/LBR-WP/LBR14adjv1.tif")
    proj4string(wp_lbr_14)
  
  # GPW4
    gpw4_2010<-raster("~/GoogleDrive/LiberiaProject/gpw-v4-population-count_2010.tif")
    gpw4_2010<-crop(gpw4_2010,extent(lbr))
    gpw4_2010<-mask(gpw4_2010,lbr)
  
  # OSM  
    dsn<-"/Users/chriselsner/GoogleDrive/LiberiaProject/LBR-OSM"
    bldgs<-readOGR(dsn = "/Users/chriselsner/GoogleDrive/LiberiaProject/LBR-OSM", layer = "gis.osm_buildings_a_free_1", stringsAsFactors=FALSE, verbose=FALSE)
    proj4string(bldgs)
    
    
##
# UTM CONVERSION ----
    
    prj_lbr <- "+proj=utm +zone=29 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
    
    lbr_clan_utm <- spTransform(lbr_clan, CRS = prj_lbr)
    lbr2_utm <- spTransform(lbr2, CRS = prj_lbr)
    
    wp_lbr_10_utm <- projectRaster (wp_lbr_10, crs = prj_lbr)
    wp_lbr_14_utm <- projectRaster (wp_lbr_14, crs = prj_lbr)
    
    
##
# CREATE WEST AFRICA SHAPEFILE -----
    
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
##
# BAIN ----
    
    bain_utm<- subset(lbr_clan_utm, CLNAME == "Bain")
    
    #wp_bain_10_utm <- crop(wb_lbr_10_utm, bain_utm)
    #wp_bain_10_utm <- mask(wp_bain_10_utm, bain_utm)
    
    wp_bain_14_utm <- crop(wp_lbr_14_utm, bain_utm)
    wp_bain_14_utm <- mask(wp_bain_14_utm, bain_utm)
    
  
##
# BAIN PLUS ----
    
    bain_plus_utm<-readOGR (dsn = "shapefiles", layer="bain_surroundings", stringsAsFactors = FALSE, verbose=FALSE)
    
    #wp_bain_plus_10_utm <- crop(wb_lbr_10_utm, bain_plus_utm)
    #wp_bain_plus_10_utm <- mask(wp_bain_10_utm, bain_plus_utm)
    
    wp_bain_plus_14_utm <- crop(wp_lbr_14_utm, bain_plus_utm)
    wp_bain_plus_14_utm <- mask(wp_bain_plus_14_utm, bain_plus_utm)
    
    
##  
# MODIFIED ECDF ----

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
# EXAMPLE GRAPH CLUSTERING -----
    
    win <- owin (c (0,10), c (0,10))
    f <- function(x, y) {x + sqrt (y)}
    pts <- rpoint(200, f = f, win = win)
    df <- as.data.frame(pts)
    df$id <- seq (1:nrow(df))
    df$weight <- 1
    quartz()
    plot (df$x, df$y)
    
    clust <- graphclust(df, .6, plot = T)
    df$clust <- NA
    for (i in 1:length (clust)){
      df[which(df$id %in% clust[[i]]),]$clust <- i
    }
    
    clust_totals<-aggregate(weight ~ clust, df, sum)
    # clust2 <- clust[which (lapply (clust, length) > 2)]
    # clust2 <- lapply (1:length (clust2), function (i) df[ which (df$id %in% clust2[[i]]),])
    # 
    # concave<-lapply (1:length (clust2), function (i) ahull (clust2[[i]]$x, clust2[[i]]$y, alpha = 1.2))
    # plot(concave[[11]], col = c(6, rep(1, 5)), xlab = "x-coordinate", ylab = "y-coordinate", add = TRUE)
    # shps <- lapply(1:length (concave), function(i) ah2sp (concave[[i]], proj4string = CRS(prj_lbr)))
    # convex <- clust2[which( lapply(1:length (shps), function(i) is.null(shps[[i]])) == TRUE)]
    # shps <- shps[which( lapply(1:length (shps), function(i) is.null(shps[[i]])) != TRUE)]
    # 
    # coords<-lapply(1:length(shps),function(i) shps[[i]]@polygons[[1]]@Polygons[[1]]@coords)
    
    ah2sp <- function(x, increment=360, rnd=10, proj4string=CRS(as.character(NA))){
      require(alphahull)
      require(maptools)
      if (class(x) != "ahull"){
        stop("x needs to be an ahull class object")
      }
      # Extract the edges from the ahull object as a dataframe
      xdf <- as.data.frame(x$arcs)
      # Remove all cases where the coordinates are all the same      
      xdf <- subset(xdf,xdf$r > 0)
      res <- NULL
      if (nrow(xdf) > 0){
        # Convert each arc to a line segment
        linesj <- list()
        prevx<-NULL
        prevy<-NULL
        j<-1
        for(i in 1:nrow(xdf)){
          rowi <- xdf[i,]
          v <- c(rowi$v.x, rowi$v.y)
          theta <- rowi$theta
          r <- rowi$r
          cc <- c(rowi$c1, rowi$c2)
          # Arcs need to be redefined as strings of points. Work out the number of points to allocate in this arc segment.
          ipoints <- 2 + round(increment * (rowi$theta / 2),0)
          # Calculate coordinates from arc() description for ipoints along the arc.
          angles <- anglesArc(v, theta)
          seqang <- seq(angles[1], angles[2], length = ipoints)
          x <- round(cc[1] + r * cos(seqang),rnd)
          y <- round(cc[2] + r * sin(seqang),rnd)
          # Check for line segments that should be joined up and combine their coordinates
          if (is.null(prevx)){
            prevx<-x
            prevy<-y
          } else if (x[1] == round(prevx[length(prevx)],rnd) && y[1] == round(prevy[length(prevy)],rnd)){
            if (i == nrow(xdf)){
              #We have got to the end of the dataset
              prevx<-append(prevx,x[2:ipoints])
              prevy<-append(prevy,y[2:ipoints])
              prevx[length(prevx)]<-prevx[1]
              prevy[length(prevy)]<-prevy[1]
              coordsj<-cbind(prevx,prevy)
              colnames(coordsj)<-NULL
              # Build as Line and then Lines class
              linej <- Line(coordsj)
              linesj[[j]] <- Lines(linej, ID = as.character(j))
            } else {
              prevx<-append(prevx,x[2:ipoints])
              prevy<-append(prevy,y[2:ipoints])
            }
          } else {
            # We have got to the end of a set of lines, and there are several such sets, so convert the whole of this one to a line segment and reset.
            prevx[length(prevx)]<-prevx[1]
            prevy[length(prevy)]<-prevy[1]
            coordsj<-cbind(prevx,prevy)
            colnames(coordsj)<-NULL
            # Build as Line and then Lines class
            linej <- Line(coordsj)
            linesj[[j]] <- Lines(linej, ID = as.character(j))
            j<-j+1
            prevx<-NULL
            prevy<-NULL
          }
        }
        # Promote to SpatialLines
        lspl <- SpatialLines(linesj)
        # Convert lines to polygons
        # Pull out Lines slot and check which lines have start and end points that are the same
        lns <- slot(lspl, "lines")
        polys <- sapply(lns, function(x) { 
          crds <- slot(slot(x, "Lines")[[1]], "coords")
          identical(crds[1, ], crds[nrow(crds), ])
        }) 
        # Select those that do and convert to SpatialPolygons
        polyssl <- lspl[polys]
        list_of_Lines <- slot(polyssl, "lines")
        sppolys <- SpatialPolygons(list(Polygons(lapply(list_of_Lines, function(x) { Polygon(slot(slot(x, "Lines")[[1]], "coords")) }), ID = "1")), proj4string=proj4string)
        # Create a set of ids in a dataframe, then promote to SpatialPolygonsDataFrame
        hid <- sapply(slot(sppolys, "polygons"), function(x) slot(x, "ID"))
        areas <- sapply(slot(sppolys, "polygons"), function(x) slot(x, "area"))
        df <- data.frame(hid,areas)
        names(df) <- c("HID","Area")
        rownames(df) <- df$HID
        res <- SpatialPolygonsDataFrame(sppolys, data=df)
        res <- res[which(res@data$Area > 0),]
      }  
      return(res)
    }   
    
##
# CLUSTERING BAIN
    wp_bain_14_utm_im <- as.im(wp_bain_14_utm)
    bain_utm_win <- as (bain_utm, "owin")
   
    bain_pop <- floor (cellStats (wp_bain_14_utm, "sum"))
    bain_pop_50per <- floor (cellStats (wp_bain_14_utm, "sum")/2)
    bain_pop_33per <- floor (cellStats (wp_bain_14_utm, "sum")/3)
    bain_pop_10per <- floor (cellStats (wp_bain_14_utm, "sum")/10)
    bain_pop_5per <- floor (cellStats (wp_bain_14_utm, "sum")/50)
    bain_pop_1per <- floor (cellStats (wp_bain_14_utm, "sum")/100)
    
    wp_bain_14_utm_ppp <- rpoint (bain_pop, f = wp_bain_14_utm_im, win = bain_utm_win)
    wp_bain_14_utm_ppp_50per <- rpoint (bain_pop_50per, f = wp_bain_14_utm_im, win = bain_utm_win)
    wp_bain_14_utm_ppp_33per <- rpoint (bain_pop_33per, f = wp_bain_14_utm_im, win = bain_utm_win)
    wp_bain_14_utm_ppp_10per <- rpoint (bain_pop_10per, f = wp_bain_14_utm_im, win = bain_utm_win)
    wp_bain_14_utm_ppp_5per <- rpoint (bain_pop_5per, f = wp_bain_14_utm_im, win = bain_utm_win)
    wp_bain_14_utm_ppp_1per <- rpoint (bain_pop_1per, f = wp_bain_14_utm_im, win = bain_utm_win)
    
    quartz()
    par (mfrow = c (2, 3))
    plot (wp_bain_14_utm_ppp, cex = .03)
    plot (wp_bain_14_utm_ppp_50per, cex = .03)
    plot (wp_bain_14_utm_ppp_33per, cex = .03)
    plot (wp_bain_14_utm_ppp_10per, cex = .03)
    plot (wp_bain_14_utm_ppp_5per, cex = .03)
    plot (wp_bain_14_utm_ppp_1per, cex = .03)
    
    wp_bain_14_utm_df <- as.data.frame (wp_bain_14_utm_ppp)
    wp_bain_14_utm_df$id <- seq (1:nrow(wp_bain_14_utm_df))
    wp_bain_14_utm_df$weight <- 1
    
    wp_bain_14_utm_df_50per <- as.data.frame (wp_bain_14_utm_ppp_50per)
    wp_bain_14_utm_df_50per$id <- seq (1:nrow(wp_bain_14_utm_df_50per))
    wp_bain_14_utm_df_50per$weight <- 2
    
    wp_bain_14_utm_df_33per <- as.data.frame (wp_bain_14_utm_ppp_33per)
    wp_bain_14_utm_df_33per$id <- seq (1:nrow(wp_bain_14_utm_df_33per))
    wp_bain_14_utm_df_33per$weight <- 3
    
    wp_bain_14_utm_df_10per <- as.data.frame (wp_bain_14_utm_ppp_10per)
    wp_bain_14_utm_df_10per$id <- seq (1:nrow(wp_bain_14_utm_df_10per))
    wp_bain_14_utm_df_10per$weight <- 10
    
    wp_bain_14_utm_df_5per <- as.data.frame (wp_bain_14_utm_ppp_5per)
    wp_bain_14_utm_df_5per$id <- seq (1:nrow(wp_bain_14_utm_df_5per))
    wp_bain_14_utm_df_5per$weight <- 50
    
    wp_bain_14_utm_df_1per <- as.data.frame (wp_bain_14_utm_ppp_1per)
    wp_bain_14_utm_df_1per$id <- seq (1:nrow(wp_bain_14_utm_df_1per))
    wp_bain_14_utm_df_1per$weight <- 100
    
    