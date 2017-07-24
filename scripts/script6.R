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

# CLUSTERING FUNCTIONS ------


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
             axes = TRUE, layout = l, xlim = c(floor (min (l[,1] - 1)), floor (max (l[,1] + 1))), ylim = c(floor (min (l[,2] - 1)), floor (max (l[,2] + 1))), rescale = FALSE, 
             mark.groups = links, mark.shape = 1)
      }
      return (links)
    }
  
  
