## This is a function to determine the vertices
## of any ploygon.  Currently it only works for 
## regular polygons.  This is useful in conjuction
## with the randPoints2 function.

rm(list=ls())

shape.Polygon<-function(n,s,point=c(0,0),type=c("reg","irreg")){ ## n=# of sides, s= side length (must be vector if type=="irreg");point= center point or statrting point
  if (type=="reg"){
    r=sqrt(((1/2)*s)^2+(s/(2*tan(pi/n)))^2)
    k=seq(1,n)
    x<-point[1] + r*sin(pi/n + k*2*pi/n)
    x<-c(x,x[1])
    y<-point[2] - r*cos(pi/n + k*2*pi/n)
    y<-c(y,y[1])
    shp<-data.frame(x,y)
  } else if (type=="irreg"){
    a<-(n-2)*pi
    angle<-vector(length = n)
    angle<-sapply(angle,function(x) runif(1,max=(a+.000000001),min=0))
    x1<-point[1]
    y1<-point[2]
    x<-s*cos(angle)
    y<-s*sin(angle)
    x<-c(x,x[,1])
    y<-c(y,y[1])
    shp<-data.frame(x,y)
  }
} 

m<-shape.Polygon(4,1,point=c(.5,.5),type="reg")
d<-randPoints2(m,1000,"polygon")
d

shape.circle <- function(center = c(a,b),r){
  tt <- seq(0,2*pi,length.out = 1000)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  shp<-data.frame(x = xx, y = yy)
}

c<-shape.circle(center = c(0,0),5)
d<-randPoints2(c,5000,"polygon")
d