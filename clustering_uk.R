setwd("C:/Santosh")
data<-read.csv("uklatlong.csv")

library(sp)
library(rgdal)
library(geosphere)
library(dismo)
library(rgeos)
library(ggplot2)
library(ggmap)

# example data from the thread


x<-c(data$lng)
y<-c(data$lat)

# convert data to a SpatialPointsDataFrame object
xy <- SpatialPointsDataFrame(
  matrix(c(x,y), ncol=2), data.frame(ID=seq(1:length(x))),
  proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(xy)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")

# define the distance threshold, in this case 40 m
d=3000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
xy$clust <- cutree(hc, h=d)



dummy<-data.frame(xy)
write.csv(dummy,"clusters.csv")

# expand the extent of plotting frame
xy@bbox[] <- as.matrix(extend(extent(xy),0.001))

# get the centroid coords for each cluster
cent <- matrix(ncol=2, nrow=max(xy$clust))
for (i in 1:max(xy$clust))
  # gCentroid from the rgeos package
  cent[i,] <- gCentroid(subset(xy, clust == i))@coords

# compute circles around the centroid coords using a 40m radius
# from the dismo package
ci <- circles(cent, d=d, lonlat=T)

plot(ci@polygons, axes=T)
plot(xy, col=rainbow(4)[factor(xy$clust)], add=T)

