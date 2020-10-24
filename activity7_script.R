#package for vector data
library(sp)
#package for reading in spacial data
library(rgdal)
#data management package
library(dplyr)

si2019 <- readOGR("C:\\Users\\Hephzibah\\Documents\\EnvDS\\sea_ice\\extent_N_201909_polygon_v3.0\\extent_N_201909_polygon_v3.0.shp")
si1979 <- readOGR("C:\\Users\\Hephzibah\\Documents\\EnvDS\\sea_ice\\extent_N_197909_polygon_v3.0\\extent_N_197909_polygon_v3.0.shp")
seaiceAll <- readOGR("C:\\Users\\Hephzibah\\Documents\\EnvDS\\sea_ice_all\\sea_ice_all.shp")

#for loop to find areas of all years (don't have this fully working yet)
areas <- numeric()
for(i in 1:3986){
  sum(area(seaiceAll[seaiceAll@data$year == i,]))
}

#for loop to generate areas of the polygons for 1979
areas1979 <- numeric()
for(i in 1:109){
  areas1979[i] <- si1979@polygons[[i]]@area
}
#find sum of the areas of the polygons for 1979
sum1979 <- sum(areas1979, na.ra=FALSE)

#for loop to generate areas of the polygons for 2019
areas2019 <- numeric()
for(i in 1:83){
  areas2019[i] <- si2019@polygons[[i]]@Polygons[[1]]@area
}
#find sum of the areas of the polygons for 2019
sum2019 <- sum(areas2019, na.ra=FALSE)

#calculate difference in areas
area_diff <- sum1979 - sum2019

#map of extent for 1979 and 2019
plot(si1979, main = "Arctic Sea Ice Extent in 1979 and 2019", col = "tomato3")
plot(si2019, col = "lightblue2", add=TRUE)

legend("topright", 
       c("1979 Extent", "2019 Extent"),
       col= c("tomato3", "lightblue2"), 
       pch=19, 
       lwd=1, 
       bty="n")