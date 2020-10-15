#package for vector data
library(sp)
#package for reading in spacial data
library(rgdal)
#data management package
library(dplyr)

#read in shapefile
#readOGR in rgdal does this
#glaciers in 1966
g1966 <- readOGR("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a06\\GNPglaciers\\GNPglaciers_1966.shp")

#glacier in 2015
g2015 <- readOGR("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a06\\GNPglaciers\\GNPglaciers_2015.shp")

str(g2015)

#map the glaciers filled in the polygons with light blue and make the border grey
plot(g1966, col = "lightblue2", border="grey50")

#data stores all accompanying info/measurments for each spatial object
head(g2015@data)

g1966@proj4string

#check glacier names
g1966@data$GLACNAME

g2015@data$GLACNAME

#fix glacier name so that it is consistent with the entire time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                                          ifelse(g2015@data$GLACNAME == "Miche Wabun",
                                                 "Miche Wabun Glacier",
                                                 as.character(g2015@data$GLACNAME)))
#lets combine area, first work with a smaller data frame
gdf66 <- data.frame(GLACNAME = g1966@data$GLACNAME,
                    area66 = g1966@data$Area1966)

gdf15 <- data.frame(GLACNAME = g2015@data$GLACNAME,
                    area15 = g2015@data$Area2015)

#join all data tables by glacier name
gAll <- full_join(gdf66, gdf15, by="GLACNAME")

#calculate the % change in area from 1966 to 2015
gAll$gdiff <- ((gAll$area66-gAll$area15)/gAll$area66)*100

plot(gAll$area66, gAll$gdiff,
     pch=19,
     xlab="Glacial Area 1966 (square km)",
     ylab="Percent Change in Area (square km)",
     main="% Change in Area vs Glacial Area in 1966")

#join data with the spatial data table and overwrite into spatial data table
g1966@data <- left_join(g1966@data, gAll, by="GLACNAME")

#use spplot to shade the polygons based on the % change of labels
#first argument is the spacial object
#second is the column in of data to display with the different colors
#add a title using main
#col changes the color of the borders. This argument sets them to transparent
checkg <- g1966@data
spplot(g1966, "gdiff", main="% change in area", col="transparent")


#look at the Vulture glacier in 1966
vulture66 <- g1966[g1966@data$GLACNAME == "Vulture Glacier",]
plot(vulture66, main = "Vulture Glacier in 1966", col="slategray")

#mean of % loss
gdiff_mean <- mean(gAll$gdiff)

#standard deviation of % loss
gdiff_sd <- sd(gAll$gdiff, na.rm = FALSE)

#max and min % loss in glaciers
gdiff_max <- max(gAll$gdiff, na.rm = FALSE)
gdiff_min <- min(gAll$gdiff, na.rm = FALSE)

#max and min areas in 1966
garea66_max <- max(gAll$area66, na.rm = FALSE)
garea66_min <- min(gAll$area66, na.rm = FALSE)


#look at the Boulder Glacier
boulder66 <- g1966[g1966@data$GLACNAME == "Boulder Glacier",]
boulder15 <- g2015[g2015@data$GLACNAME == "Boulder Glacier",]
plot(boulder66, main = "Boulder Glacial Extent in 1966 and 2015", col="slategray")
plot(boulder15, col="tomato3", add=TRUE)

#add legend
legend("topright", 
       c("1966 Extent", "2015 Extent"),
       col= c("slategray", "tomato3"), 
       pch=19, 
       lwd=1, 
       bty="n")

#look at the Gem Glacier
gem66 <- g1966[g1966@data$GLACNAME == "Gem Glacier",]
gem15 <- g2015[g2015@data$GLACNAME == "Gem Glacier",]
plot(gem66, main = "Gem Glacial Extent in 1966 and 2015", col="slategray")
plot(gem15, col="tomato3", add=TRUE)

#add legend
legend("topright", 
       c("1966 Extent", "2015 Extent"),
       col= c("slategray", "tomato3"), 
       pch=19, 
       lwd=1, 
       bty="n")