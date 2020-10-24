library(raster)
library(rgdal)
library(ggplot2)

####unrelated to activity 8
seaiceAll <- readOGR("C:\\Users\\Hephzibah\\Documents\\EnvDS\\sea_ice_all\\sea_ice_all.shp")
head(seaiceAll@data)
plot(seaiceAll)
plot(seaiceAll[seaiceAll@data$year == 1979,])
sum(area(seaiceAll[seaiceAll@data$year == 1979,]))
seaiceAll@proj4string

dirR <- "C:\\\Users\\Hephzibah\\Documents\\EnvDS\\a08\\oneida"
rdatB2 <- raster(paste0(dirR, "\\sentinel\\T18TVN_20190814T154911_B02_20m"))
rdatB3 <- raster(paste0(dirR, "\\sentinel\\T18TVN_20190814T154911_B03_20m"))
rdatB4 <- raster(paste0(dirR, "\\sentinel\\T18TVN_20190814T154911_B04_20m"))
rdatB8 <- raster(paste0(dirR, "\\sentinel\\T18TVN_20190814T154911_B08_20m"))

plot(rdatB2/10000)

rgbS <- stack(rdatB4, rdatB3, rdatB2)/10000

#looks like nothing because we need contrast
#most values are very low so they aren't visible
plotRGB(rgbS, scale=2)

#apply contrast
plotRGB(rgbS, stretch="lin")
help(plotRGB)
#make less pixely
plotRGB(rgbS, stretch="lin", maxpixels=rgbS@ncols*rgbS@nrows)

#can use to look at vegetation
ndvi <- (rdatB8 - rdatB4)/(rdatB8 + rdatB4)
plot(ndvi)
