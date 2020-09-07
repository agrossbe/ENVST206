#activity 2

heights <- c(3, 2, 3)

#read in weather station file from data folder
datW <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\Data\\a02\\noaa2011124.csv")

#make new section in excel data that divides PRCP values by 10
datW$PRCP_cm <- datW$PRCP/10

#na.rm=TRUE removed all data that can't use for mean
mean(datW$PRCP_cm, na.rm=TRUE)

#subseting
#makes dataset of PRCP from 1930
yr1930 <- datW$PRCP_cm[datW$year == 1930]
#makes data set of PRCP for years less than or equal to 1950
yrs <- datW$PRCP_cm[datW$year <= 1950]

#histogram of max temp for 1930 at Morrisville
hist(datW$TMAX[datW$year == 1930 & datW$NAME == "MORRISVILLE 6 SW, NY US"],
     freq = FALSE,
     main = paste("Morrisville Maximum Temperature from 1930"),
     xlab = "Temperature (degrees C)",
     ylab = "Relative Frequency",
     col = "grey75",
     border = "white")
