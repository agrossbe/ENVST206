#activity 2

heights <- c(3, 2, 3)

#read in weather station file from data folder
datW <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\Data\\a02\\noaa2011124.csv")

#make new section in excel data that divides PRCP values by 10
datW$PRCP_cm <- datW$PRCP/10

#na.rm=TRUE removed all data that can't use for mean
mean(datW$PRCP_cm, na.rm=TRUE)
