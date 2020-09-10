#activity 2

#read in weather station file from the data folder
datW <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\Data\\a02\\noaa2011124.csv")

#get more info about the data frame
str(datW)

#reassign NAME column as factor data
datW$NAME <- as.factor(datW$NAME)

#vector of numeric data
num <- matrix(c(11.5, 4, 17.2, 13, 56), ncol=5, byrow=FALSE)
num

#vector of character data
cha <- matrix(c("color", "shape", "orientation", "size", "material"), nrow=5)
cha

#vector of factor data
fac <- as.factor(matrix(c("site 1", "site 2", "site 3", "site 4", "site 5"), nrow=5))
fac

#vector of integer data
int <- as.integer(matrix(c(13, 72, 11, 54, 62), nrow=5))
int

#find out all unique site names
levels(datW$NAME)

#mean maximum temperature for Aberdeen
#with na.rm argument set to true to ignore NA
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#standard deviation for above
sd(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#average daily temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX - datW$TMIN)/2)

#get the mean across all sites
#by function is a list of one or more variables to index over
#FUN indicates the function we want to use
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
averageTemp
#change automatic output of column names to be more meaningful
#MAAT = Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME", "MAAT")
averageTemp

#convert level to number for factor data type
#will need to reference the level output or look at the row of data to see the
#character designation
datW$siteN <- as.numeric(datW$NAME)

#make a histogram for the first site in our levels
#main = is the title name argument
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful
hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE,
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey75",
     border = "white")
#can't get the below function to work. Don't fully know what I'm doing with it
#plot(curve(dnorm(datW$TAVE[datW$siteN == 2], mean = 0, sd = 1, log = FALSE)))

#pnorm(value to evaluate at (note this will evaluate for all values and below),mean, standard deviation)
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#qnorm gives me the value at which all values and below equal the probability in my argument
#Here I'm calculating the value of the 95th quantile or a probability of 0.95
qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

#mean of Aberdeen
Aberdeen.mean <- mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE)
Aberdeen.mean

#frequency at which there are temperatures over the threshold for extreme temp
1 - pnorm(18.5,
          mean = Aberdeen.mean + 4,
          sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

#histogram of Aberdeen, WA precipitation
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Daily precipitation",
     ylab = "Relative frequency",
     col = "grey75",
     border = "white")

#aggregated annual precipitation
totalPrcp <- aggregate(datW$PRCP, by=list(datW$siteN, datW$year), FUN="sum", na.rm=TRUE)
colnames(totalPrcp) <- c("NAME", "YEAR", "PRCP")
totalPrcp

#make NAME in totalPrcp factor data
totalPrcp$siteN <- as.numeric(totalPrcp$NAME)

#histogram of annual prcp for Aberdeen
hist(totalPrcp$PRCP[totalPrcp$siteN == 1],
     freq=FALSE,
     main = paste(levels(totalPrcp$NAME)[1]),
     xlab = "Total Annual Precipitation (mm)",
     ylab = "Relative Frequency",
     col = "grey75",
     border = "white")

#histogram of annual prcp for Mandan
hist(totalPrcp$PRCP[totalPrcp$siteN == 3],
     freq=FALSE,
     main = paste(levels(totalPrcp$NAME)[3]),
     xlab = "Total Annual Precipitation (mm)",
     ylab = "Relative Frequency",
     col = "grey75",
     border = "white")

#likelihood of a year with 700mm or less in Aberdeen
pnorm(700,
      mean(totalPrcp$PRCP[totalPrcp$siteN == 1],na.rm=TRUE),
      sd(totalPrcp$PRCP[totalPrcp$siteN == 1],na.rm=TRUE))

#likelihood of a year with 700mm or less in Madan
pnorm(700,
      mean(totalPrcp$PRCP[totalPrcp$siteN == 3],na.rm=TRUE),
      sd(totalPrcp$PRCP[totalPrcp$siteN == 3],na.rm=TRUE))
