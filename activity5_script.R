library(ggplot2)

#read in weather data
datW <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a05\\noaa2011124.csv")
#make name column factor
datW$NAME <- as.factor(datW$NAME)
#set up a vector of all names for each level
nameS <- levels(datW$NAME)
nameS

#make a dataframe with just precipitation, year, and site name
#remove NA using na.omit
datP <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           PRCP=datW$PRCP))
#total annual precipitation (mm)
precip <- aggregate(datW$PRCP, by=list(datW$NAME, datW$year), FUN="sum", na.rm=TRUE)
#use aggregate to get total annual precipitation
precip <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="sum", na.rm=TRUE)
#rename columns
colnames(precip) <- c("NAME", "year", "totalP")
#add the x column from aggregate looking at the length of observations in each year
precip$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="length")$x

#make a new dataframe
pr <- precip[precip$ncount >=364, ]

#look at only livermore california and morrisville new york precipitation
ca <- pr[pr$NAME == nameS[2], ]
ny <- pr[pr$NAME == nameS[5], ]


#make a plot of california precip
plot(ca$year, ca$totalP)

#make a plot of california precip
plot(ca$year, ca$totalP,
     type = "b",
     pch = 19,
     ylab = "Annual precipitation (mm)",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(0, 1600))
#add y axis
axis(2, seq(0,1600, by=400), las=2 )
#add arizona
points(ny$year, ny$totalP,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend

legend("topleft", #position
       c("California", "New York"), #labels
       col= c("black", "tomato3"), #colors
       pch=19, #point shape
       lwd=1, #line thickness 1, anytime both point & line arguments are given both will be drawn
       bty="n") #always use this argument otherwise an ugly box is drawn

datT <- na.omit(data.frame(NAME=datW$NAME,
                           year=datW$year,
                           TMAX=datW$TMAX))

#use aggregate to get total annual tmax
temp <- aggregate(datT$TMAX, by=list(datT$NAME, datT$year), FUN="max", na.rm=TRUE)
#rename columns
colnames(temp) <- c("NAME", "year", "totalT")
#add the x column from aggregate looking at the length of observations in each year
temp$ncount <- aggregate(datT$TMAX, by=list(datT$NAME, datT$year), FUN="length")$x

#make a new dataframe
tm <- temp[temp$ncount >=364, ]

nyt <- tm[tm$NAME == nameS[5], ]
ndt <- tm[tm$NAME == nameS[3], ]

#make a plot of NY Maximum Temp
plot(nyt$year, nyt$totalT,
     type = "b",
     pch = 19,
     ylab = "Annual Maximum Temperature",
     xlab = "Year", 
     yaxt = "n",
     ylim =c(25, 50),
     main = "Annual Maximum Temperature for Morrisville, NY and Mandan, ND")
#add y axis
axis(2, seq(25, 50, by=5), las=2 )
#add ND
points(ndt$year, ndt$totalT,
       type = "b",
       pch = 19,
       col="tomato3")
#add legend
legend("topright", 
       c("Morrisville, NY", "Mandan, ND"), #labels
       col= c("black", "tomato3"), 
       pch=19, 
       lwd=1, 
       bty="n")

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+ #data for plot
        geom_point()+ #make points at data point
        geom_path()+ # use lines to connect data points
        labs(x="year", y="Annual Precipitation")+ #make axis labels
        theme_classic() #change plot theme

ggplot(data = pr, aes(x = year, y=totalP, color=NAME ) )+
        geom_point(alpha=0.5)+
        geom_path(alpha=0.5)+
        labs(x="year", y="Annual Precipitation")+
        theme_classic()+
        scale_color_manual(values = c("#7FB3D5","#CB08E9", "#E7B800", "#027044","#AF0A0A"))

ggplot(data = datW, aes(x=NAME, y=TMIN))+ #look at daily tmin
        geom_violin(fill=rgb(0.933,0.953,0.98))+ #add a violin plot with blue color
        geom_boxplot(width=0.2,size=0.25, fill="grey90")+ #add grey boxplots and make them about 20% smaller than normal with 25% thinner lines than normal
        theme_classic() #git rid of ugly gridlines

sub <- datW[datW$NAME == nameS[4] & datW$year == 1974,]

#specify date format
#%Y means a four number year
#- indicates that the data uses dashes to separate
#%m means month
#%d means day
sub$DATE <- as.Date(sub$DATE, "%Y-%m-%d")

ggplot(data=sub, aes(x=DATE, y=TMAX))+
        geom_point()+
        geom_path()+
        theme_classic()+
        labs(x="year", y="Maximimum temperature (C)")

ggplot(data=sub, aes(x=DATE, y=PRCP))+
        geom_col(fill="royalblue3")+
        theme_classic()+
        labs(x="year", y="Daily precipitation (mm)")

sub2 <- datW[datW$NAME == nameS[2] & datW$year == 1974,]

#specify date format
sub2$DATE <- as.Date(sub$DATE, "%Y-%m-%d")

ggplot(data=sub2, aes(x=DATE, y=TMAX))+
        geom_point()+
        geom_path()+
        theme_classic()+
        labs(x="year", y="Maximimum temperature (C)")

ggplot(data=sub2, aes(x=DATE, y=PRCP))+
        geom_col(fill="royalblue3")+
        theme_classic()+
        labs(x="year", y="Daily precipitation (mm)")

datA <- data.frame(NAME=datW$NAME,
                   year=datW$year,
                   DATE=datW$DATE,
                   TMIN=datW$TMIN)
datL <- datA[datA$NAME == nameS[2] & datA$year >= 2000, ]

#specify date format
datL$DATE <- as.Date(datL$DATE, "%Y-%m-%d")

ggplot(data=datL, aes(x=DATE, y=TMIN))+
        geom_col(fill="royalblue3")+
        theme_classic()+
        labs(x="Date", y="Minimum Temperature (C)")+
        ggtitle("Liverpool, CA Minimum Temperature (C) between 2000 and 2019")
