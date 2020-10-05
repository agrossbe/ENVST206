#install.packages("ggplot2")
library(ggplot2)

datW <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a05\\noaa2011124.csv")
datW$NAME <- as.factor(datW$NAME)

#create vector of names
nameS <- levels(datW$NAME)
nameS

#make dataframe
datP <- na.omit(data.frame(PRCP = datW$PRCP,
                  NAME = datW$NAME,
                  year = datW$year))

#total annual precipitation for each site
pr <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="sum")
colnames(pr) <- c("NAME", "year", "totalP")
pr$ncount <- aggregate(datP$PRCP, by=list(datP$NAME, datP$year), FUN="length")$x

pr <- pr[pr$ncount >= 364, ]

#base r plot
plot(pr$year, pr$totalP)

ggplot(data = pr,
       aes(x =year,
           y = totalP,
           color = NAME))+
  geom_point()+
  geom_path()+
  labs(x= "year", y= "Annual precipitation (mm)")+
  theme_classic()