#install.packages("sp","rgdal","dplyr")
library(sp)
library(rgdal)
library(dplyr)

g1966 <- readOGR("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a06\\GNPglaciers\\GNPglaciers_1966.shp")

plot(g1966, col="skyblue")

g1966@data$GLACNAME

g1966@data$Area1966
g2015@data$Area2015

plot(g1966, col="skyblue")
plot(g2015, col="tomato3", add=TRUE)

exp1 <- data.frame(NAME=as.factor(c("a", "b", "c")),
                   per.change = c(50, 40, 65))
exp2 <- data.frame(NAME=as.factor(c("a", "b", "c")),
                   mass.gt = c(70, 8, 10, 30))

glac <- full_join(exp1, exp2, by="NAME")
View(glac)
