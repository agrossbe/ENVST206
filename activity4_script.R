datB <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a04\\beaver_dam.csv")
head(datB)

plot(datB$dams.n, datB$area.ha,
     pch = 19,
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab = "Number of deaver dams")

#set up regression
dam.mod <- lm(datB$area.ha ~ datB$dams.n)
#get standardized residuals
dam.res <- rstandard(dam.mod)

#set up qq plot
qqnorm(dam.res)
#add qq line
qqline(dam.res)

shapiro.test(dam.res)

#make a residual plot
plot(datB$dams.n, dam.res,
     xlab = "beaver dams",
     ylab = "standardized residual")
#add a horizontal line to zero
abline(h=0)

summary(dam.mod)

#make plot of beaver dams and surface water
plot(datB$dams.n, datB$area.ha,
     pch = 19,
     col = "royalblue4",
     ylab = "Surface water area (ha)",
     xlab = "Number of beaver dams")
#add regression line
#make line thicker
abline(dam.mod, lwd=2)

pheno <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a04\\red_maple_pheno.csv")

#set up panel of plots with one row and two columns
par(mfrow=c(1,2))
plot(pheno$Tmax, pheno$doy,
     pch = 19,
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab = "Maximum temperature (C)")
plot(pheno$Prcp, pheno$doy,
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Precipitation (mm)")

#elevation
par(mfrow=c(2,2))
plot(pheno$elev, pheno$doy,
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Elevation (m)")

#max temp
plot(pheno$Tmax, pheno$doy,
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Maximum temperature (C)")

#urban/rural
plot(pheno$urID, pheno$doy,
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Desciption of site")

#latitude
plot(pheno$Lat, pheno$doy,
     pch = 19, 
     col = "royalblue4",
     ylab = "Day of leaf out",
     xlab =  "Latitude")

plot( ~  pheno$Lat + pheno$Tmax+ pheno$Tmin +pheno$Prcp + pheno$elev + pheno$siteDesc)

pheno$urID <- ifelse(pheno$siteDesc == "Urban",1,0)

#set up residuals
mlr <- lm(pheno$doy ~ pheno$Tmax + pheno$Prcp + pheno$elev + pheno$urID)

#get standardized residuals
mlr.res <- rstandard(mlr)

mlFitted <- fitted(mlr)

dev.off()
qqnorm(mlr.res)
qqline(mlr.res)

shapiro.test(mlr.res)

#make residual plot
plot(mlFitted, mlr.res, 
     xlab = "Fitted residual", 
     ylab = "standardized residual")
#add a horizontal line at zero
abline(h=0)

summary(mlr)
