datB <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a04\\beaver_dam.csv")

plot(datB$dams.n, datB$area.ha, pch = 19)

dam.mod <- lm(datB$area.ha ~ datB$dams.n) 

dam.res <- rstandard(dam.mod)

qqnorm(dam.res)
qqline(dam.res)
shapiro.test(dam.res)

plot(datB$dams.n, dam.res, pch = 19)
abline(h=0)

summary(dam.mod)
