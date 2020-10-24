jc <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\Historic_rates_Jamaica.csv")

jc$Daily.Exchange.Rates <- as.Date(jc$Daily.Exchange.Rates,"%m/%d/%Y")

jc_use <- na.omit(data.frame(DATE=jc$Daily.Exchange.Rates,
                             VALUE=jc$X.2))

plot(jc_use$DATE, jc_use$VALUE,
     pch=19,
     col = "royalblue",
     xlab="Year",
     ylab="Value of Jamaican Dollar (USD)",
     main="Echange Rate of Jamaican Dollar from 1971-2020")

#annual_rate <- aggregate(jc_use$VALUE, by=list(jc_use$DATE), FUN="mean", na.rm=TRUE)