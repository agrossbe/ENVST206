#read in lemming data
ch4 <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a03\\lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)

#plot with CH4_Flux as independent and herbivory as dependent
plot(ch4$CH4_Flux ~ ch4$herbivory)

#Shapiro-Wilk normality test
#results of the below tests yield p-values < 0.05 so null hypotheses NOT rejected
#both are normally distributed
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#is variation in control different from variation in experimental group
#null hyp. is that variances are not different
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

t.test(ch4$CH4_Flux ~ ch4$herbivory)
