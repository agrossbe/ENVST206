#activity 3
ch4 <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a03\\lemming_herbivory.csv")

ch4$herbivory <- as.factor(ch4$herbivory)

plot(ch4$CH4_Flux ~ ch4$herbivory, xlab ="Treatment", 
     ylab="CH4 Fluxes (mgC m -2 day-1) ")

#use the shapiro wilks test to look for normality in each treatment
#shapiro-wilk test on grazing plots
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ctl"])

#shapiro-wilk test on grazing exclusion plots
#test if normally distributed
shapiro.test(ch4$CH4_Flux[ch4$herbivory == "Ex"])

#use bartlett test since testing for equal variance
bartlett.test(ch4$CH4_Flux ~ ch4$herbivory)

#can now conduct t-test because data meets assumptions of being normally distributed and having equal variances
t.test(ch4$CH4_Flux ~ ch4$herbivory)

#read in insect data
datI <- read.csv("C:\\Users\\Hephzibah\\Documents\\EnvDS\\a03\\insect_richness.csv")

datI$urbanName <- as.factor(datI$urbanName)

levels(datI$urbanName)

#test if urbanName groups are normally distributed
shapiro.test(datI$Richness[datI$urbanName == "Dense"])
shapiro.test(datI$Richness[datI$urbanName == "Developed"])
shapiro.test(datI$Richness[datI$urbanName == "Natural"])
shapiro.test(datI$Richness[datI$urbanName == "Suburban"])

#test for equal variance
bartlett.test(datI$Richness ~ datI$urbanName)

#specify model for species richness and urban type
in.mod <- lm(datI$Richness ~ datI$urbanName)
#run the ANOVA
in.aov <- aov(in.mod)
#print out ANOVA table
summary(in.aov)

#run Tukey HSD
tukeyT <- TukeyHSD(in.aov)
#view results
tukeyT

#make a plot
#make axes labels smaller than usual to fit on plot using cex.axis 
plot(tukeyT, cex.axis=0.75)

#calculates means for all groups
tapply(datI$Richness, datI$urbanName, "mean")

#set up contigency table
species <- matrix(c(18,8,15,32), ncol=2, byrow = TRUE) 
colnames(species) <- c("Not protected", "Protected")
rownames(species) <- c("Declining", "Stable/Increase")
species

#make a mosaic plot with an informative title and axes labels
mosaicplot(species, xlab="population status", ylab="legal protection",
           main="Legal protection impacts on populations")

#Conduct a chi-squared test
chisq.test(species)
