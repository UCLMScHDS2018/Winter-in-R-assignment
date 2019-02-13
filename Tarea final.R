rm(list = ls()) # Erases all objects

setwd("D:/Respaldo disco c 2/Trabajo INIA/Pruebas R") 

library(agricolae) # Loading the library

data(cotton) # Cotton data is loaded as cotton dataset

data(soil) # Soil data is loaded as soil dataset

#Shapiro-Wilks for Normality

shapiro.test(cotton$yield) #p.value barely below 0.05 => values aren't normaly distributed

#Bartlett for Variance Homogeneity

bartlett.test(cotton$yield, cotton$epoca) #p-value > 0.05 No difference between variances
bartlett.test(cotton$yield, cotton$block) #p-value > 0.05 No difference between variances
bartlett.test(cotton$yield, cotton$lineage) #p-value > 0.05 No difference between variances
#bartlett.test(cotton$yield, cotton$lineage+site) => ERROR

#Running ANOVA 


aovblock <- aov(yield~block, data = cotton)
summary(aovblock) # No difference according to block
testblock <- HSD.test(aovblock, "block")
testblock # no significance

aovlineage <- aov(yield~lineage, data = cotton)
summary(aovlineage) #there is a difference according to lineage
testlineage <- HSD.test(aovlineage, "lineage")
testlineage # no significance observed

aovepoca <- aov(yield~epoca, data = cotton)
summary(aovepoca) # there is a difference according to season (epoca = season)
testepoca <- HSD.test(aovepoca, "epoca")
testepoca # there is significance according to season

#aovLyE <- aov(yield ~ lineage + epoca, data = cotton)
#summary(aovLyE) #there is a difference according to additive model of "lineage + season"
#testLyE <- HSD.test(aovLyE, "lineage" + "epoca")
#testLyE # Why NULL?

aovLyE <- aov(yield ~ lineage + epoca, data = cotton)
summary(aovLyE) #there is a difference according to additive model of "lineage + season"
testLyE1 <- HSD.test(aovLyE, "lineage")
testLyE1 #There is significance between 1 and 4

aovLyE <- aov(yield ~ lineage + epoca, data = cotton)
summary(aovLyE) #there is a difference according to additive model of "lineage + season"
testLyE2 <- HSD.test(aovLyE,"epoca")
testLyE2 #there is significance between 2 and 4

############################################################

cor(soil[,2:ncol(soil)]) # Indicates correlation above 0.6 for clay-pH and clay-Ca

# CLAY ~ PH
modclayPH <- lm(clay~pH, data=soil)
modclayPH
summary(modclayPH)
resid.clayPH <- resid(modclayPH)

par(mfrow=c(2,1)) #Only needs to be executed once 
plot(soil$pH,soil$clay, main = "Clay v/s pH", ylab = "Clay", xlab = "pH")
abline(modclayPH,col="red",lty=2)
plot(soil$pH, resid.clayPH, main = "Errores de estimación", ylab = "Clay", xlab = "pH") #

# CLAY ~ CA
modclayCA <- lm(clay~Ca, data=soil)
modclayCA
summary(modclayCA)
resid.clayCA<- resid(modclayCA)

plot(soil$Ca,soil$clay, main = "Clay v/s Calcio", ylab = "Clay", xlab = "Calcio")
abline(modclayPH,col="red",lty=2)
plot(soil$pH, resid.clayCA, main = "Errores de estimación", ylab = "Clay", xlab = "Calcio")

#############################################################

pp <- read.csv("Pp_IVRegión.csv", header = T)

dias_lluviosos <- apply(pp[,4:ncol(pp)],2,function(x)length(which(x>0)))
max_dias <- max(dias_lluviosos) # it works but says 300 and it doesn't print the name of the Río Choapa in Cuncumen station where there were more rain events
#max_dias <- function(y){y == max(dias_lluviosos)} # 
max_dias