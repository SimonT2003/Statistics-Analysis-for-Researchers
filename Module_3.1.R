#Module 3.1 - Regression and Correlation
gestation = read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/Animals.csv") 
View(gestation)
plot(gestation$Gestational.Period, gestation$Longevity, xlab="Gestational Period (days)", ylab="Longevity (years)")
cor(gestation$Gestational.Period, gestation$Longevity)

plot(gestation$Weight, gestation$Heartrate, xlab="Weight", ylab="Heartrate")
cor(gestation$Weight, gestation$Heartrate) # another example


reg1=lm(gestation$Longevity~gestation$Gestational.Period )
plot(gestation$Gestational.Period, gestation$Longevity, xlab="Gestational Period (days)", ylab="Longevity (years)")
abline(reg1) # add the line to the plot
summary(reg1)


