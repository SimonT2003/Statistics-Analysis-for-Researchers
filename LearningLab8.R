library(dplyr)
NoiseLevel_data <- read.csv('https://raw.githubusercontent.com/jenbroatch/STP281/main/bookex1325.csv')

mylogit <- glm(Acceptable ~ NoiseLevel, data = NoiseLevel_data, family = "binomial")
summary(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

#define new data frame that contains predictor variable
newdata <- data.frame(NoiseLevel=seq(min(NoiseLevel_data$NoiseLevel), max(NoiseLevel_data$NoiseLevel),len=500))

newdata$Acceptable = predict(mylogit, newdata, type="response")

#plot logistic regression curve
plot(Acceptable ~ NoiseLevel, data = NoiseLevel_data, col="steelblue")
lines(Acceptable ~ NoiseLevel, newdata, lwd=2)

newdata1=data.frame(NoiseLevel=c(60))
newdata1$Acceptable = predict(mylogit, newdata1, type="response")
newdata1$Acceptable

