#Module 4.3
library(dplyr)

oring =read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/orings.csv") 
View(oring)
oring <- oring %>%
  mutate(failure =ifelse(damaged > 0, 1, 0))
View(oring)

plot(oring$temperature, oring$failure, xlab="Temperature at Launch", ylab="At least one oring failure"
)

mylogit <- glm(failure ~ temperature, data = oring, family = "binomial")
summary(mylogit)
exp(cbind(OR = coef(mylogit), confint(mylogit)))

#define new data frame that contains predictor variable
newdata <- data.frame(temperature=seq(min(oring$temperature), max(oring$temperature),len=500))

#use fitted model to predict values of failure
newdata$failure = predict(mylogit, newdata, type="response")

#plot logistic regression curve
plot(failure ~ temperature, data = oring, col="steelblue")
lines(failure ~ temperature, newdata, lwd=2)

#use fitted model to predict values of failure with one value
newdata1=data.frame(temperature=c(31))
newdata1$failure = predict(mylogit, newdata1, type="response")
newdata1$failure
