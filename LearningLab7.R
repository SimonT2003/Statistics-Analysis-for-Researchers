# Module 4.2
library(openintro)
library(dplyr)
library(ggplot2)
library(e1071)

View(mammals)
plot(mammals$body_wt, mammals$brain_wt, xlab="Body Weight", ylab="Brain Weight")

# Investigate a transformation on y
hist(mammals$brain_wt, xlab="Brain Weight (kg)", main="Histogram")
hist(log(mammals$brain_wt), xlab="ln(Brain Weight (kg))", main="Histogram") 
plot(mammals$body_wt, log(mammals$brain_wt), xlab="body weight", ylab="ln(brain weight)")

# Investigate a transformation on x
hist(mammals$body_wt, xlab="Body Weight (kg)", main="Histogram")
hist(log(mammals$body_wt), xlab="ln(Body Weight (kg))", main="Histogram")
plot(mammals$body_wt, log(mammals$brain_wt), xlab="body weight", ylab="ln(brain weight)")

plot(log(mammals$body_wt), log(mammals$brain_wt), xlab="ln(Body Weight (kg))", ylab="ln(Brain Weight (kg))")

cor(log(mammals$body_wt), log(mammals$brain_wt))
reg1=lm(log(mammals$brain_wt)~log(mammals$body_wt))
abline(reg1) # add the line to the plot
summary(reg1)

help(mammals)
