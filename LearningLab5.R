library(dplyr)
library(ggplot2)

lion=read.csv(url("http://www.zoology.ubc.ca/~schluter/WhitlockSchluter/wp-content/data/chapter17/chap17e1LionNoses.csv"))
head(lion)
View(lion)
lion2=lion %>% mutate(percentblack = proportionBlack*100)
plot(lion2$percentblack,lion2$ageInYears, xlab="Percentages of Black Pigment on Lion Nose", ylab="Age in Years")

cor(lion2$percentblack,lion2$ageInYears) # calculates the correlation coefficient

reg1=lm(lion2$ageInYears~lion2$percentblack)
plot(lion2$percentblack,lion2$ageInYears, xlab="Percentages of Black Pigment on Lion Nose", ylab="Age in Years")
abline(reg1) # add the line to the plot
summary(reg1)

# Create a new data set with new observation
reg2 = lm(ageInYears~percentblack, data=lion2)
lion.new = data.frame(percentblack=c(50, 43))
predict(reg2, lion.new, interval="confidence")
predict(reg2, lion.new, interval="prediction")



p1 <- lion2 %>% 
  ggplot(aes(x = percentblack, y = ageInYears)) +
  geom_point(colour = "black") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "95% Confidence Interval") +
  theme_bw() +
  theme(plot.title = element_text(face = "bold",hjust = 0.5))

p1

ci95 <- predict(reg1, lion2, interval = "confidence", level = 0.95)
View(ci95)


final_data <- bind_cols(lion2, ci95)
View(final_data)




temp_var <- predict(reg2, interval="prediction")

new_df <- cbind(lion2, temp_var)

ggplot(new_df, aes(percentblack, ageInYears))+
  geom_point() +
  geom_line(aes(y=lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y=upr), color = "red", linetype = "dashed")+
  geom_smooth(method=lm, se=TRUE)


# End of code.