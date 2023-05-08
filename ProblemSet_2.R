# Problem Set 2 - Simon Tran
library(mosaic)
library(openintro)
library(dplyr)
library(ggplot2)
library("ggpubr")

## 1. Chapter 10 - Problem 37
motorNoise <- read.csv('https://raw.githubusercontent.com/jenbroatch/STP281/main/bookex1037.csv')
motorNoise <- motorNoise %>% select('Brand','vibration..microns.')
View(motorNoise)
str(motorNoise)

motorNoise$Brand <- factor(motorNoise$Brand, 
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("B1", "B2", "B3", "B4","B5")) # makes R think this is a factor, not an integer
head(motorNoise)

motorNoise_sum <- motorNoise %>%    # Get mean & standard deviation by group
  group_by(Brand) %>%
  summarise_at(vars(vibration..microns.),
               list(mean = mean,
                    sd = sd)) %>% 
  as.data.frame()
View(motorNoise_sum)   # View 

ggplot(motorNoise_sum,         # ggplot2 plot with means & standard deviation
       aes(x = Brand,
           y = mean)) + 
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd)) +
  geom_point() + labs(y = "The amount of motor vibration (measured in microns)", x = "Motor Bearing Brands")

anovaResult = aov(vibration..microns.~Brand, data=motorNoise) # aov function is for single-factor ANOVA
summary(anovaResult)

# Comparing each Group
TukeyHSD(anovaResult, conf.level=.95) 
plot(TukeyHSD(anovaResult, conf.level=.95), las = 2)



## 2. Chapter 11 - Problem 56
spruce_seedlings <- read.csv('https://raw.githubusercontent.com/jenbroatch/STP281/main/bookex1156.csv')
head(spruce_seedlings)
str(spruce_seedlings)

ggline(spruce_seedlings, x = "Health", y = "Response", color = "pH",
       add = c("mean_se"),
       palette = c("Red", "Blue", "Dark Green"))

#conduct the ANOVA with interaction
seedlings_mod <- aov(Response ~ Health * pH, data = spruce_seedlings)
summary(seedlings_mod)

#conduct the ANOVA - additive model 
seedlings_mod_add <- aov(Response ~ Health + pH, data = spruce_seedlings)
summary(seedlings_mod_add)

TukeyHSD(seedlings_mod_add)



## 3. 
View(SaratogaHouses)
plot(SaratogaHouses$livingArea, SaratogaHouses$price, 
     xlab="Living Area (square feet)", ylab="Price of a Home (US dollars)")
cor(SaratogaHouses$livingArea, SaratogaHouses$price)

reg1=lm(SaratogaHouses$price~SaratogaHouses$livingArea)
abline(reg1)
summary(reg1)

reg2=lm(price~livingArea, data=SaratogaHouses)
SaratogaHouses.new = data.frame(livingArea=c(3000))
predict(reg2, SaratogaHouses.new, interval="confidence")


#residuals
res <- resid(reg1)
#produce residual vs. fitted plot
plot(fitted(reg1), res, xlab="Predicted Value of y- price", ylab='Residual')
#add a horizontal line at 0 
abline(0,0)

#produce residual vs. x
plot(SaratogaHouses$livingArea, res, xlab="Living Area (sq. ft.)", ylab='Residual')
#add a horizontal line at 0 
abline(0,0)

plot(reg1)


# End of code.