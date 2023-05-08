library(mosaic)
View(SaratogaHouses)

reg1=lm(SaratogaHouses$price~SaratogaHouses$livingArea)
summary(reg1)


reg2=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$waterfront)
summary(reg2)
ggplot(data = SaratogaHouses, aes(x = livingArea, y = price, color = waterfront)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Living Area sq ft",
       y = "Price $")



levels(SaratogaHouses$heating)
reg3=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$heating)
summary(reg3)
ggplot(data = SaratogaHouses, aes(x = livingArea, y = price, color = heating)) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Living Area sq ft",
       y = "Price $")



levels(SaratogaHouses$sewer)
reg4=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$sewer)
summary(reg4)
ggplot(data = SaratogaHouses, aes(x = livingArea, y = price, color = sewer)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Living Area sq ft",
       y = "Price $")

################################################################################

# All comparisons 
library(emmeans)
emm1 = emmeans(reg3, specs = pairwise ~ heating)
summary(emm1) # this is another way to compare the three types of heating


# Post Learning Lab

#checks for interaction between living area and waterfront
reg5=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$waterfront+ SaratogaHouses$livingArea*
          SaratogaHouses$waterfront)
summary(reg5) # There is no interaction between living area and waterfront b/c p-value = 0.43077 > 0.05

#Model we want b/c interaction not significant
reg2=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$waterfront)
summary(reg2)

#plot using same x -values and predicted price from reg2
dd_m = data.frame(livingarea=SaratogaHouses$livingArea, price=predict(reg2, SaratogaHouses), waterfront=SaratogaHouses$waterfront)
View(dd_m)
ggplot(SaratogaHouses) + geom_point(aes(livingArea, price, colour=waterfront))+ 
  geom_line(data=dd_m, aes(livingarea, price, colour=SaratogaHouses$waterfront))




#checks for interaction between living area and newConstruction
reg6=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$newConstruction+ SaratogaHouses$livingArea*
          SaratogaHouses$newConstruction)
summary(reg6) # There is no interaction between living area and waterfront b/c p-value = 0.43077 > 0.05

#Model we want b/c interaction not significant
reg7=lm(SaratogaHouses$price~SaratogaHouses$livingArea+SaratogaHouses$newConstruction)
summary(reg7)

#plot using same x -values and predicted price from reg2
dd_m1 = data.frame(livingarea=SaratogaHouses$livingArea, price=predict(reg7, SaratogaHouses), newConstruction=SaratogaHouses$newConstruction)
View(dd_m1)
ggplot(SaratogaHouses) + geom_point(aes(livingArea, price, colour=newConstruction))+ 
  geom_line(data=dd_m, aes(livingarea, price, colour=SaratogaHouses$newConstruction))


# End of Code