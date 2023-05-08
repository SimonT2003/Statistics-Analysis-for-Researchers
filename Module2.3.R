#Module 2.3 - Two-Way ANOVA
hyper<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/hypertension.csv") 
head(hyper)
str(hyper)

# wrap in “as.factor()” OR recode into a factor variable
# Convert drug  as a factor and recode the levels
# as "D1", "D2", "D3"
hyper$Drug <- factor(hyper$Drug, 
                     levels = c(1, 2, 3),
                     labels = c("D1", "D2", "D3")) # makes R think this is a factor, not an integer
head(hyper)
library("ggpubr")

ggboxplot(hyper, x = "Drug", y = "HP", color = "Special.Diet",
          palette = c("Red", "Blue"))


ggline(hyper, x = "Drug", y = "HP", color = "Special.Diet",
       add = c("mean_se", "dotplot"),
       palette = c("Red", "Blue"))

ggline(hyper, x = "Drug", y = "HP", color = "Special.Diet",
       add = c("mean_se"),
       palette = c("Red", "Blue"))

#conduct the ANOVA with interaction
hypermod <- aov(HP ~ Drug * Special.Diet, data = hyper)
summary(hypermod)

#conduct the ANOVA - additive model 
hypermod_add <- aov(HP ~ Drug + Special.Diet, data = hyper)
summary(hypermod_add)


#We don’t need to perform the test for the Special Diet 
#variable because it has only two levels, 
#which have been already tested.

TukeyHSD(hypermod_add, which = "Drug")

#Check assumptions/ reasonable test
plot(hypermod_add, 1)
plot(hypermod_add, 2)

#Now look at Biofeed
ggline(hyper, x = "Drug", y = "HP", color = "BioFeed",
       add = c("mean_se"),
       palette = c("Red", "Blue"))

#conduct the ANOVA with interaction
hypermod2 <- aov(HP ~ Drug * BioFeed, data = hyper)
summary(hypermod2) # this indicate that there's should be no interaction

#conduct the ANOVA - additive model 
hypermod_add2 <- aov(HP ~ Drug + BioFeed, data = hyper)
summary(hypermod_add2)

#conduct the ANOVA - three level

p= ggboxplot(hyper, x = "Drug", y = "HP", 
             color = "Special.Diet", palette = c("Red", "Blue")
)
print(p)
facet(p + theme_bw(), facet.by = "BioFeed",
      short.panel.labs = FALSE
)


hypermod_3<- aov(HP ~ Drug*BioFeed*Special.Diet, data = hyper)
summary(hypermod_3)
