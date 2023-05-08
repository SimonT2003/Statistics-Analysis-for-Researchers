library(dplyr)

anorexia<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/Anorexia.csv")

boxplot(anorexia$Cognitive.Therapy, anorexia$Family.Therapy, anorexia$Control,
        names=c("Cognitive Therapy", "Family Therapy", "Control"), 
        ylab="Weight Loss/Gain (lbs)") 

anorexia_stack=na.omit(stack(anorexia)) # 'na.omit' omits any missing values
View(anorexia_stack)
                       
anovamod =aov(values~ind, data=anorexia_stack) # aov function is for single-factor ANOVA
summary(anovamod) # when you see the term 'residuals' it is the 'error'
