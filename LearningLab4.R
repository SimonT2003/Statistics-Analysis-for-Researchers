library("ggpubr")

mood <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/main/mood.csv")
head(mood)
str(mood)

ggline(mood, x = "drug", y = "mood.gain", color = "therapy",
       add = c("mean_se"),
       palette = c("Red", "Blue"), ylab=('Mood Gain'), xlab=('Drugs'))

#conduct the ANOVA with interaction
moodmod <- aov(mood.gain ~ drug * therapy, data = mood)
summary(moodmod)

#conduct the ANOVA - additive model 
moodmod_add <- aov(mood.gain ~ drug + therapy, data = mood)
summary(moodmod_add)

TukeyHSD(moodmod_add)
