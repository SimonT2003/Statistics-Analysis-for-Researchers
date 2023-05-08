# Learning Lab 3: ANOVA and Multiple Comparisons
library(dplyr)
library(ggplot2)

TBI <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/main/TBI.csv")

TBI_Stack = stack(TBI)
View(TBI_Stack)

TBIsum <- TBI_Stack %>%    # Get mean & standard deviation by group
  group_by(ind) %>%
  summarise_at(vars(values),
               list(mean = mean,
                    sd = sd)) %>% 
  as.data.frame()
View(TBIsum)   # View 


ggplot(TBIsum,         # ggplot2 plot with means & standard deviation
       aes(x = ind,
           y = mean)) + 
  geom_errorbar(aes(ymin = mean - sd,
                    ymax = mean + sd)) +
  geom_point() + labs(y = "Brain Water Content (%)", x = "Rat Groups")

anovaResult = aov(values~ind, data=TBI_Stack) # aov function is for single-factor ANOVA
summary(anovaResult)


# Comparing each Group
TukeyHSD(anovaResult, conf.level=.95) 
plot(TukeyHSD(anovaResult, conf.level=.95), las = 2)

library(multcomp)

# Tukey HSD test:
post_test <- glht(anovaResult,
                  linfct = mcp(ind = "Tukey")
)
summary(post_test)
par(mfrow = c(1, 1)) # combine plots
plot(post_test)

