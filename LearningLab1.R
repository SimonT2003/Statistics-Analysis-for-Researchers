library(tidyverse)
resumedata <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/resume.csv")
View(resumedata)

library(dplyr)
LL1example<-resumedata %>% select('race','received_callback')

library(ggplot2)
results <- data.frame(table(LL1example))
results

table(LL1example)

ggplot(data = results, aes(x = received_callback, y = Freq, fill = race)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.75) +
  geom_text(aes(label = Freq), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  labs(x = "\n Callback Status", y = "Frequency\n", title = "\n Callback Results by race \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold",  size = 12),
        axis.title.y = element_text(face="bold",  size = 12),
        legend.title = element_text(face="bold", size = 10))

prop.test(x = c(157, 235), n = c(2435, 2435))



#End of work
