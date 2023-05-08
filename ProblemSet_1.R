# Problem Set 1 - Simon Tran
library(dplyr)
library(mosaic)
library(mosaicData)
library(ggplot2)

## 1. Chapter 9 - Problem 31
dof <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/main/bookex0931.csv")
View(dof)

boxplot(dof$Polymerization1, dof$Polymerization2,
        names=c("Polymerization 1", "Polymerization 2"), 
        ylab="Degree of Polymerization")

favstats(~Polymerization1, data=dof)
favstats(~Polymerization2, data=dof)
t.test(dof$Polymerization1, dof$Polymerization2, data=dof)


## 1. Chapter 9 - Problem 88
anteropos_transl <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/main/bookex0988.csv")
View(anteropos_transl)

anteropos_transl <- anteropos_transl %>%
  mutate(
    diff1 = PIT_Dom_-Pit_ND_T,
    diff2 = Pos_Dom_-Pos_ND_T
  )
View(anteropos_transl)

t.test(~diff1, data=anteropos_transl)
t.test(~diff2, data=anteropos_transl)

anteropos_transl_2 <- anteropos_transl %>% select('diff1', 'diff2')
boxplot(anteropos_transl_2$diff1, anteropos_transl_2$diff2,
        names=c("Difference in Pitchers", "Difference in Position Players"))

anteropos_transl_3 <- stack(anteropos_transl_2)
anteropos_transl_4 <- na.omit(anteropos_transl_3)

t.test(values~ind, data=anteropos_transl_4)


## 2.
View(Gestation)

update_gestation <- Gestation %>% select('smoke')
update_gestation <- update_gestation %>%
  mutate (
    smoke_now = case_when(    # use case_when for multiple if statements, etc.
      smoke=="now" ~1,
      smoke=="never" | smoke=="once did, not now" | smoke=="until current pregnancy" ~ 0
    )
  )

smoke_proportion <- data.frame(table(update_gestation$smoke_now))
colnames(smoke_proportion)[1] <- "Smoke_now"
smoke_proportion

prop.test(x=c(484), n=c(1226), p=0.30)


## 3.
ReadingEaseFile <- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/main/ReadingEase.csv")

favstats(~Wikipedia, data=ReadingEaseFile)
favstats(~WebMD, data=ReadingEaseFile)

boxplot(ReadingEaseFile$Wikipedia, ReadingEaseFile$WebMD, ylab=('The Flesch Reading Ease Score'),
        xlab=('Sources'), names=c("Wikipedia", "WebMD"))

REF_update = stack(ReadingEaseFile)
t.test(values~ind, data = REF_update)


## 4. 
youtube<- read.csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-02/youtube.csv')

#Remove ads in both or neither category
youtube2<- youtube %>% filter(celebrity!=animals)   %>%
  mutate(ComCategory=case_when(celebrity=="TRUE" ~ "Celebrity", 
                               animals=="TRUE" ~"Animals"))

youtube3 <- youtube2 %>% select('view_count', 'ComCategory')

favstats(log(view_count)~ComCategory, data=youtube3)

boxplot(log(view_count)~ComCategory, data=youtube3, ylab=('View Counts'), xlab=('Two Popular Categories'),
        main=("Comparing The Popularity Between Animals and Celebrities in Superbowl Ads"))

t.test(log(view_count)~ComCategory, data=youtube3)


# End of code