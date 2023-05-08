library(openintro)
library(dplyr)
library(mosaic)
View(mlb_players_18)

d = filter(mlb_players_18, AB >= 100)
d2 = filter(d, position != "DH")
d3 = filter(d2, position != "P")

d4 <- d3 %>%
  mutate(
    pos = case_when(
      position=="LF" | position=="CF" | position=="RF" ~ "OF",
      position=="1B" | position=="2B" | position=="3B"| position=="SS"    ~ "IF",
      position=="C"                      ~ "C"
    )
  )

d5 = filter(d4, pos != "C")
bwplot(OBP~pos, data=d5, main="Boxplots", xlab="Positions", ylab="OBP (On-based Percentage)")

favstats(OBP~pos, data=d5)
t.test(OBP~pos, data=d5)

#Comparing on-base percentage of OF and C
d6 = filter(d4,pos != "IF")
bwplot(OBP~pos, data=d6, main="Boxplots", xlab="Positions", ylab="OBP (On-based Percentage)")

favstats(OBP~pos, data=d6)
t.test(OBP~pos, data=d6)

#Comparing on-base percentage of IF and C
d7 = filter(d4,pos != "OF")
bwplot(OBP~pos, data=d7, main="Boxplots", xlab="Positions", ylab="OBP (On-based Percentage)")

favstats(OBP~pos, data=d7)
t.test(OBP~pos, data=d7)

#End of code