#MODULE 1.3 
library(dplyr)
library(mosaic)

cellphone<- read.csv("https://raw.githubusercontent.com/jenbroatch/STP281/master/cellphonereaction.csv")
View(cellphone)

cellphone <- cellphone %>%   # "%>%" is a pipe operator
  mutate(
    diff = Cell.Phone-No.cell.phone
  )
View(cellphone)

hist(cellphone$diff, xlab='Difference in reaction times: Cell-no cell', main='Histogram')   # "$" is used to access, add, delete elements from the named list

favstats(~diff, data=cellphone)

t.test(~diff, data=cellphone)   

#Alternative syntax that does not require calculation of differences
t.test(cellphone$Cell.Phone,cellphone$No.cell.phone, paired=T )  
