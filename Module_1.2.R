#MODULE 1.2 

##Comparing the average wages between single men and single women
library(mosaic)
library(mosaicData)
View(CPS85) #view our main data

men_and_women85=filter(CPS85, married=="Single")   #this filters through CPS85
m_and_w_Summary=favstats(wage~sex, data=men_and_women85)  #favstats creates a brief summary
m_and_w_Summary

m_and_w_Summary$mean[1]
m_and_w_Summary$mean[2]

#Using the t-test function
t.test(wage~sex, data=men_and_women85)


#End of code