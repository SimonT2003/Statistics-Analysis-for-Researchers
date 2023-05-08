library(mosaic)
library(mosaicData)
FIFA_data <- read.csv('https://raw.githubusercontent.com/jenbroatch/STP281/main/FIFADataadjusted.csv')

##Comparing the average wages between single left-footed and right-footed players
FIFA_data2 <- FIFA_data %>% select('Value','Preferred.Foot') 
FIFA_data2b=FIFA_data2 %>% mutate(Value.in.USD = Value*1.23) # converting from Euros to USD
View(FIFA_data2b)
FIFA_data2b_summary=favstats(log(Value.in.USD)~Preferred.Foot, data=FIFA_data2b)
FIFA_data2b_summary

bwplot(log(Value.in.USD)~Preferred.Foot, data=FIFA_data2b, main="Comparing Average Values Between Left-Footed 
       and Right-Footed Soccer Players", xlab='Preferred Foot')
t.test(log(Value.in.USD)~Preferred.Foot, data=FIFA_data2b)


##Comparing the average wages between different positions
FIFA_data3 <- FIFA_data %>% select('Value','Position')
FIFA_data3b=FIFA_data3 %>% mutate(Value.in.USD = Value*1.23) # converting from Euros to USD
View(FIFA_data3b)
FIFA_data3b_update <- FIFA_data3b %>%
  mutate(
    Position_modified = case_when(
      
      Position=='CF'| Position=='RF'| Position=='LF'| Position=='LS'| Position=='LW'| 
        Position=='RW'| Position=='ST'| Position=='LWB'| Position=='RWB'| Position=='RS' ~ "F", 
      
      Position=='CAM'| Position=='CDM'| Position=='CM'| Position== 'LAM'| Position== 'LCM'| 
        Position=='LDM'| Position=='LM'| Position=='RAM'| Position=='RCM'| Position=='RDM'| 
        Position== 'RM' ~ "MF",
      
      Position=='CB'| Position=='LB'| Position=='LCB'| Position== 'RB'| Position== 'RCB' ~ "D",
      
      Position=='GK'~"GK"
    )
  )

FIFA_data3c <- FIFA_data3b_update %>% mutate(log_value = log(Value.in.USD))
FIFA_data3c_summary=favstats(log_value~Position_modified, data=FIFA_data3c)
FIFA_data3c_summary

bwplot(log_value~Position_modified, data=FIFA_data3c, xlab='Soccer Positions',
       main='Comparing Average Values Between Different Soccer Positions')


anova_result =aov(log_value~Position_modified, data=FIFA_data3c)
summary(anova_result)

TukeyHSD(anova_result, conf.level=.95)

plot(TukeyHSD(anova_result, conf.level=.95), las = 2)

# End of Code