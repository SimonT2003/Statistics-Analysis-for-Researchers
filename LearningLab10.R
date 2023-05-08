titanic_data <- read.csv('https://raw.githubusercontent.com/jenbroatch/STP281/main/titanic.csv')
View(titanic_data)

# Is there an association between the Sex of the passenger and survival? 
observed = table(titanic_data$sex, titanic_data$survive)
observed
prop.table(observed,2)
barplot(prop.table(observed,2),beside=TRUE,legend.text=TRUE,
        ylim=c(0,1),ylab="Proportions", xlab="Survive")

x=addmargins(observed)
x
res=chisq.test(observed)
res
res$expected 
res$stdres


# Is there an association between the class of the passenger and survival? 
observed2 = table(titanic_data$passenger_class, titanic_data$survive)
observed2
prop.table(observed2,2)
barplot(prop.table(observed2,2),beside=TRUE,legend.text=TRUE,
        ylim=c(0,1),ylab="Proportions", xlab="Survive")

x2=addmargins(observed2)
x2
res2=chisq.test(observed2)
res2
res2$expected 
res2$stdres

# End of code
