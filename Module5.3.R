# 5.3 - Chi squared test of independence

Observed <- cbind(c(2, 39, 131, 175, 78), c(8, 113, 138, 129, 32), 
                  c(46, 202, 120, 65,  8))

Observed
rownames(Observed)=c("Post-Grad Degree", "College Degree", "Some College", 
                     "HS Grad", "No HS Degree")

colnames(Observed)=c("<$30k","$30k to $75k", " >$75k")
Observed

prop.table(Observed,2)
barplot(prop.table(Observed,2),beside=TRUE,legend.text=TRUE,
        ylim=c(0,1),ylab="Proportions")

x=addmargins(Observed)
x
res1=chisq.test(Observed)
res1
res1$expected 
res1$stdres