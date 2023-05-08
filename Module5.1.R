# Example of using Chi-Squared Goodness of Fit Test

ObsAdmit <- c(432, 247, 226, 1118) # Observed counts
res <- chisq.test(ObsAdmit, p = c(.574, .031, 0.008, .387))
res
res$expected


# Another example
queens = 2287000
bronx = 1435000
westchester = 968000

popsize = queens + bronx + westchester
pq = queens/popsize
pb = bronx/popsize
pw = westchester/popsize

obsVac <- c(204, 132, 164) # observed count
res2 <- chisq.test(obsVac, p = c(pq, pb, pw))
res2
res2$expected
