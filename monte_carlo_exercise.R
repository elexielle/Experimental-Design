#bayes stat
#monte carlo exercise

#install.packages("Bolstad")
library(Bolstad)

help("sscsample.data")
str(sscsample.data)
# to get the mean of the pop
mean(sscsample.data$income)
choose(100,20)

#a.
sscsample.data
boxplot(income ~ ethnicity, data = sscsample.data,
        main = "income of the three ethinic groups")

#b
mySamples <- list(simple = NULL, stat = NULL, cluster = NULL)
mySamples$simple <- sscsample(20, 200)
mySamples$simple

#c
mySamples$strat <- sscsample(20, 200, "stratified")
mySamples$strat

#d
mySamples$cluster <- sscsample(20, 200, "cluster")
mySamples$cluster

#e
sapply(mySamples, function(x)sd(x$means))
sapply(mySamples, function(x)IQR(x$means))


#
#i. 