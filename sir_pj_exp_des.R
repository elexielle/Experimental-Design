mortar <- data.frame(modif = c(18.85, 16.4, 17.21, 16.35, 16.52, 17.04, 16.96, 17.15, 16.59,16.57), 
                     unmodif = c(16.62, 16.75,17.37,17.12,16.98,16.87,17.34,17.02,17.08,17.27)) 

mortar
library(ggplot2)
ggplot(mortar, aes())
dotchart(as.matrix(mortar), cex = 1.3, color = "red")
summary(mortar) #summary<-gives the 6-number summary

ggplot(data.frame(mortar), aes(x=10,y=20))

#--------------------------

#generate random numbers
set.seed(1)
sort(sample(1:1500, 400, replace=FALSE))


#--------------------------

qnorm(.10, lower.tail=F)

qt(.9, 29)


#--------------------------

#anova
#ex6

rfp.etch <- data.frame(RFPower = c(rep("160W",5), rep("180W",5), rep("200W",5), rep("220W",5)),
                       etchrate = c(575,542,530,539,570,
                                    565,593,590,579,610,
                                    600,651,610,637,629,
                                    725,700,715,685,710))

rfp.etch
str(rfp.etch)
rfp.etch[,1] <- as.factor(rfp.etch$RFPower)
rfp.anova <- aov(etchrate~RFPower, data = rfp.etch)
summary(rfp.anova)
mean(rfp.etch$etchrate)
#to compute the means/sd per radio frequenct power
by(rfp.etch$etchrate, rfp.etch$RFPower, mean)
by(rfp.etch$etchrate, rfp.etch$RFPower, sd)

