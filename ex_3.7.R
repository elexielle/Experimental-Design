#exp des  #3.7 tensile strength


#install.packages("agricolae")
#install.packages("tidyverse")
library(tidyverse)
library(agricolae)

tech1 <- c(3129,3000,2865,2890)
tech2 <- c(3200,3300,2975,3150)
tech3 <- c(2800,2900,2985,3050)
tech4 <- c(2600,2700,2600,2765)

mixing_techniques <- cbind.data.frame(tech1, tech2, tech3, tech4)
mixing_techniques
mixing <- pivot_longer(data = mixing_techniques, c(tech1, tech2, tech3, tech4))
mixing


anova_table <- aov(value~name , data = mixing)
summary(anova_table)

plot(anova_table, ask = FALSE)

boxplot(mixing_techniques, main = "Mean of the Tensile Strength by Treatment",
        xlab = "Mixing Technique", ylab = "Tensile Strength")

plot(mixing_techniques, main = "Mean of the Tensile Strength by Treatment",
     xlab = "Mixing Technique", ylab = "Tensile Strength")

plot.design(mixing_techniques)

plotmeans(mixing_techniques~mixing_techniques, mixing_techniques)
            

            
            
                        
#install.packages("BHH2")
library(BHH2)
anovaPlot(anova_table)

#install.packages("gplots")
library(gplots)


