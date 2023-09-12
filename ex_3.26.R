#exp des  #ex 3.26 batteries brand

library(agricolae)
library(tidyverse)

brand1 <- c(100,96,92,96,92)
brand2 <- c(76,80,75,84,82)
brand3 <- c(108,100,96,98,100)

brand.baterries <- cbind.data.frame(brand1, brand2, brand3)
life.battery <- pivot_longer(brand.baterries,c(brand1, brand2, brand3))
anova.test <- aov(value~name , data = life.battery)
summary(anova.test)
plot(anova.test, ask = FALSE)
