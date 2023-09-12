#chapter 5
#2. Two-way ANOVA in R
library(ggplot2)
library(qqplotr)
library(ggpubr)
#data frame
wrp.cu.plt <- data.frame(cu_cont = rep(c(rep("40", 2), rep("60", 2), 
                                         rep("80", 2), rep("100", 2)), 4),
                         temp = c(rep("50", 8), rep("75", 8),
                                  rep("100", 8), rep("125", 8)),
                         amnt_warp = c(17, 20, 16, 21, 24, 22, 28, 27,
                                       12, 9, 18, 13, 17, 12, 27, 31,
                                       16, 12, 18, 21, 25, 23, 30, 23,
                                       21, 17, 23, 21, 23, 22, 29, 31))
wrp.cu.plt 
str(wrp.cu.plt)
#convert to factor
wrp.cu.plt$cu_cont <- factor(wrp.cu.plt$cu_cont, 
                             levels = c(40, 60, 80, 100),
                             labels = c("40%", "60%", "80%", "100%"))
wrp.cu.plt$temp <- factor(wrp.cu.plt$temp,
                          levels = c(50, 75, 100, 125),
                          labels = c("50C", "75C", "100C", "125C"))

table(wrp.cu.plt$temp, wrp.cu.plt$cu_cont)

#boxplot
plot(amnt_warp ~ cu_cont + temp, data = wrp.cu.plt,
     main = "warp copper plates boxplot")
ggboxplot(wrp.cu.plt, x = "cu_cont", y = "amnt_warp", color = "temp", 
          palette = c("#00AFBB", "#E7B800", "#CC0000", "#006600"))

#two-way anova
wrp.anova <- aov(amnt_warp~temp*cu_cont, data = wrp.cu.plt)
summary(wrp.anova)

#residuals
resid.wrp <- resid(wrp.anova)
qplot(wrp.cu.plt$cu_cont, resid.wrp)

wrp.cu.plt <- cbind(wrp.cu.plt, resid.wrp) #insert residual column to wrp df
#residual vs copper content
ggplot(wrp.cu.plt, aes(x = cu_cont, y = resid.wrp)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "residuals vs copper content")

#residual vs temperature
ggplot(wrp.cu.plt, aes(x = temp, y = resid.wrp)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "residuals vs temperature")

#residual vs fitted
resid_fit <- data.frame(resid.wrp, fit.wrp = fitted(wrp.anova)) #make a new df w/ resid & fitted values
ggplot(resid_fit, aes(x = fit.wrp, y = resid.wrp)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "residuals vs fitted values")


###----------------######
#geeks for geeks site
ggplot(mapping = aes(sample = resid.wrp)) +
  stat_qq_point(size = 2,color = "red") +
  stat_qq_line(color="green") +
  xlab("x-axis") + ylab("y-axis")


#int plot from pdf site about anova
interaction.plot(wrp.cu.plt$cu_cont, wrp.cu.plt$temp,
                 + wrp.cu.plt$amnt_warp)





yt <- fortify(wrp.anova)
str(yt)
names(yt)
identical(yt$.resid, wrp.cu.plt$resid.wrp)
str(resid.wrp)
ggplot(yt, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "residuals vs fitted values")











