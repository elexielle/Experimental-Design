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
                             levels = c(40, 60, 80, 100))
wrp.cu.plt$temp <- factor(wrp.cu.plt$temp,
                          levels = c(50, 75, 100, 125))

table(wrp.cu.plt$temp, wrp.cu.plt$cu_cont)

#boxplot
plot(amnt_warp ~ cu_cont + temp, data = wrp.cu.plt,
     main = "warp copper plates boxplot")
ggboxplot(wrp.cu.plt, x = "cu_cont", y = "amnt_warp", color = "temp", 
          palette = c("#00AFBB", "#E7B800", "#CC0000", "#006600"))

#two-way anova
wrp.anova <- aov(amnt_warp~temp*cu_cont, data = wrp.cu.plt)
summary(wrp.anova)

#residuals & fitted
wrp_df <- fortify(wrp.anova) #wrp.cu.plt df w/ resid & fitted
wrp_df #use this new df for the plots

#residual vs copper content
ggplot(wrp_df, aes(x = cu_cont, y = .resid)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "residuals vs copper content") +
  theme(plot.title =  element_text(size = rel(2)))

#residual vs temperature
ggplot(wrp_df, aes(x = temp, y = .resid)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "residuals vs temperature", x = "copper content (%)",
       y = "residual") +
  theme(plot.title =  element_text(size = rel(2)))

#residual vs fitted
ggplot(wrp_df, aes(x = .fitted, y = .resid)) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "residuals vs fitted values") +
  theme(plot.title =  element_text(size = rel(2)))

#normal qq plot
ggplot(mapping = aes(sample = wrp_df$.resid)) +
  stat_qq_point(size = 2) +
  stat_qq_line(color = "red") +
  labs(title = "normal qq plot", 
       x = "theoretical quantiles",
       y = "standardized residuals") +
  theme(plot.title =  element_text(size = rel(2)))
#summary
par(mfrow=c(2,2)); plot(wrp.anova); par(mfrow=c(1,1))
#normality test
shapiro.test(wrp_df$.resid) #p>0.05 f2rej. ho => normally dist.

#interaction plot 
interaction.plot(wrp.cu.plt$cu_cont, wrp.cu.plt$temp,
                 + wrp.cu.plt$amnt_warp,
                 xlab = "copper content (%)", ylab = "amount of warping",
                 main = "interaction plot",
                 trace.label = "temperature", type = "b",
                 col=c("red","orange", "blue", "purple"),
                 pch = c(19,17,18,15), fixed = TRUE)

#plot of average warping at each level of copper content
wrp_mean <- aggregate(amnt_warp ~ cu_cont, data = wrp_df, mean) #mean for the amnt warp

ggplot(wrp_mean, aes(x = cu_cont, y = amnt_warp, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(title = "average warping amount vs copper content",
       x = "copper content", y = "acerage warping") +
  theme_bw()




###------------------------------------
#scratch
model1 <- lmer(amnt_warp ~ cu_cont +  (1|temp), wrp_df)


model2 <- lmer(amnt_warp ~ temp + (1|cu_cont), wrp_df)
(model1.emmeans <- emmeans(model1, "cu_cont"))
m1 <- emmeans(model1, specs = pairwise ~ "cu_cont")
# --> error as Turtle is a random effect

(model2.emmeans <- emmeans(model2, "temp"))
###-----------
model3 <- lmer(amnt_warp ~ cu_cont + (cu_cont||temp), wrp_df)

with(wrp_df, model3.emmeans <- emmeans(model3, specs = pairwise ~ "cu_cont/temp"), type = "cu_cont")
#----------
summary(model1, model2)
emm1 = emmeans(model1, specs = pairwise ~ cu_cont)
fm2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy)
####----------------
t.test(as.numeric(wrp_df$cu_cont), conf.level = 0.95)

mean(wrp_df$amnt_warp)
View(wrp_df)
###----------------

  
tukey.plot.test<-TukeyHSD(wrp.anova)
plot(tukey.plot.test, las = 1)













