library(ggplot2)
library(ggthemes)
library(ggpubr)

# our color scale for the book
cols = viridisLite::viridis(3)
liniendicke = 0.75
achsendicke = 1

#dat represents the data of american.woman
# x = age
# y = blood pressure
dat <- data.frame(x=c(22,41,52,23,41,54,24,46,56,27,47,57,28,48,58,9,49,59,30,49,63,32,50,67,33,51,71,35,51,77,40,51,81),
                  y=c(131,139,128,128,171,105,116,137,145,106,111,141,114,115,153,123,133,157,117,128,155,122,183,176,99,130,172,121,133,178,147,144,217))

# fit linear regression model - that is fitted mean values
fit = lm(y ~ x, data=dat)
summary(fit)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   87.671     10.076   8.701 7.97e-10 ***
#   x           1.105      0.206    5.364 7.56e-06 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 19.28 on 31 degrees of freedom


# get residuals - that is response minus fitted valus
dat$res <- residuals(lm(y ~ x, data=dat)) 

# select 3 women: first of age 22, second of age 47, third of age 67 
select = c(1,11,24)
selected_women = dat[select,]
#display ages of selected women
selected_women$x

# use our fitted model to predict blood pressure of the three selected women
pred = predict(fit, newdata = selected_women, se.fit = T)
#display fitted results
pred$fit
mean_woman1 = pred$fit[1]
sd_woman1 = pred$residual.scale  # standard deviation of prediction for woman 1

# plot deviation for woman 1
mein_y= 50:200
density_woman1 = dnorm(mein_y, mean = mean_woman1, sd = sd_woman1)
plot(mein_y, density_woman1, type = "l")


# p1 <- ggplot(data = data.frame(A = c(-2.5, 2.5)), aes(A)) + 
#   
#   stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 0.75)) + 

p_w1 = ggplot(dat,aes(x=x, y=y)) + 
  xlim(c(40,180)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean_woman1, sd = sd_woman1)) +
  rotate() +
  theme_transparent()
p_w1       

p_w1_grob <- ggplotGrob(p_w1)

density_selected_women = dnorm(mein_y, mean= select, sd = pred$residual.scale  )


p = ggplot(dat,aes(x=x, y=y)) + 
  geom_point(size=2) + 
  xlab('x (age)') +  # Beschriftung der x-Achse
  ylab('y (sbp)') +  # Beschriftung der y-Achse
  xlim(c(0,100)) +   # Altersrange auf x-Achse
  ylim(c(90,250)) +  # Blutdruckrange auf y-Achse
  # plot regression line
  #geom_abline(intercept = 87.6, slope=1.105, colour=cols[2], alpha=0.5, size=2) +
  geom_smooth(method="lm", colour=cols[2], alpha=0.5, size=2, fill=NA) +
  geom_vline(xintercept=selected_women$x, lty=2)  # add vertical lines for selected ages
  #theme_transparent()
#theme_hc(base_size = 30) 
p

# Insert p_w1_grob inside p
p + annotation_custom(grob = p_w1_grob, xmin = 18, xmax = 45, 
                       ymin = mean_woman1-40, ymax = mean_woman1+40) 

#save as pdf (with scale you change the thickness of lines, width=8.5cm is half a page width in the book)
ggsave(filename = "./figur 4.7.pdf",
       p1, width = 8.5, height = 7, units = "cm", scale = 1.5)
