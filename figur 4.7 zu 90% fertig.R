library(ggplot2)
library(ggthemes)
library(ggpubr)

# our color scale for the book
cols = viridisLite::viridis(3)
liniendicke = 0.75
achsendicke = 1

# dat represents the data of american.woman
# x = age
# y = blood pressure
dat <- data.frame(x=c(22,41,52,23,41,54,24,46,56,27,47,57,28,48,58,9,49,59,30,49,63,32,50,67,33,51,71,35,51,77,40,51,81),
                  y=c(131,139,128,128,171,105,116,137,145,106,111,141,114,115,153,123,133,157,117,128,155,122,183,176,99,130,172,121,133,178,147,144,217))

# fit linear regression model 
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

# select 3 women
select = c(1,11,27)
selected_women = dat[select,]
#display ages of selected women
selected_women$x

# use our fitted model to predict blood pressure of the three selected women
pred = predict(fit, newdata = selected_women, se.fit = T)
#display fitted results
pred$fit
mean_woman1 = pred$fit[1]
mean_woman2 = pred$fit[2]
mean_woman3 = pred$fit[3]
sd_woman = pred$residual.scale  # standard deviation of prediction 


my_y_range = 50:200   # blood pressure range
density_woman1 = dnorm(my_y_range, mean = mean_woman1, sd = sd_woman)
density_woman2 = dnorm(my_y_range, mean = mean_woman2, sd = sd_woman)
density_woman3 = dnorm(my_y_range, mean = mean_woman3, sd = sd_woman)

# plot containing normal distribution graf for expected blood pressure of selected women
# create a plot per woman, which will later be included into the main plot of linear regression
distance_factor = 3.5  # distance factor related to mean position for displaying the distribution graf

# first woman
xmin = mean_woman1 - sd_woman * distance_factor  
xmax = mean_woman1 + sd_woman * distance_factor
p_w1 = ggplot(dat,aes(x=x, y=y)) + 
  xlim(c(xmin,xmax)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean_woman1, sd = sd_woman)) +
  rotate() +
  theme_transparent()
p_w1   

# second woman
xmin = mean_woman2 - sd_woman * distance_factor  
xmax = mean_woman2 + sd_woman * distance_factor
p_w2 = ggplot(dat,aes(x=x, y=y)) + 
  xlim(c(xmin,xmax)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean_woman2, sd = sd_woman)) +
  rotate() +
  theme_transparent()
p_w2       

# third woman
xmin = mean_woman3 - sd_woman * distance_factor  
xmax = mean_woman3 + sd_woman * distance_factor
p_w3 = ggplot(dat,aes(x=x, y=y)) + 
  xlim(c(xmin,xmax)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean_woman3, sd = sd_woman)) +
  rotate() +
  theme_transparent()
p_w3     

# transform plots into graphical objects called a "grop" in Grid terminology
p_w1_grob <- ggplotGrob(p_w1)
p_w2_grob <- ggplotGrob(p_w2)
p_w3_grob <- ggplotGrob(p_w3)

density_selected_women = dnorm(my_y_range, mean= select, sd = pred$residual.scale  )

# plot containing data points and regression line
p = ggplot(dat,aes(x=x, y=y)) + 
  geom_point(size=2, colour=cols[1], alpha=0.5) + 
  xlab('x (age)') +  # labeling for x axis
  ylab('y (sbp)') +  # labeling for y axis
  xlim(c(0,100)) +   # range of ages on x axis
  ylim(c(90,250)) +  # range of blood pressure on y axis
  # plot regression line
  #geom_abline(intercept = 87.6, slope=1.105, colour=cols[2], alpha=0.5, size=2) +
  geom_smooth(method="lm", colour=cols[2], alpha=0.5, size=2, fill=NA) +
  geom_vline(xintercept=selected_women$x, lty=2)  # add vertical lines for selected ages
  #theme_transparent()
#theme_hc(base_size = 30) 
p

# insert grobs into main plot
xmin_offset = -4
xmax_offset = 23
ymin_offset = -40
ymax_offset = 40
p + annotation_custom(grob = p_w1_grob, 
                       xmin = selected_women$x[1] + xmin_offset, 
                       xmax = selected_women$x[1] + xmax_offset, 
                       ymin = mean_woman1 + ymin_offset, 
                       ymax = mean_woman1 + ymax_offset) +
    annotation_custom(grob = p_w2_grob, 
                       xmin = selected_women$x[2] + xmin_offset, 
                       xmax = selected_women$x[2] + xmax_offset, 
                       ymin = mean_woman2 + ymin_offset, 
                       ymax = mean_woman2 + ymax_offset) + 
    annotation_custom(grob = p_w3_grob, 
                    xmin = selected_women$x[3] + xmin_offset, 
                    xmax = selected_women$x[3] + xmax_offset, 
                    ymin = mean_woman3 + ymin_offset, 
                    ymax = mean_woman3 + ymax_offset)  

#save as pdf (with scale you change the thickness of lines, width=8.5cm is half a page width in the book)
ggsave(filename = "./figur 4.7.pdf",
       p1, width = 8.5, height = 7, units = "cm", scale = 1.5)
