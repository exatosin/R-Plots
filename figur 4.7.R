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
# display fitted results
pred$fit
mean_woman1 = pred$fit[1]
mean_woman2 = pred$fit[2]
mean_woman3 = pred$fit[3]
sd_woman = pred$residual.scale  # standard deviation of prediction 


my_y_range = 50:200   # blood pressure range

# this is part of the calculation on which range the normal distribution graf is to be displayed 
distance_factor = 3.5    

# create a plot, which will later be included into the main plot of linear regression
# the plot contains the normal distribution graf for expected blood pressure
# this graf is the same for each selected woman, with different mean values, therefor just
# the plot for the first woman is created and will also be used for the other selected women

# first woman
density_woman1 = dnorm(my_y_range, mean = mean_woman1, sd = sd_woman)

xmin = mean_woman1 - sd_woman * distance_factor  
xmax = mean_woman1 + sd_woman * distance_factor

p_w1 = ggplot(dat,aes(x=x, y=y)) + 
  xlim(c(xmin,xmax)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = mean_woman1, sd = sd_woman)) +
  # shading region under function curve
  stat_function(fun = dnorm, n = 101, geom = "area", fill = "gray", alpha = 0.4,
              args = list(mean = mean_woman1, sd = sd_woman)) +
  rotate() +
  theme_transparent()
p_w1   

# transform plot into graphical object called a "grop" in Grid terminology
p_w1_grob <- ggplotGrob(p_w1)

# create plot containing data points and regression line
p = ggplot(dat,aes(x=x, y=y)) + 
  geom_point(size=2, colour=cols[1], alpha=0.5) + 
  # plot regression line
  #geom_abline(intercept = 87.6, slope=1.105, colour=cols[2], alpha=0.5, size=2) 
  geom_smooth(method="lm", colour=cols[2], alpha=0.5, size=2, fill=NA) +
  ylab("") +
  xlab("") +
  scale_y_continuous(breaks = NULL) +  # hide axis scaling
  scale_x_continuous(breaks = NULL) +
  xlim(c(-5,100)) +   # range for x-axis
  ylim(c(60,230)) +   # range for y-axis
  geom_segment(aes(x = 0, y = 65, xend = 0, yend = 230),    # y-axis
               size = achsendicke) +
  geom_segment(aes(x = 0, y = 65, xend = 100, yend = 65),   # x-axis
               size = achsendicke) +
  geom_text(x=90, y=61, label= "age",                       # label for x-axis
            size=6, angle = 0) +  
  geom_text(x=-5, y=210, label= "sbp",                      # label for y-axis
            size=6, angle = 90) +      
  geom_segment(aes(x = selected_women$x[1], y = 65, 
                xend = selected_women$x[1], yend = 225),    # vertical dashed line 
                size = 0.5, linetype="dashed" ) +
  geom_text(x=selected_women$x[1], y=60,                    # age label of dashed line
            size=4, label=selected_women$x[1]) + 
  geom_segment(aes(x = selected_women$x[2], y = 65, 
                xend = selected_women$x[2], yend = 225),    # vertical dashed line 
                size = 0.5, linetype="dashed" ) +
  geom_text(x=selected_women$x[2], y=60,                    # age label of dashed line
            size=4, label=selected_women$x[2]) + 
  geom_segment(aes(x = selected_women$x[3], y = 65, 
                xend = selected_women$x[3], yend = 225),    # vertical dashed line 
                size = 0.5, linetype="dashed" ) +
  geom_text(x=selected_women$x[3], y=60,                    # age label of dashed line
            size=4, label=selected_women$x[3]) + 
  geom_segment(aes(x = 0, y = mean_woman1,                  # horizontal dashed line 
                   xend = 42, yend = mean_woman1),    
               size = 0.5, linetype="dashed" ) +
  geom_text(x=-5, y=mean_woman1, size=4,                    # sbp label of dashed line
            label=as.integer(mean_woman1), angle = 90) +
  geom_segment(aes(x = 0, y = mean_woman2,                  # horizontal dashed line 
                  xend = 67, yend = mean_woman2),    
              size = 0.5, linetype="dashed" ) +
  geom_text(x=-5, y=mean_woman2, size=4,                    # sbp label of dashed line
            label=as.integer(mean_woman2), angle = 90) +
  geom_segment(aes(x = 0, y = mean_woman3,                  # horizontal dashed line 
                   xend = 91, yend = mean_woman3),    
               size = 0.5, linetype="dashed" ) +
  geom_text(x=-5, y=mean_woman3, size=4,                    # sbp label of dashed line
            label=as.integer(mean_woman3), angle = 90) +
  theme_transparent() +
  theme_hc(base_size = 30) 
p

# insert grobs into main plot
xmin_offset = -3
xmax_offset = 23
ymin_offset = -55
ymax_offset = 55
p + annotation_custom(grob = p_w1_grob, 
                       xmin = selected_women$x[1] + xmin_offset, 
                       xmax = selected_women$x[1] + xmax_offset, 
                       ymin = mean_woman1 + ymin_offset, 
                       ymax = mean_woman1 + ymax_offset) +
    annotation_custom(grob = p_w1_grob, 
                       xmin = selected_women$x[2] + xmin_offset, 
                       xmax = selected_women$x[2] + xmax_offset, 
                       ymin = mean_woman2 + ymin_offset, 
                       ymax = mean_woman2 + ymax_offset) + 
    annotation_custom(grob = p_w1_grob, 
                    xmin = selected_women$x[3] + xmin_offset, 
                    xmax = selected_women$x[3] + xmax_offset, 
                    ymin = mean_woman3 + ymin_offset, 
                    ymax = mean_woman3 + ymax_offset)  

#save as pdf (with scale you change the thickness of lines, width=8.5cm is half a page width in the book)
ggsave(filename = "./figur 4.7.pdf",
       p1, width = 8.5, height = 7, units = "cm", scale = 1.5)
