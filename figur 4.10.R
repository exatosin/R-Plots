library(ggplot2)
library(ggthemes)
library(ggpubr)
library(magrittr)
library(grid)

# our color scale for the book
cols = viridisLite::viridis(3)
wendepunkt_hoehe = 0.219
liniendicke = 0.75
achsendicke = 1


p1 <- ggplot(data = data.frame(A = c(-3.5, 2.5)), aes(A)) + 
            
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 0.75)) + 
  ylab("") +
  xlab("") +
  scale_y_continuous(breaks = NULL) +  # hide axis scaling
  scale_x_continuous(breaks = NULL) +
  geom_segment(aes(x = 0, y = -0.002, xend = 0, yend = 0.53), 
               size = liniendicke) + # vertical line to max
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = wendepunkt_hoehe), 
               size = liniendicke) + # vertical line to the right inflection point
  geom_segment(aes(x = -1, y = 0, xend = -1, 
                   yend = wendepunkt_hoehe),      # vertical red line to the left inflection point
               colour = "red", size = liniendicke ) + 
  geom_segment(aes(x = -1, y = wendepunkt_hoehe,  # horizontal red line to the left inflection point
                   xend = -2.9, yend = wendepunkt_hoehe), 
               colour = "red", size = liniendicke ) +
  geom_rect(aes(xmin=-1.1, xmax=-0.9, ymin=0,       # 2 epsilon bar
                ymax=wendepunkt_hoehe), fill="red", alpha=0.3) + 
  geom_text(x=-1, y=0.24,                          # 2 epsilon label
            label=expression(paste("2",epsilon)), size=5, colour = "red") + 
  geom_segment(aes(x = 0.3, y = 0.12, xend = 0, yend = 0.12), # Left arrow  to sigma
               size = liniendicke,
               arrow = arrow(length = unit(0.3, "cm"))) + 
  geom_segment(aes(x = 0.7, y = 0.12, xend = 1, yend = 0.12), # right arrow  to sigma
               size = liniendicke,
               arrow = arrow(length = unit(0.3, "cm"))) + 
  geom_text(x=0.5, y=0.12, label=expression(sigma), size=6) + # sigma text
  geom_segment(aes(x = -2.9, y = -0.02, xend = -2.9, yend = 0.55), # y-axis
               size = achsendicke,
              arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = -3.9, y = 0, xend = 2.9, yend = 0),         # x-axis
               size = achsendicke,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(x=0, y=-0.03,                # mu[xi] bottom text 
            label=expression(mu[x[i]]), 
            size=6) +
  geom_text(x=-1, y=-0.03,               # y[i] red bottom text
            label=expression(y[i]), 
            size=6 , colour = "red") +
  geom_text(x=2.8, y=-0.03, label=expression(Y), size=6, angle = 0) +
  geom_text(x=-3.3, y=0.54, label="f(Y)", size=6, angle = 0) +
  geom_text(x=-3.3, y=wendepunkt_hoehe, # f(...) red left text 
            label=expression(f(y[i])), 
            size=6, colour = "red", angle = 0) +
  # draw white square on top of right end of too long x-axis (work-around)
  geom_rect(aes(xmin=-4.0, xmax=-3.17, ymin=-0.02,  ymax=0.02), 
            fill="white", alpha=1) +
  theme_hc(base_size = 30) 
p1 

#save as pdf (with scale you change the thickness of lines,
#width=8.5cm is half a page width in the book)
ggsave(filename = "./figur4.10.pdf",
       p1, width = 8.5, height = 7, units = "cm", scale = 1.5)


