library(ggplot2)
library(ggthemes)
library(ggpubr)
library(magrittr)
library(grid)

# our color scale for the book
cols = viridisLite::viridis(3)

liniendicke = 0.75
achsendicke = 1

p_Value_low  = 0.25
p_value_high = 0.37


p1 <- ggplot(data = data.frame(A = c(-3.5, 2.5)), aes(A)) + 
  ylab("") +
  xlab("") +
  #scale_y_continuous(breaks = NULL) +  # hide axis scaling
  #scale_x_continuous(breaks = NULL) +
  geom_text(x=1, y=-0.025, label="1", size=6) +       # "1" bottom text 
  geom_text(x=-1, y=-0.025,label="0",  size=6) +      # "0" bottom text
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = p_value_high), 
               size = liniendicke) +                  # vertical line with "1" label
  geom_segment(aes(x = -1, y = 0, xend = -1, yend = p_Value_low), 
               size = liniendicke ) +                 # vertical line with "0" label
  geom_segment(aes(x = -2.9, y = -0.02, xend = -2.9, yend = 0.55), # draw y-axis
               size = achsendicke) +
  #            arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = -3.9, y = 0, xend = 2.9, yend = 0),         # draw x-axis
               size = achsendicke) +
  geom_text(x=2.8, y=-0.03, label=expression(Y), size=6, angle = 0) +  # y axis label
  geom_text(x=-3.5, y=0.54, label="P(Y)", size=6, angle = 0)         # P(Y) x axis label
  # draw white square on top of right end of too long x-axis (work-around)
  geom_rect(aes(xmin=-4.0, xmax=-3.17, ymin=-0.02,  ymax=0.02), 
            fill="white", alpha=1) 
  #theme_hc(base_size = 30) 
p1 

#save as pdf (with scale you change the thickness of lines,
#width=8.5cm is half a page width in the book)
ggsave(filename = "./figur4.unknown.pdf",
       p1, width = 8.5, height = 7, units = "cm", scale = 1.5)


