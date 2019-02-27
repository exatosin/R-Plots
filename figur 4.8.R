library(ggplot2)
library(ggthemes)
library(ggpubr)
library(magrittr)

# our color scale for the book
cols = viridisLite::viridis(3)
wendepunkt_hoehe = 0.219
liniendicke = 0.75
achsendicke = 1

p1 <- ggplot(data = data.frame(A = c(-2.5, 2.5)), aes(A)) + 
    
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 0.75)) + 
  ylab("") +
  xlab("") +
  scale_y_continuous(breaks = NULL) +  # hide axis scaling
  scale_x_continuous(breaks = NULL) +
  geom_segment(aes(x = 0, y = -0.002, xend = 0, yend = 0.53), 
               size = liniendicke) + #vertical line to max
  geom_segment(aes(x = 1, y = 0, xend = 1, yend = wendepunkt_hoehe), 
               size = liniendicke) + # vertical line to the right inflection point
  geom_segment(aes(x = 0.3, y = 0.12, xend = 0, yend = 0.12), # Left arrow  to delta
               size = liniendicke,
               arrow = arrow(length = unit(0.3, "cm"))) + 
  geom_segment(aes(x = 0.7, y = 0.12, xend = 1, yend = 0.12), # Right arrow  to delta
               size = liniendicke,
               arrow = arrow(length = unit(0.3, "cm"))) + 
  geom_segment(aes(x = -2.9, y = -0.02, xend = -2.9, yend = 0.55), # y-axis
               size = achsendicke,
              arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = -3.2, y = 0, xend = 2.9, yend = 0),         # x-axis
               size = achsendicke,
               arrow = arrow(length = unit(0.3, "cm"))) +          # delta text
  geom_text(x=0.5, y=0.12, label=expression(delta), size=6) +      
  geom_text(x=0, y=-0.03,                                          # mü text bottom
            label=expression(mu), 
            size=6) +
  geom_text(x=2.65, y=-0.02, label=expression(Y),                  # "Y" label
            size=6, angle = 0) +                                  
  geom_text(x=-3.15, y=0.45,                                        # "density" label
            label="density", 
            size=6, angle = 90) +
  theme_hc(base_size = 30)
p1

#save as pdf (with scale you change the thickness of lines, width=8.5cm is half a page width in the book)
ggsave(filename = "./kurve1_v2.pdf",
      p1, width = 8.5, height = 7, units = "cm", scale = 1.5)


