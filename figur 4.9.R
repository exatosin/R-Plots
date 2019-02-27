library(ggplot2)
library(ggthemes)
library(ggpubr)


# our color scale for the book
cols = viridisLite::viridis(3)

liniendicke = 0.75
achsendicke = 1


# Return dnorm(x) for -1 < x < 2, and NA for all other x
dnorm_limit <- function(x) {
  y <- dnorm(x)
  y[x < -1 | x > 2] <- NA
  return(y)
}

# ggplot() with dummy data
p <- ggplot(data.frame(x=c(-3.5, 3.5)), aes(x = x)) +
  stat_function(fun = dnorm_limit, geom = "area", fill = cols[2], 
                alpha = 0.4, n = 500) +
  stat_function(fun = dnorm) + 
  scale_y_continuous(breaks = NULL) +  # hide axis scaling
  scale_x_continuous(breaks = NULL) +
  ylab("") +
  xlab("") +
  geom_text(x=-4.28, y=0.45,            # "density" label at y axis
            label="density", size=5, angle = 90) +
  geom_text(x=3.7, y=-0.03,             # "Y" label at x axis
            label="Y", size=5, angle = 0) +                    # a label x axis
  geom_text(x=-1, y=-0.03,              
            label=expression(a), size=5, angle = 0) +          # b label x axis
  geom_text(x=2, y=-0.03,               
           label=expression(b), size=5, angle = 0) +
  geom_segment(aes(x = -1, y = 0, xend = -1, yend = 0.24),     # vertical dashed a-line 
               size = liniendicke, linetype="dashed" ) +   
  geom_segment(aes(x = 2, y = 0, xend = 2, yend = 0.050),      # vertical dashed b-line 
               size = liniendicke, linetype="dashed" ) +   
  geom_segment(aes(x = -4, y = -0.02, xend = -4, yend = 0.55), # y-axis
               size = achsendicke,
               arrow = arrow(length = unit(0.3, "cm"))) +
  geom_segment(aes(x = -4, y = 0, xend = 4, yend = 0),         # x-axis
               size = achsendicke,
               arrow = arrow(length = unit(0.3, "cm"))) +
  theme_hc(base_size = 30) 
p

#save as pdf (with scale you change the thickness of lines, width=8.5cm is half a page width in the book)
ggsave(filename = "./kurve3_v2.pdf",
       p1, width = 8.5, height = 7, units = "cm", scale = 1.5)


