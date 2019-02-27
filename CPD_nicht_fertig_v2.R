library(ggplot2)
library(ggthemes)
library(ggpubr)

# our color scale for the book
cols = viridisLite::viridis(3)
liniendicke = 0.75
achsendicke = 1

#dat represents the data of american.woman
dat <- data.frame(x=c(22,41,52,23,41,54,24,46,56,27,47,57,28,48,58,9,49,59,30,49,63,32,50,67,33,51,71,35,51,77,40,51,81),
                  y=c(131,139,128,128,171,105,116,137,145,106,111,141,114,115,153,123,133,157,117,128,155,122,183,176,99,130,172,121,133,178,147,144,217))
fit = lm(y ~ x, data=dat)
summary(fit)

number_of_age_sections = 4
max_age = max(dat$x)
min_age = min(dat$x)

# breaks: where you want to compute densities
breaks <- seq(min_age, max_age, len=number_of_age_sections)
dat$section <- cut(dat$x, breaks)

## Get the residuals
dat$res <- residuals(lm(y ~ x, data=dat)) 

## Compute densities for each section, and flip the axes, and add means of sections
## Note: the densities need to be scaled in relation to the section size (2000 here)
dens <- do.call(rbind, lapply(split(dat, dat$section), function(x) {
  d <- density(x$res, n=50)
  res <- data.frame(x=max(x$x)- d$y*2000, y=d$x+mean(x$y))
  res <- res[order(res$y), ]
  ## Get some data for normal lines as well
  xs <- seq(min(x$res), max(x$res), len=50)
  res <- rbind(res, data.frame(y=xs + mean(x$y),
                               x=max(x$x) - 800*dnorm(xs, 0, sd(x$res))))
  res$type <- rep(c("empirical", "normal"), each=50)
  res
}))
dens$section <- rep(levels(dat$section), each=100)

## Just normal
p = ggplot(dat, aes(x, y)) +
  geom_point(size=2) +
  xlab('x (age)') +  # Beschriftung der x-Achse
  ylab('y (sbp)') +  # Beschriftung der y-Achse
  xlim(c(0,100)) +   # Altersrange auf x-Achse
  ylim(c(90,250)) +  # Blutdruckrange auf y-Achse
  scale_y_continuous(breaks = NULL) +  # hide axis scaling
  scale_x_continuous(breaks = NULL) +
  geom_smooth(method="lm", fill=NA, lwd=2) +
  geom_path(data=dens[dens$type=="normal",], aes(x, y, group=section), 
            colour="green", lwd=1.1) +
  geom_vline(xintercept=breaks, lty=2) +
  theme_hc(base_size = 30) 
p

#save as pdf (with scale you change the thickness of lines, width=8.5cm is half a page width in the book)
ggsave(filename = "./CPD_nicht_fertig_v2.pdf",
       p1, width = 8.5, height = 7, units = "cm", scale = 1.5)
