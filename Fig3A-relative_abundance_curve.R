setwd("E:/Biology_data/5.Bluehole/ms-bluehole")
dat <- read.table('relative_abundance_sum.txt', header=TRUE, sep = '\t')
dat
library(ggplot2)

pdf(file="FLandPA_lineplot_1.pdf", width=10, height=15)

ggplot(dat, aes(x=Depth, y=Relative.Abundance, group=Life.style)) +
  geom_line(aes(linetype=Life.style, color=Life.style), size=2)+
  geom_point(aes(color=Life.style), size=5)+
  scale_x_reverse() + coord_flip() +
  theme_bw(base_size=30)+
  labs(x='Depth (m)', y='Relative Abundance (%)') +
  theme(legend.position = c(.7, .8),
        legend.box.background = element_rect(colour = "black")
        )

dev.off()