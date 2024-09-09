setwd("E:/Biology_data/5.Bluehole")
dat <- read.table('Table_physicochemical.txt', header=TRUE, sep = '\t')
dat
library(ggplot2)
library(reshape2)
library(grid)
d <- melt(dat, id.vars="Depth")

d.N <- d[d$variable %in% c('NH4.N', 'NO3.N.NO2.N', 'NO2.N'), ]
d.CTD <- d[d$variable %in% c('Salinity','Temperature','DO'), ]
d.S <- d[d$variable %in% c('H2S','SO42.'), ]
d.other <- d[d$variable %in% c('DOC','PO4','SiO4'), ]



# N compounds
pdf(file="N_compounds_lineplot.pdf", width=6, height=5)

colorpallet.N <- RColorBrewer::brewer.pal(8, "RdPu")[3:5]

p.N <- ggplot(d.N, aes(Depth, value)) + 
  geom_line(aes(color=variable), size = 1) + geom_point(aes(color=variable), size = 2)  + 
  scale_x_reverse() + coord_flip() + 
  facet_grid(cols = vars(variable), scales="free_x") +
  theme_bw(base_size=18) + scale_color_manual(values = colorpallet.N) +
  theme(legend.position = "none", axis.title.x=element_blank()) +
  labs(x='Depth (m)')
p.N

dev.off()

# CTD
pdf(file="CTD_lineplot.pdf", width=6, height=5)

colorpallet.CTD <- RColorBrewer::brewer.pal(8, "YlGn")[3:5]

p.CTD <- ggplot(d.CTD, aes(Depth, value)) + 
  geom_line(aes(color=variable), size = 1) + geom_point(aes(color=variable), size = 2)  + 
  scale_x_reverse() + coord_flip() + 
  facet_grid(cols = vars(variable), scales="free_x") +
  theme_bw(base_size=18) + scale_color_manual(values = colorpallet.CTD) +
  theme(legend.position = "none", axis.title.x=element_blank()) +
  labs(x='Depth (m)')
p.CTD

dev.off()

# S
pdf(file="S_compounds_lineplot.pdf", width=5, height=5)

colorpallet.S <- RColorBrewer::brewer.pal(8, "OrRd")[3:5]

p.S <- ggplot(d.S, aes(Depth, value)) + 
  geom_line(aes(color=variable), size = 1) + geom_point(aes(color=variable), size = 2)  + 
  scale_x_reverse() + coord_flip() + 
  facet_grid(cols = vars(variable), scales="free_x") +
  theme_bw(base_size=18) + scale_color_manual(values = colorpallet.S) +
  theme(legend.position = "none", axis.title.x=element_blank()) +
  labs(x='Depth (m)')
p.S

dev.off()

# other
pdf(file="other_lineplot.pdf", width=6, height=5)

colorpallet.other <- RColorBrewer::brewer.pal(8, "Blues")[3:5]

p.other <- ggplot(d.other, aes(Depth, value)) + 
  geom_line(aes(color=variable), size = 1) + geom_point(aes(color=variable), size = 2)  + 
  scale_x_reverse() + coord_flip() + 
  facet_grid(cols = vars(variable), scales="free_x", labeller = labeller(variable=para.labs)) +
  theme_bw(base_size=18) + scale_color_manual(values = colorpallet.other) +
  theme(legend.position = "none", axis.title.x=element_blank()) +
  labs(x='Depth (m)')
p.other

dev.off()

#qplot(variable, value, data=d.N)+facet_grid(.~variable)
#for (i in tail(colnames(dat), -1)) {
#  d.current = d[which(d$variable == i), ]
#  ggplot(d.current, aes(Depth, value)) + geom_line() + geom_point() + coord_flip() + scale_x_reverse()  + labs(title = i) + theme(plot.title = element_text(hjust = 0.5))
#}
#p1 <- ggplot(d, aes(Depth, NH4.N)) + geom_line() + geom_point() + coord_flip() + scale_x_reverse()  + labs(title = i) + theme(plot.title = element_text(hjust = 0.5))


                                                                                                                                 