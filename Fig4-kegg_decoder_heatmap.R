mydata <- read.table("38bins_decoder.out.reduced.out",header=TRUE,sep = '\t')

library(reshape2)

meltdata <- melt(mydata, value.name = 'Pathway Completeness', variable.name = 'MAGs')
meltdata$Category <- factor(meltdata$Category, levels = c("Hg","Oxidation","S","N","Transporter","Other"))

library(splitstackshape)
meltdata <- cSplit(meltdata, "MAGs", ".")
names(meltdata)[names(meltdata) == "MAGs_1"] <- "MAGs"
names(meltdata)[names(meltdata) == "MAGs_2"] <- "Classification"

library(ggplot2)

ggplot(meltdata, aes(x=MAGs, y=KEGG, fill=`Pathway Completeness`)) + 
  geom_tile(colour="white",size=0.5) + 
  facet_grid(Category~Classification, 
             scales = "free", 
             space = "free", 
             switch = "y") +
  #scale_fill_gradient(
  #                    low = "#e7e7e7",
  #                    high = "#a72632")+
  scale_fill_gradientn(colours = c("#e7e7e7", "#fde725", "#450d54"), 
                       values = c(0,0.2,1)) +
  theme_bw() +
  theme(panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave("KEGG_decoder_heatmap2.pdf", width = 13, height = 6)
       