library(dplyr)
library(reshape2)
library(splitstackshape)
library(ggraph)
library(igraph)
library(patchwork)

# hgcA	                                  Taxonomy	          FL_0m	PA_0m	FL_30m	PA_30m
# YBH_bin48_Ctg30_53-Desulfobacterales	  Deltaproteobacteria	0.04	0.03	0.99	  0.33
# YBH_bin181_Ctg9_114-Deltaproteobacteria	Deltaproteobacteria	0.09	0.43	0.12	  0.33


### handle data
setwd("E:/Biology_data/5.Bluehole")
mydata <- read.table("all-hgcA-abundance.tab",header=T,sep = '\t', encoding = 'UTF-8')
mydata_melt <- melt(mydata, value.name = 'Abundance', variable.name = 'Samples')
mydata_melt <- mydata_melt %>%
  filter(Abundance != 0)


mydata_melt <- cSplit(mydata_melt, "Samples", "_")
names(mydata_melt)[names(mydata_melt) == "Samples_1"] <- "Lifestyle"
names(mydata_melt)[names(mydata_melt) == "Samples_2"] <- "Depth"

mydata_melt <- mydata_melt %>%
  mutate(hgcA_lifestyle = paste(hgcA, Depth, Lifestyle, sep = '_'))  #rename subitems to make them different in PA and FL samples

# case_when(
#   !mydata_melt$Taxonomy %in% node_color$bins ~ "Others"
# )

depth_list <- c("0m", "30m", "50m", "90m", "120m", "140m", "170m")
plot_list <- list("p_0m", "p_30m", "p_50m", "p_90m", "p_120m", "p_140m", "p_170m")

n=1
for (this_depth in depth_list) {
    
    ### Design edges
    edges <- mydata_melt %>%
      filter(Depth == this_depth) %>%
      select(c('Lifestyle','hgcA_lifestyle')) %>%
      add_row(Lifestyle = this_depth, hgcA_lifestyle = 'PA')
    
    if(this_depth != '0m'){
      edges <- edges %>%
        add_row(Lifestyle = this_depth, hgcA_lifestyle = 'FL')
    }
    
    ### Design vertices
    vertices <- mydata_melt %>%
      filter(Depth == this_depth) %>%
      select(c('hgcA_lifestyle', 'Abundance', 'Taxonomy')) %>%
      add_row(hgcA_lifestyle = this_depth, Abundance = 0, Taxonomy = 'root') %>%
      add_row(hgcA_lifestyle = 'PA', Abundance = 0, Taxonomy = 'PA')
    
    if(this_depth != '0m'){
      vertices <- vertices %>%
        add_row(hgcA_lifestyle = 'FL', Abundance = 0, Taxonomy = 'FL')
    }
    
    # vertices$Taxonomy <- factor(vertices$Taxonomy, levels = c("Bacteroidetes", "Chloroflexi", "Deltaproteobacteria", "Marinimicrobia", "Myxococcota", "Nitrospina", "Others", "Planctomycetes", "Vecturithrix", "Verrucomicrobiota", "FL", "PA", "root"))
    
    ### draw pictures
    set.seed(1)
    graph <- graph_from_data_frame(edges, vertices = vertices)
    plot_list[[n]] <- ggraph(graph, layout = "circlepack", weight = Abundance) + 
      geom_node_circle(aes(fill=Taxonomy, color=Taxonomy)) +
      scale_fill_discrete(na.value = NA) +  # do not color NA values
      coord_fixed() + # square
      ggtitle(this_depth) +
      theme_void() + 
      scale_fill_manual(values = c(
                                    "Bacteroidetes"="#efec5e",
                                    "Chloroflexi"="#33a02c",
                                    "Deltaproteobacteria"="#ff7f00",
                                    "Marinimicrobia"="#1f78b4",
                                    "Myxococcota"="#fdbf6f",
                                    "Nitrospina"="#a6cee3",
                                    "Others"="#cab2d6",
                                    "Planctomycetes"="#b2df8a",
                                    "Vecturithrix"="#e31a1c",
                                    "Verrucomicrobiota"="#6a3d9a",
                                   "PA"=NA,
                                   "FL"=NA,
                                   "root"=NA)) +
      scale_color_manual(values = c(
                                  "Bacteroidetes"=NA,
                                  "Chloroflexi"=NA,
                                  "Deltaproteobacteria"=NA,
                                  "Marinimicrobia"=NA,
                                  "Myxococcota"=NA,
                                  "Nitrospina"=NA,
                                  "Others"=NA,
                                  "Planctomycetes"=NA,
                                  "Vecturithrix"=NA,
                                  "Verrucomicrobiota"=NA,
                                   "PA"="#00bfc4",
                                   "FL"="#f8766d",
                                  "root"=NA))
      n <- n+1
}

plot_list[[1]]/plot_list[[2]]/plot_list[[3]]/plot_list[[4]]/plot_list[[5]]/plot_list[[6]]/plot_list[[7]]
ggsave('packing_circles_7_depths.pdf', width = 5, height = 35, limitsize = FALSE)
