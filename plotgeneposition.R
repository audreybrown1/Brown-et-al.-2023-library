##Code to plot gene positions from a csv file
#csv in form:Orthogroup  Species/chromosome Genename  Scaffold  start end 
#plots start and stop positions of genes

genepos=syntdata4

setwd("~/path/to/directory/")

#load in required libraries
library(ggplot2)
library(ggsignif)
library(RColorBrewer)

#Load in csv file containing gene positions 
genepos <- read.csv("genepositions.csv")
genepos$Orthogroup<-as.factor(genepos$Orthogroup)

#get genes so they can be plotted
sp=unique(genepos$Species)
for (n in 1:length(sp)) {
  sub=subset(genepos, Species == sp[n])
  M=median(sub$start)
  print(M)
  for (j in 1:dim(genepos)[1]){
    if (genepos$Species[j] == sp[n]) {
      genepos$tographst[j] <- genepos$start[j]-M
      genepos$tographend[j] <- genepos$end[j]-M
    }
  }
}

#Getting color palate
n <- 23
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

#plotting gene positions
ggplot(data=genepos) +
  geom_line(aes(x=tographst, y=Species))+
  geom_point(aes(x=tographst, y=Species,color=Orthogroup))+
  geom_point(aes(x=tographend, y=Species,color=Orthogroup))+
  scale_y_discrete(limits = c("names", "of", "species"))+ 
  scale_color_manual(values = col_vector)+
  theme(legend.position = "none")
 

