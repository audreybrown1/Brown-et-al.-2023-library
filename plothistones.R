#to create chromosomal diagram of P. pacificus or C. elegans histone positions
#Required is csv file with following columns: Species, Histonetype,Canonical/Variant/H1, Chromosome, Start 
#CSV files used to create plots in manuscript included (PacificusHistones.csv, ElegansHistones.csv), these also include 
#information on Gene name and whether the contains a 3' UTR hairpin

setwd("~/path/to/directory")

# Import the CSV file with gene positions
data <- read.csv("PacificusHistones.csv")

#Make Dataframe with C. elegans and P. pacificus chromosome lengths
Pchrom<-c(39556110,23310661,21666382,30933000,23914600,17019893)
Cchrom<-c(15072434,15279421,13783801,17493829,20924180,17718942)
vchrom<-c(1,2,3,4,5,6)
nchrom<-c('1','2','3','4','5','X')
chrom<-data.frame(Pchrom, Cchrom, nchrom,vchrom)

#Count the number of Chromosomes
num_unique_chromosomes <- length(unique(data$Chromosome))

# Define a unique color for each Histone type
His_colors <- rainbow(length(unique(data$Histonetype)))

#Define a point shape for canonical/variant/H1 histones
His_shape <- c(17, 18, 15)

#Define whether to use C. elegans or P. pacificus chromosome lengths
sp=data$Species[1]
if (sp == "Pacificus"){
  chrdata=chrom$Pchrom
} else {
  chrdata=chrom$Cchrom
}

# Making plot
plot(x = c(0, max(data$Start)+10000000), y = c(1, num_unique_chromosomes + 1), type = "n", xlab = "nucleotides", ylab = "Chromosomes", main = data$Species[1])
chrom_y_values <- numeric(num_unique_chromosomes)

for (i in 1:num_unique_chromosomes) {
  y_value <- i
  chrom_y_values[i] <- y_value
  
  chrom_data <- subset(data, Chromosome == unique(data$Chromosome)[i])
  min_Start <- 0
  max_Start <- chrdata[i]
  segments(x0 = min_Start, x1 = max_Start, y0 = y_value, y1 = y_value, col = "black")
}

# Add labels for the Chromosomes on the left side of the lines
for (i in 1:num_unique_chromosomes) {
  y_value <- chrom_y_values[i]
  text(x = 2, y = chrom$vchrom[i], labels = chrom$nchrom[i], pos = 2, col = "black")
}

# Plotting histone genes
for (i in 1:nrow(data)) {
  y_value <- match(data$Chromosome[i], chrom$nchrom)
  col_idx <- match(data$Histonetype[i], unique(data$Histonetype))
  shape_idx<-match(data$Canonical.Variant.H1[i], unique(data$Canonical.Variant.H1))
  points(data$Start[i], y_value, pch=His_shape[shape_idx],col = His_colors[col_idx])
}
     
     

     
     
     
     
