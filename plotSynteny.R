#to create synteny plots from gene position and orthogroup information
#Required is csv file with following columns: Orthogroup, Species, Scaffold, Start, End
#CSV files used to create plots in manuscript included (genepositions_MES2.csv, genepositions_MES6.csv)

setwd("~/Path/to/Directory")

# Import the CSV file with gene positions
data <- read.csv("genepositions_MES2.csv")

# For plotting purposes, calculate "new" start and end values for each gene centered around 0
median_starts <- aggregate(Start ~ Species, data=data, FUN=median)
colnames(median_starts) <- c("Species", "Median_Start")
data <- merge(data, median_starts, by="Species")
data$nStart <- data$Start - data$Median_Start
data$nEnd <- data$End - data$Median_Start

#Count the number of unique Species
num_unique_species <- length(unique(data$Species))

# Define a unique color for each Orthogroup
orthogroup_colors <- rainbow(length(unique(data$Orthogroup)))

# Making plot
plot(x = c(min(data$nStart), max(data$nStart)), y = c(1, num_unique_species + 1), type = "n", xlab = "nStart", ylab = "", main = "Horizontal Line Plot")
species_y_values <- numeric(num_unique_species)

for (i in 1:num_unique_species) {
  y_value <- i
  species_y_values[i] <- y_value
  
  species_data <- subset(data, Species == unique(data$Species)[i])
  min_nStart <- min(species_data$nStart)
  max_nStart <- max(species_data$nStart)
  segments(x0 = min_nStart, x1 = max_nStart, y0 = y_value, y1 = y_value, col = "black")
}

# Add labels for the Species on the left side of the lines
for (i in 1:num_unique_species) {
  y_value <- species_y_values[i]
  text(x = min(data$nStart) - 2, y = y_value, labels = unique(data$Species)[i], pos = 2, col = "black")
}

# Plotting genes
for (i in 1:nrow(data)) {
  species_idx <- match(data$Species[i], unique(data$Species))
  y_value <- species_y_values[species_idx]
  col_idx <- match(data$Orthogroup[i], unique(data$Orthogroup))
  rect(data$nStart[i], y_value - 0.125, data$nEnd[i], y_value + 0.125, col = orthogroup_colors[col_idx], border = "black")
}



