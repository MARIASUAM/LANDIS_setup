#### Ecoregions definition ####

## Pre-code notes
# Input data to be used for clustering need to be aligned. This is done in qGIS
# - Check source CRS of all layers: 3042
# - Reproject if needed
# - Align rasters (Raster > Align Rasters...)
#   - Add all layers
#   - Give output filename
#   - Rescale values: NO
#   - Alignment configuration: choose reference layer
#   - CRS: EPSG 3042
#   - cell size: 100 x 100
#   - Clip to extent (user defined):
        # xmin       : 439672.6 
        # xmax       : 539668.2 
        # ymin       : 4081143 
        # ymax       : 4151141 

# Packages and folders
# devtools::install_github("https://github.com/bleutner/RStoolbox.git") # to avoid a warning message fixed in latest version of the package
library(RStoolbox)
library(raster)

# Define inputs and outputs
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"
input_files <- list.files(paste(di, inputs, sep = ""), pattern = c("*_aligned")) #
  
# Load raster of mask
aoi <- raster(paste(di, outputs, "study_area_raster_3042.asc", sep = ""))
plot(aoi)
extent(aoi)

# Create brick with climate cropped data
my_stack <- stack()

# Some layers in input folder
some_inputs <- c(...) # indices for files: "p_1971_2000_aligned", "tmax_1971_2000_aligned", "tmed_1971_2000_aligned", "tmin_1971_2000_aligned")
for (i in 1:length(some_inputs)) {
  j <- some_inputs[i]
  print(input_files[j])
  assign("temp", raster(paste(di, inputs, input_files[j], sep = "")))
  my_stack <- stack(my_stack, temp)
}
plot(my_stack)

my_brick <- NULL
my_brick <- brick(my_stack)

my_brick <- my_brick * aoi
plot(my_brick)

my_stack <- my_stack * aoi
plot(my_stack)

## Run example unsupervised classifications with different algorithms
# Notes: https://www.mailman.columbia.edu/research/population-health-methods/cluster-analysis-using-k-means
set.seed(25)
nClass <- 7
name <- paste(nlayers(my_brick), nClass, sep = "_")

# Hartigan-Wong kmeans algorithm
# algorithm <- "Hartigan-Wong"
# classHW <- unsuperClass(my_brick, nClasses=nClass, nStarts=nClass*4, nIter=2000, norm=T, clusterMap=F, algorithm = algorithm) # If Warning: "Quick-TRANSfer stage steps exceeded maximum", better other algorithm
# plot(classHW$map)
# classHW; t.classHW <- table(classHW$map[]); t.classHW
# writeRaster(classHW$map, filename = paste(di, outputs, name, algorithm, sep = ""), format = "GTiff")

# Lloyd kmeans algorithm
# algorithm <- "Lloyd"
# classLL <- unsuperClass(my_brick, nClasses=nClass, nStarts=nClass*4, nIter=2000, norm=T, clusterMap=F, algorithm = algorithm)
# plot(classLL$map)
# classLL; t.classLL <- table(classLL$map[]); t.classLL
# writeRaster(classLL$map, filename = paste(di, outputs, name, algorithm, sep = ""), format = "GTiff")

# MacQueen kmeans algorithm
# algorithm <- "MacQueen"
# classMQ <- unsuperClass(my_brick, nClasses=nClass, nStarts=nClass*4, nIter=2000, norm=T, clusterMap=F, algorithm = algorithm)
# plot(classMQ$map)
# classMQ; t.classMQ <- table(classMQ$map[]); t.classMQ
# writeRaster(classMQ$map, filename = paste(di, outputs, name, algorithm, sep = ""), format = "GTiff")

# Example run shows no differences among algorithms

## Choose appropiate number of classes
algorithm <- "MacQueen"
wss <- 0 # Initialize total within sum of squares error: wss
for (i in 1:10) { # Look over 1 to 10 possible clusters
  out <- unsuperClass(my_brick, nClasses=i, nStarts=i*4, nIter = 2000, norm=T, clusterMap=F, algorithm = algorithm)
  wss[i] <- out$model$tot.withinss # Save the within cluster sum of squares
}
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within groups sum of squares")

## Run and export clustering for optimal number of classes
optimal_nclasses <- 4 # ** BASED ON PREVIOUS PLOT **
clust <- unsuperClass(my_brick, 
                      nClasses = optimal_nclasses, 
                      nStarts = optimal_nclasses*4, 
                      nIter=2000, norm=T, clusterMap=F, algorithm = "MacQueen")
plot(clust$map)
clust_name <- paste(nlayers(my_brick), optimal_nclasses, sep = "_")
writeRaster(clust$map, filename = paste(di, outputs, clust_name, "_clustering",sep = ""), format = "GTiff", overwrite = TRUE)

## Overview of clusters characteristics
stack_with_layer <- stack(my_stack, clust$map)
brick_with_layer <- brick(stack_with_layer)
# plot(brick_with_layer)

names(brick_with_layer) <- c(input_files[some_inputs[1]], 
                             input_files[some_inputs[2]],
                             input_files[some_inputs[3]], 
                             input_files[some_inputs[4]],
                             "clust_classes")

summary_classes <- data.frame(class = c(1:optimal_nclasses), 
                              mean_precip = NA, min_precip = NA, max_precip = NA, 
                              mean_tmax = NA, min_tmax = NA, max_tmax = NA,
                              mean_tmed = NA, min_tmed = NA, max_tmed = NA,
                              mean_tmin = NA, min_tmin = NA, max_tmin = NA)


for (i in 1:optimal_nclasses) {
  cellvalues <- which(getValues(brick_with_layer$clust_classes == i))
  subset <- raster::extract(x = brick_with_layer, 
                            y = cellvalues, 
                            nl = 4)
  summary_classes$mean_precip[i] <- mean(subset[,1])
  summary_classes$min_precip[i] <- min(subset[,1])
  summary_classes$max_precip[i] <- max(subset[,1])
  summary_classes$mean_tmax[i] <- mean(subset[,2])
  summary_classes$min_tmax[i] <- min(subset[,2])
  summary_classes$max_tmax[i] <- max(subset[,2])
  summary_classes$mean_tmed[i] <- mean(subset[,3])
  summary_classes$min_tmed[i] <- min(subset[,3])
  summary_classes$max_tmed[i] <- max(subset[,3])
  summary_classes$mean_tmin[i] <- mean(subset[,4])
  summary_classes$min_tmin[i] <- min(subset[,4])
  summary_classes$max_tmin[i] <- max(subset[,4])
}

write.csv(summary_classes, 
          paste(di, outputs, "summary_classes_", clust_name, ".csv", sep = ""), 
          row.names = FALSE)
