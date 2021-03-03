# Transform ecoregions and IC maps datatypes

library(raster)

# Setup folders
di <- "..."
outputs <- "output_files/"

# Load files
IC_map <- raster(paste(di, outputs, "IC_map.tif", sep = ""))
ecoregions_map <- raster(paste(di, outputs, "ecoregions_map.tif", sep = ""))

# Export maps
# writeRaster(IC_map, paste(di, outputs, "LANDIS_IC_map.tif", sep = ""), datatype='INT4S', NAflag=0)
# writeRaster(ecoregions_map, paste(di, outputs, "LANDIS_ecoregions_map.tif", sep = ""), datatype='INT4S', NAflag=0)
