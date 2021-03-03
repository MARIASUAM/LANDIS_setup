# Calcula ecoregions soil depth

# Packages and folders
library(raster)
library(rgdal)

di <- "..."
inputs <- "input_files/"
outputs <- "output_files"
path <- "..."

# Load mask as reference
mask_shp <- readOGR(dsn = paste(di, inputs, sep = ""), layer = "study_area")
proj4string(mask_shp)
plot(mask_shp)

# Load soil depth classes file and mask
soildepth_classes <- raster(paste(di, inputs, "sta.adf", sep = ""))
soildepth_classes <- crop(soildepth_classes, mask_shp, snap = 'in')
crs(soildepth_classes) <- "+proj=utm +zone=30 +ellps=intl +units=m +no_defs" # EPSG 23030
plot(soildepth_classes)
soildepth_classes@data@values <- as.factor(soildepth_classes@data@values)

# Generate soil depth legend
soildepth_legend <- data.frame(Depth_code = c(1,2,3,4,5),
                               Soil_depth_range_mm = c("0-250", "250-500", "500-1000", "1000-1500", ">1500"),
                               mean_soil_depth_mm = c(125, 375, 750, 1250, 1750))

# Reproject soil depth classes map
proj3042 <- "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
soil_depth_3042 <- projectRaster(soildepth_classes, crs = proj3042, method = "ngb") # Reproject
# plot(soil_depth_3042)

# Export map and legend
writeRaster(x = soil_depth_3042, filename = paste(di, outputs, "soil_depth_3042", sep = "/"),
            format = "GTiff", overwrite = TRUE)
write.csv(soildepth_legend, paste(di, outputs, "soil_depth_legend.csv", sep = "/"))
