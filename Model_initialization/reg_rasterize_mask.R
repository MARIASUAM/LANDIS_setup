library(rgdal)
library(raster)

di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load mask shape
mask_shp <- readOGR(dsn = paste(di, inputs, sep = ""), layer = "study_area_3042")
crs(mask_shp)
ext <- extent(mask_shp)

# Load reference raster
ref <- raster(paste(di, inputs, "soil_texture_aligned", sep = ""))
ref

# Create empty raster
res(ref)
gridsize <- 100
mask_raster <- raster(extent(ref), res = gridsize, vals = NA)

# Rasterize mask
mask_raster <- rasterize(mask_shp, mask_raster, field = mask_shp@data[,1])
crs(mask_raster) <- crs(mask_shp)
plot(mask_raster)
mask_raster <- mask_raster - 1
mask_raster[mask_raster == 0] <- NA
plot(mask_raster)

writeRaster(mask_raster, filename = paste(di, outputs, "study_area_raster_3042.asc", sep = ""), format="ascii", overwrite=TRUE) 
