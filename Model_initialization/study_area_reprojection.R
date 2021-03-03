library(raster)
library(rgdal)
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

mask_shp <- readOGR(dsn = paste(di, inputs, sep = ""), layer = "study_area")
projection(mask_shp) <- "+proj=utm +zone=30 +ellps=intl +units=m +no_defs"

proj3042 <- "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
mask_shp_3042 <- spTransform(mask_shp, CRS(proj3042))

plot(mask_shp_3042)
extent(mask_shp_3042)
