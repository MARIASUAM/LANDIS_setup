# Calculate soil textures based on USDA classes

# Packages and folders
library(raster)
library(dplyr)
library(soiltexture)
library(tidyr)
di <- "..."
inputs <- "input_files"
outputs <- "output_files"

# Load input files
sand <- raster(paste(di, inputs, "sand_aoi.tif", sep = "/"))
names(sand) <- c("SAND")
clay <- raster(paste(di, inputs, "clay_aoi.tif", sep = "/"))
names(clay) <- c("CLAY")
silt <- raster(paste(di, inputs, "silt_aoi.tif", sep = "/"))
names(silt) <- c("SILT")
# plot(clay)

textures <- brick(sand, clay, silt)

# Build table
soil_composition <- data.frame(cell = 1:ncell(textures),
  CLAY = values(textures$CLAY),
  SILT = values(textures$SILT),
  SAND = values(textures$SAND))

# Calculate soil classes
soil_composition <- cbind(soil_composition,
                          TT.points.in.classes(tri.data = soil_composition, class.sys = "USDA.TT"))
colnames(soil_composition) <- c("cell", "perc_CLAY", "perc_SILT", "perc_SAND", "CLAY", "SICL", "SACL", "CLLO", "SLCL", "SNCL", "LOAM", "SILO", "SALO", "SILT", "LOSA", "SAND")
soil_composition <- soil_composition %>% 
  mutate(check = CLAY + SICL + SACL + CLLO + SLCL + SNCL + LOAM + SILO + SALO + SILT + LOSA + SAND)
# classes$cell <- as.factor(rownames(classes))
missing_classes <- soil_composition %>% filter(check == 0) %>% dplyr::select(cell)

soil_classes <- soil_composition[,c(1,5:16)] %>%
  gather(key = classes, value = Soil_class, c(CLAY, SICL, SACL, CLLO, SLCL, SNCL, LOAM, SILO, SALO, SILT, LOSA, SAND)) %>%
  filter(Soil_class >= 1) %>%
  dplyr::select(cell, classes)
colnames(soil_classes) <- c("cell", "soil_class")

missing_classes$soil_class <- NA # include cells without soil class assigned
soil_classes <- rbind(soil_classes, missing_classes) 

soil_classes$soil_class <- as.factor(soil_classes$soil_class) # soil class as factor
rownames(soil_classes) <- soil_classes$cell # assign cell code to rownames

# Produce raster file

soil_classes <- soil_classes[order(soil_classes$cell),]

soil_map <- raster(nrows = nrow(textures), ncols = ncol(textures), 
                       xmn = xmin(textures), xmx = xmax(textures),
                       ymn = ymin(textures), ymx = ymax(textures),
                       vals = as.factor(soil_classes$soil_class))

# plot(soil_map)
soil_texture_map_legend <- data.frame(soil_map@data@attributes[[1]])
crs(soil_map) <- "+proj=utm +zone=30 +ellps=intl +units=m +no_defs" # EPSG 23030
writeRaster(x = soil_map, filename = paste(di, outputs, "soil_texture_map_23030", sep = "/"),
            format = "GTiff", overwrite = TRUE)

# Reproject soil_map
soil_map@data@values <- as.factor(soil_map@data@values)
proj3042 <- "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
soil_map_3042 <- projectRaster(soil_map, crs = proj3042, method = "ngb") # Reproject
# plot(soil_map_3042)
writeRaster(x = soil_map_3042, filename = paste(di, outputs, "soil_texture_map_3042", sep = "/"),
            format = "GTiff", overwrite = TRUE)

write.csv(soil_texture_map_legend, paste(di, outputs, "soil_texture_map_legend.csv", sep = "/"))

