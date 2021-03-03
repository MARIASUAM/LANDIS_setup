# Read vegetation map and generate raster files for relevant attributes

library(rgdal)
library(rgeos)
library(raster)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load IC raster map
ic_map <- raster(paste(di, outputs, "ic_map_3042.asc", sep = ""))

# Generate mask for shrubs and crops
shrublands_crops_map <- ic_map
shrublands_crops_map[shrublands_crops_map == 1001] <- 1003
shrublands_crops_map[shrublands_crops_map == 1002] <- 1003
shrublands_crops_map[shrublands_crops_map != 1003] <- NA
# plot(shrublands_crops_map)

# Generate mask for whole active area
active <- ic_map
active[active == 999] <- NA
active[active == 998] <- NA
active[active == 997] <- NA
active[active == 996] <- NA
active[is.na(active) == FALSE] <- 1
plot(active)

# Load input file: vege10_aoi_active cut from VEGETACION10000 (REDIAM) by Curro with ArcGIS
vege10 <- readOGR(dsn = paste(di, inputs, sep = ""), layer = "vege10_aoi_active")
crs(vege10)

# Select relevant attributes
attributes <- c(#"D_COM1", # "D_COM2", "D_COM3", "D_COM4", "D_COM5", "D_COM6", "D_COM7", "D_COM8", # not all considered
                #"D_COM1_COB", # "D_COM2_COB", "D_COM3_COB", "D_COM4_COB", "D_COM5_COB", "D_COM6_COB", "D_COM7_COB", "D_COM8_COB", # not all levels considered
                # "D_CO1_ETAP", # "D_CO2_ETAP", "D_CO3_ETAP", "D_CO4_ETAP",  "D_CO5_ETAP", "D_CO6_ETAP", "D_CO7_ETAP", "D_CO8_ETAP", # not all levels considered
                # "D_ARBO_PRE", "D_ARBU_PRE", "D_HERB_PRE", "D_SUEL_PRE",
                "D_ARBO1_SP", "D_ARBO2_SP", "D_ARBO3_SP", "D_ARBO4_SP", "D_ARBO5_SP", "D_ARBO6_SP",
                "D_ARBU1_FO", "D_ARBU2_FO", "D_ARBU3_FO", "D_ARBU4_FO",
                "D_HERB1_FO", "D_HERB2_FO", "D_HERB3_FO",
                "COMENTARIO")

### Rasterize relevant attributes and mask by shrubs and crops
vege_stack <- stack()
for (i in 1:length(attributes)) {
  print(attributes[i])
  temp <- rasterize(vege10, shrublands_crops_map, field = attributes[i], fun = 'last', background = NA)
  temp <- mask(temp, shrublands_crops_map)
  writeRaster(temp, paste(di, outputs, "shrubs_crops_vege10_", attributes[i], "_25830.asc", sep = ""), overwrite = TRUE)
  vege_stack <- stack(vege_stack, temp)
}

# Generate legends and export
for (i in 1:length(attributes)) {
  legend <- data.frame(Code = 1:length(levels(vege10@data[, attributes[i]])),
                       Value = levels(vege10@data[, attributes[i]]))
  write.csv(legend, paste(di, outputs, "legend_shrubs_crops_vege10_", attributes[i], ".csv", sep = ""), row.names = FALSE)
}

### Rasterize relevant attributes and mask for whole active area

attributes_whole <- c("D_ARBU1_FO", "D_ARBU2_FO", "D_ARBU3_FO", "D_ARBU4_FO",
                "D_HERB1_FO", "D_HERB2_FO", "D_HERB3_FO",
                "COMENTARIO")

vege_stack <- stack()

for (i in 1:length(attributes_whole)) {
  print(attributes_whole[i])
  temp <- rasterize(vege10, active, field = attributes_whole[i], fun = 'last', background = NA)
  temp <- mask(temp, active)
  writeRaster(temp, paste(di, outputs, "active_vege10_", attributes_whole[i], "_25830.asc", sep = ""), overwrite = TRUE)
  vege_stack <- stack(vege_stack, temp)
}

# Generate legends and export
for (i in 1:length(attributes_whole)) {
  legend <- data.frame(Code = 1:length(levels(vege10@data[, attributes_whole[i]])),
                       Value = levels(vege10@data[, attributes_whole[i]]))
  write.csv(legend, paste(di, outputs, "legend_active_vege10_", attributes_whole[i], ".csv", sep = ""), row.names = FALSE)
}
