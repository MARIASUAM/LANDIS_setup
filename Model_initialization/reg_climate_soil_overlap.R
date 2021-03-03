# Climate regions overlap with soil textures

library(raster)
library(dplyr)

di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load input data
## Climate regions
climate_regions <- raster(paste(di, outputs, "4_4_clustering.tif", sep = ""))
plot(climate_regions)

# Load soil textures, crop by aoi and legend
soiltext <- raster(paste(di, inputs, "soil_texture_aligned.tif", sep = ""))
plot(soiltext)
aoi <- raster(paste(di, outputs, "study_area_raster_3042.asc", sep = ""))
plot(aoi)
soiltext <- soiltext * aoi
soillegend <- read.csv(paste(di, outputs, "soil_texture_map_legend.csv", sep = ""))
soillegend$ID <- as.factor(soillegend$ID)

# Combine the climatic zones with the soil types to build the ecoregions code
regions <- data.frame(mask=aoi[], 
                      clim=climate_regions[], 
                      soiltext=soiltext[])
regions$clim[is.na(regions$mask)] <- NA
regions$soil[is.na(regions$mask)] <- NA
regions$eco <- paste0(regions$clim, regions$soil)
regions$eco[regions$eco=="NANA"] <- NA
# counts.ini <- as.data.frame(table(regions$eco))
# counts.ini; sum(counts.ini$Freq)

## Write the ecoregions raster
ECOREGION <- aoi
ECOREGION[] <- as.numeric(regions$eco)
# writeRaster(ECOREGION, file=paste(di, outputs, "ecoregions.asc", sep = ""), format="ascii", overwrite=T, NAflag=0)

## GENERATE ECOREGIONS TABLE OF PARAMETERS
eco.code <- names(table(regions$eco)) # Generate ecoregion codes
eco.file <- data.frame(Ecoregion = eco.code) # Create ecoregions file
clim.code <- substr(eco.code, 1, 1) # Generate climate codes
eco.file$ClimateFileName <- paste0(".../clim_reg", clim.code, ".txt") # Generate climate files
soil.code <- data.frame(id=as.character(substr(eco.code, 2, 2))) # Generate soil codes
soil.code <- left_join(soil.code, soillegend, by = c("id" = "ID")) # Add soil types
eco.file$SoilType <- soil.code$VALUE
eco.file$EcoregionParameters = paste("eco", eco.code, sep = "") # Add EcoregionParameters

# Add Rooting Depth
soil_depth_classes <- raster(paste(di, inputs, "soil_depth_aligned.tif", sep = "")) # Load soil depth
# res(soil_depth_classes)
soil_depth_classes <- soil_depth_classes * aoi # Crop by aoi
plot(soil_depth_classes)
soil_depth_legend <- read.csv(paste(di, outputs, "soil_depth_legend.csv", sep = "")) # Load soil depth legend
soil_depth_legend$Depth_code <- as.factor(soil_depth_legend$Depth_code)
depthvalues <- as.data.frame(raster::extract(soil_depth_classes, coordinates(soil_depth_classes), cellnumbers = T)) # Extract depth cell values 
depthvalues$layer <- as.factor(depthvalues$layer)
depthvalues <- depthvalues %>% # Add legend
  left_join(soil_depth_legend, by = c("layer" = "Depth_code"))
regions$soildepth <- depthvalues$mean_soil_depth_mm # Merge with regions
soil_depth_by_ecoregion <- regions %>% # Calculate soildepth by ecoregion as the modal of its cell values
  dplyr::select(eco, soildepth) %>%
  group_by(eco) %>%
  summarise(RootingDepth = modal(soildepth))
eco.file <- eco.file %>% # Add RootingDepth to eco.file
  left_join(soil_depth_by_ecoregion, by = c("Ecoregion" = "eco"))

# Reproyect ECOREGION: from 3042 (projected) to 4326 (unprojected)
ECOREGION_3042 <- ECOREGION
crs(ECOREGION_3042) <- crs(climate_regions)
proj_4326 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
ECOREGION_4326 <- projectRaster(from = ECOREGION_3042, crs = proj_4326, method = "ngb") 

# Add latitude of Ecoregions
eco.file$Latitude <- NA
cellvalues <- as.data.frame(raster::extract(ECOREGION_4326, coordinates(ECOREGION_4326), cellnumbers = T))
cellvalues$Lon <- coordinates(ECOREGION_4326)[,1]
cellvalues$Lat <- coordinates(ECOREGION_4326)[,2]

for (i in 1:length(eco.file$Ecoregion)) {
  cellval <- cellvalues %>% 
    dplyr::filter(study_area_raster_3042 == eco.file$Ecoregion[i])
  eco.file$Latitude[i] <- mean(cellval$Lat)
}

# Include hydraulic properties
eco.file$SnowSublimFrac <- 0.15 # default value
eco.file$PrecLossFrac <- 0.6 # default value
eco.file$LeakageFrac <- 1 # default value

# Fetch eco.file
eco.file <- eco.file %>%
  dplyr::select(EcoregionParameters,
                SoilType,
                Latitude,
                RootingDepth,
                PrecLossFrac, 
                LeakageFrac,
                SnowSublimFrac,
                ClimateFileName)

write.table(eco.file, paste(di, outputs, "ecoregions_file.csv", sep = ""), 
            row.names = F, quote=F, sep="\t") 

# plot(ECOREGION, col=rainbow(nrow(eco.file)), legend=T, axes=T, box=T, main="Ecoregions map")
