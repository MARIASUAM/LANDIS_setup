### Climate files generation - PERIOD 2005-2019 ###

library(raster)
library(dplyr)

# Define inputs and outputs
di <- "..."
outputs <- "output_files/"

# Input files
# Input files come from REDIAM. 
# Raster layers for the whole Andalusia have been downloaded and then cropped and aligned through qGIS:
# - Align rasters (Raster > Align Rasters...)
#   - Add all layers (done year by year)
#   - Give output filename
#   - Resampling method: default (it won't be used)
#   - Rescale values: NO
#   - Alignment configuration: reference layer --> ecoregions.asc
#   - CRS: EPSG 3042
#   - cell size: 100 x 100
#   - Clip to extent (user defined):
# xmin       : 439672.6 
# xmax       : 539668.2 
# ymin       : 4081143 
# ymax       : 4151141

# Load raster of mask
aoi <- raster(paste(di, outputs, "study_area_raster_3042.asc", sep = ""))
# plot(aoi)
# extent(aoi)

# Load ecoregions file
climateregions <- raster(paste(di, outputs, "4_4_clustering.tif", sep = ""))
# plot(climateregions)
df <- data.frame(Climate_region = climateregions[]) # Create data frame to fill it with climate data

# Maximum temperatures
di_tmax <- "..."
input_files_tmax <- list.files(di_tmax, pattern = c("*_aligned")) # tmax input files
tmax_df <- data.frame() # Create df with Tmax data
for (i in 1:length(input_files_tmax)) {
  print(input_files_tmax[i])
  temp <- raster(paste(di_tmax, input_files_tmax[i], sep = "/")) * aoi
  temp_df <- df
  temp_df$tmax <- temp[]
  temp_df <- temp_df %>%
    group_by(Climate_region) %>%
    summarise_all(mean) %>%
    filter(is.na(Climate_region) == FALSE)
  temp_df <- as.data.frame(t(as.matrix(temp_df)))[2,]
  temp_df$Year <- strsplit(input_files_tmax[i], split = "_")[[1]][2]
  temp_df$Month <- strsplit(input_files_tmax[i], split = "_")[[1]][3]
  tmax_df <- rbind(tmax_df, temp_df)
}
colnames(tmax_df) <- c("tmax_climate_region_1", 
                       "tmax_climate_region_2", 
                       "tmax_climate_region_3", 
                       "tmax_climate_region_4", 
                       "Year", "Month")

# Minimum temperatures
di_tmin <- "..."
input_files_tmin <- list.files(di_tmin, pattern = c("*_aligned")) # tmin input files
tmin_df <- data.frame() # Create df with Tmin data
for (i in 1:length(input_files_tmin)) {
  print(input_files_tmin[i])
  temp <- raster(paste(di_tmin, input_files_tmin[i], sep = "/")) * aoi
  temp_df <- df
  temp_df$tmin <- temp[]
  temp_df <- temp_df %>%
    group_by(Climate_region) %>%
    summarise_all(mean) %>%
    filter(is.na(Climate_region) == FALSE)
  temp_df <- as.data.frame(t(as.matrix(temp_df)))[2,]
  temp_df$Year <- strsplit(input_files_tmin[i], split = "_")[[1]][2]
  temp_df$Month <- strsplit(input_files_tmin[i], split = "_")[[1]][3]
  tmin_df <- rbind(tmin_df, temp_df)
}
colnames(tmin_df) <- c("tmin_climate_region_1", 
                       "tmin_climate_region_2", 
                       "tmin_climate_region_3", 
                       "tmin_climate_region_4", 
                       "Year", "Month")

# Precipitation
di_precip <- "..."
input_files_precip <- list.files(di_precip, pattern = c("*_aligned")) # precip input files
prec_df <- data.frame() # Create df with prec data
for (i in 1:length(input_files_precip)) {
  print(input_files_precip[i])
  temp <- raster(paste(di_precip, input_files_precip[i], sep = "/")) * aoi
  temp_df <- df
  temp_df$prec <- temp[]
  temp_df <- temp_df %>%
    group_by(Climate_region) %>%
    summarise_all(mean) %>%
    filter(is.na(Climate_region) == FALSE)
  temp_df <- as.data.frame(t(as.matrix(temp_df)))[2,]
  temp_df$Year <- strsplit(input_files_precip[i], split = "_")[[1]][2]
  temp_df$Month <- strsplit(input_files_precip[i], split = "_")[[1]][3]
  prec_df <- rbind(prec_df, temp_df)
}
colnames(prec_df) <- c("prec_climate_region_1", 
                       "prec_climate_region_2", 
                       "prec_climate_region_3", 
                       "prec_climate_region_4", 
                       "Year", "Month")

prec_df$prec_climate_region_1 <- as.numeric(prec_df$prec_climate_region_1)
prec_df$prec_climate_region_2 <- as.numeric(prec_df$prec_climate_region_2)
prec_df$prec_climate_region_3 <- as.numeric(prec_df$prec_climate_region_3)
prec_df$prec_climate_region_4 <- as.numeric(prec_df$prec_climate_region_4)

# Generate climate files

climate <- full_join(tmax_df, tmin_df) %>%
  full_join(prec_df)

clim_2005_2019_reg1 <- climate %>%
  select(Year, Month, tmax_climate_region_1, tmin_climate_region_1, prec_climate_region_1)
colnames(clim_2005_2019_reg1) <- c("Year", "Month", "Tmax", "Tmin", "Prec")
write.table(clim_2005_2019_reg1, 
            file = paste(di, outputs, "clim_2005_2019_reg1.txt", sep = ""), row.names = FALSE)

clim_2005_2019_reg2 <- climate %>%
  select(Year, Month, tmax_climate_region_2, tmin_climate_region_2, prec_climate_region_2)
colnames(clim_2005_2019_reg2) <- c("Year", "Month", "Tmax", "Tmin", "Prec")
write.table(clim_2005_2019_reg2, 
            file = paste(di, outputs, "clim_2005_2019_reg2.txt", sep = ""), row.names = FALSE)

clim_2005_2019_reg3 <- climate %>%
  select(Year, Month, tmax_climate_region_3, tmin_climate_region_3, prec_climate_region_3)
colnames(clim_2005_2019_reg3) <- c("Year", "Month", "Tmax", "Tmin", "Prec")
write.table(clim_2005_2019_reg3, 
            file = paste(di, outputs, "clim_2005_2019_reg3.txt", sep = ""), row.names = FALSE)

clim_2005_2019_reg4 <- climate %>%
  select(Year, Month, tmax_climate_region_4, tmin_climate_region_4, prec_climate_region_4)
colnames(clim_2005_2019_reg4) <- c("Year", "Month", "Tmax", "Tmin", "Prec")
write.table(clim_2005_2019_reg4, 
            file = paste(di, outputs, "clim_2005_2019_reg4.txt", sep = ""), row.names = FALSE)
