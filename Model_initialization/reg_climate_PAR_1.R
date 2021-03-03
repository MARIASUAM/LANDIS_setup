### Climate files generation - PAR ###

# install.packages("ncdf4")
library(ncdf4)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(rgdal)
library(raster)
library(sp)
# install.packages("proj4")
library(proj4)

# Define inputs and outputs
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load input files - global radiation (surface downwelling shortwave flux in air)
# Daily data for Europe 1980-2019 downloaded from https://www.ecad.eu/download/ensembles/download.php
# Units: W/m2
# file_name_full <- "qq_ens_mean_0.1deg_reg_v20.0e.nc" # Full file
# par_full <- nc_open(filename = paste(di_inputs, file_name_full, sep = "/"))
# nc_close(par_full)

# aoi coordinates (needed to cut .nc file)
# aoi <- readOGR(dsn = paste(di, "input_files/", sep = ""), layer = "study_area_3042")
# plot(aoi)
# aoi_ext <- extent(aoi)
# crs(aoi)
# coor_ext <- data.frame(LONG = c(aoi_ext@xmin, aoi_ext@xmin, aoi_ext@xmax, aoi_ext@xmax), 
#                        LAT = c(aoi_ext@ymin, aoi_ext@ymax, aoi_ext@ymin, aoi_ext@ymax))
# Transform aoi extent coordinates
# proj4string <- "+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# coor_ext$long_UTM <- c()
# coor_ext$lat_UTM <- c()
# for (i in 1:length(coor_ext$LONG)) {
#   pj <- project(coor_ext[i, c(1:2)], proj4string, inverse = TRUE)
#   coor_ext$long_UTM[i] <- pj$x
#   coor_ext$lat_UTM[i] <- pj$y
# }

# Load input file - monthly average for aoi
# File obtained from executing (in terminal): 
# cdo monmean qq_ens_mean_0.1deg_reg_v20.0e.nc monmean.nc
# cdo sellonlatbox,-3.683726,-2.552329,36.87308,37.50515 europe_monmean.nc aoi_monmean.nc
file_name <- "aoi_monmean.nc"  
par <- nc_open(filename = paste(di, inputs, file_name, sep = "/"))
print(par)
attributes(par)$names
# par_values <- ncvar_get(par)

# Transfor to raster stack
mi_stack <- stack()
for (i in 1:475) {
  mi_raster <- raster(paste(di, inputs, file_name, sep = "/"), 
                        band = i, varname = "qq")
  mi_stack <- stack(mi_stack, mi_raster)
}

# Reproject and resample to be aligned and matching with 20200406_4_4_clustering (climate regions)
# Load climate regions file
climregions <- raster(paste(di, outputs, "4_4_clustering.tif", sep = ""))
# plot(climregions)
# extent(climregions)
par_proj <- projectRaster(from = mi_stack, to = climregions)
# plot(par_proj$surface.downwelling.shortwave.flux.in.air.2.1.2.2.1.1.1.1)

# Mask with aoi
aoi <- raster(paste(di, "output_files/", "study_area_raster_3042.asc", sep = ""))
par_proj <- par_proj * aoi
# plot(par_proj$layer.20)

# Transform units: W/m2 -> μmol/m2*sec
par_proj_transformed <- par_proj * 2.02

# Extract time
time <- data.frame(nc_time = ncvar_get(par, "time")) %>%
  mutate(my_date = as.Date(nc_time, origin = "1950-01-01"),
                Year = year(my_date),
                Month = month(my_date))

# Extract values for climate regions and grouped
df <- data.frame(Climate_region = climregions[]) # Create data frame to fill it with climate data
PAR_df <- data.frame() # Create df with PAR data

for (i in 1:dim(par_proj_transformed)[3]) {
  temp <- par_proj_transformed[[i]]
  temp_df <- df
  temp_df$PAR <- temp[]
  temp_df <- temp_df %>%
    group_by(Climate_region) %>%
    summarise_all(mean, na.rm = TRUE) %>%
    filter(is.na(Climate_region) == FALSE)
  temp_df <- as.data.frame(t(as.matrix(temp_df)))[2,]
  temp_df$Year <- time$Year[i]
  temp_df$Month <- time$Month[i]
  PAR_df <- rbind(PAR_df, temp_df)
}
colnames(PAR_df) <- c("PAR_climate_region_1", 
                      "PAR_climate_region_2",
                      "PAR_climate_region_3",
                      "PAR_climate_region_4",
                      "Year", "Month")
PAR_df[PAR_df == "NaN"] <- NA

# Export as csv files

clim_par_1980_2019_reg1 <- PAR_df %>%
  dplyr::select(Year, Month, PAR_climate_region_1)
colnames(clim_par_1980_2019_reg1) <- c("Year", "Month", "PAR")
write.table(clim_par_1980_2019_reg1, 
            file = paste(outputs, "clim_par_1980_2019_reg1.csv", sep = ""), row.names = FALSE)

clim_par_1980_2019_reg2 <- PAR_df %>%
  dplyr::select(Year, Month, PAR_climate_region_2)
colnames(clim_par_1980_2019_reg2) <- c("Year", "Month", "PAR")
write.table(clim_par_1980_2019_reg2, 
            file = paste(outputs, "clim_par_1980_2019_reg2.txt", sep = ""), row.names = FALSE)

clim_par_1980_2019_reg3 <- PAR_df %>%
  dplyr::select(Year, Month, PAR_climate_region_3)
colnames(clim_par_1980_2019_reg3) <- c("Year", "Month", "PAR")
write.table(clim_par_1980_2019_reg3, 
            file = paste(outputs, "clim_par_1980_2019_reg3.txt", sep = ""), row.names = FALSE)

clim_par_1980_2019_reg4 <- PAR_df %>%
  dplyr::select(Year, Month, PAR_climate_region_4)
colnames(clim_par_1980_2019_reg4) <- c("Year", "Month", "PAR")
write.table(clim_par_1980_2019_reg4, 
            file = paste(outputs, "clim_par_1980_2019_reg4.txt", sep = ""), row.names = FALSE)
