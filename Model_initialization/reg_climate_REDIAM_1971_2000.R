### Climate files generation  - PERIOD 1971-2000 ###

library(raster)
library(dplyr)

# Define inputs and outputs
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"
input_files <- list.files(paste(di, inputs, sep = ""), pattern = c("*_aligned"))
climate_indexes <- c(2:13, 16:27, 40:51)
prec_files <- list.files(paste(di, inputs, sep = ""), pattern = c("p_1971_2000_*"))
tmax_files <- list.files(paste(di, inputs, sep = ""), pattern = c("tmax_1971_2000_*"))
tmin_files <- list.files(paste(di, inputs, sep = ""), pattern = c("tmin_1971_2000_*"))

# Load raster of mask
aoi <- raster(paste(di, outputs, "study_area_raster_3042.asc", sep = ""))
# plot(aoi)
# extent(aoi)

# Load ecoregions file
climateregions <- raster(paste(di, outputs, "4_4_clustering.tif", sep = ""))
# plot(climateregions)

# Test with one file
# prec_01 <- raster(paste(di, inputs, prec_files[1], sep = "")) * aoi
# plot(prec_01)
# df$prec_01 <- prec_01[]

# Create df with precipitation data
precipitation <- data.frame(Climate_region = climateregions[]) # Create data frame to fill it with climate data
for (i in 1:length(prec_files)) {
  print(prec_files[i])
  temp <- raster(paste(di, inputs, prec_files[i], sep = "")) * aoi
  precipitation[,i+1] <- temp[]
}
colnames(precipitation) <- c("Climate_region",
                  "Prec_1", "Prec_2", "Prec_3", 
                  "Prec_4", "Prec_5", "Prec_6", 
                  "Prec_7", "Prec_8", "Prec_9", 
                  "Prec_10", "Prec_11", "Prec_12")

precipitation <- precipitation %>%
  group_by(Climate_region) %>%
  summarise_all(mean) %>%
  filter(is.na(Climate_region) == FALSE)
  
# Create df with Tmax data
Tmax <- data.frame(Climate_region = climateregions[]) # Create data frame to fill it with climate data
for (i in 1:length(tmax_files)) {
  print(tmax_files[i])
  temp <- raster(paste(di, inputs, tmax_files[i], sep = "")) * aoi
  Tmax[,i+1] <- temp[]
}
colnames(Tmax) <- c("Climate_region",
                    "Tmax_1", "Tmax_2", "Tmax_3", 
                    "Tmax_4", "Tmax_5", "Tmax_6", 
                    "Tmax_7", "Tmax_8", "Tmax_9", 
                    "Tmax_10", "Tmax_11", "Tmax_12")

Tmax <- Tmax %>%
  group_by(Climate_region) %>%
  summarise_all(mean) %>%
  filter(is.na(Climate_region) == FALSE)

# Create df with Tmin data
Tmin <- data.frame(Climate_region = climateregions[]) # Create data frame to fill it with climate data
for (i in 1:length(tmin_files)) {
  print(tmin_files[i])
  temp <- raster(paste(di, inputs, tmin_files[i], sep = "")) * aoi
  Tmin[,i+1] <- temp[]
}
colnames(Tmin) <- c("Climate_region",
                    "Tmin_1", "Tmin_2", "Tmin_3", 
                    "Tmin_4", "Tmin_5", "Tmin_6", 
                    "Tmin_7", "Tmin_8", "Tmin_9", 
                    "Tmin_10", "Tmin_11", "Tmin_12")

Tmin <- Tmin %>%
  group_by(Climate_region) %>%
  summarise_all(mean) %>%
  filter(is.na(Climate_region) == FALSE)

# Generate climate data files for each climate region

Year <- rep("1971-2000", 12) # Generate Year column
Month <- c(1:12)
for (i in 1:length(precipitation$Climate_region)) {
  # Extract prec data for each reagion and fetch
  prec_cr <- precipitation %>% filter(Climate_region == i)
  prec_cr <- prec_cr[,2:13]
  prec_cr <- as.data.frame(t(as.matrix(prec_cr)))
  colnames(prec_cr) <- c("Prec")
  
  # Extract tmax data for each reagion and fetch
  tmax_cr <- Tmax %>% filter(Climate_region == i)
  tmax_cr <- tmax_cr[,2:13]
  tmax_cr <- as.data.frame(t(as.matrix(tmax_cr)))
  colnames(tmax_cr) <- c("Tmax")
  
  # Extract tmin data for each reagion and fetch
  tmin_cr <- Tmin %>% filter(Climate_region == i)
  tmin_cr <- tmin_cr[,2:13]
  tmin_cr <- as.data.frame(t(as.matrix(tmin_cr)))
  colnames(tmin_cr) <- c("Tmin")
  
  # Generate climate region df
  my_df <- cbind(Year, Month, tmax_cr, tmin_cr, prec_cr)
  # assign(paste("clim_reg", i, sep = ""), my_df)   
  write.table(my_df,
              file = paste(di, outputs, "clim_1971_2000_reg", i, ".txt", sep = ""),
              row.names = FALSE)
  }
