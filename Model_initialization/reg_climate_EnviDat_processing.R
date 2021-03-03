## Download EnviDat data

# Packages
library(ncdf4)
library(raster)
library(dplyr)

# Setup folders
di <- "..."
folder_local <- "..."
folder_harddisk <- "..."

# Create reference table - done once, read afterwards
# files <- read.table(paste(folder_local, "file_names.txt", sep = ""), header = FALSE)
# files$files_names <- as.character(files$V1)
# ref_df <- data.frame(files_name = files$files_names)
# ref_df$variable <- c()
# ref_df$scenario <- c()
# ref_df$temporary <- c()
# ref_df$period <- c()
# ref_df$start_year <- c()
# ref_df$end_year <- c()
# for (i in 1:length(ref_df$files_name)) {
#   ref_df$variable[i] <- strsplit(as.character(ref_df$files_name[i]), split = "_")[[1]][2]
#   ref_df$scenario[i] <- strsplit(as.character(ref_df$files_name[i]), split = "_")[[1]][4]
#   ref_df$temporary[i] <- strsplit(as.character(ref_df$files_name[i]), split = "_")[[1]][5]
#   
#   ref_df$start_year[i] <- strsplit(as.character(ref_df$temporary[i]), split = '-')[[1]][1]
#   ref_df$period[i] <- strsplit(as.character(ref_df$temporary[i]), split = "[.]")[[1]][1]
#   ref_df$end_year[i] <- strsplit(as.character(ref_df$period[i]), split = '-')[[1]][2]
# }
# write.table(ref_df, file = paste(folder_local, "ref_table.txt", sep = ""), row.names = FALSE)

# Load reference table
ref_df <- read.table(paste(folder_harddisk, "ref_table.txt", sep = ""), header = TRUE)
ref_df$start_year <- as.numeric(ref_df$start_year)
ref_df$end_year <- as.numeric(ref_df$end_year)

# Create subsets of ref_df by variables
precip_df <- ref_df %>% filter(variable == "pr")
tasmin_df <- ref_df %>% filter(variable == "tasmin")
tasmax_df <- ref_df %>% filter(variable == "tasmax")

# Load climate regions file and create df from it
climregions <- raster(paste(di, outputs, "4_4_clustering.tif", sep = ""))
df <- data.frame(Climate_region = climregions[]) # Create data frame to fill it with climate data

# Load aoi  map
aoi <- raster(paste(di, "study_area_raster_3042.asc", sep = ""))

# Process:
  # for each variable table
  # Identify file name
  # Identify time period and create time table
  # Load stack from downloaded files
  # Reproject and resample with climregions
  # Mask with aoi
  # Extract values for climate regions and group in variable table

months <- c(1:12)

# Precipitation files
# process in batches
for(j in 1:18) {
  print("j")
  print(j)
  my_stack_df <- data.frame() # Create df for data
  # Identify file name
  file_name <- precip_df$files_name[j]
  print("file_name")
  print(file_name)

  # Identify time period and create time table
  file_years <- c(precip_df$start_year[j]:precip_df$end_year[j])
  file_dates <- merge(months, file_years)

  # Load stack from downloaded files -> done in script reg_download_envidat.R
  my_stack <- stack(paste(folder_harddisk, file_name, sep = ""), varname = "precipitation_flux")
  print("stack loaded")

  # Reproject and resample with climregions
  my_stack_proj <- projectRaster(from = my_stack, to = climregions)
  print("stack reprojected")

  # Mask with aoi
  my_stack_proj <- my_stack_proj * aoi

  # Extract values for climate regions and group in variable table
  for (k in 1:dim(my_stack_proj)[3]) {
    print("second loop: k")
    print(k)
    print("of")
    print(dim(my_stack_proj)[3])

    temp <- my_stack_proj[[k]]
    temp_df <- df
    temp_df$pr <- temp[]
    temp_df <- temp_df %>%
      group_by(Climate_region) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      filter(is.na(Climate_region) == FALSE)
    temp_df <- as.data.frame(t(as.matrix(temp_df)))[2,]
    temp_df$Year <- file_dates$y[k]
    temp_df$Month <- file_dates$x[k]
    my_stack_df <- rbind(my_stack_df, temp_df)
  }

  colnames(my_stack_df) <- c("prec_region_1",
                             "prec_region_2",
                             "prec_region_3",
                             "prec_region_4",
                             "Year", "Month")

  # Export my_stack_df
  name_table <- paste("envidat", precip_df$variable[j], precip_df$scenario[j], precip_df$period[j], sep = "_")
  write.table(my_stack_df, file = paste(di, outputs, name_table, ".txt", sep = ""), row.names = FALSE)
  print("exporting table")
}
  
# tasmin files
for(j in 1:18) {
  print("j")
  print(j)
  my_stack_df <- data.frame() # Create df for data
  # Identify file name
  file_name <- tasmin_df$files_name[j]
  print("file_name")
  print(file_name)

  # Identify time period and create time table
  file_years <- c(tasmin_df$start_year[j]:tasmin_df$end_year[j])
  file_dates <- merge(months, file_years)

  # Load stack from downloaded files -> done in script reg_download_envidat.R
  my_stack <- stack(paste(folder_harddisk, file_name, sep = ""), varname = "air_temperature")
  print("stack loaded")

  # Reproject and resample with climregions
  my_stack_proj <- projectRaster(from = my_stack, to = climregions)
  print("stack reprojected")

  # Mask with aoi
  my_stack_proj <- my_stack_proj * aoi

  # Extract values for climate regions and group in variable table
  for (k in 1:dim(my_stack_proj)[3]) {
    print("second loop: k")
    print(k)
    print("of")
    print(dim(my_stack_proj)[3])

    temp <- my_stack_proj[[k]]
    temp_df <- df
    temp_df$tasmin <- temp[]
    temp_df <- temp_df %>%
      group_by(Climate_region) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      filter(is.na(Climate_region) == FALSE)
    temp_df <- as.data.frame(t(as.matrix(temp_df)))[2,]
    temp_df$Year <- file_dates$y[k]
    temp_df$Month <- file_dates$x[k]
    my_stack_df <- rbind(my_stack_df, temp_df)
  }

  colnames(my_stack_df) <- c("tasmin_df_region_1",
                             "tasmin_df_region_2",
                             "tasmin_df_region_3",
                             "tasmin_df_region_4",
                             "Year", "Month")

  # Export my_stack_df
  name_table <- paste("envidat", tasmin_df$variable[j], tasmin_df$scenario[j], tasmin_df$period[j], sep = "_")
  write.table(my_stack_df, file = paste(di, outputs, name_table, ".txt", sep = ""), row.names = FALSE)
  print("exporting table")
}

# tasmax files
for(j in 1:18) {
  print("j")
  print(j)
  my_stack_df <- data.frame() # Create df for data
  # Identify file name
  file_name <- tasmax_df$files_name[j]
  print("file_name")
  print(file_name)
  
  # Identify time period and create time table
  file_years <- c(tasmax_df$start_year[j]:tasmax_df$end_year[j])
  file_dates <- merge(months, file_years)
  
  # Load stack from downloaded files -> done in script reg_download_envidat.R
  my_stack <- stack(paste(folder_harddisk, file_name, sep = ""), varname = "air_temperature")
  print("stack loaded")
  
  # Reproject and resample with climregions
  my_stack_proj <- projectRaster(from = my_stack, to = climregions)
  print("stack reprojected")
  
  # Mask with aoi
  my_stack_proj <- my_stack_proj * aoi
  
  # Extract values for climate regions and group in variable table
  for (k in 1:dim(my_stack_proj)[3]) {
    print("second loop: k")
    print(k)
    print("of")
    print(dim(my_stack_proj)[3])
    
    temp <- my_stack_proj[[k]]
    temp_df <- df
    temp_df$tasmax <- temp[]
    temp_df <- temp_df %>%
      group_by(Climate_region) %>%
      summarise_all(mean, na.rm = TRUE) %>%
      filter(is.na(Climate_region) == FALSE)
    temp_df <- as.data.frame(t(as.matrix(temp_df)))[2,]
    temp_df$Year <- file_dates$y[k]
    temp_df$Month <- file_dates$x[k]
    my_stack_df <- rbind(my_stack_df, temp_df)
  }
  
  colnames(my_stack_df) <- c("tasmax_df_region_1",
                             "tasmax_df_region_2",
                             "tasmax_df_region_3",
                             "tasmax_df_region_4",
                             "Year", "Month")
  
  # Export my_stack_df
  name_table <- paste("envidat", tasmax_df$variable[j], tasmax_df$scenario[j], tasmax_df$period[j], sep = "_")
  write.table(my_stack_df, file = paste(di, outputs, name_table, ".txt", sep = ""), row.names = FALSE)
  print("exporting table")
}