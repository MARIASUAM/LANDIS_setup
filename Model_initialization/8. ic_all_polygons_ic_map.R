# Generate Initial Communities map

library(rgdal)
library(raster)
library(dplyr)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load input files

# Active forest polygons with plots in it
ic_polygons_with_plots <- read.csv(paste(di, outputs, "ic_polygons_with_plots.csv", sep = "")) %>%
  mutate_all(as.factor)

# Active forest polygons with no plot in it but equivalent polygon
ic_all_equiv_polygons <- read.csv(paste(di, outputs, "ic_all_equiv_polygons.csv", sep = "")) %>%
  mutate_all(as.factor)

# Active forest polygons with no plot in it and no equivalent polygon
ic_all_no_equiv_polygons <- read.csv(paste(di, outputs, "ic_all_no_equiv_polygons.csv", sep = "")) %>%
  mutate_all(as.factor)

# Active no forest polygons
all_poly_point_match <- read.csv(paste(di, outputs, "all_poly_point_match.csv", sep = ""))
all_poly_point_match$objectid <- as.factor(all_poly_point_match$objectid)
all_poly_point_match$Manual_IC <- as.factor(all_poly_point_match$Manual_IC)

ic_noforest <- all_poly_point_match %>%
  dplyr::filter(Active == TRUE) %>%
  dplyr::filter(Forest == FALSE) %>%
  mutate(IC_Code = Manual_IC) %>%
  dplyr::select(objectid, IC_Code) %>%
  mutate_all(as.factor)

# Inactive polygons 
ic_inactive_polygons <- all_poly_point_match %>%
  dplyr::filter(Active == FALSE) %>%
  mutate(IC_Code = Manual_IC) %>%
  dplyr::select(objectid, IC_Code) %>%
  mutate_all(as.factor)

# Exception --> active to inactive: choperas and eucaliptales
ic_active_to_inactive <- read.csv(paste(di, outputs, "no_equivalent_polygons_decisions.csv", sep = ""), sep = ";") %>%
  filter(Active == FALSE) %>%
  mutate(IC_Code = 996) %>% # IC_Code 996 -> plantations with Populus x canadensis and Eucaliptus sp.
  dplyr::select(objectid, IC_Code) %>%
  mutate_all(as.factor)

# Merge all
ic_all_polygons <- rbind(ic_polygons_with_plots,
                         ic_all_equiv_polygons,
                         ic_all_no_equiv_polygons,
                         ic_noforest,
                         ic_inactive_polygons,
                         ic_active_to_inactive)
ic_all_polygons <- ic_all_polygons %>%
  mutate_all(as.character)

# Exception --> inactive to active: agriculture polygons closed to pine plantations --> IC_Code == 1002
agriculture_with_trees <- readOGR(dsn = paste(di, inputs, sep = ""), layer = "agricola_con_arboles")
agriculture_with_trees_data <- as.data.frame(agriculture_with_trees@data) %>%
  dplyr::select(objectid) %>%
  mutate_all(as.character)

for (i in 1:length(agriculture_with_trees_data$objectid)) {
  ic_all_polygons$IC_Code[which(ic_all_polygons$objectid == agriculture_with_trees_data$objectid[i])] <- "1002"
}
# check <- ic_all_polygons %>% filter(IC_Code == 1002)

write.csv(ic_all_polygons, 
          paste(di, outputs, "ic_all_polygons.csv", sep = ""), row.names = FALSE)

# Build IC map
# Join mfe map with ic_all_polygons
mfe <- readOGR(dsn = paste(di, inputs, sep = ""), layer = "mfe_aoi_active_23030")
mfe@data$objectid <- as.character(mfe@data$objectid)
mfe@data <- left_join(mfe@data, ic_all_polygons, by = "objectid")
mfe@data$IC_Code <- as.numeric(mfe@data$IC_Code)

mfe@data <- mfe@data[,c(1,33)]

# Export mfe in 23030
writeOGR(mfe,
         dsn = paste(di, outputs, sep = ""),
         layer = "ic_polygons_23030", driver="ESRI Shapefile")

# Reproject shp: from 23030 to 3042
proj3042 <- "+proj=utm +zone=30 +ellps=GRS80 +units=m +no_defs"
mfe_3042 <- spTransform(mfe, CRS(proj3042))
extent(mfe_3042)
writeOGR(mfe_3042,
         dsn = paste(di, outputs, sep = ""),
         layer = "ic_polygons_3042", driver="ESRI Shapefile")

# Generate raster 
aoi_3042 <- raster(paste(di, "output_files/", "study_area_raster_3042.asc", sep = ""))
mfe_ic <- rasterize(mfe_3042, aoi_3042, field = 'IC_Code', 
                    fun = 'last', background = NA)
writeRaster(mfe_ic, paste(di, outputs, "ic_map_3042.asc", sep = ""), overwrite = TRUE)

# IC statistics
IC_data <- data.frame(IC_Code = mfe_ic[]) %>%
  mutate_all(as.factor) %>%
  filter(is.na(IC_Code) == FALSE)

tot_nr_cells <- length(IC_data$IC_Code)
percentage_IC <- IC_data %>%
  group_by(IC_Code) %>%
  summarise(nr_cells = n()) %>%
  mutate(perc_total_area = (nr_cells * 100) / tot_nr_cells)

write.csv(percentage_IC, paste(di, outputs, "IC_summary.csv", sep = ""), row.names = FALSE)

## Empty cells analysis - IC_Code 436 (old 132)

# Extract empty polygons by case
with_plots_436 <- ic_polygons_with_plots %>% 
  filter(IC_Code == 436) %>%
  mutate(empty_reason = 1)

equiv_436 <- ic_all_equiv_polygons %>% 
  filter(IC_Code == 436) %>%
  mutate(empty_reason = 2)

no_equiv_436 <- ic_all_no_equiv_polygons %>% 
  filter(IC_Code == 436) %>%
  mutate(empty_reason = 3)

noforest_436 <- ic_noforest %>% 
  filter(IC_Code == 436) %>%
  mutate(empty_reason = 4)

# inactive_132 <- ic_inactive_polygons %>% filter(IC_Code == 132)
# act_to_inact_132 <- ic_active_to_inactive %>% filter(IC_Code == 132)

# Merge empty polygons by case and join to mfe_3042
all_empty <- rbind(with_plots_436, equiv_436, no_equiv_436, noforest_436)
all_empty$objectid <- as.character(all_empty$objectid)

mfe_3042@data <- left_join(mfe_3042@data, all_empty, by = "objectid")
mfe_3042@data$empty_reason <- as.numeric(mfe_3042@data$empty_reason)

# Rasterize
mfe_empty_reason <- rasterize(mfe_3042, aoi_3042, field = 'empty_reason', 
                    fun = 'last', background = NA, na.rm = TRUE)
writeRaster(mfe_empty_reason, paste(di, outputs, "mfe_empty_reason.asc", sep = ""), overwrite = TRUE)

# empty cells statistics
empty_data <- data.frame(empty_reason = mfe_empty_reason[]) %>%
  mutate_all(as.factor) %>%
  filter(is.na(empty_reason) == FALSE)

tot_nr_empty_cells <- length(empty_data$empty_reason)
percentage_empty <- empty_data %>%
  group_by(empty_reason) %>%
  summarise(nr_cells = n()) %>%
  mutate(perc_empty_area = (nr_cells * 100) / tot_nr_empty_cells,
         perc_total_area = (nr_cells * 100) / tot_nr_cells)

write.csv(percentage_empty, paste(di, outputs, "empty_summary.csv", sep = ""), row.names = FALSE)

# IC not used - check

used_IC <- ic_all_polygons %>% 
  dplyr::select(IC_Code) %>%
  group_by(IC_Code) %>%
  summarise(count = n())

all_IC_codes_created <- data.frame(IC_Code = c(1:466, 1001, 1002, 999, 998, 997, 996))
all_IC_codes_created$IC_Code <- as.character(all_IC_codes_created$IC_Code)

disappeared_IC <- anti_join(all_IC_codes_created, used_IC)

