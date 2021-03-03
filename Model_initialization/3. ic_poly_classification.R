## MFE polygons classification

library(tidyr)
library(dplyr)
library(rgdal)
library(raster)
library(rgeos)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

## STEP I: Active TRUE/FALSE and Forest TRUE/FALSE for each polygon

# Load mfe map and data
mfe <- readOGR(dsn = paste(di, inputs, sep = ""), layer = "mfe_aoi_active_23030")
mfe_data <- mfe@data
# write.csv(mfe_data, paste(di, outputs, "mfe_aoi_active_data.csv", sep = ""), row.names = FALSE)

mfe_data <- mfe_data %>%
  mutate(arbolado_form_arb_d = ifelse(form_arb_d == "No arbolado", "No arbolado", "Arbolado"))
mfe_data$objectid <- as.factor(mfe_data$objectid)

usos <- mfe_data %>%
  dplyr::select(tipo_estru, arbolado_form_arb_d, usoifn, usomfe) %>%
  group_by_all() %>%
  summarise(count = n())
# write.csv(usos, paste(di, outputs, "usos.csv", sep = ""), row.names = FALSE)

## Active and Forest columns added manually to usos:
usos_cl <- read.csv(paste(di, inputs, "usos_classified.csv", sep = ""), sep = ",") 

polygons <- mfe_data %>%
  full_join(usos_cl) %>%
  dplyr::select(objectid, Active, Forest, Manual_IC)
# write.csv(polygons, paste(di, outputs, "polygons_classified.csv", sep = ""), row.names = FALSE)

## STEP II: Find intersecting plots in Active Forest polygons

# Load plot data and build SpatialPointsDataFrame with it
# parcelas_aoi <- read.csv(paste(di, inputs, "parcelas_en_aoi.csv", sep = ""), sep = ",") # old
# parcelas_aoi$plot_id <- c(1:length(parcelas_aoi$Provincia)) # old
parc_aoi_mayores_regenera <- read.csv(paste(di, outputs, "parc_aoi_mayores_regenera.csv", sep = ""), sep = ",") # parcelas where mayores and regeneration were reported
parc_aoi_mayores_regenera$plot_id <- as.factor(parc_aoi_mayores_regenera$plot_id)
parc_aoi_mayores_regenera$Tabla_Provincia <- as.factor(parc_aoi_mayores_regenera$Tabla_Provincia)
parc_aoi_mayores_regenera$Estadillo <- as.factor(parc_aoi_mayores_regenera$Estadillo)

parcelas_aoi <- parc_aoi_mayores_regenera # parc_aoi_mayores_regenera substitutes old parcelas_aoi

coords <- cbind(parcelas_aoi$CoorX_parcelas, parcelas_aoi$CoorY_parcelas)
plot_points <- SpatialPointsDataFrame(coords = coords,
                                      proj4string = crs(mfe),
                                      data = parcelas_aoi[,-c(5,6)])

# Build data frame showing intersecting polygon-plots
forest_poly_ids <- polygons %>% # Extract active polygon ids
  dplyr::filter(Active == TRUE) %>%
  dplyr::filter(Forest == TRUE) %>%
  dplyr::select(objectid)
  
poly_point_match <- data.frame(objectid = c(), plot_id = c()) # Build empty df for matches
for (i in 1:length(forest_poly_ids$objectid)) { # for each polygon in forest_poly_ids
  my_polygon <- mfe[mfe$objectid == forest_poly_ids$objectid[i],] # extract polygon
  overlap <- over(plot_points, my_polygon, fn = NULL) # find overlap with plot_points
  overlap$plot_id <- rownames(overlap) # include plot_id in overlap list
  overlap <- overlap %>% # extract only objectid and plot_id
    filter(is.na(objectid) == FALSE) %>%
    dplyr::select(objectid, plot_id)
  poly_point_match <- rbind(poly_point_match, overlap) # append row to data frame
}
poly_point_match$plot_id <- as.factor(poly_point_match$plot_id)

all_poly_point_match <- poly_point_match %>%
  full_join(polygons) %>%
  dplyr::select(objectid, Active, Forest, plot_id, Manual_IC)

case1 <- all_poly_point_match %>% # polygons in case 1: no plot-poly intersection
  filter(Active == TRUE,
         Forest == TRUE,
         is.na(plot_id) == TRUE)

case2_3 <- all_poly_point_match %>% # polygons in case 2 and 3 (case 2: 1 plot-poly, case 3: >1plot-poly)
  filter(Active == TRUE,
         Forest == TRUE,
         is.na(plot_id) == FALSE)
case2_3$plot_id <- as.character(case2_3$plot_id)
case2_3$objectid <- as.character(case2_3$objectid)

## STEP III: Find common species in overlapping polygon-plots

# Load list of selected species
selection_species <- read.csv(paste(di, inputs, "especies_ocupa.csv", sep = ""), sep = ";")
selection_species$Species_tree <- as.factor(selection_species$Species_tree)

# List of species per polygon - only selected species
poly_sp <- mfe_data %>%
  gather(key = niveles, value = Especie, c(especie1, especie2, especie3), factor_key = TRUE) %>% # wide to long - especie1-3 to 1 column "Especie"
  filter(Especie != "sin datos") %>%
  inner_join(selection_species) %>% # filter out non selected species
  dplyr::select(objectid, Species_tree)
poly_sp$objectid <- as.character(poly_sp$objectid)
poly_sp$Species_tree <- as.character(poly_sp$Species_tree)

# List of species per plot - only selected species
plot_sp <- read.csv(paste(di, outputs, "plots_species_presence.csv", sep = ""), sep = ",")
plot_sp$Tabla_Provincia <- as.factor(plot_sp$Tabla_Provincia)
plot_sp$Estadillo <- as.factor(plot_sp$Estadillo)
plot_sp$Cla <- as.factor(plot_sp$Cla)
plot_sp$Subclase <- as.factor(plot_sp$Subclase)
plot_sp$Species_tree <- as.factor(plot_sp$Species_tree)

plot_sp <- plot_sp %>%
  inner_join(parcelas_aoi) %>% # to include plot_id variable
  dplyr::select(plot_id, Species_tree)

# Is there common species between each polygon-plot match?
case2_3$inter_species <- c()

for (i in 1:length(case2_3$objectid)) {
  object <- case2_3$objectid[i]
  my_poly_sp <- poly_sp %>% 
    dplyr::filter(objectid == object) %>% 
    dplyr::select(Species_tree)
  
  plot <- case2_3$plot_id[i]
  my_plot_sp <- plot_sp %>% 
    dplyr::filter(plot_id == plot) %>% 
    dplyr::select(Species_tree)
  
  case2_3$inter_species[i] <- as.list(dplyr::intersect(my_poly_sp, my_plot_sp))
}

case2_3$inter_species <- as.character(case2_3$inter_species)
case2_3 <- case2_3 %>%
  mutate(overlap = ifelse(inter_species == "character(0)", FALSE, TRUE))

all_poly_point_match <- all_poly_point_match %>%
  left_join(case2_3)

# write.csv(all_poly_point_match, paste(di, outputs, "all_poly_point_match.csv", sep = ""), row.names = FALSE)

# STEP IV: Define origin of IC for each polygon

inactive <- all_poly_point_match %>%
  dplyr::filter(Active == FALSE) %>%
  mutate(IC_origin = "Not applicable")

active_noforest <- all_poly_point_match %>%
  dplyr::filter(Active == TRUE,
                Forest == FALSE) %>%
  mutate(IC_origin = "derived from other sources")

active_forest_no_plot <- all_poly_point_match %>%
  dplyr::filter(Active == TRUE,
                Forest == TRUE, 
                is.na(plot_id)) %>%
  mutate(IC_origin = "derived from similar plots")

# Case 1 polygon - 1 plot
poly_cases_1_1 <- all_poly_point_match %>%
  filter(is.na(plot_id) == FALSE) %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n()) %>%
  filter(count == 1) %>%
  dplyr::select(objectid)

active_forest_plot_1_1 <- all_poly_point_match %>%
  inner_join(poly_cases_1_1) %>%
  mutate(IC_origin = as.factor(plot_id))

# Case 1 polygon - >1 plot
poly_cases_1_more_1 <- all_poly_point_match %>%
  filter(is.na(plot_id) == FALSE) %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n()) %>%
  filter(count > 1)

objectids <- all_poly_point_match %>%
  inner_join(poly_cases_1_more_1 %>% dplyr::select(objectid))

active_forest_plot_1_more_1 <- data.frame(objectid = c(), 
                                          Active = c(), 
                                          Forest = c(),
                                          plot_id = c(),
                                          Manual_IC = c(),
                                          inter_species = c(),
                                          overlap = c(),
                                          IC_origin = c())

for (i in 1:length(poly_cases_1_more_1$objectid)) {
  my_subset <- objectids %>%
    dplyr::filter(objectid == poly_cases_1_more_1$objectid[i])
    # dplyr::filter(objectid == 39527)
  
  my_row <- data.frame(objectid = poly_cases_1_more_1$objectid[i], Active = TRUE, Forest = TRUE,
                       plot_id = "multiple plots", Manual_IC = NA, inter_species = NA, overlap = NA,
                       IC_origin = paste0(my_subset$plot_id, collapse = ", "))
  
  active_forest_plot_1_more_1 <- rbind(active_forest_plot_1_more_1, my_row)
  }

write.csv(active_forest_plot_1_more_1, paste(di, outputs, "active_forest_plot_1_more_1.csv", sep = ""), row.names = FALSE)

# Merge all dataframe defining IC_origin for each polygon
polygons_IC_origin <- rbind(inactive, # inactive polygons
                     active_noforest, # active no forest polygons
                     active_forest_no_plot, # active forest without plots
                     active_forest_plot_1_1, # active forest with 1 plot
                     active_forest_plot_1_more_1) # active forest with multiple plots
polygons_IC_origin$objectid <- as.factor(polygons_IC_origin$objectid)
polygons_IC_origin$plot_id <- as.factor(polygons_IC_origin$plot_id)
polygons_IC_origin$inter_species <- as.factor(polygons_IC_origin$inter_species)
polygons_IC_origin$overlap <-  as.logical(polygons_IC_origin$overlap)
polygons_IC_origin$IC_origin <-  as.factor(polygons_IC_origin$IC_origin)

write.csv(polygons_IC_origin, paste(di, outputs, "polygons_IC_origin.csv", sep = ""), row.names = FALSE)
