# Shrublands & Crops trees

library(dplyr)
library(tidyr)
library(raster)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load raster layers and legends containing tree species in shrubs and crops areas
D_ARBO1_SP  <- raster(paste(di, outputs, "shrubs_crops_vege10_D_ARBO1_SP_25830.asc", sep = ""))
D_ARBO2_SP  <- raster(paste(di, outputs, "shrubs_crops_vege10_D_ARBO2_SP_25830.asc", sep = ""))
D_ARBO3_SP  <- raster(paste(di, outputs, "shrubs_crops_vege10_D_ARBO3_SP_25830.asc", sep = ""))
D_ARBO4_SP  <- raster(paste(di, outputs, "shrubs_crops_vege10_D_ARBO4_SP_25830.asc", sep = ""))
D_ARBO5_SP  <- raster(paste(di, outputs, "shrubs_crops_vege10_D_ARBO5_SP_25830.asc", sep = ""))
D_ARBO6_SP  <- raster(paste(di, outputs, "shrubs_crops_vege10_D_ARBO6_SP_25830.asc", sep = ""))

legend_D_ARBO1_SP <- read.csv(paste(di, outputs, "legend_shrubs_crops_vege10_D_ARBO1_SP.csv", sep = ""), sep = ",")
legend_D_ARBO2_SP <- read.csv(paste(di, outputs, "legend_shrubs_crops_vege10_D_ARBO2_SP.csv", sep = ""), sep = ",")
legend_D_ARBO3_SP <- read.csv(paste(di, outputs, "legend_shrubs_crops_vege10_D_ARBO3_SP.csv", sep = ""), sep = ",")
legend_D_ARBO4_SP <- read.csv(paste(di, outputs, "legend_shrubs_crops_vege10_D_ARBO4_SP.csv", sep = ""), sep = ",")
legend_D_ARBO5_SP <- read.csv(paste(di, outputs, "legend_shrubs_crops_vege10_D_ARBO5_SP.csv", sep = ""), sep = ",")
legend_D_ARBO6_SP <- read.csv(paste(di, outputs, "legend_shrubs_crops_vege10_D_ARBO6_SP.csv", sep = ""), sep = ",")

# Load selected species and generate "Genus" spp. data frame to avoid discarding non defined species
common_cohorts_selection_species <- read.csv(paste(di, outputs, "selection_species_common_cohorts.csv", sep = ""), sep = ",") %>% 
  mutate_all(as.character)
genus_spp <- data.frame(Species_tree = c(NA, NA, NA, NA), 
                        Especie = c("Juniperus", "Pinus", "Populus", "Quercus"),
                        species_LANDIS = c("juniperus", "pinus", "populus", "quercus"),
                        sp_common_cohort = c(NA, NA, NA, NA))
selection_species <- rbind(common_cohorts_selection_species, genus_spp)

# Load ic_map and extract common IC Codes assigned to forest areas
ic_forest_map  <- raster(paste(di, outputs, "ic_map_3042.asc", sep = ""))
common_IC_codes <- data.frame(Cell = 1:length(ic_forest_map[]),
                              IC_Code = ic_forest_map[]) %>%
  dplyr::select(IC_Code) %>%
  group_by(IC_Code) %>%
  summarise(count = n())

# Load IC_coding
ic_coding <- read.csv(paste(di, outputs, "ic_coding_fourth.csv", sep = ""), sep = ",") %>%
  mutate_all(as.character)
ic_coding$IC_Code <- as.integer(ic_coding$IC_Code)

# Extract tree species for each cell and discard non selected species
cell_tree_spp <- data.frame(Cell = 1:length(D_ARBO1_SP[]), # Assign cell number
                            D_ARBO1_SP_code = D_ARBO1_SP[], # Extract cell values
                            D_ARBO2_SP_code = D_ARBO2_SP[], # Extract cell values 
                            D_ARBO3_SP_code = D_ARBO3_SP[], # Extract cell values 
                            D_ARBO4_SP_code = D_ARBO4_SP[], # Extract cell values 
                            D_ARBO5_SP_code = D_ARBO5_SP[], # Extract cell values 
                            D_ARBO6_SP_code = D_ARBO6_SP[]) %>% # Extract cell values 
  filter(is.na(D_ARBO1_SP_code) == FALSE) %>% # Discard empty rows
  left_join(legend_D_ARBO1_SP, by = c("D_ARBO1_SP_code" = "Code")) %>% # Join with legend to have species names
  left_join(legend_D_ARBO2_SP, by = c("D_ARBO2_SP_code" = "Code")) %>% # Join with legend to have species names
  left_join(legend_D_ARBO3_SP, by = c("D_ARBO3_SP_code" = "Code")) %>% # Join with legend to have species names
  left_join(legend_D_ARBO4_SP, by = c("D_ARBO4_SP_code" = "Code")) %>% # Join with legend to have species names
  left_join(legend_D_ARBO5_SP, by = c("D_ARBO5_SP_code" = "Code")) %>% # Join with legend to have species names
  left_join(legend_D_ARBO6_SP, by = c("D_ARBO6_SP_code" = "Code")) %>% # Join with legend to have species names
  dplyr::select(Cell, Value.x, Value.y, Value.x.x, Value.y.y, Value.x.x.x, Value.y.y.y) %>%
  gather(spp_level, species, Value.x:Value.y.y.y, factor_key=TRUE) # Wide to long - all species (1-6) in same column
  
## Fetch data: NA values and subspecies not considered
cell_tree_spp[cell_tree_spp == "-"] <- NA
cell_tree_spp[cell_tree_spp == "Sin valor"] <- NA
cell_tree_spp[cell_tree_spp == "Pinus nigra subsp. salzmannii"] <- "Pinus nigra"
cell_tree_spp[cell_tree_spp == "Quercus ilex subsp. ballota"] <- "Quercus ilex"

## Discard non selected species
cell_tree_spp <- cell_tree_spp %>%
  inner_join(selection_species, by = c("species" = "Especie")) %>% # Discard non selected species
  dplyr::select(Cell, species_LANDIS) # species dominance level not used, include it here if needed

# Generate tree species combinations for each cell
cell_tree_spp_combinations <- data.frame()
for (j in 1:length(unique(cell_tree_spp$Cell))) {
  cell_subset <- cell_tree_spp %>% 
    dplyr::filter(Cell == unique(cell_tree_spp$Cell)[j]) %>%
    arrange(species_LANDIS)
  cell_row <- data.frame(Cell = unique(cell_tree_spp$Cell)[j],
                         spp_combination = paste0(cell_subset$species_LANDIS, collapse = "-"))
  cell_tree_spp_combinations <- rbind(cell_tree_spp_combinations, cell_row)  
}

# IC codes spp combinations 
ic_spp <- ic_coding %>%
  gather(key = species_cohort,
         value = species_cohort_presence,
         jcommunis_10:qpyrenaica_50,
         factor_key = TRUE) %>%
  filter(is.na(species_cohort_presence) == FALSE)
ic_spp$species_cohort <- as.character(ic_spp$species_cohort)
ic_spp$species_cohort_presence <- as.character(ic_spp$species_cohort_presence)

ic_spp$species_LANDIS <- c()
for (i in 1:length(ic_spp$IC_Code)) {
  ic_spp$species_LANDIS[i] <- strsplit(ic_spp$species_cohort[i], split = "_")[[1]][1]
}

ic_spp <- ic_spp %>%
  dplyr::select(IC_Code, species_LANDIS) %>%
  group_by(IC_Code, species_LANDIS) %>%
  summarise(count = n()) %>%
  dplyr::select(IC_Code, species_LANDIS)

ic_spp_combinations <- data.frame(IC_Code = c(),
                                  spp_combination = c())
for (j in 1:length(unique(ic_spp$IC_Code))) {
  IC_subset <- ic_spp %>%
    filter(IC_Code == unique(ic_spp$IC_Code)[j]) %>%
    arrange(species_LANDIS)
  
  IC_row <- data.frame(IC_Code = unique(ic_spp$IC_Code)[j],
                       spp_combination = paste0(IC_subset$species_LANDIS, collapse = "-"))
  ic_spp_combinations <- rbind(ic_spp_combinations, IC_row)  
}

## Include IC_Code count
ic_spp_combinations <- ic_spp_combinations %>%
  left_join(common_IC_codes)

# Assign most common IC Code for each cell spp combination
cell_IC_Codes <- cell_tree_spp_combinations %>%
  left_join(ic_spp_combinations)

ic_shrubs_crops_trees <- c()
for (g in 1:length(unique(cell_IC_Codes$Cell))) {
  Cell_IC_subset <- cell_IC_Codes %>% 
    dplyr::filter(Cell == unique(cell_IC_Codes$Cell)[g])
  IC_row <- data.frame(Cell = Cell_IC_subset$Cell[which.max(Cell_IC_subset$count)],
                       IC_Code = Cell_IC_subset$IC_Code[which.max(Cell_IC_subset$count)])
  ic_shrubs_crops_trees <- rbind(ic_shrubs_crops_trees, IC_row)
}

# Generate shrubs raster containing IC_Code for tree species for cells
## Regenerate table with all cell values and IC_Codes
nr_cells <- data.frame(Cell = 1:length(D_ARBO1_SP[]))
ic_shrubs_crops_all_cells <- ic_shrubs_crops_trees %>% 
  full_join(nr_cells)
## Order table by cell number
ic_shrubs_crops_all_cells <- ic_shrubs_crops_all_cells[order(ic_shrubs_crops_all_cells$Cell),]
## Create raster with IC_Code
ic_shrubs_crops_trees_raster <- raster(D_ARBO1_SP)
values(ic_shrubs_crops_trees_raster) <- ic_shrubs_crops_all_cells$IC_Code
plot(ic_shrubs_crops_trees_raster$layer)
## Export raster
writeRaster(ic_shrubs_crops_trees_raster, paste(di, outputs, "ic_shrubs_crops_trees.asc", sep = ""), overwrite = TRUE)
