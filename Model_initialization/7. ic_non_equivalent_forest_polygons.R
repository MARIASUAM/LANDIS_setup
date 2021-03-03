# Assign IC to polygons without equivalent polygons

library(dplyr)
library(tidyr)
library(rgdal)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Input files
# Generated in qGIS: 
  # - mfe_active and no_equivalent_polys.csv loaded and joined by objectid
  # - polygons without equivalent selected and selected features exported as shape no_equivalent_polys.shp
# no_equiv <- readOGR(dsn = paste(di, outputs, sep = ""), layer = "no_equivalent_polys") 
# no_equiv_data <- as.data.frame(no_equiv@data)
# write.csv(no_equiv_data, paste(di, outputs, "no_equivalent_mfe_data.csv", sep = ""), row.names = FALSE)
# Manually classified and decisions to be applied added in columns:
no_equiv_data <- read.csv(paste(di, outputs, "no_equivalent_polygons_decisions.csv", sep = ""), sep = ";")

## Active polygons - assign IC based on species ##

# Load list of selected species
selection_species <- read.csv(paste(di, inputs, "especies_ocupa.csv", sep = ""), sep = ";") %>%
  mutate_all(as.character)
species_LANDIS <- read.csv(paste(di, inputs, "species_LANDIS_names.csv", sep = ""), sep = ";") %>%
  mutate_all(as.character)
selection_species <- selection_species %>%
  left_join(species_LANDIS, by = c("Species_tree" = "species_code_IFN"))

# Load IC_Code list and reorganise
ic_coding_third <- read.csv(paste(di, outputs, "ic_coding_third.csv", sep = ""), sep = ",") %>%
  mutate_all(as.character)
ic_coding <- ic_coding_third %>%
  gather(key = species_cohort,
         value = species_cohort_presence,
         jcommunis_10:qpyrenaica_50,
         factor_key = TRUE) %>%
  filter(is.na(species_cohort_presence) == FALSE) %>%
  mutate_all(as.character)

ic_coding$Name_LANDIS <- c() # Extract species in species_cohort as Name_LANDIS
for (i in 1:length(ic_coding$IC_Code)) {
  ic_coding$Name_LANDIS[i] <- strsplit(ic_coding$species_cohort[i], split = "_")[[1]][1]
}

ic_coding_only_species <- ic_coding %>%
  dplyr::select(IC_Code, Name_LANDIS) %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  dplyr::select(IC_Code, Name_LANDIS)

# Extract non equiv. active polygons and filter out non selected species
active <- no_equiv_data %>%
  dplyr::filter(Active == TRUE) %>% # Select active polygons
  dplyr::select(objectid, especie1, especie2, especie3) %>%
  mutate_all(as.character) %>%
  gather(key = species_level, # reorganise species data: wide to long
         value = species,
         especie1:especie3,
         factor_key = TRUE) %>%
  left_join(selection_species, by = c("species" = "Especie")) %>% # filter out non selected species
  dplyr::select(objectid, Species_tree, species, species_LANDIS) %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  dplyr::select(objectid, Species_tree, species, species_LANDIS)
  
# Find compatible IC_Codes: 
# same combination of species - cohort age not considered

# Merge species in one variable - alphabetical order
active_merged <- data.frame(objectid = unique(active$objectid),
                            species_combination = NA)
for (i in 1:length(active_merged$objectid)) {
  my_subset <- active %>% 
    dplyr::filter(objectid == active_merged$objectid[i]) %>%
    dplyr::filter(is.na(species_LANDIS) == FALSE) %>%
    arrange(species_LANDIS)
  active_merged$species_combination[i] <- paste0(my_subset$species_LANDIS, collapse = "-")
}

ic_coding_only_species_merged <- data.frame(IC_Code = unique(ic_coding_only_species$IC_Code),
                                            species_combination = NA)
for (i in 1:length(ic_coding_only_species_merged$IC_Code)) {
  my_subset <- ic_coding_only_species %>% 
    dplyr::filter(IC_Code == ic_coding_only_species_merged$IC_Code[i]) %>%
    dplyr::filter(is.na(Name_LANDIS) == FALSE) %>%
    arrange(Name_LANDIS)
  ic_coding_only_species_merged$species_combination[i] <- paste0(my_subset$Name_LANDIS, collapse = "-")
}

active_IC <- left_join(active_merged, ic_coding_only_species_merged)

# Find most common IC_Codes
ic_polygons_with_plots <- read.csv(paste(di, outputs, "ic_polygons_with_plots.csv", sep = ""))
ic_all_equiv_polygons <- read.csv(paste(di, outputs, "ic_all_equiv_polygons.csv", sep = ""), sep = ",")

ic_used <- rbind(ic_polygons_with_plots, ic_all_equiv_polygons)

common_IC_Codes <- ic_used %>%
  mutate_all(as.character) %>%
  dplyr::select(IC_Code) %>%
  group_by(IC_Code) %>%
  summarise(count = n())

write.csv(common_IC_Codes, 
          paste(di, outputs, "common_IC_Codes.csv", sep = ""), row.names = FALSE)

# Assign most common IC_Code to polygons with multiple possible IC
active_IC_possible <- active_IC %>% 
  filter(is.na(IC_Code) == FALSE) %>%
  mutate_all(as.character)

ic_non_equiv_polygons_possible_IC <- data.frame(objectid = unique(active_IC_possible$objectid), IC_Code = NA)
for (i in 1:length(ic_non_equiv_polygons_possible_IC$objectid)) {
  my_subset2 <- active_IC_possible %>%
    dplyr::filter(objectid == ic_non_equiv_polygons_possible_IC$objectid[i]) %>%
    left_join(common_IC_Codes, by = "IC_Code")
  ic_non_equiv_polygons_possible_IC$IC_Code[i] <- my_subset2$IC_Code[which.max(my_subset2$count)]
}

# Crear IC_Codes for polygons without possible IC
# Find most common species cohorts
ic_coding_third$IC_Code <- as.character(ic_coding_third$IC_Code)
ic_used$IC_Code <- as.character(ic_used$IC_Code)

sp_common_cohorts <- ic_used %>%
  left_join(ic_coding_third) %>% # attach species cohort present in each IC_Code
  gather(key = species_cohort, # wide to long - cohorts in one column
         value = species_cohort_presence,
         jcommunis_10:qpyrenaica_50,
         factor_key = TRUE) %>%
  filter(is.na(species_cohort_presence) == FALSE) %>%
  dplyr::select(species_cohort) %>%
  group_by(species_cohort) %>% # find occurrency of cohorts
  summarise(count = n())
sp_common_cohorts$species_cohort <- as.character(sp_common_cohorts$species_cohort)
for (i in 1:length(sp_common_cohorts$species_cohort)){
  sp_common_cohorts$sp[i] <- strsplit(sp_common_cohorts$species_cohort[i], split = "_")[[1]][1]
}

selection_species$sp_common_cohort <- c()
for (i in 1:length(selection_species$Species_tree)){
  my_subset3 <- sp_common_cohorts %>%
    filter(sp == selection_species$species_LANDIS[i])
  selection_species$sp_common_cohort[i] <- my_subset3$species_cohort[which.max(my_subset3$count)]
}
# selection_species$sp_common_cohort[1] <- "jcommunis_10" # OLD in the lack of data, same as joxycedrus
selection_species$sp_common_cohort[3] <- "phalepensis_40" # same as all other pines, even though it is not the most commonly assigned
selection_species$sp_common_cohort[7] <- "qfaginea_30" # cohort 10 and 30 equally common, R finds max as the first one (10), but 30 makes more sense ecologically speaking
selection_species$sp_common_cohort[8] <- "qilex_30" # cohort 10 and 30 equally common, R finds max as the first one (10), but 30 makes more sense ecologically speaking
selection_species$sp_common_cohort[9] <- "qpyrenaica_30" # cohort 10 and 30 equally common, R finds max as the first one (10), but 30 makes more sense ecologically speaking

write.csv(selection_species, 
          paste(di, outputs, "selection_species_common_cohorts.csv", sep = ""), row.names = FALSE)

# Extract combination of species in polygon without compatible IC
active_non_IC_possible <- active_IC %>% 
  filter(is.na(IC_Code) == TRUE) %>%
  mutate_all(as.character)

new_IC_Codes <- active_non_IC_possible %>% dplyr::select(species_combination) %>%
  group_by(species_combination) %>% summarise(count = n()) # Extract unique species combinations
for (i in 1:length(new_IC_Codes$species_combination)){ # spread species in different columns
  new_IC_Codes$sp1[i] <- strsplit(new_IC_Codes$species_combination[i], split = "-")[[1]][1]
  new_IC_Codes$sp2[i] <- strsplit(new_IC_Codes$species_combination[i], split = "-")[[1]][2]
  new_IC_Codes$sp3[i] <- strsplit(new_IC_Codes$species_combination[i], split = "-")[[1]][3]
}
new_IC_Codes$IC_Code <- max(as.numeric(ic_coding_third$IC_Code)) + 1:length(new_IC_Codes$species_combination) # Add new IC_Codes

new_IC_Codes_clean <- new_IC_Codes %>% # Join to identify most common cohort for each species
  left_join(selection_species, by = c("sp1" = "species_LANDIS")) %>%
  left_join(selection_species, by = c("sp2" = "species_LANDIS")) %>%
  left_join(selection_species, by = c("sp3" = "species_LANDIS")) %>%
  dplyr::select(IC_Code, sp_common_cohort, sp_common_cohort.x, sp_common_cohort.y) %>%
  gather(key = species_cohort_level, # wide to long - all cohorts in same column
         value = species_cohort,
         sp_common_cohort:sp_common_cohort.y) %>%
  mutate_all(as.factor) %>%
  filter(is.na(species_cohort) == FALSE) %>% # discard empty rows (corresponding to ICs with less than 3 species)
  mutate(species_cohort_presence = "species-cohort present") %>%
  dplyr::select(IC_Code, species_cohort, species_cohort_presence) %>%
  spread(key = species_cohort,
         value = species_cohort_presence) # long to wide - cohorts as columns
  
# Add row for empty IC: IC_Code 436
empty_IC <- ic_coding_third[1,c(2:44)]
empty_IC[empty_IC == "species-cohort present"] <- NA
empty_IC$IC_Code[1] <- 436
empty_IC$IC_Code <- as.factor(empty_IC$IC_Code)

new_ic_coding <- full_join(new_IC_Codes_clean, empty_IC) %>%
  mutate_all(as.factor)

# Export new list of IC_codes
ic_coding_fourth <- rbind(ic_coding_third, new_ic_coding)
write.csv(ic_coding_fourth, 
          paste(di, outputs, "ic_coding_fourth.csv", sep = ""), row.names = FALSE)

# Assing IC_Code to polygons without compatible IC
IC_species_combination <- new_IC_Codes %>%
  dplyr::select(IC_Code, species_combination)

ic_non_equiv_polygons_no_possible_IC <- active_non_IC_possible %>%
  dplyr::select(objectid, species_combination) %>%
  left_join(IC_species_combination, by = c("species_combination")) %>%
  dplyr::select(objectid, IC_Code) %>%
  mutate_all(as.factor)

# Merge ic_non_equiv_polygons_possible_IC and ic_non_equiv_polygons_no_possible_IC
ic_all_no_equiv_polygons <- rbind(ic_non_equiv_polygons_possible_IC,
                                  ic_non_equiv_polygons_no_possible_IC)
write.csv(ic_all_no_equiv_polygons, 
          paste(di, outputs, "ic_all_no_equiv_polygons.csv", sep = ""), row.names = FALSE)
