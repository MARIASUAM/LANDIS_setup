## Initial Communities - Biomass

library(dplyr)
library(tidyr)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load allometric coefficients
coef <- read.csv(paste(di, inputs, "funciones_alometricas.csv", sep = ""), sep = ";", skip = 2, dec = ",")
coef$Cod_Especie <- as.factor(coef$Cod_Especie)
coef <- coef[,c(1:7, 28:31)]

# Load other information
e <- 2.718281828459
selection_species <- read.csv(paste(di, inputs, "especies_ocupa.csv", sep = ""), sep = ";")
selection_species$Species_tree <- as.factor(selection_species$Species_tree)

sp_names <- read.csv(paste(di, inputs, "species_LANDIS_names.csv", sep = ""), sep = ";")
sp_names$species_code_IFN <- as.character(sp_names$species_code_IFN)

# Load age assignation per diameter class and species
age_assignation <- read.csv(paste(di, inputs, "age_assignation.csv", sep = ""), sep = ",") %>%
  select(Species, class, type, Asign_Age_Size)
colnames(age_assignation) <- c("Species_tree", "diameter_class_mm", "type", "Assigned_cohort")
age_assignation$Species_tree <- as.factor(age_assignation$Species_tree)
age_assignation$Assigned_cohort <- as.character(age_assignation$Assigned_cohort)
age_assign_adult <- age_assignation %>% filter(type == "adults")
age_assign_regenera <- age_assignation %>% filter(type == "regeneration")

# Load plot coordinates
parcelas_aoi <- read.csv(paste(di, outputs, "parcelas_en_aoi.csv", sep = ""), sep = ",")

# Load tree observations
PCMayores <- read.csv(paste(di, inputs, "PCMayores.csv", sep = ""), sep = ";")
PCMayores$Especie <- as.factor(PCMayores$Especie)

PCRegenera <- read.csv(paste(di, inputs, "PCRegenera.csv", sep = ""), sep = ";")
PCRegenera$Especie <- as.factor(PCRegenera$Especie)

# Filter parcelas - some have no reported data in PCMayores and PCRegenera
## Otherwise some are assigned empty IC (IC_Code 132) for the wrong reason:
## It is not that the reported species is none of the selected ones, 
## but that there are no species reported at Mayores and Regenera levels 
## (even though they were reported in PCEspParc)
## Besides, only selected species should be considered
parc_mayores <- PCMayores %>%
  inner_join(parcelas_aoi) %>% # select parcelas in aoi with data reported for PCMayores
  inner_join(selection_species, by = c("Especie" = "Species_tree")) %>% # filter out data for non selected species
  dplyr::select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas) %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  dplyr::select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas)

parc_regenera <- PCRegenera %>%
  inner_join(parcelas_aoi) %>% # select parcelas in aoi with data reported for PCRegenera
  inner_join(selection_species, by = c("Especie" = "Species_tree")) %>%
  dplyr::select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas) %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  dplyr::select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas)

parcelas_with_data <- rbind(parc_mayores, parc_regenera) %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  dplyr::select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas)

parcelas_with_data$plot_id <- 1:length(parcelas_with_data$Tabla_Provincia)
# write.csv(parcelas_with_data, paste(di, outputs, "parc_aoi_mayores_regenera.csv", sep = ""), row.names = FALSE)
  
# Substitute parcelas_aoi by parcelas_with_data (to avoid renaming in the whole code)
parcelas_aoi <-  parcelas_with_data

# Calculate biomass of measured old trees
## Load and organise data
mayores_aoi <- PCMayores %>%
  inner_join(parcelas_aoi, by = c("Estadillo", "Cla", "Subclase", "Tabla_Provincia")) %>% # filter plots within area of interest
  mutate(avg_dbh = (Dn1 + Dn2)/20, # calculate normal diameter in cm
         Species_tree = as.factor(Especie)) %>%
  filter(is.na(Species_tree) == FALSE) %>% # * NA species could be assigned a species group (Otras frondosas, Otras coniferas, Otras laurisilvas, Otros pinos)
  left_join(coef, by = c("Species_tree" = "Cod_Especie")) %>%
  mutate(BT = (e^((SSE_BT^2)/2))*(e^a_BT)*(avg_dbh^b_BT), # apply allometric equations
         Br = (e^((SSE_Br^2)/2))*(e^a_Br)*(avg_dbh^b_Br)) %>%
  mutate(Especie = Especie.y) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas, Species_tree, Especie, Equivalente, avg_dbh, Ht, Distanci, BT, Br) %>%
  inner_join(selection_species, by = c("Species_tree" = "Species_tree")) # filter out non selected species

## Add plot radius
## Metodología IFN4: "Un pie de cualquier especie forestal arbórea, cuya relación figura en el documento
## "Clave de las especies forestales arbóreas para el IFN", entra o no entra en dicha muestra en función
## de su diámetro normal y de su distancia al centro de la parcela con arreglo a las siguientes normas:
## 75 mm <= Dn < 125 mm - distancia del árbol al rejón menor o igual a 5 m
## 125 mm <= Dn < 225 mm - distancia del árbol al rejón menor o igual a 10 m
## 225 mm <= Dn < 425 mm - distancia del árbol al rejón menor o igual a 15 m
## 425 mm <= Dn - distancia del árbol al rejón menor o igual a 25 m
mayores_aoi$radius <- c()
for (i in 1:length(mayores_aoi$Tabla_Provincia)) {
  ifelse(mayores_aoi$avg_dbh[i] >= 42.5, mayores_aoi$radius[i] <- 25,
         ifelse(mayores_aoi$avg_dbh[i] >= 22.5, mayores_aoi$radius[i] <- 15,
                ifelse(mayores_aoi$avg_dbh[i] >= 12.5, mayores_aoi$radius[i] <- 10,
                       mayores_aoi$radius[i] <- 5)))
}

# Create all combinations data frame
plot_radius <- data.frame(diameter_class_mm = c("75_125", "125_225", "225_425", ">425"),
                          radius = c(5, 10, 15, 25))
species <- data.frame(Species_tree = levels(as.factor(mayores_aoi$Species_tree)))
all_adult_combinations <- merge(parcelas_aoi, plot_radius) 
all_adult_combinations <- merge(all_adult_combinations, species)
# all_adult_combinations <- all_adult_combinations[,c(5, 2, 3, 4, 6, 7, 8, 9, 10)]# not needed anymore
all_adult_combinations <- all_adult_combinations %>%
  full_join(age_assign_adult) # here a NA estadillo is created, as there is no estadillo with jcommunis

## Calculate biomass per plot, species and diameter class
biom_mayores_grouped <- mayores_aoi %>%
  mutate(Especie = Especie.x) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas,
         Species_tree, Especie, BT, Br, radius) %>%
  group_by(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas,
           Species_tree, Especie, radius) %>%
  summarise(BT_tot_kg = sum(BT, na.rm = TRUE),
            Br_tot_kg = sum(Br, na.rm = TRUE)) %>%
  mutate(BT_adult_tn_ha = (BT_tot_kg / 1000) / (pi * (radius^2)) * 10000,
         Br_adult_tn_ha = (Br_tot_kg / 1000) / (pi * (radius^2)) * 10000) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas,
         Species_tree, Especie, radius, BT_adult_tn_ha, Br_adult_tn_ha)

# Fill in all_adult_combinations dataframe with biomass data
full_adult_combinations <- full_join(all_adult_combinations, biom_mayores_grouped) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas, 
         Species_tree, Assigned_cohort, BT_adult_tn_ha) %>%
  left_join(sp_names, by = c("Species_tree" = "species_code_IFN"))

# Calculate biomass of young trees
dic_densidad <- data.frame(Densidad = c(1, 2, 3), # from IFN3 documentation
                           Densidad_pies = c("1_4", "5_15", ">15"), 
                           Densidad_pies_asignada = c(3, 6, 20))

dic_desarrollo <- data.frame(CatDes = c(1, 2, 3, 4), # from IFN3 documentation
                             Caract = c("Ht<30cm", "Ht30_130cm", "Ht>130cm_dn<25mm", "Ht>130cm_dn25_75mm"))

regenera_aoi <- PCRegenera %>%
  mutate(Species_tree = as.factor(Especie)) %>%
  inner_join(parcelas_aoi) %>% # filter plots within area of interest
  left_join(dic_densidad) %>% # assign density from dic_densidad
  left_join(dic_desarrollo) # assign Caract from dic_desarrollo

for (i in 1:length(regenera_aoi$Estadillo)) { # assign density from NumPies (for CatDes = 4, where seedling and sapling are counted)
  ifelse(is.na(regenera_aoi$Densidad_pies_asignada[i]) == TRUE, 
         regenera_aoi$Densidad_pies_asignada[i] <- regenera_aoi$NumPies[i],
         regenera_aoi$Densidad_pies_asignada[i] <- regenera_aoi$Densidad_pies_asignada[i])
}
regenera_aoi <- regenera_aoi %>%
  mutate(regenerado_pies_m2 = Densidad_pies_asignada / (pi * (5^2)),  # The radious of regeneration plots is always 5
         Ht_regenerado_cm = Hm * 10) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas, 
         Species_tree, Caract, regenerado_pies_m2, Ht_regenerado_cm) %>%
  inner_join(selection_species) # filter out non selected species

# Create all combinations data frame
regenera_species <- data.frame(Species_tree = levels(as.factor(regenera_aoi$Species_tree)))
all_regenera_combinations <- merge(parcelas_aoi, dic_desarrollo)
all_regenera_combinations <- merge(all_regenera_combinations, regenera_species)
# all_regenera_combinations <- all_regenera_combinations[,c(5, 2, 3, 4, 6, 7, 10, 9)] # not needed anymore
all_regenera_combinations <- all_regenera_combinations %>%
  full_join(age_assign_regenera)

# Fill in all_regenera_combinations dataframe with regenerado_pies_m2 data
full_regenera_combinations <- full_join(all_regenera_combinations, regenera_aoi) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas, 
         Species_tree, Assigned_cohort, regenerado_pies_m2) %>%
  left_join(sp_names, by = c("Species_tree" = "species_code_IFN"))

# Generate table with all present cohorts per plot
full_adult_combinations$regenerado_pies_m2 <- NA
full_regenera_combinations$BT_adult_tn_ha <- NA
full <- rbind(full_adult_combinations, full_regenera_combinations) %>% # Merge full_adult_combinations and full_regenera_combinations
  dplyr::filter(is.na(Estadillo) == FALSE) %>% # see check below
  group_by(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas, # Group by cohorts
           Species_tree, species_LANDIS, Assigned_cohort) %>% # Group by plot ids and cohorts
  summarise(BT_tot_adult_tn_ha = sum(BT_adult_tn_ha, na.rm = TRUE), # na.rm needed!!!!
            regenerado_tot_pies_m2 = sum(regenerado_pies_m2, na.rm = TRUE)) %>% # na.rm needed!!!!
  mutate(Species_cohort = paste(species_LANDIS, Assigned_cohort, sep = "_"),
         Sp_Coh_presence = ifelse(BT_tot_adult_tn_ha != 0.0 | regenerado_tot_pies_m2 != 0.0,
                           "species-cohort present",
                           NA))

# write.csv(full, paste(di, outputs, "biomass_calculations.csv", sep = ""), row.names = FALSE)

full_spread <- full[,c(1:6,12,13)] %>%
  spread(Species_cohort, Sp_Coh_presence)

# Check - full spread length == 889, parcelas_with_data == 888
# check <- full_spread %>% left_join(parcelas_with_data) %>%
#   dplyr::select(plot_id) %>%
#   group_by(plot_id) %>%
#   summarise(count = n())
# An empty line was created with no plot data (Estadillo, ...), line 178 added to correct this mistake

# IC codification
ic_coding <- full_spread[,c(7:47)] %>%
  mutate(jcommunis_40 = NA,
         jcommunis_50 = NA) %>%
  group_by_all() %>%
  summarise(count = n())

ic_coding$IC_Code <- 1:length(ic_coding$jcommunis_10)
ic_coding <- ic_coding[,c(45,1:3, 42, 43, 4:41)] # reorganise columns

# write.csv(ic_coding, paste(di, outputs, "ic_coding.csv", sep = ""), row.names = FALSE)

# Generate ic_plots: plot with associated IC code
ic_plots <- full_spread %>%
  left_join(ic_coding) %>% # Add IC code, jcommunis_40 and jcommunis_50
  full_join(parcelas_with_data) %>% # Add plot_id
  select(plot_id, IC_Code,
         Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas)

# write.csv(ic_plots, paste(di, outputs, "ic_plots.csv", sep = ""), row.names = FALSE)

# Generate dataframe with presence/absence of species (to be merged with MFE)
# species_presence <- biom_mayores_grouped %>% old, corrected as it only considered adult trees
species_presence <- full %>%
  filter(is.na(Sp_Coh_presence) == FALSE) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas, 
         Species_tree, species_LANDIS) %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas, 
         Species_tree, species_LANDIS)

# write.csv(species_presence, paste(di, outputs, "plots_species_presence.csv", sep = ""), row.names = FALSE)

# Calculate biomass of shrubs
# matorral_aoi <- read.csv(paste(di, inputs, "PCMatorral.csv", sep = ""), sep = ";") %>% 
#   inner_join(parcelas_aoi) %>% # filter plots within area of interest
#   mutate(Species_shrub = as.factor(Especie),
#          Coverage_shrub_percentage = Fcc,
#          Ht_shrub_cm = Hm * 10) %>%
#   select(Tabla_Provincia, Estadillo, Cla, Subclase, CoorX_parcelas, CoorY_parcelas, Species_shrub, Coverage_shrub_percentage, Ht_shrub_cm)
