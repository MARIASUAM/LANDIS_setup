# Reclasify IC legend and map

library(dplyr)
library(tidyr)
library(raster)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load input files
ic_coding_full <- read.csv(paste(di, outputs, "ic_coding_full.csv", sep = ""))
ic_long <- gather(data = ic_coding_full, # Reorganise data wide to long
                  key = species_cohort, 
                  value = cohort_presence,
                  jcommunis_10:tall_shrubs,
                  factor_key = TRUE) %>%
  mutate_all(as.character) %>%
  filter(is.na(cohort_presence) == FALSE)

# Create species column
species <- c()
for (i in 1:length(ic_long$IC_Code)) {
  species[i] <- strsplit(ic_long$species_cohort[i], split = "_")[[1]][1]
}
ic_long$species <- species

# Ungroup species into genus
ic_long[ic_long == "jcommunis"] <- "Juniper"
ic_long[ic_long == "joxycedrus"] <- "Juniper"
ic_long[ic_long == "ppinaster"] <- "Pinus"
ic_long[ic_long == "phalepensis"] <- "Pinus"
ic_long[ic_long == "psylvestris"] <- "Pinus"
ic_long[ic_long == "pnigra"] <- "Pinus"
ic_long[ic_long == "qilex"] <- "Quercus"
ic_long[ic_long == "qfaginea"] <- "Quercus"
ic_long[ic_long == "qpyrenaica"] <- "Quercus"
ic_long[ic_long == "short"] <- "Shrubs"
ic_long[ic_long == "medium"] <- "Shrubs"
ic_long[ic_long == "tall"] <- "Shrubs"

# Select IC_Code and species column
ic_species <- ic_long %>%
  dplyr::select(IC_Code, species) %>%
  group_by_all() %>%
  summarise(count = n())

# Generate groups column
ic_species[order(IC_Code, species),]
IC_codes <- unique(ic_species$IC_Code)
ic_combinations <- data.frame(IC_code = c(), spp_combination = c())
for (i in 1:length(IC_codes)) {
  subset <- ic_species %>%
    filter(IC_Code == IC_codes[i])
  my_row <- data.frame(IC_code = IC_codes[i],
                       spp_combination = paste0(subset$species, collapse = "-"))
  ic_combinations <- rbind(ic_combinations, my_row)
}

# Extract unique groups
spp_comb <- data.frame(Group_code = 1:length(unique(ic_combinations$spp_combination)),
                       spp_combination = unique(ic_combinations$spp_combination))
# write.csv(spp_comb, paste(di, outputs, "IC_map_reclas_legend.csv", sep = ""), row.names = FALSE)

# Join IC_Code and Group_Code
ic_reclassification <- ic_combinations %>% 
  full_join(spp_comb)%>%
  mutate(IC_map = IC_code, becomes = Group_code) %>%
  dplyr::select(IC_map, becomes)

ic_reclassification <- rbind(ic_reclassification, 
                             data.frame(IC_map = c("996", "997", "998", "999"), 
                                        becomes = c(0, 0, 0, 0)))
ic_reclassification$IC_map <- as.character(ic_reclassification$IC_map)
ic_reclassification$IC_map <- as.numeric(ic_reclassification$IC_map)

# Reclasify IC_map
IC_map <- raster(paste(di, "output_files/", "LANDIS_IC_map.tif", sep = ""))

IC_map_reclassified <- subs(IC_map,
                            ic_reclassification,
                            filename=paste(di, outputs, "IC_map_reclassified.tif", sep = ""), overwrite = TRUE)
