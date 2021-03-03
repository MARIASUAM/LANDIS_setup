# Assign IC to polygons with equivalent polygons

library(dplyr)
library(tidyr)
library(rgdal)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Input files
equivalencies <- read.csv(paste(di, outputs, "polys_equivalencies.csv", sep = "")) %>%
  dplyr::filter(count != 0)
equivalencies$objectid <- as.factor(equivalencies$objectid)

# Disaggregate ICs_to_combine and add IC_Code
equivalencies$nr_elements <- c()
for (i in 1:length(equivalencies$objectid)) {
  equivalencies$nr_elements[i] <- length(strsplit(as.character(equivalencies$ICs_to_combine[i]), split = ", ")[[1]])
}
max(equivalencies$nr_elements) # 16

for (i in 1:length(equivalencies$objectid)) {
  for (j in 1:max(equivalencies$nr_elements)) {
    equivalencies[i,5+j] <- strsplit(as.character(equivalencies$ICs_to_combine[i]), split = ", ")[[1]][j]
  }
}

equivalencies_long <- gather(data = equivalencies, # dissaggregate IC_to_combine
                      key = IC_Code_key,
                      value = IC_Code,
                      V6:V21, # old: V6:V14
                      factor_key = TRUE) %>%
  filter(is.na(IC_Code) == FALSE) %>%
  select(objectid, source, nr_elements, IC_Code)

# Polygons with 1 IC assigned
polygons_1IC <- equivalencies_long %>%
  dplyr::filter(nr_elements == 1) %>%
  dplyr::select(objectid, IC_Code) %>%
  dplyr::mutate_all(as.factor)

# Polygons with 2 IC assigned: IC_Code 132 and another one
polygons_more1IC <- equivalencies_long %>%
  dplyr::filter(nr_elements > 1)

# Export polygons with more than one IC
# summary_polygons_real_more1IC <- polygons_real_more1IC %>% # Old: IC_Code 132 emtpy code
summary_polygons_real_more1IC <- polygons_more1IC %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n()) %>%
  left_join(equivalencies, by = c("objectid" = "objectid")) %>%
  dplyr::select(objectid, ICs_to_combine, source)
write.csv(summary_polygons_real_more1IC, paste(di, outputs, "polygons_more1IC.csv", sep = ""), row.names = F)

# Combine IC of multiple IC polygons, generate new IC_Codes
# Load IC legend
ic_coding_sec <- read.csv(paste(di, outputs, "ic_coding_second.csv", sep = "")) %>%
  mutate_all(as.factor)

# ic_polygons_real_more1IC <- polygons_real_more1IC %>% # old: IC_Code 132 empty IC
ic_polygons_real_more1IC <- polygons_more1IC %>%
  left_join(ic_coding_sec, by = c("IC_Code" = "IC_Code")) %>% 
  mutate_all(as.character)

ic_polygons_real_more1IC[ic_polygons_real_more1IC == "species-cohort present"] <- 1

ic_polygons_real_more1IC <- ic_polygons_real_more1IC[,c(1,5:47)] %>%
  mutate_all(as.numeric) %>%
  group_by(objectid) %>%
  summarise_at(c("jcommunis_10", "jcommunis_20", "jcommunis_30", "jcommunis_40", "jcommunis_50", 
                 "joxycedrus_10", "joxycedrus_20", "joxycedrus_30", "joxycedrus_40", "joxycedrus_50", 
                 "phalepensis_10", "phalepensis_30", "phalepensis_40", "phalepensis_50", 
                 "pnigra_10", "pnigra_20", "pnigra_30", "pnigra_40", "pnigra_50", 
                 "popnigra_10", "popnigra_20", "popnigra_30", "popnigra_40", 
                 "ppinaster_10", "ppinaster_30", "ppinaster_40", "ppinaster_50", 
                 "psylvestris_10", "psylvestris_20", "psylvestris_40", "psylvestris_50", 
                 "qfaginea_10", "qfaginea_30", "qfaginea_40", "qfaginea_50", 
                 "qilex_10", "qilex_30", "qilex_40", "qilex_50", 
                 "qpyrenaica_10", "qpyrenaica_30", "qpyrenaica_40", "qpyrenaica_50"), 
               sum, na.rm = TRUE)

# Recodify presence/absence
max(ic_polygons_real_more1IC[,c(2:44)]) # 15
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 0] <- NA 
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 1] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 2] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 3] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 4] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 5] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 6] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 7] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 8] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 9] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 10] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 11] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 12] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 13] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 14] <- "species-cohort present"
ic_polygons_real_more1IC[ic_polygons_real_more1IC == 15] <- "species-cohort present"

# Generate and export new list of IC_Codes
new_IC_Codes <- ic_polygons_real_more1IC[,c(2:44)] %>%
  group_by_all() %>%
  summarise(count = n())

new_IC_Codes <- as.data.frame(new_IC_Codes[,c(1:43)]) %>%
  mutate_all(as.factor) %>%
  left_join(ic_coding_sec) # check whether some IC are already in ic_coding and add corresponding IC_Code

not_really_new_IC_Codes <- new_IC_Codes %>%
  dplyr::filter(is.na(IC_Code) == FALSE) %>%
  mutate_all(as.factor)

really_new_IC_Codes <- new_IC_Codes %>%
  dplyr::filter(is.na(IC_Code) == TRUE)
really_new_IC_Codes$IC_Code <- max(as.numeric(ic_coding_sec$IC_Code)) + 1:length(really_new_IC_Codes$jcommunis_10)
really_new_IC_Codes <- really_new_IC_Codes %>%
  mutate_all(as.factor)

new_ic_coding <- rbind(ic_coding_sec, 
                       really_new_IC_Codes)

write.csv(new_ic_coding, 
          paste(di, outputs, "ic_coding_third.csv", sep = ""), row.names = FALSE)

# Add IC_Code to multiple_IC and select objectid, IC_Code

ic_polygons_real_more1IC <- ic_polygons_real_more1IC %>% 
  mutate_all(as.factor) %>%
  left_join(new_ic_coding) %>%
  dplyr::select(objectid, IC_Code)

# Polygons with plots or equivalent polygons
ic_polygons_without_plots <- rbind(polygons_1IC,
                                   # polygons_1IC_and_IC132, # old: IC_Code 132 empty IC
                                   ic_polygons_real_more1IC)

write.csv(ic_polygons_without_plots, paste(di, outputs, "ic_all_equiv_polygons.csv", sep = ""), row.names = F)
