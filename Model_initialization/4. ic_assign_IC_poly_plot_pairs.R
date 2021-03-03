# Assign IC to polygons

library(dplyr)
library(tidyr)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

## Load input files

# IC of plots
ic_plots <- read.csv(paste(di, outputs, "ic_plots.csv", sep = "")) %>%
  select(plot_id, IC_Code) %>%
  mutate_all(as.factor)

ic_coding <- read.csv(paste(di, outputs, "ic_coding.csv", sep = "")) %>%
  mutate_all(as.factor)

# Polygon-plots equivalencies
polygons_IC_origin <- read.csv(paste(di, outputs, "polygons_IC_origin.csv", sep = ""))
polygons_IC_origin$objectid <- as.factor(polygons_IC_origin$objectid)

# Polygons with overlapping plot/s
polygons_with_plots <- polygons_IC_origin %>%
  filter(is.na(plot_id) == FALSE) %>%
  select(objectid, IC_origin)

# Dissaggregate IC_origin and add IC_Code
polygons_with_plots$nr_elements <- c()
for (i in 1:length(polygons_with_plots$objectid)) {
  polygons_with_plots$nr_elements[i] <- length(strsplit(as.character(polygons_with_plots$IC_origin[i]), split = ", ")[[1]])
}
max(polygons_with_plots$nr_elements)

for (i in 1:length(polygons_with_plots$objectid)) {
  for (j in 1:max(polygons_with_plots$nr_elements)) {
    polygons_with_plots[i,3+j] <- strsplit(as.character(polygons_with_plots$IC_origin[i]), split = ", ")[[1]][j]
  }
}

polygons_IC <- gather(data = polygons_with_plots, # dissaggregate IC_origin
                      key = plot_id_key,
                      value = plot_id,
                      V4:V7,
                      factor_key = TRUE) %>%
  filter(is.na(plot_id) == FALSE) %>%
  select(objectid, nr_elements, plot_id) %>%
  left_join(ic_plots) %>% # Assign IC Code to polygons
  select(objectid, IC_Code) %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  select(objectid, IC_Code)

length(unique(polygons_IC$objectid))

# Check polygons IC_Codes
nr_IC <- polygons_IC %>%
  select(objectid) %>%
  group_by(objectid) %>%
  summarise(nr_different_IC = n()) # how many different IC has each polygon

# Extract IC_Code of unique_IC cases
unique_IC <- nr_IC %>%
  filter(nr_different_IC == 1) # filter polygons with 1 IC
unique_IC <- left_join(unique_IC, polygons_IC) %>%
  select(objectid, IC_Code)

# Decide IC for multiple IC_Code polygons

## Identify whether there is an empty IC_Code
empty_ic_check <- ic_coding[,c(2:44)] %>%
  mutate_all(as.character)
empty_ic_check[empty_ic_check == "species-cohort present"] <- 1
empty_ic_check[is.na(empty_ic_check) == TRUE] <- 0
empty_ic_check <- empty_ic_check %>%
  mutate_all(as.integer) %>%
  mutate(sp_coh_sum = jcommunis_10+jcommunis_20+jcommunis_30+jcommunis_40+jcommunis_50+
           joxycedrus_10+joxycedrus_20+joxycedrus_30+joxycedrus_40+joxycedrus_50+
           phalepensis_10+phalepensis_30+phalepensis_40+phalepensis_50+
           pnigra_10+pnigra_20+pnigra_30+pnigra_40+pnigra_50+
           popnigra_10+popnigra_20+popnigra_30+popnigra_40+
           ppinaster_10+ppinaster_30+ppinaster_40+ppinaster_50+
           psylvestris_10+psylvestris_20+psylvestris_40+psylvestris_50+
           qfaginea_10+qfaginea_30+qfaginea_40+qfaginea_50+
           qilex_10+qilex_30+qilex_40+qilex_50+
           qpyrenaica_10+qpyrenaica_30+qpyrenaica_40+qpyrenaica_50)

# Combine ICs to create new IC_Codes (see old code at the end of doc if needed)
multiple_IC <- nr_IC %>%
  filter(nr_different_IC > 1) %>% # filter polygons with multiple IC
  left_join(polygons_IC) %>% # to see ICs assigned to each polygon 
  select(objectid, nr_different_IC, IC_Code) %>%
  left_join(ic_coding) %>% # Add IC composition
  mutate_all(as.character)

write.csv(multiple_IC, 
          paste(di, outputs, "multiple_IC_polygons.csv", sep = ""), 
          row.names = FALSE)

multiple_IC[multiple_IC == "species-cohort present"] <- 1
multiple_IC <- multiple_IC %>% mutate_all(as.numeric)
multiple_IC <- multiple_IC[,c(1,4:46)] 
multiple_IC <- multiple_IC %>%
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

max(multiple_IC[,c(2:44)])
multiple_IC[multiple_IC == 0] <- NA
multiple_IC[multiple_IC == 1] <- "species-cohort present"
multiple_IC[multiple_IC == 2] <- "species-cohort present"
multiple_IC[multiple_IC == 3] <- "species-cohort present"

# Export new list of IC_Codes
new_IC_Codes <- multiple_IC[,c(2:44)] %>% 
  group_by_all() %>%
  summarise(count = n())

new_IC_Codes <- as.data.frame(new_IC_Codes[,c(1:43)]) %>%
  mutate_all(as.factor) %>% 
  left_join(ic_coding) # check whether some IC are already in ic_coding and add corresponding IC_Code

not_really_new_IC_Codes <- new_IC_Codes %>%
  filter(is.na(IC_Code) == FALSE) %>%
  mutate_all(as.factor)

really_new_IC_Codes <- new_IC_Codes %>%
  filter(is.na(IC_Code) == TRUE)
really_new_IC_Codes$IC_Code <- max(as.numeric(ic_coding$IC_Code)) + 1:length(really_new_IC_Codes$jcommunis_10)
really_new_IC_Codes <- really_new_IC_Codes %>%
  mutate_all(as.factor)

new_ic_coding <- rbind(ic_coding, 
                       really_new_IC_Codes)

write.csv(new_ic_coding, 
          paste(di, outputs, "ic_coding_second.csv", sep = ""), row.names = FALSE)

## Identify whether there is an empty IC_Code in new codes
empty_ic_check <- really_new_IC_Codes[,c(1:43)] %>%
  mutate_all(as.character)
empty_ic_check[empty_ic_check == "species-cohort present"] <- 1
empty_ic_check[is.na(empty_ic_check) == TRUE] <- 0
empty_ic_check <- empty_ic_check %>%
  mutate_all(as.integer) %>%
  mutate(sp_coh_sum = jcommunis_10+jcommunis_20+jcommunis_30+jcommunis_40+jcommunis_50+
           joxycedrus_10+joxycedrus_20+joxycedrus_30+joxycedrus_40+joxycedrus_50+
           phalepensis_10+phalepensis_30+phalepensis_40+phalepensis_50+
           pnigra_10+pnigra_20+pnigra_30+pnigra_40+pnigra_50+
           popnigra_10+popnigra_20+popnigra_30+popnigra_40+
           ppinaster_10+ppinaster_30+ppinaster_40+ppinaster_50+
           psylvestris_10+psylvestris_20+psylvestris_40+psylvestris_50+
           qfaginea_10+qfaginea_30+qfaginea_40+qfaginea_50+
           qilex_10+qilex_30+qilex_40+qilex_50+
           qpyrenaica_10+qpyrenaica_30+qpyrenaica_40+qpyrenaica_50)

# No empty IC

# Add IC_Code to multiple_IC and select objectid, IC_Code

multiple_IC <- multiple_IC %>%
  mutate_all(as.factor) %>%
  left_join(new_ic_coding) %>%
  select(objectid, IC_Code)
 
# Make list of all polygons and IC (unique and multiple)

ic_polygons <- rbind(unique_IC, multiple_IC)

write.csv(ic_polygons, 
          paste(di, outputs, "ic_polygons_with_plots.csv", sep = ""), row.names = FALSE)

