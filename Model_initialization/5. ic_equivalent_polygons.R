# Find equivalent polygons to forest polygons without plot

library(dplyr)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Input files
polygons <- read.csv(paste(di, outputs, "polygons_IC_origin.csv", sep = ""), sep = ",")
polygons$objectid <- as.factor(polygons$objectid)

ic_polygons_with_plots <- read.csv(paste(di, outputs, "ic_polygons_with_plots.csv", sep = ""), sep = ",")
ic_polygons_with_plots <- ic_polygons_with_plots %>% mutate_all(as.factor)

all_matches <- read.csv(paste(di, outputs, "all_poly_point_match.csv", sep = ""), sep = ",")
all_matches$objectid <- as.factor(all_matches$objectid)
all_matches$plot_id <- as.factor(all_matches$plot_id)

selection_species <- read.csv(paste(di, inputs, "especies_ocupa.csv", sep = ""), sep = ";")
selection_species$Species_tree <- as.factor(selection_species$Species_tree)

mfe_data <- read.csv(paste(di, outputs, "mfe_aoi_active_data.csv", sep = ""), sep = ",")
mfe_data$objectid <- as.factor(mfe_data$objectid)
mfe_data[mfe_data == "sin datos"] <- NA
mfe_data[mfe_data == "Sin datos"] <- NA

# Include only selected species
mfe_data$especie1 <- as.character(mfe_data$especie1)
for (i in 1:length(mfe_data$especie1)) {
  ifelse(is.na(mfe_data$especie1[i]), mfe_data$especie1[i] <- mfe_data$especie1[i],
         ifelse(mfe_data$especie1[i] %in% selection_species$Especie,
                mfe_data$especie1[i] <- mfe_data$especie1[i],
                mfe_data$especie1[i] <- NA))
}

mfe_data$especie2 <- as.character(mfe_data$especie2)
for (i in 1:length(mfe_data$especie2)) {
  ifelse(is.na(mfe_data$especie2[i]), mfe_data$especie2[i] <- mfe_data$especie2[i],
         ifelse(mfe_data$especie2[i] %in% selection_species$Especie,
                mfe_data$especie2[i] <- mfe_data$especie2[i],
                mfe_data$especie2[i] <- NA))
}

mfe_data$especie3 <- as.character(mfe_data$especie3)
for (i in 1:length(mfe_data$especie3)) {
  ifelse(is.na(mfe_data$especie3[i]), mfe_data$especie3[i] <- mfe_data$especie3[i],
         ifelse(mfe_data$especie3[i] %in% selection_species$Especie,
                mfe_data$especie3[i] <- mfe_data$especie3[i],
                mfe_data$especie3[i] <- NA))
}

# Transform fcctot variable to ranks
levels(as.factor(mfe_data$fcctot))
plot(mfe_data$fcctot)
mfe_data <- mfe_data %>%
  mutate(fcctot_rank = ifelse(fcctot >= 75,
                              "75-100",
                              ifelse(fcctot >= 50,
                                     "50-75",
                                     ifelse(fcctot >= 25,
                                            "25-50",
                                            "0-25"))))
mfe_data$fcctot_rank <- as.factor(mfe_data$fcctot_rank)
# plot(mfe_data$fcctot_rank)

# FOREST POLYGONS WITHOUT NFI PLOTS
noplot_polys <- polygons %>%
  filter(IC_origin == "derived from similar plots") %>% # extract forest polygons without plot
  dplyr::select(objectid) %>% # select only objectid variable
  left_join(mfe_data) %>% # join mfe_data and extract useful variables for comparison
  dplyr::select(objectid, form_arb_d, 
                especie1, estado1, especie2, estado2, especie3, estado3,
                fcctot_rank) %>% 
  mutate(all_variables = paste(form_arb_d, especie1, estado1, especie2, estado2, especie3, estado3, fcctot_rank, sep = "_"), # Create variable as the combination of other variables
         some_variables_level1 = paste(form_arb_d, especie1, especie2, especie3, fcctot_rank, sep = "_"),
         some_variables_level2 = paste(especie1, especie2, especie3, fcctot_rank, sep = "_"),
         some_especies = paste(especie1, especie2, especie3, sep = "_"))

# FOREST POLYGONS WITH NFI PLOTS & MATCHING SPECIES --> plot_polys
true_matches <- all_matches %>%
  filter(overlap == TRUE) %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n()) %>%
  dplyr::select(objectid)
  
plot_polys <- true_matches %>% # polygons with matching species
  left_join(mfe_data) %>% # join mfe_data and extract useful variables for comparison
  mutate(all_variables = paste(form_arb_d, especie1, estado1, especie2, estado2, especie3, estado3, fcctot_rank, sep = "_"), # Create variable as the combination of other variables
         some_variables_level1 = paste(form_arb_d, especie1, especie2, especie3, fcctot_rank, sep = "_"),
         some_variables_level2 = paste(especie1, especie2, especie3, fcctot_rank, sep = "_"),
         some_especies = paste(especie1, especie2, especie3, sep = "_")) %>% # Generate variables for comparison
  left_join(ic_polygons_with_plots) %>%
  dplyr::select(objectid, IC_Code,
                all_variables, some_variables_level1, some_variables_level2, some_especies)

## STEP I: Compare polygons without plot with polygons with plots (with matching species only, plot_polys)

## BASED ON ALL SELECTED VARIABLES
noplot_polys_all_variables <- noplot_polys %>% dplyr::select(objectid, all_variables)

plot_polys_all_variables <- plot_polys %>% dplyr::select(objectid, all_variables, IC_Code)

comparison <- noplot_polys_all_variables %>%
  left_join(plot_polys_all_variables, by = "all_variables")
colnames(comparison) <- c("objectid", "all_variables", "equivalent_objectid", "IC_Code")

# Full equivalent polygons
full_equiv_poly <- comparison %>% 
  filter(is.na(equivalent_objectid) == FALSE) %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n())

# Assign ICs_to_combine to full equivalent polygons
full_equiv_poly$ICs_to_combine <- c()
for (i in 1:length(full_equiv_poly$objectid)) {
  my_subset <- comparison %>%
    dplyr::filter(objectid == full_equiv_poly$objectid[i]) %>%
    dplyr::select(objectid, IC_Code) %>%
    group_by_all() %>%
    summarise(count = n()) %>%
    dplyr::select(objectid, IC_Code)
  full_equiv_poly$ICs_to_combine[i] <- paste0(my_subset$IC_Code, collapse = ", ")
  }

# No full equivalent polygons - level 1

noplot_polys_some_variables <- comparison %>%
  filter(is.na(equivalent_objectid) == TRUE) %>%
  left_join(noplot_polys, by = c("objectid" = "objectid")) %>%
  select(objectid, some_variables_level1)

plot_polys_some_variables <- plot_polys %>%
  dplyr::select(objectid, some_variables_level1, IC_Code)

comparison_some <- noplot_polys_some_variables %>%
  left_join(plot_polys_some_variables, by = "some_variables_level1")
colnames(comparison_some) <- c("objectid", "some_variables_level1", "equivalent_objectid", "IC_Code")

partly_equiv_poly <- comparison_some %>% 
  filter(is.na(equivalent_objectid) == FALSE) %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n())

# Assign IC_origin to partly equivalent polygons
partly_equiv_poly$ICs_to_combine <- c()
for (i in 1:length(partly_equiv_poly$objectid)) {
  my_subset <- comparison_some %>%
    dplyr::filter(objectid == partly_equiv_poly$objectid[i]) %>%
    dplyr::select(objectid, IC_Code) %>%
    group_by_all() %>%
    summarise(count = n()) %>%
    dplyr::select(objectid, IC_Code)
  partly_equiv_poly$ICs_to_combine[i] <- paste0(my_subset$IC_Code, collapse = ", ")
}

comparison_some %>% 
  filter(is.na(equivalent_objectid) == TRUE) %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n())

# No full equivalent polygons - level 2

noplot_polys_some_variables2 <- comparison_some %>%
  filter(is.na(equivalent_objectid) == TRUE) %>%
  left_join(noplot_polys, by = c("objectid" = "objectid")) %>%
  select(objectid, some_variables_level2)

plot_polys_some_variables2 <- plot_polys %>%
  dplyr::select(objectid, some_variables_level2, IC_Code)

comparison_some2 <- noplot_polys_some_variables2 %>%
  left_join(plot_polys_some_variables2, by = "some_variables_level2")
colnames(comparison_some2) <- c("objectid", "some_variables_level2", "equivalent_objectid", "IC_Code")

partly_equiv_poly2 <- comparison_some2 %>% 
  filter(is.na(equivalent_objectid) == FALSE) %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n())

# Assign IC_origin to partly equivalent polygons level 2
partly_equiv_poly2$ICs_to_combine <- c()
for (i in 1:length(partly_equiv_poly2$objectid)) {
  my_subset <- comparison_some2 %>%
    dplyr::filter(objectid == partly_equiv_poly2$objectid[i])  %>%
    dplyr::select(objectid, IC_Code) %>%
    group_by_all() %>%
    summarise(count = n()) %>%
    dplyr::select(objectid, IC_Code)
  partly_equiv_poly2$ICs_to_combine[i] <- paste0(my_subset$IC_Code, collapse = ", ")
}

comparison_some2 %>% 
  filter(is.na(equivalent_objectid) == TRUE) %>%
  dplyr::select(objectid) %>%
  group_by(objectid) %>%
  summarise(count = n())

# What to do with remaining polygons

remaining <- comparison_some2 %>%
  filter(is.na(equivalent_objectid) == TRUE) %>%
  dplyr::select(objectid) %>%
  left_join(noplot_polys, by = c("objectid" = "objectid")) %>%
  mutate(count = 0,
         ICs_to_combine = NA,
         source = "NO equivalent polygon") %>%
  dplyr::select(objectid, count, ICs_to_combine, source)

write.csv(remaining, 
          paste(di, outputs, "no_equivalent_polys.csv", sep = ""), 
          row.names = FALSE)

## Generate whole table of equivalent polygons 

full_equiv_poly$source <- "FULL equivalent polygon"
partly_equiv_poly$source <- "LEVEL 1 equivalent polygon"
partly_equiv_poly2$source <- "LEVEL 2 equivalent polygon"

equivalencies <- rbind(full_equiv_poly, 
                       partly_equiv_poly, 
                       partly_equiv_poly2, 
                       remaining)

write.csv(equivalencies, 
          paste(di, outputs, "polys_equivalencies.csv", sep = ""), 
          row.names = FALSE)
