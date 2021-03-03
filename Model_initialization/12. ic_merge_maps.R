# Merge IC maps: ic_map_3042, ic_shrubs_trees and ic_shrubs_crops

library(dplyr)
library(raster)

# Setup folders
di <- "..."
outputs <- "output_files/"

# Load maps
trees_forest <- raster(paste(di, outputs, "ic_map_3042.asc", sep = ""))
trees_no_forest <- raster(paste(di, outputs, "ic_shrubs_crops_trees.asc", sep = ""))
shrubs <- raster(paste(di, outputs, "ic_shrubs_map_25830.asc", sep = ""))

# Load legends and assimilate to same expression
trees_leg <- read.csv(paste(di, outputs, "ic_coding_fourth.csv", sep = ""), sep = ",") %>%
  mutate_all(as.character)
trees_leg$IC_Code <- as.integer(trees_leg$IC_Code)

shrubs_leg <- read.csv(paste(di, outputs, "ic_coding_shrubs.csv", sep = ""), sep = ",")
shrubs_leg <- shrubs_leg %>% 
  mutate_all(as.character) %>%
  dplyr::select(IC_Code, short_shrubs, medium_shrubs, tall_shrubs)
shrubs_leg$IC_Code <- as.integer(shrubs_leg$IC_Code)
shrubs_leg[shrubs_leg == "present"] <- "species-cohort present"

# Process:
# - Generate a data frame with all ic codes for all cells
# - Inactive cells remain inactive
# - 1001&1002 -> if value in trees no forest or shrubs, value (combination), if no value, 436
# - 436 --> if value in shrubs, shrubs code, ifnot 436

# Generate dataframe with ic codes per cell
df <- data.frame(Cell = 1:length(trees_forest[]),
                 trees_forest = trees_forest[],
                 trees_no_forest = trees_no_forest[],
                 shrubs = shrubs[])

# Subgroups
inactive <- df %>% # ok
  filter(trees_forest >=996, trees_forest <=999)
  
noforest <- df %>% # ok
  filter(trees_forest >=1001, trees_forest <=1002)

empty_1 <- df %>%
  filter(trees_forest == 436)

empty_2 <- df %>%
  filter(trees_no_forest == 436)

# Select cells within aoi, keep no aoi cells for later merging
df <- df %>%
  mutate(aoi = ifelse(is.na(trees_forest) == TRUE, 0, 1))

df_aoi <- df %>% filter(aoi == 1)
df_no_aoi <- df %>% filter(aoi == 0)

# Discard inactive cells in aoi
df_aoi_active <- df_aoi %>%
  filter(trees_forest != 996,
         trees_forest != 997,
         trees_forest != 998,
         trees_forest != 999)

# Setup IC_Code 1001, 1002 and 436 as NA for posterior code merging
df_aoi_active[df_aoi_active == 1001] <- NA
df_aoi_active[df_aoi_active == 1002] <- NA
df_aoi_active[df_aoi_active == 436] <- NA

# Code merging
df_aoi_active <- df_aoi_active %>%
  mutate(IC_merged = ifelse(is.na(trees_forest) == FALSE,
                            paste(trees_forest, shrubs, sep = "-"),
                                  paste(trees_no_forest, shrubs, sep = "-")))
         
# List of new codes
ic_merged_codes <- data.frame(Code = 1:length(unique(df_aoi_active$IC_merged)),
                              IC_merged = unique(df_aoi_active$IC_merged)) %>%
  mutate(Code = 2000 + Code) # New codification starts at 2000, to avoid overlapping with previous codes. 
  
# Join df with list of codes
df_aoi_active_coding <- df_aoi_active %>%
  full_join(ic_merged_codes)

# Regenerate full df - all cells
## Active cells with IC_Code recodified
df_aoi_active_coding <- df_aoi_active_coding %>% 
  dplyr::select(Cell, Code)
## Inactive cells
inactive <- inactive %>%
  mutate(Code = trees_forest) %>%
  dplyr::select(Cell, Code)
## Cells out of aoi
df_no_aoi <- df_no_aoi %>%
  dplyr::select(Cell) %>%
  mutate(Code = NA)
## Merging
df_codes <- rbind(df_aoi_active_coding, inactive)
df_codes <- rbind(df_codes, df_no_aoi)

# Generate and export map
## Order table by cell number
df_codes <- df_codes[order(df_codes$Cell),]
## Create raster with IC_Code
df_codes_raster <- raster(trees_forest)
values(df_codes_raster) <- df_codes$Code
# plot(df_codes_raster$layer)
## Export raster
# writeRaster(df_codes_raster, paste(di, outputs, "IC_map_trees_and_shrubs.asc", sep = ""), overwrite = TRUE)

# Generate presence/absence list of species (shrubs & trees) with the corresponding IC_Code
## Extract trees and shrubs codes in list of new codes
ic_merged_codes$trees_code <- c()
ic_merged_codes$shrubs_code <- c()
ic_merged_codes$IC_merged <- as.character(ic_merged_codes$IC_merged)
for (i in 1:length(ic_merged_codes$Code)) {
  ic_merged_codes$trees_code[i] <- strsplit(ic_merged_codes$IC_merged[i], split = "-")[[1]][1]
  ic_merged_codes$shrubs_code[i] <- strsplit(ic_merged_codes$IC_merged[i], split = "-")[[1]][2]
}
ic_merged_codes[ic_merged_codes == "NA"] <- NA
ic_merged_codes$trees_code <- as.integer(ic_merged_codes$trees_code)
ic_merged_codes$shrubs_code <- as.integer(ic_merged_codes$shrubs_code)

## Create empty trees and shrubs IC data frames
empty_trees_IC <- trees_leg %>% filter(IC_Code == 436)
empty_trees_IC <- empty_trees_IC[,-1]

empty_shrubs_IC <- shrubs_leg[1,-1]
empty_shrubs_IC$short_shrubs[1] <- NA

## Create dataframe ic_coding_full with all species (trees and shrubs combinations)
ic_coding_full <- data.frame()
for(j in 1:length(ic_merged_codes$Code)) {
  trees_IC <- trees_leg %>%
    filter(IC_Code == ic_merged_codes$trees_code[j])
  ifelse(dim(trees_IC)[1] == 0,
         trees_IC <- empty_trees_IC,
         trees_IC <- trees_IC[,-1])
  
  shrubs_IC <- shrubs_leg %>%
    filter(IC_Code == ic_merged_codes$shrubs_code[j])
  ifelse(dim(shrubs_IC)[1] == 0,
         shrubs_IC <- empty_shrubs_IC,
         shrubs_IC <- shrubs_IC[,-1])
  
  temp <- cbind(trees_IC, shrubs_IC)
  
  ic_coding_full <- rbind(ic_coding_full, temp)
}
ic_coding_full$IC_Code <- ic_merged_codes$Code
  
# write.csv(ic_coding_full, paste(di, outputs, "ic_coding_full.csv", sep = ""))

# Analysis of the maps
values_trees_forest <- data.frame(Cell = 1:length(trees_forest[]), # Assign cell number
                                  trees_forest = trees_forest[])
values_trees_forest$trees_forest[values_trees_forest$trees_forest == 996] <- 0
values_trees_forest$trees_forest[values_trees_forest$trees_forest == 997] <- 0
values_trees_forest$trees_forest[values_trees_forest$trees_forest == 998] <- 0
values_trees_forest$trees_forest[values_trees_forest$trees_forest == 999] <- 0
values_trees_forest$trees_forest[values_trees_forest$trees_forest != 0] <- 1

sup_trees_forest <- sum(values_trees_forest$trees_forest, na.rm = TRUE)

values_trees_no_forest <- data.frame(Cell = 1:length(trees_no_forest[]),
                                  trees_no_forest = trees_no_forest[])
values_trees_no_forest$trees_no_forest[is.na(values_trees_no_forest$trees_no_forest) == FALSE] <- 1

sup_trees_no_forest <- sum(values_trees_no_forest$trees_no_forest, na.rm = TRUE)

sup_trees_no_forest / sup_trees_forest * 100
