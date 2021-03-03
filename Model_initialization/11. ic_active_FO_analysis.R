## Shrublands and crops FO analysis

library(dplyr)
library(tidyr)
library(raster)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load raster layers 

ARBU1 <- raster(paste(di, outputs, "active_vege10_D_ARBU1_FO_25830.asc", sep = ""))
ARBU2 <- raster(paste(di, outputs, "active_vege10_D_ARBU2_FO_25830.asc", sep = ""))
ARBU3 <- raster(paste(di, outputs, "active_vege10_D_ARBU3_FO_25830.asc", sep = ""))
ARBU4 <- raster(paste(di, outputs, "active_vege10_D_ARBU4_FO_25830.asc", sep = ""))

HERB1 <- raster(paste(di, outputs, "active_vege10_D_HERB1_FO_25830.asc", sep = ""))
HERB2 <- raster(paste(di, outputs, "active_vege10_D_HERB2_FO_25830.asc", sep = ""))
HERB3 <- raster(paste(di, outputs, "active_vege10_D_HERB3_FO_25830.asc", sep = ""))

plot(ARBU1)

# Load legends
legend_ARBU1 <- read.csv(paste(di, outputs, "legend_active_vege10_D_ARBU1_FO.csv", sep = ""), sep = ",") %>%
  mutate(ARBU1_Code = Code, ARBU1_Value = Value) %>%
  dplyr::select(ARBU1_Code, ARBU1_Value)

legend_ARBU2 <- read.csv(paste(di, outputs, "legend_active_vege10_D_ARBU2_FO.csv", sep = ""), sep = ",") %>%
  mutate(ARBU2_Code = Code, ARBU2_Value = Value) %>%
  dplyr::select(ARBU2_Code, ARBU2_Value)

legend_ARBU3 <- read.csv(paste(di, outputs, "legend_active_vege10_D_ARBU3_FO.csv", sep = ""), sep = ",") %>%
  mutate(ARBU3_Code = Code, ARBU3_Value = Value) %>%
  dplyr::select(ARBU3_Code, ARBU3_Value)

legend_ARBU4 <- read.csv(paste(di, outputs, "legend_active_vege10_D_ARBU4_FO.csv", sep = ""), sep = ",") %>%
  mutate(ARBU4_Code = Code, ARBU4_Value = Value) %>%
  dplyr::select(ARBU4_Code, ARBU4_Value)

legend_HERB1 <- read.csv(paste(di, outputs, "legend_active_vege10_D_HERB1_FO.csv", sep = ""), sep = ",") %>%
  mutate(HERB1_Code = Code, HERB1_Value = Value) %>%
  dplyr::select(HERB1_Code, HERB1_Value)

legend_HERB2 <- read.csv(paste(di, outputs, "legend_active_vege10_D_HERB2_FO.csv", sep = ""), sep = ",") %>%
  mutate(HERB2_Code = Code, HERB2_Value = Value) %>%
  dplyr::select(HERB2_Code, HERB2_Value)

legend_HERB3 <- read.csv(paste(di, outputs, "legend_active_vege10_D_HERB3_FO.csv", sep = ""), sep = ",") %>%
  mutate(HERB3_Code = Code, HERB3_Value = Value) %>%
  dplyr::select(HERB3_Code, HERB3_Value)

# Create data frame with cell-FO data

df <- data.frame(Cell = 1:length(ARBU1[]), 
                 ARBU1_Code = ARBU1[],
                 ARBU2_Code = ARBU2[],
                 ARBU3_Code = ARBU3[],
                 ARBU4_Code = ARBU4[],
                 HERB1_Code = HERB1[],
                 HERB2_Code = HERB2[],
                 HERB3_Code = HERB3[]) %>%
  dplyr::filter(is.na(ARBU1_Code) == FALSE) %>% # discard empty rows (NA cells)
  left_join(legend_ARBU1) %>%
  left_join(legend_ARBU2) %>%
  left_join(legend_ARBU3) %>%
  left_join(legend_ARBU4) %>%
  left_join(legend_HERB1) %>%
  left_join(legend_HERB2) %>%
  left_join(legend_HERB3) %>% 
  dplyr::select(Cell, ARBU1_Value, ARBU2_Value, ARBU3_Value, ARBU4_Value,
                HERB1_Value, HERB2_Value, HERB3_Value) %>%
  gather(com_level, community, ARBU1_Value:HERB3_Value, factor_key=TRUE)
  
# Discard no data rows
df[df == "Sin valor"] <- NA
df <- df %>% dplyr::filter(is.na(community) == FALSE)

# Create FO classification by functional groups
funct_groups <- data.frame(funct_group = c("very_tall_shrubs", "tall_shrubs", "medium_shrubs", "short_shrubs"),
                           height = c("5-50 m", "2-5 m", "0.5-2 m", "< 0.5 m")) %>%
  mutate_all(as.character)

FO_cell_class <- data.frame(community = levels(as.factor(df$community)))
FO_cell_class$community <- as.character(FO_cell_class$community)
FO_cell_class$height <- c("< 0.5 m", NA, NA, "2-5 m", "0.5-2 m","0.5-2 m", NA)

FO_cell_class <- left_join(FO_cell_class, funct_groups)

# Join cell info with cell classification
community_class <- df %>% left_join(FO_cell_class)

# Extract unique functional groups per cell
funct_cell <- community_class %>%
  dplyr::select(Cell, funct_group) %>%
  dplyr::filter(is.na(funct_group) == FALSE) %>%
  group_by_all() %>%
  summarise(count = n()) %>%
  dplyr::select(Cell, funct_group)

# Find present groups combinations per cell

funct_cell <- funct_cell %>%
  mutate(presence = "present")

cell_comb <- spread(data = funct_cell, funct_group, presence)

# Create IC_Code for shrub groups combinations
ic_coding_shrubs <- data.frame(IC_Code = 1010:1016,
                        short_shrubs = c("present", NA, NA, "present", NA, "present", "present"),
                        medium_shrubs = c(NA, "present", NA, "present", "present", NA, "present"),
                        tall_shrubs = c(NA, NA, "present", NA, "present", "present", "present"))

write.csv(ic_coding_shrubs, paste(di, outputs, "ic_coding_shrubs.csv", sep = ""))

# Assign IC_Codes to cells

cell_comb <- cell_comb %>%
  left_join(ic_coding_shrubs)

# Add empty cells to table
all_cells <- data.frame(Cell = 1:length(ARBU1[]))
ic_shrubs <- full_join(cell_comb, all_cells) %>%
  dplyr::select(Cell, IC_Code)
# Order table by cell number
ic_shrubs <- ic_shrubs[order(ic_shrubs$Cell),]

# Generate raster with IC_Code
ic_shrubs_map <- raster(ARBU1)
values(ic_shrubs_map) <- ic_shrubs$IC_Code
plot(ic_shrubs_map)

# Export raster

writeRaster(ic_shrubs_map, paste(di, outputs, "ic_shrubs_map_25830.asc", sep = ""), overwrite = TRUE)
