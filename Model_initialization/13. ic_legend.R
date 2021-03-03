# Generate Initial Communities legend

library(dplyr)
library(tidyr)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load input file
ic_coding_full <- read.csv(paste(di, outputs, "ic_coding_full.csv", sep = ""))

# Create output file
ic_legend <- paste(di, outputs, "ic_legend.txt", sep = "") #This becomes the output IC file.

# Create heading
title1 <- noquote("LandisData ")     #First half of first line.
title2 <- ('"Initial Communities"')  #Second half of first line.
cat(title1, file=ic_legend, append=FALSE)                 #Write first half of first line to output file 
cat(title2, file=ic_legend, sep="\n", append=TRUE)        #Write second half of first line to output file
cat(NULL, file=ic_legend, sep="\n", append=TRUE) # write empty line

## Create lines of inactive map codes
map_code_999 <- noquote(paste("MapCode","999 << Water", sep=" "))
cat(map_code_999, file=ic_legend, sep="\n",append=TRUE)  #Write map code
cat(NULL, file=ic_legend, sep="\n", append=TRUE) # write empty line

map_code_998 <- noquote(paste("MapCode","998 << Artificial", sep=" "))
cat(map_code_998, file=ic_legend, sep="\n",append=TRUE)  #Write map code
cat(NULL, file=ic_legend, sep="\n", append=TRUE) # write empty line

map_code_997 <- noquote(paste("MapCode","997 << Agriculture crops and firebreaks", sep=" "))
cat(map_code_997, file=ic_legend, sep="\n",append=TRUE)  #Write map code
cat(NULL, file=ic_legend, sep="\n", append=TRUE) # write empty line

map_code_996 <- noquote(paste("MapCode","996 << Plantations of allochthonous species (Populus x canadiensis and Eucaliptus spp.)", sep=" "))
cat(map_code_996, file=ic_legend, sep="\n",append=TRUE)  #Write map code
cat(NULL, file=ic_legend, sep="\n", append=TRUE) # write empty line


## Create lines of active Map codes 
# Reorganise data frame
ic_long <- gather(data = ic_coding_full, # Reorganise data wide to long
                  key = species_cohort, 
                  value = cohort_presence,
                  jcommunis_10:tall_shrubs,
                  factor_key = TRUE) %>%
  mutate_all(as.character) %>%
  filter(is.na(cohort_presence) == FALSE)

# Create species and cohort columns
species <- c()
cohort <- c()
for (i in 1:length(ic_long$IC_Code)) {
  species[i] <- strsplit(ic_long$species_cohort[i], split = "_")[[1]][1]
  cohort[i] <- strsplit(ic_long$species_cohort[i], split = "_")[[1]][2]
}
ic_long$species <- species
ic_long$cohort <- cohort

# Add lines to ic_legend file
all_IC_Codes <- unique(ic_coding_full$IC_Code)

for (i in 1:length(all_IC_Codes)) {
  map_code_line <- noquote(paste("MapCode", all_IC_Codes[i], sep=" "))
  cat(map_code_line, file=ic_legend, sep="\n",append=TRUE)  #Write map code
  
  IC_subset <- ic_long %>% filter(IC_Code == all_IC_Codes[i])
  
  if (length(IC_subset$IC_Code) > 0){
    for (j in 1:length(unique(IC_subset$species))) {
      sp <- unique(IC_subset$species)[j]
      coh <- paste0(IC_subset$cohort[which(IC_subset$species == sp)], collapse = " ")
      sp_line <- noquote(paste(sp, coh, sep = " "))
      cat(sp_line, file = ic_legend, sep = "\n", append = TRUE)  #Write species line
      }
  }
  cat(NULL, file=ic_legend, sep="\n", append=TRUE) # write empty line
}
