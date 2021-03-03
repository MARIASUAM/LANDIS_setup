## Identify key species based on basal area

library(dplyr)
# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Load inputs
parcelas_aoi <- read.csv(paste(di, outputs, "parcelas_en_aoi.csv", sep = ""), sep = ",")
coef <- read.csv(paste(di, inputs, "funciones_alometricas.csv", sep = ""), sep = ";", skip = 2, dec = ",")
coef$Cod_Especie <- as.factor(coef$Cod_Especie)

basal_area <- read.csv(paste(di, inputs, "PCMayores.csv", sep = ""), sep = ";") %>%
  inner_join(parcelas_aoi) %>% # filter plots within area of interest
  mutate(avg_dbh = (Dn1 + Dn2)/20, # calculate normal diameter in cm
         basal_a = pi * ((avg_dbh / 2) ^ 2),
         Especie = as.factor(Especie)) %>%
  select(Especie, basal_a) %>%
  group_by(Especie) %>%
  summarise(species_ba = sum(basal_a)) %>%
  left_join(coef, by = c("Especie" = "Cod_Especie")) %>%
  select(Especie, Especie.y, species_ba) %>%
  filter(is.na(species_ba) == FALSE)

write.csv(basal_area, paste(di, outputs, "species_basal_area.csv", sep = ""), row.names = FALSE)
