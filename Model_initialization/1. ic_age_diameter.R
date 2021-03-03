## Age to height- and diameter-classes
library(dplyr)

# Setup folders
di <- "..."
inputs <- "input_files/"
outputs <- "output_files/"

# Calculate average height and diameter per plot and species
mayores_avg_Ht_Dn <- read.csv(paste(di, inputs, "PCMayores.csv", sep = ""), sep = ";") %>%
  filter(is.na(Especie) == FALSE) %>%
  mutate(Dn_mm = (Dn1 + Dn2)/2) %>%
  select(Tabla_Provincia, Estadillo, Cla, Subclase, Especie, Ht, Dn_mm) %>%
  group_by(Tabla_Provincia, Estadillo, Cla, Subclase, Especie) %>%
  summarise(avg_Ht = mean(Ht),
            avg_Dn = mean(Dn_mm),
            count = n())

espparc_age <- read.csv(paste(di, inputs, "PCEspParc.csv", sep = ""), sep = ";") %>% 
  select(Tabla_Provincia, Estadillo, Cla, Subclase, Especie, Edad) %>%
  filter(is.na(Edad) == FALSE)

age_Dn_Ht_classes <- left_join(espparc_age, mayores_avg_Ht_Dn) %>%
  select(Especie, Edad, avg_Ht, avg_Dn, count)

write.csv(age_Dn_Ht_classes, paste(di, outputs, "age_Dn_Ht_classes.csv", sep = ""), row.names = FALSE)
