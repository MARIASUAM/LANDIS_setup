## Generate LANDIS climate file based on REDIAM data

library(dplyr)

# Define inputs and outputs
di <- "..."
outputs <- "output_files/"

# Climate 1971-2000
clim_1971_2000_reg1 <- read.csv(paste(di, outputs, "clim_1971_2000_reg1.txt", sep = ""), sep = " ") %>%
  select(Month, Tmax, Tmin, Prec)
clim_1971_2000_reg2 <- read.csv(paste(di, outputs, "clim_1971_2000_reg2.txt", sep = ""), sep = " ") %>%
  select(Month, Tmax, Tmin, Prec)
clim_1971_2000_reg3 <- read.csv(paste(di, outputs, "clim_1971_2000_reg3.txt", sep = ""), sep = " ") %>%
  select(Month, Tmax, Tmin, Prec)
clim_1971_2000_reg4 <- read.csv(paste(di, outputs, "clim_1971_2000_reg4.txt", sep = ""), sep = " ") %>%
  select(Month, Tmax, Tmin, Prec)

# Generate Year column as individual years instead of period 1971-2000
Years <- data.frame(Year = 1971:2000)
clim_1971_2000_reg1 <- merge(clim_1971_2000_reg1, Years)
clim_1971_2000_reg2 <- merge(clim_1971_2000_reg2, Years)
clim_1971_2000_reg3 <- merge(clim_1971_2000_reg3, Years)
clim_1971_2000_reg4 <- merge(clim_1971_2000_reg4, Years)

# Climate 2005-2019
clim_2005_2019_reg1 <- read.csv(paste(di, outputs, "clim_2005_2019_reg1.txt", sep = ""), sep = " ")
clim_2005_2019_reg2 <- read.csv(paste(di, outputs, "clim_2005_2019_reg2.txt", sep = ""), sep = " ")
clim_2005_2019_reg3 <- read.csv(paste(di, outputs, "clim_2005_2019_reg3.txt", sep = ""), sep = " ")
clim_2005_2019_reg4 <- read.csv(paste(di, outputs, "clim_2005_2019_reg4.txt", sep = ""), sep = " ")

# PAR
par_reg1 <- read.csv(paste(di, outputs, "clim_par_1980_2019_reg1.csv", sep = ""), sep = " ")
par_reg2 <- read.csv(paste(di, outputs, "clim_par_1980_2019_reg2.txt", sep = ""), sep = " ")
par_reg3 <- read.csv(paste(di, outputs, "clim_par_1980_2019_reg3.txt", sep = ""), sep = " ")
par_reg4 <- read.csv(paste(di, outputs, "clim_par_1980_2019_reg4.txt", sep = ""), sep = " ")

# CO2 data
CO2 <- read.csv(paste(di, outputs, "CO2_data.csv", sep = ""), sep = ",") %>%
  select(Year, Month, CO2)

# Join variables
reg1 <- full_join(clim_1971_2000_reg1, clim_2005_2019_reg1) %>%
  full_join(par_reg1) %>%
  full_join(CO2) %>%
  select(Year, Month, Tmax, Tmin, Prec, PAR, CO2)

reg2 <- full_join(clim_1971_2000_reg2, clim_2005_2019_reg2) %>%
  full_join(par_reg2) %>%
  full_join(CO2) %>%
  select(Year, Month, Tmax, Tmin, Prec, PAR, CO2)

reg3 <- full_join(clim_1971_2000_reg3, clim_2005_2019_reg3) %>%
  full_join(par_reg3) %>%
  full_join(CO2) %>%
  select(Year, Month, Tmax, Tmin, Prec, PAR, CO2)

reg4 <- full_join(clim_1971_2000_reg4, clim_2005_2019_reg4) %>%
  full_join(par_reg4) %>%
  full_join(CO2) %>%
  select(Year, Month, Tmax, Tmin, Prec, PAR, CO2)

# Export
write.table(reg1, paste(di, "calibration/0_climate_inputs/", "clim_reg1.txt", sep = ""), sep = " ", row.names = FALSE)
write.table(reg2, paste(di, "calibration/0_climate_inputs/", "clim_reg2.txt", sep = ""), sep = " ", row.names = FALSE)
write.table(reg3, paste(di, "calibration/0_climate_inputs/", "clim_reg3.txt", sep = ""), sep = " ", row.names = FALSE)
write.table(reg4, paste(di, "calibration/0_climate_inputs/", "clim_reg4.txt", sep = ""), sep = " ", row.names = FALSE)
