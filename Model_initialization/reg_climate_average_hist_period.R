## Generate average climate file for historical period

library(dplyr)

# Define inputs and outputs folder
di <- "..."

# Load files
reg1 <- read.csv(paste(di, "clim_reg1.txt", sep = ""), sep = " ") %>%
  select(Month, Tmax, Tmin, Prec, PAR, CO2) %>%
  group_by(Month) %>%
  summarise(TMax = mean(Tmax, na.rm = TRUE),
            TMin = mean(Tmin, na.rm = TRUE),
            Prec = mean(Prec, na.rm = TRUE),
            PAR = mean(PAR, na.rm = TRUE),
            CO2 = mean(CO2, na.rm = TRUE))
reg1$Year <- '1800-2100'
reg1 <- reg1 %>%
  select(Year, Month, Tmax, Tmin, Prec, PAR, CO2)

reg2 <- read.csv(paste(di, "clim_reg2.txt", sep = ""), sep = " ") %>%
  select(Month, Tmax, Tmin, Prec, PAR, CO2) %>%
  group_by(Month) %>%
  summarise(TMax = mean(Tmax, na.rm = TRUE),
            TMin = mean(Tmin, na.rm = TRUE),
            Prec = mean(Prec, na.rm = TRUE),
            PAR = mean(PAR, na.rm = TRUE),
            CO2 = mean(CO2, na.rm = TRUE))
reg2$Year <- '1800-2100'
reg2 <- reg2 %>%
  select(Year, Month, Tmax, Tmin, Prec, PAR, CO2)

reg3 <- read.csv(paste(di, "clim_reg3.txt", sep = ""), sep = " ") %>%
  select(Month, Tmax, Tmin, Prec, PAR, CO2) %>%
  group_by(Month) %>%
  summarise(TMax = mean(Tmax, na.rm = TRUE),
            TMin = mean(Tmin, na.rm = TRUE),
            Prec = mean(Prec, na.rm = TRUE),
            PAR = mean(PAR, na.rm = TRUE),
            CO2 = mean(CO2, na.rm = TRUE))
reg3$Year <- '1800-2100'
reg3 <- reg3 %>%
  select(Year, Month, Tmax, Tmin, Prec, PAR, CO2)

reg4 <- read.csv(paste(di, "clim_reg4.txt", sep = ""), sep = " ") %>%
  select(Month, Tmax, Tmin, Prec, PAR, CO2) %>%
  group_by(Month) %>%
  summarise(TMax = mean(Tmax, na.rm = TRUE),
            TMin = mean(Tmin, na.rm = TRUE),
            Prec = mean(Prec, na.rm = TRUE),
            PAR = mean(PAR, na.rm = TRUE),
            CO2 = mean(CO2, na.rm = TRUE))
reg4$Year <- '1800-2100'
reg4 <- reg4 %>%
  select(Year, Month, Tmax, Tmin, Prec, PAR, CO2)

write.table(reg1, paste(di, "hist_climate_reg1.txt", sep = ""), sep = " ", row.names = FALSE)
write.table(reg2, paste(di, "hist_climate_reg2.txt", sep = ""), sep = " ", row.names = FALSE)
write.table(reg3, paste(di, "hist_climate_reg3.txt", sep = ""), sep = " ", row.names = FALSE)
write.table(reg4, paste(di, "hist_climate_reg4.txt", sep = ""), sep = " ", row.names = FALSE)