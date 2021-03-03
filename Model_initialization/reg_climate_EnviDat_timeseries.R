## Generate LANDIS climate file based on EnviDat data

library(dplyr)
library(ggplot2)
library(lubridate)

# Define inputs and outputs folder
di <- "..."
calib_folder <- "..."

# TMin timeseries
## tasmin hist
tasmin_hist_files <- list.files(di, pattern = "envidat_tasmin_historical_*")
tasmin_hist <- data.frame()
for(i in 1:length(tasmin_hist_files)) { # Load files and transform units (K -> ºC)
  temp <- read.csv(paste(di, tasmin_hist_files[i], sep = ""), sep = " ") %>%
    mutate(TMin_reg1_celsius = tasmin_df_region_1 - 273.15,
           TMin_reg2_celsius = tasmin_df_region_2 - 273.15,
           TMin_reg3_celsius = tasmin_df_region_3 - 273.15,
           TMin_reg4_celsius = tasmin_df_region_4 - 273.15) %>%
    select(Year, Month, TMin_reg1_celsius, TMin_reg2_celsius, TMin_reg3_celsius, TMin_reg4_celsius)
  tasmin_hist <- rbind(tasmin_hist, temp) # Merge files
}

## tasmin rcp45
tasmin_rcp45_files <- list.files(di, pattern = "envidat_tasmin_rcp45_*")
tasmin_rcp45 <- data.frame()
for(i in 1:length(tasmin_rcp45_files)) { # Load files and transform units (K -> ºC)
  temp <- read.csv(paste(di, tasmin_rcp45_files[i], sep = ""), sep = " ") %>%
    mutate(TMin_reg1_celsius = tasmin_df_region_1 - 273.15,
           TMin_reg2_celsius = tasmin_df_region_2 - 273.15,
           TMin_reg3_celsius = tasmin_df_region_3 - 273.15,
           TMin_reg4_celsius = tasmin_df_region_4 - 273.15) %>%
    select(Year, Month, TMin_reg1_celsius, TMin_reg2_celsius, TMin_reg3_celsius, TMin_reg4_celsius)
  tasmin_rcp45 <- rbind(tasmin_rcp45, temp) # Merge files
}

## tasmin rcp85
tasmin_rcp85_files <- list.files(di, pattern = "envidat_tasmin_rcp85_*")
tasmin_rcp85 <- data.frame()
for(i in 1:length(tasmin_rcp85_files)) { # Load files and transform units (K -> ºC)
  temp <- read.csv(paste(di, tasmin_rcp85_files[i], sep = ""), sep = " ") %>%
    mutate(TMin_reg1_celsius = tasmin_df_region_1 - 273.15,
           TMin_reg2_celsius = tasmin_df_region_2 - 273.15,
           TMin_reg3_celsius = tasmin_df_region_3 - 273.15,
           TMin_reg4_celsius = tasmin_df_region_4 - 273.15) %>%
    select(Year, Month, TMin_reg1_celsius, TMin_reg2_celsius, TMin_reg3_celsius, TMin_reg4_celsius)
  tasmin_rcp85 <- rbind(tasmin_rcp85, temp) # Merge files
}

## Generate TMin scenarios
TMin_rcp45 <- rbind(tasmin_hist, tasmin_rcp45)
TMin_rcp85 <- rbind(tasmin_hist, tasmin_rcp85)


# TMax timeseries
## tasmax hist
tasmax_hist_files <- list.files(di, pattern = "envidat_tasmax_historical_*")
tasmax_hist <- data.frame()
for(i in 1:length(tasmax_hist_files)) { # Load files and transform units (K -> ºC)
  temp <- read.csv(paste(di, tasmax_hist_files[i], sep = ""), sep = " ") %>%
    mutate(TMax_reg1_celsius = tasmax_df_region_1 - 273.15,
           TMax_reg2_celsius = tasmax_df_region_2 - 273.15,
           TMax_reg3_celsius = tasmax_df_region_3 - 273.15,
           TMax_reg4_celsius = tasmax_df_region_4 - 273.15) %>%
    select(Year, Month, TMax_reg1_celsius, TMax_reg2_celsius, TMax_reg3_celsius, TMax_reg4_celsius)
  tasmax_hist <- rbind(tasmax_hist, temp) # Merge files
}

## tasmin rcp45
tasmax_rcp45_files <- list.files(di, pattern = "envidat_tasmax_rcp45_*")
tasmax_rcp45 <- data.frame()
for(i in 1:length(tasmax_rcp45_files)) { # Load files and transform units (K -> ºC)
  temp <- read.csv(paste(di, tasmax_rcp45_files[i], sep = ""), sep = " ") %>%
    mutate(TMax_reg1_celsius = tasmax_df_region_1 - 273.15,
           TMax_reg2_celsius = tasmax_df_region_2 - 273.15,
           TMax_reg3_celsius = tasmax_df_region_3 - 273.15,
           TMax_reg4_celsius = tasmax_df_region_4 - 273.15) %>%
    select(Year, Month, TMax_reg1_celsius, TMax_reg2_celsius, TMax_reg3_celsius, TMax_reg4_celsius)
  tasmax_rcp45 <- rbind(tasmax_rcp45, temp) # Merge files
}

## tasmax rcp85
tasmax_rcp85_files <- list.files(di, pattern = "envidat_tasmax_rcp85_*")
tasmax_rcp85 <- data.frame()
for(i in 1:length(tasmax_rcp85_files)) { # Load files and transform units (K -> ºC)
  temp <- read.csv(paste(di, tasmax_rcp85_files[i], sep = ""), sep = " ") %>%
    mutate(TMax_reg1_celsius = tasmax_df_region_1 - 273.15,
           TMax_reg2_celsius = tasmax_df_region_2 - 273.15,
           TMax_reg3_celsius = tasmax_df_region_3 - 273.15,
           TMax_reg4_celsius = tasmax_df_region_4 - 273.15) %>%
    select(Year, Month, TMax_reg1_celsius, TMax_reg2_celsius, TMax_reg3_celsius, TMax_reg4_celsius)
  tasmax_rcp85 <- rbind(tasmax_rcp85, temp) # Merge files
}

## Generate TMax scenarios
TMax_rcp45 <- rbind(tasmax_hist, tasmax_rcp45)
TMax_rcp85 <- rbind(tasmax_hist, tasmax_rcp85)

# Prec timeseries
## pr hist
pr_hist_files <- list.files(di, pattern = "envidat_pr_historical_*")
pr_hist <- data.frame()

for(i in 6:8) { # Load only 1950-2005 files; no need to transform units (kg/m2*s == mm) but need to calculate month sum
  temp <- read.csv(paste(di, pr_hist_files[i], sep = ""), sep = " ") %>%
    mutate(month_start_date = ymd(paste(Year, Month, "01", sep = "/")) ,
           next_month_start_date = month_start_date + months(1),
           nr_days_in_month = as.integer(as.duration(interval(month_start_date, next_month_start_date)) / 86400),
           Prec_reg1_monthsum = prec_region_1*86400*nr_days_in_month,
           Prec_reg2_monthsum = prec_region_2*86400*nr_days_in_month,
           Prec_reg3_monthsum = prec_region_3*86400*nr_days_in_month,
           Prec_reg4_monthsum = prec_region_4*86400*nr_days_in_month) %>%
    select(Year, Month, Prec_reg1_monthsum, Prec_reg2_monthsum, Prec_reg3_monthsum, Prec_reg4_monthsum)
  pr_hist <- rbind(pr_hist, temp) # Merge files
}

## pr rcp45
pr_rcp45_files <- list.files(di, pattern = "envidat_pr_rcp45_*")
pr_rcp45 <- data.frame()
for(i in 1:length(pr_rcp45_files)) { # Load files; no need to transform units (kg/m2*s == mm)
  temp <- read.csv(paste(di, pr_rcp45_files[i], sep = ""), sep = " ") %>%
    mutate(month_start_date = ymd(paste(Year, Month, "01", sep = "/")) ,
           next_month_start_date = month_start_date + months(1),
           nr_days_in_month = as.integer(as.duration(interval(month_start_date, next_month_start_date)) / 86400),
           Prec_reg1_monthsum = prec_region_1*86400*nr_days_in_month,
           Prec_reg2_monthsum = prec_region_2*86400*nr_days_in_month,
           Prec_reg3_monthsum = prec_region_3*86400*nr_days_in_month,
           Prec_reg4_monthsum = prec_region_4*86400*nr_days_in_month) %>%
    select(Year, Month, Prec_reg1_monthsum, Prec_reg2_monthsum, Prec_reg3_monthsum, Prec_reg4_monthsum)
  pr_rcp45 <- rbind(pr_rcp45, temp) # Merge files
}

## pr rcp85
pr_rcp85_files <- list.files(di, pattern = "envidat_pr_rcp85_*")
pr_rcp85 <- data.frame()
for(i in 1:length(pr_rcp85_files)) { # Load files; no need to transform units (kg/m2*s == mm)
  temp <- read.csv(paste(di, pr_rcp85_files[i], sep = ""), sep = " ") %>%
    mutate(month_start_date = ymd(paste(Year, Month, "01", sep = "/")) ,
           next_month_start_date = month_start_date + months(1),
           nr_days_in_month = as.integer(as.duration(interval(month_start_date, next_month_start_date)) / 86400),
           Prec_reg1_monthsum = prec_region_1*86400*nr_days_in_month,
           Prec_reg2_monthsum = prec_region_2*86400*nr_days_in_month,
           Prec_reg3_monthsum = prec_region_3*86400*nr_days_in_month,
           Prec_reg4_monthsum = prec_region_4*86400*nr_days_in_month) %>%
    select(Year, Month, Prec_reg1_monthsum, Prec_reg2_monthsum, Prec_reg3_monthsum, Prec_reg4_monthsum)
  pr_rcp85 <- rbind(pr_rcp85, temp) # Merge files
}

## Generate Prec scenarios
Prec_rcp45 <- rbind(pr_hist, pr_rcp45)
Prec_rcp85 <- rbind(pr_hist, pr_rcp85)

# Load CO2 and PAR timeseries
CO2 <- read.csv(paste(di, "CO2_timeseries.csv", sep = ""), sep = ",") %>%
  dplyr::select(Year, Month, CO2)
ggplot(data = CO2, aes(x = Year, y = CO2)) +
  geom_line() +
  theme_minimal() +
  ggtitle("CO2 time series")

PAR <- read.csv(paste(di, "PAR_timeseries.csv", sep = ""), sep = ",") %>%
  dplyr::select(Year, Month, PAR_reg1, PAR_reg2, PAR_reg3, PAR_reg4)
# ggplot(data = PAR, aes(x = Year)) +
#   geom_line(aes(y = PAR_reg1, colour = "red")) +
#   geom_line(aes(y = PAR_reg2, colour = "blue")) +
#   geom_line(aes(y = PAR_reg3, colour = "green")) +
#   geom_line(aes(y = PAR_reg4, colour = "orange")) +
#   theme_minimal() +
#   ggtitle("PAR time series")

# Generate sample climate files
hist <- left_join(tasmin_hist, tasmax_hist) %>% # Merge all variables
  left_join(pr_hist) %>%
  left_join(PAR) %>%
  left_join(CO2) %>%
  filter(Year >= 1994)

year_1999 <- left_join(tasmin_hist, tasmax_hist, by = c("Year", "Month")) %>% # Merge all variables
  left_join(pr_hist, by = c("Year", "Month")) %>%
  left_join(PAR, by = c("Year", "Month")) %>%
  left_join(CO2, by = c("Year", "Month")) %>%
  filter(Year == 1999) %>%
  mutate(TMax = TMax_reg1_celsius,
         TMin = TMin_reg1_celsius,
         PAR = PAR_reg1,
         Prec = Prec_reg1_monthsum) %>%
  select(Year, Month, TMax, TMin, PAR, Prec, CO2)  # Units: celsius, celsius, μmol/m2*sec, kg/m2 == mm, ppm
year_1999$Year <- '1800-2100'
# write.table(year_1999, paste(calib_folder, "initial_inputs/", "clim_year_1999.txt", sep = ""), sep = " ", row.names = FALSE)

year_1999 <- left_join(tasmin_hist, tasmax_hist, by = c("Year", "Month")) %>% # Merge all variables
  left_join(pr_hist, by = c("Year", "Month")) %>%
  left_join(PAR, by = c("Year", "Month")) %>%
  left_join(CO2, by = c("Year", "Month")) %>%
  filter(Year == 1999) %>%
  mutate(TMax = TMax_reg1_celsius,
         TMin = TMin_reg1_celsius,
         PAR = PAR_reg1,
         Prec = Prec_reg1_monthsum) %>%
  select(Year, Month, TMax, TMin, PAR, Prec, CO2)  # Units: celsius, celsius, μmol/m2*sec, kg/m2 == mm, ppm
year_1999$Year <- '1800-2100'
# write.table(year_1999, paste(calib_folder, "clim_year_1999_reg1.txt", sep = ""), sep = " ", row.names = FALSE)

# Generate whole time times for each region and scenario

## rcp45 - 2070-2089 missing
# Units: celsius, celsius, μmol/m2*sec, kg/m2 == mm, ppm
dataset_rcp45 <- left_join(TMax_rcp45, TMin_rcp45) %>%
  left_join(Prec_rcp45) %>%
  left_join(PAR) %>%
  left_join(CO2)

reg1 <- dataset_rcp45 %>%
  select(Year, Month, 
         TMax_reg1_celsius, TMin_reg1_celsius, Prec_reg1_monthsum,
         PAR_reg1, CO2)
colnames(reg1) <- c("Year", "Month", "TMax", "TMin", "Prec", "PAR", "CO2")
write.table(reg1, paste(di, "clim_reg1_rcp45.txt", sep = ""), sep = " ", row.names = FALSE)

reg2 <- dataset_rcp45 %>%
  select(Year, Month, 
         TMax_reg2_celsius, TMin_reg2_celsius, Prec_reg2_monthsum,
         PAR_reg2, CO2)
colnames(reg2) <- c("Year", "Month", "TMax", "TMin", "Prec", "PAR", "CO2")
write.table(reg2, paste(di, "clim_reg2_rcp45.txt", sep = ""), sep = " ", row.names = FALSE)

reg3 <- dataset_rcp45 %>%
  select(Year, Month, 
         TMax_reg3_celsius, TMin_reg3_celsius, Prec_reg3_monthsum,
         PAR_reg3, CO2)
colnames(reg3) <- c("Year", "Month", "TMax", "TMin", "Prec", "PAR", "CO2")
write.table(reg3, paste(di, "clim_reg3_rcp45.txt", sep = ""), sep = " ", row.names = FALSE)

reg4 <- dataset_rcp45 %>%
  select(Year, Month, 
         TMax_reg4_celsius, TMin_reg4_celsius, Prec_reg4_monthsum,
         PAR_reg4, CO2)
colnames(reg4) <- c("Year", "Month", "TMax", "TMin", "Prec", "PAR", "CO2")
write.table(reg4, paste(di, "clim_reg4_rcp45.txt", sep = ""), sep = " ", row.names = FALSE)

## rcp85
# Units: celsius, celsius, μmol/m2*sec, kg/m2 == mm, ppm
dataset_rcp85 <- left_join(TMax_rcp85, TMin_rcp85) %>%
  left_join(Prec_rcp85) %>%
  left_join(PAR) %>%
  left_join(CO2)

reg1 <- dataset_rcp85 %>%
  select(Year, Month, 
         TMax_reg1_celsius, TMin_reg1_celsius, Prec_reg1_monthsum,
         PAR_reg1, CO2)
colnames(reg1) <- c("Year", "Month", "TMax", "TMin", "Prec", "PAR", "CO2")
write.table(reg1, paste(di, "clim_reg1_rcp85.txt", sep = ""), sep = " ", row.names = FALSE)

reg2 <- dataset_rcp85 %>%
  select(Year, Month, 
         TMax_reg2_celsius, TMin_reg2_celsius, Prec_reg2_monthsum,
         PAR_reg2, CO2)
colnames(reg2) <- c("Year", "Month", "TMax", "TMin", "Prec", "PAR", "CO2")
write.table(reg2, paste(di, "clim_reg2_rcp85.txt", sep = ""), sep = " ", row.names = FALSE)

reg3 <- dataset_rcp85 %>%
  select(Year, Month, 
         TMax_reg3_celsius, TMin_reg3_celsius, Prec_reg3_monthsum,
         PAR_reg3, CO2)
colnames(reg3) <- c("Year", "Month", "TMax", "TMin", "Prec", "PAR", "CO2")
write.table(reg3, paste(di, "clim_reg3_rcp85.txt", sep = ""), sep = " ", row.names = FALSE)

reg4 <- dataset_rcp85 %>%
  select(Year, Month, 
         TMax_reg4_celsius, TMin_reg4_celsius, Prec_reg4_monthsum,
         PAR_reg4, CO2)
colnames(reg4) <- c("Year", "Month", "TMax", "TMin", "Prec", "PAR", "CO2")
write.table(reg4, paste(di, "clim_reg4_rcp85.txt", sep = ""), sep = " ", row.names = FALSE)


