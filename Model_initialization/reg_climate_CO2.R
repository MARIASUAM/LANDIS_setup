### Climate files generation - CO2 ###

library(dplyr)
library(ggplot2)
library(reshape)

# Define inputs and outputs
di_inputs <- "..."
outputs <- "..."

# Input files
# Load ICOS data for SSL station (mol/mol): lat = 47.92, long = 7.92, alt = 1205.0 https://doi.org/10.18160/6MXY-S1PH
icos <- read.csv(paste(di_inputs, "Datos_CO2_ICOS", "SSL_12m_air.hdf.all.COMBI_Drought2018_20190522.co2", sep = "/"), skip = 30, sep = ";")
icos[icos == -999.99] <- NA
icos[icos == -9.99] <- NA

# Calculate monthly means
icos_ts <- icos %>%
  select(Year, Month, co2) %>% group_by(Year, Month) %>%
  summarise(icos_CO2 = mean(co2, na.rm = TRUE)) %>%
  mutate(date = as.Date(paste(Year, Month, "01", sep = "-")),
         CO2 = icos_CO2)
ggplot(icos_ts, aes(x = date, y = icos_CO2)) + geom_line()

# Export
icos_ts <- icos_ts %>%
  select(Year, Month, CO2)
write.csv(icos_ts, paste(outputs, "CO2_month_mean_original_data.csv", sep = "/"))

# Generate whole time series_ 1950-2100

CO2 <- icos_ts %>%
  select(Year, Month, CO2)

# Fill in period 1950-1971 --> same as 1972 (original data for 1971 (1 month) is lost)
CO2_prev_ref_year <- CO2 %>%
  filter(Year == 1972)
CO2_prev_ref_year <- data.frame(Month = CO2_prev_ref_year$Month,
                                CO2 = CO2_prev_ref_year$CO2)

years_to_fill_prev <- data.frame(Year = 1950:1971)
years_to_fill_prev <- merge(years_to_fill_prev, CO2_prev_ref_year)

# Fill in gaps in period 1972-2017
check <- CO2 %>% # check for gaps
  mutate(Data = ifelse(is.na(CO2) == FALSE, 1, 0)) %>%
  select(Year, Data) %>%
  group_by(Year) %>%
  summarise(Sum = sum(Data))

CO2_ts <- data.frame(Year = 1972:2018)
months <- data.frame(Month = 1:12)
CO2_ts <- merge(CO2_ts, months)

CO2_subset <- CO2 %>%
  filter(Year > 1971, Year < 2019)

CO2_ts <- CO2_ts %>%
  left_join(CO2_subset)

CO2_ts <- CO2_ts[order(CO2_ts$Year),]

for (i in 1:length(CO2_ts$Year)) {
  CO2_ts$CO2[i] <- ifelse(is.na(CO2_ts$CO2[i]) == FALSE,
                          CO2_ts$CO2[i],
                          CO2_ts$CO2[i - 1])
}

check <- CO2_ts %>% # check for gaps
  mutate(Data = ifelse(is.na(CO2_ts) == FALSE, 1, 0)) %>%
  select(Year, Data) %>%
  group_by(Year) %>%
  summarise(Sum = sum(Data))

# Fill in period 2018-2100 --> same as 2017 (original data for 2018 (5 months) are lost)
CO2 %>%
  filter(is.na(CO2) == TRUE)

CO2_post_ref_year <- CO2 %>%
  filter(Year == 2018)

CO2_post_ref_year <- data.frame(Month = CO2_post_ref_year$Month,
                                CO2 = CO2_post_ref_year$CO2)

years_to_fill_post <- data.frame(Year = 2019:2100)
years_to_fill_post <- merge(years_to_fill_post, CO2_post_ref_year)

# Generate full CO2 timeseries and export
CO2_ts <- rbind(CO2_ts, years_to_fill_prev)
CO2_ts <- rbind(CO2_ts, years_to_fill_post)

write.csv(CO2_ts, paste(outputs, "CO2_timeseries.csv", sep = "/"))
