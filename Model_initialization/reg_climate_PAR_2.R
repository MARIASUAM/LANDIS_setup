## Generate PAR time series

library(dplyr)
library(ggplot2)

# Define inputs and outputs folder
di <- "..."
calib_folder <- "..."

## Load regions data and merge
par_reg1 <- read.csv(paste(di, "clim_par_1980_2019_reg1.csv", sep = ""), sep = " ") %>%
  mutate(PAR_reg1 = PAR) %>% select(Year, Month, PAR_reg1)
par_reg2 <- read.csv(paste(di, "clim_par_1980_2019_reg2.txt", sep = ""), sep = " ") %>%
  mutate(PAR_reg2 = PAR) %>% select(Year, Month, PAR_reg2)
par_reg3 <- read.csv(paste(di, "clim_par_1980_2019_reg3.txt", sep = ""), sep = " ") %>%
  mutate(PAR_reg3 = PAR) %>% select(Year, Month, PAR_reg3)
par_reg4 <- read.csv(paste(di, "clim_par_1980_2019_reg4.txt", sep = ""), sep = " ") %>%
  mutate(PAR_reg4 = PAR) %>% select(Year, Month, PAR_reg4)

PAR <- left_join(par_reg1, par_reg2) %>%
  left_join(par_reg3) %>% left_join(par_reg4)

ggplot(data = PAR, aes(x = Year)) +
  geom_line(aes(y = PAR_reg1), color = "blue")

## Identify full years with data and gaps in the series
PAR_years <- PAR %>%
  mutate(Data = ifelse(is.na(PAR_reg1) == TRUE, 0, 1)) %>%
  select(Year, Data) %>%
  group_by(Year) %>% summarise(Sum = sum(Data))

PAR_full_years <- PAR_years %>%
  filter(Sum == 12) %>%
  mutate(data_available = "full year") %>% select(Year, data_available)

PAR_gap_years <- PAR_years %>%
  filter(Sum != 12) %>% filter(Sum > 0) %>%
  mutate(data_available = "gap year") %>% select(Year, data_available)

PAR_data_years <- rbind(PAR_full_years, PAR_gap_years)

## Assign random year to assimilate with for years without data
years_missing <- data.frame(Year = 1950:2100) %>%
  left_join(PAR_data_years, by = "Year") %>%
  filter(is.na(data_available) == TRUE)

years_sampling <- data.frame(Year = years_missing$Year,
                             Year_to_sample = sample(PAR_full_years$Year,
                                                     length(years_missing$Year), replace = TRUE))

PAR_ts <- data.frame(Year = 1950:2100) %>%
  left_join(PAR_data_years, by = "Year") %>%
  left_join(years_sampling) %>%
  mutate(Year_to_join = ifelse(is.na(data_available) == FALSE,
                               Year, Year_to_sample)) %>%
  select(Year, data_available, Year_to_join) %>%
  left_join(PAR, by = c("Year_to_join" = "Year"))

## Fill in monthly missing data - consecutive missing months are given data from consecutive months
monthly_gaps <- PAR_ts %>% filter(is.na(PAR_reg1) == TRUE) %>%
  select(Year, Month)

monthly_sampling <- data.frame(Year_original = unique(monthly_gaps$Year),
                               Year_to_join_month = sample(PAR_full_years$Year,
                                                           length(unique(monthly_gaps$Year)), replace = TRUE))

monthly_gaps <- monthly_gaps %>%
  left_join(monthly_sampling, by = c("Year" = "Year_original")) %>%
  left_join(PAR, by = c("Year_to_join_month" = "Year", "Month" = "Month")) %>%
  select(Year, Month, PAR_reg1, PAR_reg2, PAR_reg3, PAR_reg4)

## Merge and produce whole PAR timeseries
PAR_ts <- PAR_ts %>%
  select(Year, Month, PAR_reg1, PAR_reg2, PAR_reg3, PAR_reg4)

PAR_ts <- rbind(PAR_ts, monthly_gaps) %>%
  filter(is.na(PAR_reg1) == FALSE)

ggplot(PAR_ts, aes(x = Year, y = PAR_reg1)) +
  geom_line()

write.csv(PAR_ts, paste(di, outputs, "PAR_timeseries.csv", sep = "/"))
