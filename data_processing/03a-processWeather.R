# Fills missing values, joins weather datasets from all sources and
# generates hourly sets.

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("stringr")
library("tidyverse")
library("lubridate")

# Functions ---------------------------------------------------------------

source("./data_processing/03a-processWeather_functions.R")


# Load files --------------------------------------------------------------

cycles_dates <- read.csv("./data/cycle_dates.csv")

codes_exp <- read.csv("./tables/codes_exp.csv")

pi_full <- read.csv("./data/weather/monitoring/weather_pi_org.csv")
radio_full <- read.csv("./data/weather/monitoring/weather_radio_org.csv")
rad_full <- read.csv("./data/weather/monitoring/weather_radiation_org.csv")

# Process data ------------------------------------------------------------

weather_full <- pi_full %>%
  rbind(radio_full) %>%
  rbind(rad_full) %>%
  mutate(date = as.character(date(ymd_hms(dateFull, truncated = 1)))) %>%
  left_join(cycles_dates) %>%
  filter(!is.na(node))

write.csv(weather_full,
          file = "./data/weather/monitoring/weather_init.csv",
          row.names=FALSE)

#weather_full <- read.csv("./data/weather/monitoring/weather_init.csv")

# Since I may have many missing values between two dates in a sense that 
# smoothing between them directly makes no sense. And since there are no 
# unreasonable outlier value, as they have been filtered when the data was 
# organized, it makes more sense to fill first and then smooth.

# Replace missing values with data from other sensors
# and time series imputation

# # Convert pi lux data here
# # photsynthetic photon flux density micromol per m2 per second PAR
# # Hall 1993, Table C16.
# # klux to mmol m-2 s-1 = klux * 18
# # lux to mmol m-2 s-1 = lux * 18 / 1000
# # PPFD = rad_hour * conv

x <- weather_full  %>%
  filter(!is.na(cycle)) %>%
  mutate(measurement = if_else((sensor == "pi") & (variable == "radiation"),
                               measurement * 0.018,
                               measurement),
         dateFull = ymd_hms(dateFull, truncated = 1),
         hour = hour(dateFull)) %>%
  mutate(sensor_var = paste(sensor, variable, sep = "_")) 

# Include transmitance for imputation from external data
transm <- 0.7
weather_full_no_na <- x %>%
  fillNAs(cycles_dates = cycles_dates) %>%
  mutate(measurement = if_else((variable == "radiation") &
                                 (imputation == "case4"),
                               measurement * transm,
                               measurement))

write.csv(weather_full_no_na,
          file = "./data/weather/monitoring/weather_fill.csv",
          row.names=FALSE)

#weather_full_no_na <- read.csv("./data/weather/monitoring/weather_fill.csv")

# Fill remaining NAs with impossible values so that they may be fixed
# by the outlier smoothing function
weather_full_mod <- weather_full_no_na %>%
  mutate(measurement = if_else(is.na(measurement), -99, measurement),
         dateFull = ymd_hms(dateFull, truncated = 1)) %>%
  group_by(node, sensor_var, sensor_id, cycle) %>%
  arrange(node, sensor_var, sensor_id, dateFull) %>%
  mutate(measurement = if_else(variable == "humidity", 
                               measurement,
                               as.numeric(smooth(measurement)))) %>%
  ungroup() %>%
  mutate(measurement = na_if(measurement, -99))

write.csv(weather_full_mod,
          file = "./data/weather/monitoring/weather_final.csv",
          row.names=FALSE)

#weather_full_mod <- read.csv("./data/weather/monitoring/weather_final.csv")

weather_hourly <- weather_full_mod %>%
  agg_hourly() %>%
  mutate(doy = yday(date)) %>%
  full_join(codes_exp) %>%
  arrange(city_exp, date, hour) %>%
  filter(!is.na(cycle))

write.csv(weather_hourly,
          file = "./data/weather/monitoring/weather_hourly.csv",
          row.names=FALSE)

#weather_hourly <- read.csv("./data/weather/monitoring/weather_hourly.csv")


# Hourly weather dataset (TOMGRO) ------------------------------------------
# Model expects maximum, average and minimum temperature in the day as well as
# average radiation in the hour.
# The SIMPLE model, however, uses global radiation instead of visible.
# Conversion is performed in the model runs.

cyc <- c(1, 2, 3, 4)
nod <- c(1, 2)
type <- c('A', 'B')
model <- c("tomgro", "simple")
type_equiv <- data.frame('A' = "radio|li1400", 'B' = 'pi')

combinations <- expand.grid(cycle = cyc,
                            node = nod, model = model,
                            type = type,
                            stringsAsFactors = FALSE)

# summaries_l <- list()
# summaries_l2 <- list()
i <- 4
for (i in 1:nrow(combinations)){

  weather_set <- weather_hourly[grep(as.character(type_equiv[combinations$type[i]]),
                                     weather_hourly$stat_sensor_var), ] %>%
    filter(cycle == combinations$cycle[i],
           node == combinations$node[i]) %>%
    separate(stat_sensor_var, into = c("stat", "sensor", "variable")) %>%
    mutate(stat_var = str_c(variable, stat, sep = "_"),
           radiation_unit = "mmolPAR") %>%
    select(-stat, -variable, -sensor, -starts_with("flag")) %>%
    spread(stat_var, value) %>%
    select(-radiation_max, -radiation_min, -humidity_max, -humidity_min,
           -contains("med")) %>%
    rename(rad = radiation_mean,
           tmax = temperature_max,
           tmin = temperature_min,
           tmean = temperature_mean,
           rh = humidity_mean) %>%
    mutate(co2 = 400) %>%
    mutate(city_exp = gsub("cps", paste0("cps", combinations$type[i]),
                           city_exp)) %>%
    filter(!is.na(rad)) %>%
    group_by(date) %>%
    mutate(flag = if_else(length(unique(hour)) != 24, 1, 0)) %>%
    filter(flag != 1) %>%
    ungroup()

  city_exp <- weather_set$city_exp[1]

  # if ((combinations$type[i] == "A") & (combinations$model[i] == "tomgro")){
  #   temp <- weather_set %>%
  #     mutate(city_exp = as.factor(city_exp))
  #   summaries_l[[paste0('h', i)]] <- as.data.frame(summary(temp))
  #   summaries_l2[[paste0('h', i)]] <- as.data.frame(summary(temp[temp$rad > 0, ]))
  # 
  # } 

  if(combinations$model[i] == "tomgro"){
    write.csv(weather_set, file = paste0("./data/weather/hourly_",
                                         city_exp, ".csv"), row.names=FALSE)

  } else {

    weather_set_s <- weather_set %>%
      group_by(city_exp, radiation_unit, cycle, date, dat, doy) %>%
      summarise(rad = sum(rad, na.rm = TRUE),
                tmax = max(tmax, na.rm = TRUE),
                tmin = min(tmin, na.rm = TRUE),
                tmean = mean(tmean, na.rm = TRUE),
                co2 = mean(co2, na.rm = TRUE)) %>%
      filter(!is.na(tmean)) %>%
      ungroup()

    write.csv(weather_set_s, file = paste0("./data/weather/daily_",
                                           city_exp, ".csv"), row.names=FALSE)

    # if ((combinations$type[i] == "A") & (combinations$model[i] == "simple")){
    #   temp <- weather_set_s %>%
    #     mutate(city_exp = as.factor(city_exp))
    #   summaries_l[[paste0('d', i)]] <- as.data.frame(summary(temp))
    # 
    # }
  }

}

# summaries <- Reduce(rbind, summaries_l) %>%
#   mutate(Var2 = as.character(Var2),
#          Var2 = trimws(Var2, "both")) %>%
#   filter(Var2 == "tmean" | Var2 == "tmax" | Var2 == "tmin" |
#            Var2 == "rad" | Var2 == "city_exp") %>%
#   separate(Freq, into = c("stat", "value"), sep = ":") %>%
#   mutate(stat = trimws(stat, "both")) %>%
#   filter(!is.na(stat), stat != "Min.", stat != "1st Qu.",
#          stat != '3rd Qu.', stat != "Max.", stat != "Median",
#          stat != "NA's")
# 
# summaries2 <- Reduce(rbind, summaries_l2) %>%
#   mutate(Var2 = as.character(Var2),
#          Var2 = trimws(Var2, "both")) %>%
#   filter(Var2 == "rad" | Var2 == "city_exp") %>%
#   separate(Freq, into = c("stat", "value"), sep = ":") %>%
#   mutate(stat = trimws(stat, "both")) %>%
#   filter(!is.na(stat), stat != "Min.", stat != "1st Qu.",
#          stat != '3rd Qu.', stat != "Max.", stat != "Median",
#          stat != "NA's")
# 
# summary_all <- rbind(summaries, summaries2)
# write.csv(summary_all,
#           file = "./tables/summary_cps.csv", row.names = FALSE)
