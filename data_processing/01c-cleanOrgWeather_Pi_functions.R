# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(stringr)

library(zoo)

# Functions ---------------------------------------------------------------

source("00-additional_functions.R")

orgVariables <- function(x, tSensors, hSensors){
  
  x_mod <- x %>%
    # In a few records, cpu_temp was not recorded. As a consequence, date
    # was recorded in its column instead and the date column is empty.
    # Since those records are not from a growth cycle, they are removed.
    filter(!is.na(date), date != "") %>%
    # There is also one instance of date wrongly recorded as in 1999. To
    # remove such cases, all dates are checked to be larger than the previous 
    # one.
    group_by(node) %>%
    mutate(date_lag = lag(date),
           flag = if_else(date_lag > date, 1, 0),
           id = row_number()) %>%
    # Including an observation that came from malfunction in the sensor
    filter(flag == 0 | id == 1, id != 84542) %>%
    ungroup() %>%
    # Reorganizing data for long format
    gather(tSensors, hSensors, lux, 
           key = "sensor_id", value = "measurement") %>%
    filter(sensor_id != "ti2c", sensor_id != "hi2c") %>%
    mutate(variable = if_else(sensor_id %in% tSensors,
                              "temperature", if_else(sensor_id %in% hSensors,
                                                     "humidity",
                                                     "radiation"))) %>%
    # Rename sensors to remove the arbitrary previously given names
    # Use the codes ascribed by factors.
    group_by(variable) %>%
    mutate(sensor_id = paste0("s0", as.numeric(as.factor(sensor_id))),
           sensor = "pi") %>%
    ungroup() %>%
    mutate(dateFull = ymd_hms(date, truncated = 1),
           date = as.character(date(dateFull))) %>%
    select(-moist, -cpu_temp, -date_lag, -flag, -id)
    
  return(x_mod)
  
}

checkSanity <- function(x){
  
  x_mod <- x %>%
    mutate(measurement = if_else(variable == "temperature",
                                 if_else(measurement < -5 | measurement > 60,
                                         -99, measurement),
                                 if_else(variable == "humidity",
                                         if_else(measurement < 0 | 
                                                   measurement > 100,
                                                 -99, measurement),
                                         measurement))) %>%
    mutate(measurement = na_if(measurement, -99))
  
  return(x_mod)
  
}
