# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(zoo)
library(tidyverse)
library(lubridate)
library(stringr)

# Functions ---------------------------------------------------------------

source("./data_processing/00-additional_functions.R")

formatData <- function(x){
  
  x_mod <- x %>%
    select(sensor, dateFull, measurement) %>%
    distinct() %>%
    mutate(dateFull = ymd_hms(dateFull, truncated = 1),
           variable = c(str_match(sensor, "Temp|Umid|temp|umid")),
           sensor_id = str_extract(sensor, "sensor[:digit:]+"),
           sensor_id = str_extract(sensor_id, "[:digit:]+"),
           node = if_else(sensor_id == "11" | sensor_id == "12" | 
                            sensor_id == "06", 1,
                          if_else(sensor_id == "13" | 
                                    sensor_id == "08" | 
                                    sensor_id == "17", 2, 3)),
           sensor = "radio") %>%
    filter(node != 3) %>%
    arrange(node, dateFull) %>%
    mutate(variable = if_else(variable == "Temp" | variable == "temp",
                              "temperature",
                              "humidity"),
           node = as.character(node))
  
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

renameSensors <- function(x){
  
  x_mod <- x %>%
    # Rename sensors to remove the arbitrary previously given names
    # Use the codes ascribed by factors.
    group_by(variable) %>%
    mutate(sensor_id = paste0("s0", as.numeric(as.factor(sensor_id)))) %>%
    ungroup()
  
  return(x_mod)
  
}

removeObs <- function(x, sensor_dates){
  
  dates_filt <- sensor_dates %>%
    filter(type == "radio", 
           str_detect(event, "down")) %>%
    mutate(sensor_id = as.character(sensor_id)) %>%
    select(node, event, date, sensor_id, comment) %>%
    spread(event, date) %>%
    select(-comment) %>%
    rename(date = down_start)
  
  dates1 <- filter(dates_filt, is.na(sensor_id)) %>%
    select(-sensor_id)
  
  x_mod1 <- x %>%
    left_join(dates1) %>%
    arrange(node, dateFull) %>%
    rownames_to_column() %>%
    mutate(down_end = if_else(rowname == 1, "1970-01-01", down_end),
           down_end = zoo::na.locf(down_end),
           measurement = if_else(as.Date(date) <= as.Date(down_end),
                                 -99, measurement))  %>%
    select(-down_end, -rowname)
  
  dates2 <- filter(dates_filt, !is.na(sensor_id))
  
  x_mod2 <- x %>%
    left_join(dates2) %>%
    arrange(node, sensor_id, dateFull) %>%
    rownames_to_column() %>%
    mutate(down_end = if_else(rowname == 1, "1970-01-01", down_end),
           down_end = zoo::na.locf(down_end),
           measurement = if_else(as.Date(date) <= as.Date(down_end),
                                 -99, measurement)) %>%
    select(-down_end, -rowname)
  
  x_mod <- x_mod1 %>%
    left_join(x_mod2, by = c("sensor", "dateFull", "date",
                             "variable", "sensor_id", "node")) %>%
    mutate(measurement = measurement.x,
           measurement = if_else(measurement != -99. & measurement.y == -99.,
                                 measurement.y, measurement),
           measurement = na_if(measurement, -99),
           date = as.Date(date)) %>%
    select(-measurement.x, -measurement.y, -date)
  

    return(x_mod)
  
}