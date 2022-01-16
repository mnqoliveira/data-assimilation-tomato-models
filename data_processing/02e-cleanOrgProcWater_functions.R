# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)

# Functions ---------------------------------------------------------------

source("00-additional_functions.R")

orgTime <- function(x){
  
  # Full timestamp is stored as y-m-d h:m:s      
  x_mod <- x %>%
    mutate(time = ymd_hms(substring(time, 1, 19)),
           date = as.character(date(time))) %>%
    rename(dateFull = time) %>%
    filter(!is.na(date))
  
  return(x_mod)
  
}

includeInfo <- function(x){
  
  x_mod <- x %>%
    gather("ec_1", "ec_2", key = "node", value = "measurement") %>%
    mutate(node = as.numeric(substring(node, nchar(node))),
           sensor_var = "ec05_mV",
           hour = hour(dateFull))
  
  return(x_mod)
  
}

calcMeanSD <- function(x, cycle_dates){
  
  # NA values should be made explicit so that they are not just "skipped".
  days <- seq(min(as.Date(x$date)), max(as.Date(x$date)), 1)
  sensors <- as.character(unique(x$sensor_var))
  
  allHours <- expand.grid(node = c(1, 2), date = days,
                          hour = c(0:23), sensor_var = sensors) %>%
    filter(!paste(date, hour, node) 
           %in% paste(x$date, x$hour, x$node)) %>%
    filter(date %in% as.Date(cycle_dates$date)) %>%
    mutate(dateFull = ymd_hms(paste0(date, "T", hour, ":00:00")),
           measurement = NA,
           sensor_var = as.character(sensor_var))
  
  # Includes mean and SD for water content values
  x_mod <- x %>%
    mutate(date = as.Date(date)) %>%
    rbind(allHours) %>%
    group_by(date, hour, sensor_var, node) %>%
    summarise(mean_mv = mean(measurement, na.rm = TRUE),
              sd_mv = sd(measurement, na.rm = TRUE)) %>%
    arrange(node, date, hour) %>%
    ungroup()
  
  return(x_mod)
  
}
