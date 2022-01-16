# Support functions for radiation data processing

# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(stringr)

# Functions ---------------------------------------------------------------
formatData <- function(x){
  
  x_mod <- x %>%
    mutate(dateFull = ymd_hms(dateFull),
           variable = "radiation",
           sensor = "li1400",
           sensor_id = "s01",
           node = as.numeric(node),
           radiation_unit = "mmolPAR") %>%
    arrange(node, dateFull)
  
  return(x_mod)
  
}

cleanDup <- function(x){
  
  # Since there could have been different inputs from the broken raw txts,
  # an average for values recorded at the same time is defined as the 
  # measurement value.
  x_mod <- x %>%
    group_by(node, dateFull, variable, sensor, sensor_id) %>%
    summarise(measurement = mean(measurement, na.rm = TRUE)) %>%
    ungroup()
  
  return(x_mod)
  
}

fillNight <- function(x){
  # Data was recorded from 5am to 8pm. So values outside this range will 
  # be filled with 0
  
  days <- seq(min(date(x$dateFull)), max(date(x$dateFull)), 1)

  allHours <- expand.grid(node = c(1, 2), 
                          date = days, 
                          hour = str_pad(c(0:23), 2, "left", "0"), 
                          minutes = str_pad(seq(0, 59, 5), 
                                                         2, "left", "0")) %>%
    mutate(hour = as.character(hour),
           minutes = as.character(minutes),
           dateFull = paste0(date, " ", hour, ":", minutes,":00")) %>%
    filter(!(hour == "00" & minutes == "00")) %>%
    mutate(dateFull = ymd_hms(dateFull)) %>%
    filter(!paste(node, dateFull) %in% paste(x$node, x$dateFull)) %>%
    mutate(measurement = NA,
           variable = "radiation",
           sensor = "li1400",
           sensor_id = "s01") %>%
    select(-date, -hour, -minutes)
  
  # Since there could have been different inputs from the broken raw txts,
  # an average for values recorded at the same time is defined as the 
  # measurement value.
  x_mod <- x %>%
    rbind(allHours) %>%
    arrange(node, dateFull)
  
  return(x_mod)
  
}
