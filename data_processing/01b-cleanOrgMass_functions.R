# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(forecast)

# Functions ---------------------------------------------------------------

source("00-additional_functions.R")

fixTime <- function(x, sensor_dates){
  # Datalogger has a programming problem
  # hour is being wrongly recorded as 65534 for after 10 pm  
  # Date is being set to the following day at the same time
  # Full timestamp is stored as d-m-y_h-m-s_ms
  
  x_mod <- x %>%
    # Change the mess that the logger programming made
    mutate(wrongTime = if_else(grepl("_655", time) == TRUE, TRUE, FALSE),
           time = if_else(wrongTime, gsub("65534", "22", time), time),
           time = if_else(wrongTime, gsub("65535", "23", time), time),
           time = substring(time, 1, 19),
           time = dmy_hms(time)) %>%
    rename(dateFull = time) %>%
    mutate(dateFull = if_else(wrongTime, dateFull - days(1), dateFull)) %>%
    select(-wrongTime)
  
  return(x_mod)
  
}

includeMass <- function(x){
  
  # Logger stored weight - mass in grams is included manually
  
  x_mod <- x %>%
    mutate(wm = channel * -1000 / 9.81)
  
  return(x_mod)
  
}