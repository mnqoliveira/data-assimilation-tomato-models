# Joins all different files referring to SHT75 data with basic formatting
# for subsequent processing.

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(stringr)

# Functions ---------------------------------------------------------------
source("./data_processing/01e-cleanOrgWeather_Radio_functions.R")

procData <- function(x, sensor_dates){
  
  x_mod <- x %>%
    formatData() %>%
    checkSanity() %>%
    fixHours(sensor_dates, "radio") %>%
    mutate(date = as.character(date(dateFull))) %>%
    removeObs(sensor_dates)  %>%
    renameSensors()
  
  return(x_mod)
  
}

# Load files --------------------------------------------------------------

radio_files <- list.files("./data/weather/monitoring/raw/radio", full.names = T)

if (file.exists("./data/weather/monitoring/raw/weather_radio.csv")){
  
  unlink("./data/weather/monitoring/raw/weather_radio.csv")
  
}

for (i in radio_files){
  
  temp <- read.csv(i, sep = ",")
  write.table(temp, file = "./data/weather/monitoring/raw/weather_radio.csv",
              sep = ";",
              append = TRUE,
              row.names = FALSE,
              col.names = FALSE,
              fileEncoding = "UTF-8")
  
}

header <-   c("sensor", "dateFull", "measurement", "proc", "comment")
weather <- read.csv("./data/weather/monitoring/raw/weather_radio.csv", sep = ";",
                    col.names = header)

sensor_dates <- read.csv("./tables/relevant_dates_sensors.csv")
colnames(sensor_dates)[1] <- "cycle"

# Process data ------------------------------------------------------------

radio_full <- procData(weather, sensor_dates)

write.csv(radio_full, 
          file = "./data/weather/monitoring/weather_radio_org.csv", 
          row.names=FALSE)
