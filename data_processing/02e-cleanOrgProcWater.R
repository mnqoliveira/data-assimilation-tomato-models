# Organizes and includes additional information to measured 
# potential difference in substrate

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)

# Functions ---------------------------------------------------------------

source("02e-cleanOrgProcWater_functions.R")

orgSensorData <- function(x, cycle_dates, sensor_dates){
  
  x_mod <- x %>%
    select(time, contains("ec")) %>%
    orgTime() %>%
    fixHours(sensor_dates, "wc") %>%
    includeInfo() %>%
    calcMeanSD(cycle_dates)
  
  return(x_mod)
  
}

# Load files --------------------------------------------------------------

wc_files <- list.files("../data/moisture/", full.names = T, 
                       pattern = "wc[0-9][0-9].csv")

if(length(wc_files) == 1){
  
  wc <- read.csv(wc_files, sep = ",")
  
} else {

  wc <- list()
  for (i in wc_files){
    temp <- read.csv(i, sep = ",")
    wc[[i]] <- temp
  }

  wc <- Reduce(rbind, wc)
  
}

sensor_dates <- read.csv("../tables/relevant_dates_sensors.csv")
colnames(sensor_dates)[1] <- "cycle"

cycle_dates <- read.csv("../data/cycle_dates.csv")

# Process data ------------------------------------------------------------

content_water <- orgSensorData(wc, cycle_dates, sensor_dates)

write.csv(content_water, file = "../data/moisture/waterContent.csv", 
          row.names=FALSE)
