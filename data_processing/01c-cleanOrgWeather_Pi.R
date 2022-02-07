# This code organizes data from the Raspberry Pi sensors. 

# No feature engineering in this code, except for attribution of to which
# node each sensor refers and which is the measured variable. 
# Dates are formatted.

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------


# Functions ---------------------------------------------------------------
source("./data_processing/01c-cleanOrgWeather_Pi_functions.R")

orgSensorData <- function(x, sensor_dates){
  # The dataset has temperature, humidity and radiation 
  # values from different sensors. 
  
  hSensors <- grep("^h", colnames(x), value = TRUE)
  tSensors <- gsub("h", "t", hSensors)
  
  x_mod <- x %>%
    orgVariables(tSensors, hSensors) %>%
    fixHours(sensor_dates, "pi") %>%
    checkSanity()
  
  return(x_mod)
  
}

# Load files --------------------------------------------------------------

weather_files <- list.files("./data/weather/monitoring/raw/sensorpi", full.names = T, 
                            pattern = "sensor[0-9][0-9].csv")

weather_list <- list()
for (i in weather_files){
  temp <- read.csv(i)
  sensor_i <- substring(i, (nchar(i) - 4), nchar(i)- 4)
  sensor_i <- if_else(as.numeric(sensor_i) %% 2 == 1, "1", "2")
  temp$node <- sensor_i
  
  if(!"hi2c" %in% colnames(temp)){
    
    temp$hi2c <- NA
    temp$ti2c <- NA
    
  }
  
  weather_list[[i]] <- temp
}

weather <- Reduce(rbind, weather_list)

sensor_dates <- read.csv("./tables/relevant_dates_sensors.csv")
colnames(sensor_dates)[1] <- "cycle"

# Process data ------------------------------------------------------------

pi_full <- orgSensorData(weather, sensor_dates)

write.csv(pi_full, file = "./data/weather/monitoring/weather_pi_org.csv", row.names=FALSE)
