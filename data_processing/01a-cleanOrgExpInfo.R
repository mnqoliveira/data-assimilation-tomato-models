# Generates general info from experiments

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(zoo)
library(lubridate)

# Functions ---------------------------------------------------------------
cycle_days <- function(dates_plants){
  # Cycle is either going to be identified by transplanting date or by the 
  # lower date available in the dataset
  
  x <- dates_plants %>%
    filter(str_detect(event, "transplanting|end")) %>%
    mutate(date = ymd(date)) %>%
    select(date, cycle, event)
  
  x_mod <- as.Date("1970-01-01")
  for (i in unique(x$cycle)){
    
    dates <- filter(x, cycle == i)
    
    beginning <- as.Date(dates$date[dates$event == "transplanting"])
    end <- as.Date(dates$date[dates$event == "end"])
    
    cycle_dates <- seq(beginning, end, 1)
    
    x_mod <- c(x_mod, cycle_dates)
    
  }
  
  x_mod <- x_mod[-1]
  
  x_full <- data.frame("date" = x_mod) %>%
    full_join(x) %>%
    select(-event) %>%
    mutate(cycle = zoo::na.locf(cycle))
  
  return(x_full)
  
}

# Load files --------------------------------------------------------------

sensor_dates <- read.csv("./tables/relevant_dates_sensors.csv")
colnames(sensor_dates)[1] <- "cycle"
plant_dates <- read.csv("./tables/relevant_dates_plants.csv")
colnames(plant_dates)[1] <- "cycle"

# Process -----------------------------------------------------------------
cycles_dates <- cycle_days(plant_dates) %>%
  arrange(date) %>%
  group_by(cycle) %>%
  mutate(dat = row_number() - 1) %>%
  ungroup()

write.csv(cycles_dates, file = "./data/cycle_dates.csv", row.names=FALSE)
