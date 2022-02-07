# Processes wet mass data to includ information regarding the plants

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)

# Source functions --------------------------------------------------------
source("./data_processing/02a-processWetMass_functions.R")

prepPlantDataset <- function(x, plant_dates, sensor_dates,
                             cycle_dates, harvest){

  x_mod <- x %>%
    initialPrep(plant_dates, sensor_dates) %>%
    includeNAs() %>%
    includeCycle(cycle_dates) 
  
  x_mod2 <- x_mod %>%
    filter(!is.na(cycle)) %>%
    idPlants(plant_dates) %>%
    includeStages(plant_dates) %>%
    includeDAP(plant_dates) %>%
    #removeRecords(plant_dates, sensor_dates) %>%
    arrange(node, dateFull) %>%
    includeHarvest(harvest) %>%
    select(-id, -channel)
  
  x_mod3 <- list()
  x_mod3[["cycle99"]] <- filter(x_mod, is.na(x_mod$cycle))
  x_mod3[["growth"]] <- x_mod2
  
  return(x_mod3)  
  
}

# Load files --------------------------------------------------------------

scales_org <- read.csv("./data/observations/monitoring/scales_org.csv")

sensor_dates <- read.csv("./tables/relevant_dates_sensors.csv")
colnames(sensor_dates)[1] <- "cycle"
plant_dates <- read.csv("./tables/relevant_dates_plants.csv")
colnames(plant_dates)[1] <- "cycle"

cycle_dates <- read.csv("./data/cycle_dates.csv") %>%
  mutate(date = as.Date(date))

harvest <- read.csv("./data/observations/monitoring/harvests.csv")


# Process data ------------------------------------------------------------

scales_proc <- scales_org %>%
  prepPlantDataset(plant_dates, sensor_dates, cycle_dates, harvest)

data_growth <- scales_proc[["growth"]]
add_data <- scales_proc[["cycle99"]]

write.csv(data_growth, 
          file = "./data/observations/monitoring/scales_proc.csv", 
          row.names=FALSE)

write.csv(add_data, 
          file = "./data/observations/monitoring/scales_notGrowth.csv", 
          row.names=FALSE)
