# This code organizes data from the load cells. It fixes the time recorded.
# It also converts the recorded weight into mass and aggregates the
# results hourly.

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("tidyverse")

# Functions ---------------------------------------------------------------

source("./01b-cleanOrgMass_functions.R")

orgScales <- function(x, sensor_dates){
  
  x_mod <- x %>%
    fixTime(sensor_dates) %>%
    mutate(dateFull = if_else((date(dateFull) >= as.Date("2021-03-08")) &
                                (date(dateFull) <= as.Date("2021-03-09")),
                              dateFull + hours(38), dateFull),
           date = as.character(date(dateFull))) %>%
    fixHours(sensor_dates, "mass")  %>%
    includeMass()
  
  return(x_mod)
  
}

# sensor_dates <- read.csv("../tables/relevant_dates_sensors.csv")
# colnames(sensor_dates)[1] <- "cycle"
# 
# x <- read.csv("../data/observations/monitoring/scales_raw.csv")
# type_s <- "mass"

# Load files --------------------------------------------------------------

scales_files <- list.files("../data/observations/monitoring/scales", full.names = T)
scales_files <- grep("scales[0-9][0-9].csv", scales_files, value = TRUE)

if (file.exists("../data/observations/monitoring/scales_raw.csv")){
  
  unlink("../data/observations/monitoring/scales_raw.csv")
  
}

for (i in scales_files){
  
  temp <- read.csv(i, sep = ";")
  temp <- temp[, 1:3]
  
  head_csv <- colnames(temp)
  
  write.table(temp, file = "../data/observations/monitoring/scales_raw.csv",
              sep = ",",
              append = TRUE,
              row.names = FALSE,
              col.names = FALSE,
              fileEncoding = "UTF-8")
  
}

scales <- read.csv("../data/observations/monitoring/scales_raw.csv", dec = ".",
                   col.names = head_csv)
#write.csv(head_csv, file = "../data/observations/monitoring/scales_raw_header.csv", row.names=FALSE)

sensor_dates <- read.csv("../tables/relevant_dates_sensors.csv")
colnames(sensor_dates)[1] <- "cycle"

# Process scales data -----------------------------------------------------

nScales <- grep("^Channel", colnames(scales), value = TRUE)
scales_raw <- scales %>%
  gather(nScales, key = 'node', value = 'channel') %>%
  rename(time = Time) %>%
  mutate(node = as.numeric(substring(node, nchar(node), nchar(node))))

scales_org <- orgScales(scales_raw, sensor_dates)

# Save file ---------------------------------------------------------------

write.csv(scales_raw,
          file = "../data/observations/monitoring/scales_raw.csv", row.names=FALSE)
write.csv(scales_org,
          file = "../data/observations/monitoring/scales_org.csv", row.names=FALSE)
