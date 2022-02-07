# Processes system wet mass and joins with potential difference data

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)

# Source functions --------------------------------------------------------

source("./data_processing/03b-processMassData_functions.R")

# Load files --------------------------------------------------------------

mv <- read.csv("./data/moisture/waterContent.csv") %>%
  mutate(date = as.Date(date))

scales_proc <- read.csv("./data/observations/monitoring/scales_proc.csv")

sensor_dates <- read.csv("./tables/relevant_dates_sensors.csv")

# temp <- x_mod %>%
#   mutate(date = date(ymd_hms(dateFull))) %>%
#   filter(as.Date(date) >= as.Date("2021-03-09"),
#          as.Date(date) <= as.Date("2021-03-10")) %>%
#   mutate(dateFull = ymd_hms(dateFull))
# 
# load("./data/plot_theme.RData")
# ggplot(temp) +
#   geom_point(aes(x = dateFull, y = wm, colour = as.factor(node))) +
#   scale_x_datetime(date_breaks = "2 hours",
#                    date_labels = "%d-%b %H:%M") +
#   mytheme


# Process data ------------------------------------------------------------
scales_proc_mod <-  scales_proc %>%
  mutate(date = as.Date(date)) %>%
  filterOutliers() %>%
  fixInitialValue(sensor_dates)

write.csv(scales_proc_mod, 
          file = "./data/observations/monitoring/scales_proc_full.csv", 
          row.names=FALSE)

scales_proc_mv <- scales_proc_mod %>%
  calcMeanSD() %>%
  left_join(mv) %>%
  select(-sensor_var) %>%
  # Remove last day as it is often problematic
  group_by(cycle) %>%
  filter(dat != max(dat, na.rm = TRUE)) %>%
  ungroup()

write.csv(scales_proc_mv, 
          file = "./data/observations/monitoring/scales_proc_mv.csv", 
          row.names=FALSE)
