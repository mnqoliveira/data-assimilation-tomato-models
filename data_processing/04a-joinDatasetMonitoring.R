# Joins the plant data obtained by the different sensors, including 
# images and weighting system

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)

# Source functions --------------------------------------------------------


# Load files --------------------------------------------------------------
# Enquanto nao sei o que fazer com a agua, carrego os dados sem processamento
scales_proc_mv <- read.csv("./data/observations/monitoring/scales_proc_mv.csv")

# Images
lai_lat <- read.csv("./data/observations/monitoring/lai/lai_lat_ids.csv")
lai_abv <- read.csv("./data/observations/monitoring/lai/lai_abv_ids.csv")
wf_lat <- read.csv("./data/observations/monitoring/dry_mass_fruit/wf_lat_ids.csv")
wm_lat <- read.csv("./data/observations/monitoring/dry_mass_mature_fruit/wm_lat_ids.csv")
height <- read.csv("./data/observations/monitoring/dry_mass_aboveground/height_ids.csv")
n <- read.csv("./data/observations/monitoring/nodes/n_lat_ids.csv")

# Additional data
codes_exp <- read.csv("./tables/codes_exp.csv")
dates_exp <- read.csv("./data/cycle_dates.csv")
harvests <- read.csv("./data/observations/monitoring/harvests.csv")

# Preprocess fruit images -------------------------------------------------
# While, differently from calibration, in this case there are pictures from 
# plants with harvested fruits, they will not be used. So after harvesting, 
# these fruits observations no longer will be included

harv_mod <- harvests %>%
  left_join(dates_exp)

full_exp <- dates_exp %>%
  full_join(codes_exp)

harv_start <- codes_exp %>%
  left_join(harv_mod) %>%
  group_by(city_exp) %>%
  filter(date == min(date)) %>%
  ungroup() %>%
  select(-city_exp, -ro, -id, -type)

wm_lat_mod <- wm_lat %>%
  full_join(harv_start) %>%
  group_by(cycle, node) %>%
  arrange(node, as.Date(date)) %>%
  mutate(fm = if_else(is.na(fm), -99, fm),
         fm = if_else(dat == min(dat), 0, fm),
         fm = na_if(fm, -99L),
         fm = zoo::na.locf(fm)) %>%
  ungroup() %>%
  filter(fm == 0, !is.na(wm_lat)) %>%
  select(-fm, -dm)

wf_lat_mod <- wf_lat %>%
  full_join(harv_start) %>%
  group_by(cycle, node) %>%
  arrange(node, as.Date(date)) %>%
  mutate(fm = if_else(is.na(fm), -99, fm),
         fm = if_else(dat == min(dat), 0, fm),
         fm = na_if(fm, -99L),
         fm = zoo::na.locf(fm)) %>%
  ungroup() %>%
  filter(fm == 0, !is.na(wf_lat)) %>%
  select(-fm, -dm)

# Version 1: Value measured, without imputation of harvests ---------------
# dataset_assim <- scales_proc %>%
#   arrange(node, date, hour) %>%
#   filter(hour == 4)

#write.csv(dataset_assim, file = "./data/dataset_hourly.csv", row.names=FALSE)

# Version 2: Include harvested fruits and pruning days --------------------
dataset_assim_mod <- dates_exp %>%
  full_join(codes_exp) %>%
  left_join(scales_proc_mv) %>%
  # Hour comes from scales, but I have no record of it for the last day,
  # although I may have for other observations
  filter(hour == 4 | is.na(hour), cycle != 99) %>%
  arrange(node, date, hour) %>%
  left_join(n) %>%
  left_join(lai_lat) %>%
  left_join(lai_abv) %>%
  left_join(height) %>%
  left_join(wf_lat_mod) %>%
  left_join(wm_lat_mod) %>%
  arrange(city_exp, date) %>%
  mutate(doy = yday(date))

write.csv(dataset_assim_mod, 
          file = "./data/observations/monitoring/monitoring_cps.csv", 
          row.names=FALSE)

