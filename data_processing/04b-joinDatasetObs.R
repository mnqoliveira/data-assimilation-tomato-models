# Joins the data obtained from images for calibration samples
# to the dataset of data obtained by destructive analyses
# as well as data from other experiments

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("zoo")
library("tidyverse")
library("lubridate")


# Load files --------------------------------------------------------------

#exp_dates <- read.csv("./tables/exp_dates.csv")


# Indirect LAI, W, Wf and Wm ----------------------------------------------
# calib lai_lat_calib
lai_lat_calib <- read.csv("./data/observations/monitoring/lai/lai_lat_calib_summ.csv")
lai_abv_calib <- read.csv("./data/observations/monitoring/lai/lai_abv_calib_summ.csv")
height_calib <- read.csv("./data/observations/monitoring/dry_mass_aboveground/height_calib_summ.csv")

wf_lat_calib <- read.csv("./data/observations/monitoring/dry_mass_fruit/wf_lat_calib_summ.csv")
wm_lat_calib <- read.csv("./data/observations/monitoring/dry_mass_mature_fruit/wm_lat_calib_summ.csv")

# Harvests ----------------------------------------------------------------

harvests <- read.csv("./data/observations/monitoring/harvests.csv")
cycle_dates <- read.csv("./data/cycle_dates.csv")

# Indirect N ------------------------------------------------------------
# # calib n_calib
# n_calib <- read.csv("./data/observations/nodes/")

# CPS ---------------------------------------------------------------------
cps <- read.csv("./data/observations/monitoring/experiments.csv") %>%
  arrange(city_exp, date) %>%
  mutate(node = "calib")

# Jones -------------------------------------------------------------------
filesPaths <- list.files("./data/observations/jones/", full.names = TRUE, 
                         pattern = "*.csv")
shortName <- list.files("./data/observations/jones/", 
                        pattern = "*.csv")
shortName <- substring(shortName, 1, nchar(shortName) - 4)

jones <- data.frame()
for (i in 1:length(shortName)){

  dataset <- read.csv(filesPaths[i]) 
  dataset_mod <- dataset %>%
    rename(dat = DAP) %>%
    mutate(city_exp = shortName[i],
           dat = zoo::na.locf(dat),
           count_NA = rowSums(is.na(.)),
           node = "calib") %>%
    filter(count_NA < ncol(.) - 3) %>%
    # mutate(Wm = if_else(is.na(Wm), 0.0, Wm)) %>%
    select(-count_NA) %>%
    rename_all(tolower)
  
  jones <- bind_rows(jones, dataset_mod)
  
}

# SIMPLE ------------------------------------------------------------------
filesPaths <- list.files("./data/observations/simple/", full.names = TRUE,
                         pattern = "*.csv")
shortName <- list.files("./data/observations/simple/",
                        pattern = "*.csv")
shortName <- substring(shortName, 1, nchar(shortName) - 4)

simple <- data.frame()
i <- 1

for (i in 1:length(shortName)){

  dataset <- read.csv(filesPaths[i])
  dataset_mod <- dataset %>%
    rename(dat = DAP,
           w = Biomass,
           lai = LAI,
           wm = Yield) %>%
    mutate(exp = shortName[i],
           dat = zoo::na.locf(dat))%>%
    select(-FSolar, -starts_with("T"))

  simple <- bind_rows(simple, dataset_mod)

}


# DSSAT -------------------------------------------------------------------
# 
# filesPaths <- list.files("./data/observations/dssat/", full.names = TRUE,
#                          pattern = "*.csv")
# shortName <- list.files("./data/observations/dssat/",
#                         pattern = "*.csv")
# shortName <- substring(shortName, 1, nchar(shortName) - 4)
# 
# dssat <- data.frame()
# i <- 13
# 
# for (i in 1:length(shortName)){
#   
#   dataset <- read.csv(filesPaths[i])
#   
#   dates <- exp_dates %>%
#     filter(city == dataset$city[1], cod == dataset$city_exp[1])
#   
#   dataset_mod <- dataset %>%
#     mutate(dat = doy - yday(as.Date(dates$pdate[1])),
#            w = if_else(w < 0, -99, w),
#            wm = if_else(wm < 0, -99, wm),
#            lai = if_else(lai < 0, -99, lai),
#            w = na_if(w, -99),
#            wm = na_if(wm, -99),
#            lai = na_if(lai, -99)) %>%
#     select(-starts_with("X"))
# 
#   dssat <- bind_rows(dssat, dataset_mod)
# 
# }
# 

# Araujo ------------------------------------------------------------------
filesPaths <- list.files("./data/observations/araujo/", full.names = TRUE,
                         pattern = "obs(.*)\\.csv")
shortName <- list.files("./data/observations/araujo/",
                        pattern = "obs(.*)\\.csv")
shortName <- substring(shortName, 1, nchar(shortName) - 4)

araujo <- data.frame()
i <- 4

for (i in 1:length(shortName)){
  
  dataset <- read.csv(filesPaths[i])
  araujo <- bind_rows(araujo, dataset)
  
}


# Remove harvested instances from image dataset ---------------------------
# Given harvests happened previous to taking the pictures, there is no
# compatibility between images and total biomass of fruits, so they
# shouldn't be included alongside.

# Instead of removing all images after a certain date, only those
# that were obtained by fractioned harvests will be removed.

wm_lat_calib_mod <- wm_lat_calib %>%
  left_join(cycle_dates) %>%
  full_join(harvests) %>%
  arrange(cycle, id, date) %>%
  group_by(cycle, id) %>%
  mutate(type_ = if_else(is.na(type), 0, 1),
         flag = if_else(sum(type_) >= 1, 1, 0),
         node = "calib") %>%
  filter(flag == 0) %>%
  ungroup() %>%
  select(-dat, -type, -type_, -flag, -fm, -dm, -id) %>%
  distinct()

wf_lat_calib_mod <- wf_lat_calib %>%
  left_join(cycle_dates) %>%
  full_join(harvests) %>%
  arrange(cycle, id, date) %>%
  group_by(cycle, id) %>%
  mutate(type_ = if_else(is.na(type), 0, 1),
         flag = if_else(sum(type_) >= 1, 1, 0),
         node = "calib") %>%
  filter(flag == 0) %>%
  ungroup() %>%
  select(-dat, -type, -type_,-flag, -fm, -dm, -id) %>%
  distinct()


# Join --------------------------------------------------------------------
all_obs <- cps %>%
  left_join(lai_lat_calib) %>%
  left_join(lai_abv_calib) %>%
  left_join(height_calib) %>%
  left_join(wf_lat_calib_mod) %>%
  left_join(wm_lat_calib_mod) %>%
  bind_rows(jones) %>%
  bind_rows(simple) %>%
  #bind_rows(dssat) %>%
  bind_rows(araujo) %>%
  arrange(city_exp, dat) %>%
  mutate(doy = yday(as.Date(date)),
         node = "calib")

write.csv(all_obs, "./data/observations/monitoring/obs_exp_all.csv", row.names=FALSE)

