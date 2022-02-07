# Organizes outputs from python scripts and include 
# information regarding plants

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(zoo)

# Load files --------------------------------------------------------------
#Calibration
cover_calib_all <- list.files('./data/observations/monitoring/lai/', 
                             pattern = '^raw_cover_calib.',
                             full.names = TRUE)

height_calib_all <- list.files('./data/observations/monitoring/dry_mass_aboveground/', 
                              pattern = '^raw_height_calib.',
                              full.names = TRUE)


nodes_calib_all <- list.files('./data/observations/monitoring/nodes/', 
                              pattern = '^raw_n_calib',
                              full.names = TRUE)

wfcover_calib_all <- list.files('./data/observations/monitoring/dry_mass_fruit/', 
                                pattern = '^raw_wfcover_calib',
                                full.names = TRUE)

wmcover_calib_all <- list.files('./data/observations/monitoring/dry_mass_mature_fruit/', 
                                pattern = '^raw_wmcover_calib',
                                full.names = TRUE)

# Monitoring
cover_all <- list.files('./data/observations/monitoring/lai/', 
                        pattern = '^raw_cover_cycle.',
                        full.names = TRUE)

nodes_all <-  list.files('./data/observations/monitoring/nodes/', 
                         pattern = '^raw_n_cycle.',
                         full.names = TRUE)

height_all <- list.files('./data/observations/monitoring/dry_mass_aboveground/', 
                         pattern = '^raw_height_cycle.',
                         full.names = TRUE)

wfcover_all <- list.files('./data/observations/monitoring/dry_mass_fruit/', 
                          pattern = '^raw_wfcover_cycle',
                          full.names = TRUE)

wmcover_all <- list.files('./data/observations/monitoring/dry_mass_mature_fruit/', 
                          pattern = '^raw_wmcover_cycle',
                          full.names = TRUE)

cycle_dates <- read.csv("./data/cycle_dates.csv")

# Aboveground cover area --------------------------------------------------
# Calibration
temp <- list()

for (filename in cover_calib_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

cover_calib <- Reduce(rbind, temp)

cover_abv <- cover_calib %>%
  separate(filename, c("angle", "date", "id", "part"), "_") %>%
  filter(angle == "abv") %>%
  mutate(id = if_else(!is.na(id), 
                      gsub("id", "", id), as.character(id)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -angle) %>%
  rename(lai_abv = cover)

write.csv(cover_abv, 
          file = "./data/observations/monitoring/lai/lai_abv_calib_ids.csv", 
          row.names = FALSE)

cover_abv_sd <- cover_abv %>%
  group_by(date) %>%
  summarise(lai_abv_mean = mean(lai_abv),
            lai_abv_calib_sd = sd(lai_abv)) %>%
  rename(lai_abv = lai_abv_mean,
         lai_abv_sd = lai_abv_calib_sd)

write.csv(cover_abv_sd, 
          file = "./data/observations/monitoring/lai/lai_abv_calib_summ.csv", 
          row.names = FALSE)

# Monitoring

temp <- list()

for (filename in cover_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

cover_temp <- Reduce(rbind, temp)

cover_abv <- cover_temp %>%
  filter(camera == "camerapi02" | camera == "camerapi04") %>%
  separate(filename, c("date", "hour", "min", "part"), "_") %>%
  mutate(node = if_else(camera == "camerapi02", 1, 
                        if_else(camera == "camerapi04", 2, 0)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -camera, -hour, -min) %>%
  rename(lai_abv = lai_)

# Include the standard deviation from the experiment. Filling the first values
# with the minimum value from the calibration dataset.
sd <- cover_abv_sd %>%
  select(date, lai_abv_sd)

# While the value will be adopted for all the photos between calibration dates,
# they will not be joined only by date, but by value also, so that
# calibration and monitoring may be kept separate
cover_abv_monit_sd <- cover_abv %>%
  full_join(sd) %>%
  left_join(cycle_dates) %>%
  group_by(cycle) %>%
  arrange(date) %>%
  mutate(lai_abv_sd = if_else(date == min(date), 
                              min(lai_abv_sd, na.rm = TRUE),
                              lai_abv_sd),
         lai_abv_sd = na_if(lai_abv_sd, -99),
         lai_abv_sd = zoo::na.locf(lai_abv_sd)) %>%
  filter(!is.na(lai_abv), !is.na(node))

write.csv(cover_abv_monit_sd, 
          file = "./data/observations/monitoring/lai/lai_abv_ids.csv", 
          row.names = FALSE)


# Lateral cover area ------------------------------------------------------
# Calibration
cover_lat <- cover_calib %>%
  separate(filename, c("angle", "date", "id", "part"), "_") %>%
  filter(angle == "lat") %>%
  mutate(id = if_else(!is.na(id), 
                       gsub("id", "", id), as.character(id)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -angle) %>%
  rename(lai_lat = cover) 

write.csv(cover_lat, 
          file = "./data/observations/monitoring/lai/lai_lat_calib_ids.csv", 
          row.names = FALSE)

cover_lat_sd <- cover_lat %>%
  group_by(date) %>%
  summarise(lai_lat_mean = mean(lai_lat),
            lai_lat_calib_sd = sd(lai_lat)) %>%
  rename(lai_lat = lai_lat_mean,
         lai_lat_sd = lai_lat_calib_sd)

write.csv(cover_lat_sd, 
          file = "./data/observations/monitoring/lai/lai_lat_calib_summ.csv", 
          row.names = FALSE)

# Monitoring
cover_lat <- cover_temp %>%
  filter(camera == "camerapi01" | camera == "camerapi03") %>%
  separate(filename, c("date", "hour", "min", "part"), "_") %>%
  mutate(node = if_else(camera == "camerapi01", 1, 
                        if_else(camera == "camerapi03", 2, 0)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -camera, -hour, -min) %>%
  rename(lai_lat = lai_)

# Include the standard deviation from the experiment.
sd <- cover_lat_sd %>%
  select(date, lai_lat_sd)

cover_lat_monit_sd <- cover_lat %>%
  full_join(sd) %>%
  left_join(cycle_dates) %>%
  group_by(cycle) %>%
  arrange(date) %>%
  mutate(lai_lat_sd = if_else(date == min(date), 
                              min(lai_lat_sd, na.rm = TRUE),
                              lai_lat_sd),
         lai_lat_sd = na_if(lai_lat_sd, -99),
         lai_lat_sd = zoo::na.locf(lai_lat_sd)) %>%
  filter(!is.na(lai_lat), !is.na(node))

write.csv(cover_lat_monit_sd, 
          file = "./data/observations/monitoring/lai/lai_lat_ids.csv", 
          row.names = FALSE)


# Height ------------------------------------------------------------------
# Calibration
temp <- list()

for (filename in height_calib_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

height_calib <- Reduce(rbind, temp)

height <- height_calib %>%
  separate(filename, c("angle", "date", "id", "part"), "_") %>%
  mutate(id = if_else(!is.na(id), 
                      gsub("id", "", id), as.character(id)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -angle)

write.csv(height, 
          file = "./data/observations/monitoring/dry_mass_aboveground/height_calib_ids.csv", 
          row.names = FALSE)

height_sd <- height %>%
  group_by(date) %>%
  summarise(height_mean = mean(height),
            height_calib_sd = sd(height)) %>%
  rename(height = height_mean,
         height_sd = height_calib_sd)

write.csv(height_sd, 
          file = "./data/observations/monitoring/dry_mass_aboveground/height_calib_summ.csv", 
          row.names = FALSE)

# Monitoring
temp <- list()

for (filename in height_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

height_temp <- Reduce(rbind, temp)

height <- height_temp %>%
  separate(filename, c("date", "hour", "min", "part"), "_") %>%
  mutate(node = if_else(camera == "camerapi01", 1, 
                        if_else(camera == "camerapi03", 2, 0)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -camera, -hour, -min)

# Include the standard deviation from the experiment. Filling the first values
# with the minimum value from the calibration dataset.
sd <- height_sd %>%
  select(date, height_sd) %>%
  left_join(height_sd)

# While the value will be adopted for all the photos between calibration dates,
# they will not be joined only by date, but by value also, so that
# calibration and monitoring may be kept separate
height_monit_sd <- height %>%
  full_join(sd) %>%
  left_join(cycle_dates) %>%
  group_by(cycle) %>%
  arrange(date) %>%
  mutate(height_sd = if_else(date == min(date),
                              min(height_sd, na.rm = TRUE),
                              height_sd),
         height_sd = na_if(height_sd, -99),
         height_sd = zoo::na.locf(height_sd)) %>%
  filter(!is.na(height), !is.na(node))

write.csv(height_monit_sd,
          file = "./data/observations/monitoring/dry_mass_aboveground/height_ids.csv",
          row.names = FALSE)


# Nodes -------------------------------------------------------------------
# # Calibration
# temp <- list()
# 
# for (filename in nodes_calib_all){
#   
#   temp[[filename]] <- read.csv(filename)
#   
# }
# 
# nodes_raw <- Reduce(rbind, temp)
# 
# 
# nodes <- nodes_raw %>%
#   separate(filename, c("angle", "date0", "id0", "part"), "_") %>%
#   mutate(id0 = if_else(!is.na(id0), 
#                        gsub("id", "", id0), as.character(id0)),
#     date0 = as.Date(date0, "%Y%m%d")) %>%
#   select(-part, -angle) %>%
#   rename(id = id0,
#          date = date0)
# 
# write.csv(nodes, 
#           file = "./data/observations/monitoring/nodes/n_calib_ids_ciclo02.csv", 
#           row.names = FALSE)
# 
# # Monitoring
# temp <- list()
# 
# for (filename in nodes_all){
#   
#   temp[[filename]] <- read.csv(filename)
#   
# }
# 
# nodes_temp <- Reduce(rbind, temp)
# 
# nodes_monit <- nodes_temp %>%
#   separate(filename, c("date", "hour", "minute", "part"), "_") %>%
#   select(-hour, -minute, -part) %>%
#   rename(n_lat = count) %>%
#   mutate(node = if_else(camera == "camerapi01", 2, 1))  %>%
#   left_join(cycle_dates)
# 
# # Include the standard deviation from the experiment.
# sd <- nodes %>%
#   group_by(date) %>%
#   summarise(n_sd = round(sd(count),0)) %>%
#   mutate(date = as.character(date)) %>%
#   select(date, n_sd)
# 
# nodes_monit_sd <- nodes_monit %>%
#   full_join(sd) %>%
#   left_join(cycle_dates) %>%
#   group_by(cycle) %>%
#   arrange(date) %>%
#   mutate(dat = if_else(is.na(dat), 999L, dat),
#          n_lat_sd = if_else(dat == min(dat), 
#                               min(n_sd, na.rm = TRUE),
#                         n_sd),
#          n_lat_sd = na_if(n_lat_sd, -99),
#          n_lat_sd = zoo::na.locf(n_lat_sd)) %>%
#   filter(!is.na(n_lat)) %>%
#   select(-n_sd, -camera)
# 
# write.csv(nodes_monit_sd, 
#           file = "./data/observations/monitoring/nodes/n_lat_ids.csv", 
#           row.names=FALSE)
# 

# Mature fruits -----------------------------------------------------------
# Calibration
temp <- list()

for (filename in wmcover_calib_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

wmcover_calib <- Reduce(rbind, temp)

wmcover <- wmcover_calib %>%
  separate(filename, c("angle", "date", "id", "part"), "_") %>%
  mutate(id = if_else(!is.na(id), 
                      gsub("id", "", id), as.character(id)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -angle) %>%
  rename(wm_lat = cover)

write.csv(wmcover, 
          file = "./data/observations/monitoring/dry_mass_mature_fruit/wm_lat_calib_ids.csv", 
          row.names = FALSE)

wmcover_sd <- wmcover %>%
  group_by(date) %>%
  summarise(wm_lat_mean = mean(wm_lat),
            wm_lat_calib_sd = sd(wm_lat)) %>%
  rename(wm_lat = wm_lat_mean,
         wm_lat_sd = wm_lat_calib_sd)

write.csv(wmcover_sd, 
          file = "./data/observations/monitoring/dry_mass_mature_fruit/wm_lat_calib_summ.csv", 
          row.names = FALSE)

# Monitoring

temp <- list()

for (filename in wmcover_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

wmcover_temp <- Reduce(rbind, temp)

wmcover_lat <- wmcover_temp %>%
  separate(filename, c("date", "hour", "min", "part"), "_") %>%
  mutate(node = if_else(camera == "camerapi01", 1, 
                        if_else(camera == "camerapi03", 2, 0)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -camera, -hour, -min) %>%
  rename(wm_lat = lai_)

# Include the standard deviation from the experiment. Filling the first values
# with the minimum value from the calibration dataset.
sd <- wmcover_sd %>%
  select(date, wm_lat_sd)

# While the value will be adopted for all the photos between calibration dates,
# they will not be joined only by date, but by value also, so that
# calibration and monitoring may be kept separate
wmcover_lat_monit_sd <- wmcover_lat %>%
  full_join(sd) %>%
  left_join(cycle_dates) %>%
  group_by(cycle) %>%
  arrange(date) %>%
  mutate(wm_lat_sd = if_else(date == min(date), 
                             min(wm_lat_sd, na.rm = TRUE),
                             wm_lat_sd),
         wm_lat_sd = na_if(wm_lat_sd, -99),
         wm_lat_sd = zoo::na.locf(wm_lat_sd)) %>%
  filter(!is.na(wm_lat), !is.na(node))

write.csv(wmcover_lat_monit_sd, 
          file = "./data/observations/monitoring/dry_mass_mature_fruit/wm_lat_ids.csv", 
          row.names = FALSE)


# Fruits ------------------------------------------------------------------

# Calibration
temp <- list()

for (filename in wfcover_calib_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

wfcover_calib <- Reduce(rbind, temp)

wfcover <- wfcover_calib %>%
  separate(filename, c("angle", "date", "id", "part"), "_") %>%
  mutate(id = if_else(!is.na(id), 
                      gsub("id", "", id), as.character(id)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -angle) %>%
  rename(wf_lat = cover) %>%
  # Should include green and mature fruits
  left_join(wmcover) %>%
  mutate(wf_lat = wf_lat + wm_lat) %>%
  select(-wm_lat)

write.csv(wfcover, 
          file = "./data/observations/monitoring/dry_mass_fruit/wf_lat_calib_ids.csv", 
          row.names = FALSE)

wfcover_sd <- wfcover %>%
  group_by(date) %>%
  summarise(wf_lat_mean = mean(wf_lat),
            wf_lat_calib_sd = sd(wf_lat)) %>%
  rename(wf_lat = wf_lat_mean,
         wf_lat_sd = wf_lat_calib_sd)

write.csv(wfcover_sd, 
          file = "./data/observations/monitoring/dry_mass_fruit/wf_lat_calib_summ.csv", 
          row.names = FALSE)

# Monitoring

temp <- list()

for (filename in wfcover_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

wfcover_temp <- Reduce(rbind, temp)

wfcover_lat <- wfcover_temp %>%
  separate(filename, c("date", "hour", "min", "part"), "_") %>%
  mutate(node = if_else(camera == "camerapi01", 1, 
                        if_else(camera == "camerapi03", 2, 0)),
         date = ymd(date),
         date = as.character(date)) %>%
  select(-part, -camera, -hour, -min) %>%
  rename(wf_lat = lai_) %>%
  left_join(wmcover_lat) %>%
  mutate(wf_lat = wf_lat + wm_lat) %>%
  select(-wm_lat)

# Include the standard deviation from the experiment. Filling the first values
# with the minimum value from the calibration dataset.
sd <- wfcover_sd %>%
  select(date, wf_lat_sd)

# While the value will be adopted for all the photos between calibration dates,
# they will not be joined only by date, but by value also, so that
# calibration and monitoring may be kept separate
wfcover_lat_monit_sd <- wfcover_lat %>%
  full_join(sd) %>%
  left_join(cycle_dates) %>%
  group_by(cycle) %>%
  arrange(date) %>%
  mutate(wf_lat_sd = if_else(date == min(date), 
                             min(wf_lat_sd, na.rm = TRUE),
                             wf_lat_sd),
         wf_lat_sd = na_if(wf_lat_sd, -99),
         wf_lat_sd = zoo::na.locf(wf_lat_sd)) %>%
  filter(!is.na(wf_lat), !is.na(node))

write.csv(wfcover_lat_monit_sd, 
          file = "./data/observations/monitoring/dry_mass_fruit/wf_lat_ids.csv", 
          row.names = FALSE)
