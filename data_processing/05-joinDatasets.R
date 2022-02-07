# Joins all plant growth data in one final dataset

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library("tidyverse")
library("lubridate")
library("zoo")

# Experiments -------------------------------------------------------------
experiments <- read.csv("./data/observations/monitoring/obs_exp_all.csv")

# Monitoring --------------------------------------------------------------
monitoring <- read.csv("./data/observations/monitoring/monitoring_cps.csv") %>%
  mutate(node = as.character(node))

# Join --------------------------------------------------------------------
full_set <- experiments %>%
  full_join(monitoring) %>%
  separate(city_exp, sep = "_", into = c("city", "exp"), remove = FALSE) %>%
  arrange(city_exp, cycle, date, node) %>%
  mutate(w_fm_full_sd = w_fm_sd) %>%
  mutate(w_fm_full_sd = if_else(!is.na(cycle) & (cycle >=2),
                                w_fm_full_sd, -99.)) %>%
  mutate(w_fm_full_sd = if_else(((dat == 0) | (dat == 1)) & (cycle >= 2),
                                0.1, w_fm_full_sd)) %>%
  group_by(exp) %>%        
  mutate(w_fm_full_sd = if_else(cycle >= 2, 
                                na.locf(w_fm_full_sd), 
                                w_fm_full_sd),
         w_fm_full_sd = na_if(w_fm_full_sd, -99.))

temp <- filter(full_set, city == "cps")
  
write.csv(full_set, "./data/observations/full_set_obs.csv", row.names=FALSE)

