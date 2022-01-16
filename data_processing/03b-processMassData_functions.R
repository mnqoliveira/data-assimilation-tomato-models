# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)

# Source functions --------------------------------------------------------

filterOutliers <- function(x){
  
  x_mod <- x %>%
    group_by(cycle, node) %>%
    arrange(node, dateFull) %>%
    mutate(dif1 = lag(wm) - wm,
           dif2 = lead(wm) - wm,
           out1 = if_else((abs(dif1) > 1000) & !is.na(dif1), TRUE, FALSE),
           out2 = if_else((abs(dif2) > 1000) & !is.na(dif2), TRUE, FALSE),
           out = out1 & out2,
           wm = if_else(out, mean(c(lag(wm), lead(wm)), na.rm = T), wm)) %>%
    select(-starts_with("dif"), -starts_with("out")) %>%
    ungroup()
  
  return(x_mod)
  
}

fixInitialValue <- function(x, sensor_dates){
  
  zero_dates <- sensor_dates %>%
    filter(str_detect(event, ("ref_zero"))) %>%
    mutate(date = as.Date(date)) %>%
    select(-node, -sensor_id, -type, -comment)
  
  # Each cycle has a particularity regarding initial value that must
  # be addressed.
  
  # Cycle 0
  # Growth had already begun when the load cells were installed. Given it
  # was not possible to zero the system without the substrate, plant mass
  # is estimated to be removed from the system
  zero1 <- x %>%
    left_join(zero_dates) %>%
    filter(!is.na(event), cycle == 1) %>%
    group_by(cycle, node) %>%
    summarise(minMass = min(wm, na.rm = TRUE) - 120)
  
  # Cycle 1 and cycle 2
  # Even though there are three days that referred to a different zero, 
  # in cycle 2, trying to fix could lead to larger errors than 
  # leaving them as is. Cycle 3 will refer to the minimum value recorded.
  zero2 <- x %>%
    left_join(zero_dates) %>%
    filter(!is.na(event), cycle == 2 | cycle == 3) %>%
    group_by(cycle, node) %>%
    summarise(minMass = min(wm, na.rm = TRUE))
  
  # Third cycle starts without accounting for field capacity in 
  # the first days
  zero3 <- x %>%
    left_join(zero_dates) %>%
    filter(!is.na(event), cycle == 4) %>%
    group_by(cycle, node) %>%
    summarise(minMass = min(wm, na.rm = TRUE))
  
  minMass <- rbind(zero1, zero2, zero3)
  
  x_mod <- x %>%
    group_by(cycle, node) %>%
    left_join(minMass) %>%
    mutate(w_fm_full = wm - minMass) %>%
    ungroup() %>%
    select(-minMass, -wm, -wm_mod)

  return(x_mod)
}

calcMeanSD <- function(x){
  # Includes mean and SD for WM values
  # Also fill values of non-measured hours with NA

  x_mod <- x %>%
    group_by(cycle, date, hour, dat, node, stage) %>%
    summarise(w_fm_full = mean(w_fm_full, na.rm = TRUE),
              w_fm_full_sd = sd(w_fm_full, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(!is.na(date))
  
  return(x_mod)
  
}