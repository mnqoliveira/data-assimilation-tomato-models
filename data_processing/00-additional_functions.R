# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(stringr)

library(zoo)


# Timezone and Daylight saving time problems ------------------------------

fixHours <- function(x, sensor_dates, type_s){
  
  # types = mass, radio, wc, pi, all
  x_mod <- x %>%
    # This will avoid Daylight saving times as each equipment has
    # to be manually modified for a lot of different cases: 
    # DST ended but equipment didn't change or DST didn't start but equipment
    # changed, etc.
    mutate(dateFull = force_tz(dateFull, tzone = "UTC"),
           date = as.character(date(dateFull)))
  
  # Both nodes, timestamp is ahead
  dates1 <- sensor_dates %>%
    filter(type == type_s,
           str_detect(event, "lead"), node == "both") 
  
  if (nrow(dates1) >= 1){
    
    dates1 <- dates1 %>%
      select(-cycle, -node, -sensor_id, -type) %>%
      spread(event, date) %>%
      mutate(date = lead_start) %>%
      select(-comment)
    
    x_mod <- x_mod %>%
      arrange(dateFull) %>%
      left_join(dates1) %>%
      rownames_to_column() %>%
      mutate(lead_end = if_else(is.na(lead_end) & rowname == 1, 
                                "1970-01-01", lead_end),
             lead_end = zoo::na.locf(lead_end),
             lead_start = if_else(is.na(lead_start) & rowname == 1, 
                                "1970-01-01", lead_start),
             lead_start = zoo::na.locf(lead_start),
             dateFull = if_else((as.Date(date) >= as.Date(lead_start)) &
                                (as.Date(date) <= as.Date(lead_end)),
                                dateFull - hours(1), dateFull))  %>%
      select(-lead_end, -lead_start, -rowname)
  }
  
  # Both nodes, timestamp is behind
  dates2 <- sensor_dates %>%
    filter(type == type_s,
           str_detect(event, "lag"), node == "both") 
  
  if (nrow(dates2) >= 1){
    
    dates2 <- dates2 %>%
      select(-cycle, -node, -sensor_id, -comment, -type) %>%
      spread(event, date) %>%
      mutate(date = lead_start)
    
    x_mod <- x_mod %>%
      arrange(dateFull) %>%
      left_join(dates2) %>%
      rownames_to_column() %>%
      mutate(lag_end = if_else(is.na(lag_end) & rowname == 1, 
                               "1970-01-01", lag_end),
             lag_end = zoo::na.locf(lag_end),
             lag_start = if_else(is.na(lag_start) & rowname == 1, 
                                  "1970-01-01", lag_start),
             lag_start = zoo::na.locf(lag_start),
             dateFull = if_else((as.Date(date) >= as.Date(lag_start)) &
                                (as.Date(date) <= as.Date(lag_end)),
                                dateFull + hours(1), dateFull))  %>%
      select(-lag_end, -rowname, -lag_start)
    
  }
 
  # Each node, timestamp is ahead
  dates3 <- sensor_dates %>%
    filter(type == type_s,
           str_detect(event, "lead"), node == "1" | node == "2") 
  
  if (nrow(dates3) >= 1){
    
    dates3 <- dates3 %>%
      select(-cycle, -sensor_id, -comment, -type) %>%
      spread(event, date) %>%
      mutate(date = lead_start)
    
    x_mod <- x_mod %>%
      arrange(node, dateFull) %>%
      left_join(dates3) %>%
      group_by(node) %>%
      mutate(lead_end = if_else(as.Date(date) == min(as.Date(date)), 
                                "1970-01-01", lead_end),
             lead_end = zoo::na.locf(lead_end),
             lead_start = if_else(as.Date(date) == min(as.Date(date)), 
                                  "1970-01-01", lead_start),
             lead_start = zoo::na.locf(lead_start),
             dateFull = if_else((as.Date(date) >= as.Date(lead_start)) &
                                (as.Date(date) <= as.Date(lead_end)),
                                dateFull - hours(1), dateFull))  %>%
      ungroup() %>%
      select(-lead_end, -lead_start)
  }
  
  # Each node, timestamp is behind
  dates4 <- sensor_dates %>%
    filter(type == type_s,
           str_detect(event, "lag"), node == "1" | node == "2") 
  
  if (nrow(dates4) >= 1){
    
    dates4 <- dates4 %>%
      select(-cycle, -sensor_id, -comment, -type) %>%
      spread(event, date) %>%
      mutate(date = lag_start)
    
    x_mod <- x_mod %>%
      arrange(node, dateFull) %>%
      left_join(dates4) %>%
      group_by(node) %>%
      mutate(lag_end = if_else(as.Date(date) == min(as.Date(date)), 
                               "1970-01-01", lag_end),
             lag_end = zoo::na.locf(lag_end),
             lag_start = if_else(as.Date(date) == min(as.Date(date)), 
                                 "1970-01-01", lag_start),
             lag_start = zoo::na.locf(lag_start),
             dateFull = if_else((as.Date(date) >= as.Date(lag_start)) &
                                (as.Date(date) <= as.Date(lag_end)),
                                dateFull + hours(1), dateFull))  %>%
      ungroup() %>%
      select(-lag_end, -lag_start)
    
  }
  
  x_mod <- x_mod %>%
    # Variable created before changing hours
    select(-date)
  
  return(x_mod)
  
}


# Daily to hourly radiation and temperature -------------------------------

degToRad <- function(deg){
  
  rad <- deg*pi/180
  
  return(rad)
  
}

# doy <- 348
# srad <- 21.72
# lat <- -22.70833
# long <- -47.63333
# 
# doy <- 1
# srad <- 17.85
# lat <- -22.70833
# long <- -47.63333
# 
# doy <- 44
# srad <- 13.5
# lat <- 27.6


hourlyRad <- function(srad, doy, lat, hour, unit){
  # Same method as in DSSAT
  
  # micromol photon / J - McCree
  parqc <- 4.6
  
  # Relation Global and PAR:
  # Or Lizaso 2003: 
  # PARFAC = (0.43 + 0.12*EXP(-SRAD/2.8)) * PARQC
  
  # PAR = 50% Global
  parfac <- 0.45
  
  if(unit != "MJg"){
    # Implicit that it is micrommolPAR m-2 s-1
    
    par <- srad*3600/10^6
    srad <- par/(parfac*parqc)
    
  }
  
  # Solar declination
  dec <- degToRad(-23.45 * cos(2.0*pi*(doy+10.0)/365.0))
  
  # Sun angles.  soc limited for latitudes above polar circles.
  ssin <- sin(dec) * sin(degToRad(lat))
  ccos <- cos(dec) * cos(degToRad(lat))
  soc <- ssin / ccos
  soc <- min(max(soc,-1.0), 1.0)
  
  
  # Integral in Eqn. 6 (used in HMET)
  dayl <- 12.0 + 24.0*asin(soc)/pi
  isinb <- (3600.0 * (dayl*(ssin + 0.4*(ssin^2 + 
                                          0.5*ccos^2)) +
                        (24.0/pi)*ccos*(1.0 + 1.5*0.4*ssin)*
                        sqrt(1.0-soc^2)))
  
  # Hour angle
  hangl <- (hour-12.0)*pi/12.0
  
  # Sin of solar elevation
  sinb <- pmin(pmax(ssin + ccos*cos(hangl), -1.0), 1.0)
  beta <- asin(sinb)/(pi/180)
  
  #   Compute instantaneous srad values with Spitters Eqn. 6.
  # RADHR (J/m2 s)
  radhr <- sinb * (1.0+0.4*sinb) * srad*1.0E6 / isinb
  # RADHR (MJ/m2 h)
  radhr <- pmax(radhr, 0.)*3600/(10^6)
  
  return(radhr)
  
}

# tmax <- 32.2
# tmin <- 19.8

hourlyTemp <- function(doy, lat, hs, tmax, tmin){
  
  A <- 2.0
  B <- 2.2
  C <- 1.0
  
  # Calculation of declination of sun (Eqn. 16). Amplitude= +/-23.45
  # deg. Minimum = doy 355 (dec 21), maximum = doy 172.5 (JUN 21/22).
  dec <- -23.45 * cos(2.0*pi*(doy+10.0)/365.0)
  
  # Sun angles.  soc limited for latitudes above polar circles.
  soc <- tan(degToRad(dec)) * tan(degToRad(lat))
  soc <- pmin(pmax(soc,-1.0), 1.0)
  
  # Calculate daylength, sunrise and sunset (Eqn. 17)
  dayl <- 12.0 + 24.0*asin(soc)/pi
  snup <- 12.0 - dayl/2.0
  sndn <- 12.0 + dayl/2.0
  
  # Calculate day length, time of maximum temperature,time of
  # minimum temperature, hours of exponential decay, temperature
  # at sunset, and temperature at infinite time.
  
  min1 <- snup + C
  max1 <- min1 + dayl/2.0 + A
  temp <- 0.5 * pi * (sndn-min1) / (max1-min1)
  tsndn <- tmin + (tmax-tmin)*sin(temp)
  tmini <- (tmin-tsndn*exp(-B)) / (1.0-exp(-B))
  hdecay <- 24.0 + C - dayl
  
  # Daylight hours--sine curve.  Minimum temperature at (snup+C) and
  # maximum temperature at (noon+A+C) hours.
  
  if ((hs >= snup+C) && (hs <= sndn)) {
    
    temp <- 0.5 * pi * (hs-min1) / (max1-min1)
    tairhr <- tmin + (tmax-tmin)*sin(temp)
    
    # Nighttime--exponential decrease from sunset temperature (tsndn)
    # to minimum (tmini) at infinite time and tmin at (snup+C).
    
  } else { 
    
    if (hs < (snup+C)) {
      
      temp <- 24.0 + hs - sndn
      
    } 
    
    if (hs > sndn) {
      
      temp <- hs - sndn
      
    }
    
    arg <- -B * temp / hdecay
    tairhr <- tmini + (tsndn-tmini)*exp(arg)
    
  }
  
  return(tairhr)
  
}



