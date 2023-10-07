# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)

# Functions ---------------------------------------------------------------

source("./data_processing/00-additional_functions.R")


testNA <- function(x){
  
  x_log <- is.na(mean(x, na.rm = TRUE)) | is.nan(mean(x, na.rm = TRUE))
  
  return (x_log)
  
}

fillNAs <- function(x, cycles_dates){
  
  # Logic: To guarantee that days in which there have been no record, for 
  # instance because the sensors broke ou the electricity went down, will
  # have an associated measurement, missing values will be replaced: 
  # they may be replaced by values of the other sensor in the same node, 
  # they may be replaced by values of the other sensors for the same variable
  # in the other node, they may be imputed using a time-series technique. 
  # Since the radiation sensors are not measuring data in the same unit, they 
  # must be treated separatelly.
  
  # To avoid missing similar timestamps that are different by minutes, 
  # values are rounded to the minute and grouped by hour and minute
  
  # Function applies only to days of cycles
  
  # NA values should be made explicit so that they are not just "skipped".
  days <- cycles_dates$date
  sensors <- as.character(unique(x$sensor_var))
  sensors_id <- as.character(unique(x$sensor_id))
  dec_min <- c("0", seq(10, 50, 10))
  
  allHours <- expand.grid(node = c(1, 2), date = days, sensor_var = sensors,
                          sensor_id = sensors_id,
                          hour = c(0:23), minute = dec_min, 
                          stringsAsFactors = FALSE) %>%
    filter(!(hour == 0 & minute == "0")) %>%
    filter(!paste(date, hour, node, 
                  sensor_var) %in% paste(x$date, x$hour,
                                         x$node, x$sensor_var)) %>%
    filter(paste(sensor_id, 
                 sensor_var) %in% paste(x$sensor_id, x$sensor_var)) %>%
    left_join(cycles_dates) %>%
    mutate(dateFull = ymd_hms(paste0(date, "T", hour, ":", minute, ":00")),
           measurement = NA,
           sensor_var = as.character(sensor_var),
           sensor_var2 = sensor_var) %>%
    separate(sensor_var2, sep = "_", into = c("sensor", "variable"))
  
  x_mod0 <- x %>%
    mutate(dateFull = ymd_hms(dateFull),
           minute = minute(dateFull)) %>%
    rbind(allHours) %>%
    arrange(node, sensor_var, dateFull) %>%
    mutate(minute = 10*trunc(as.numeric(minute)/10, 0)) %>%
    # Initializing imputation variable for not missing cases
    mutate(imputation = if_else(!is.na(measurement), 
                                # In this first case, include empty for 
                                # those which require imputation and none for 
                                # those which do not.
                                "none", "")) %>%
    mutate(imputation = na_if(imputation, "")) 
  
  # In the first case, an average of sensor values of the same node, for the
  # same variable and sensor type, removing NAs, 
  # will be attempted for replacement.
  x_mod1 <- x_mod0 %>%
    group_by(variable, sensor, node, date, hour, minute) %>%
    mutate(imputation = if_else(is.na(measurement) & variable != "radiation", 
                                if_else(testNA(measurement), 
                                        # If the outcome of the test is NA, there was
                                        # no imputation, otherwise, it's case 1.
                                        "", "case1"), 
                                # If the first test is not the case, keep the 
                                # first attribution
                                imputation),
           measurement = if_else(is.na(measurement) & variable != "radiation", 
                                 if_else(testNA(measurement), -99,
                                         mean(measurement, na.rm = TRUE)),
                                 measurement)) %>%
    ungroup() %>%
    mutate(measurement = na_if(measurement, -99),
           imputation = na_if(imputation, "")) 
  rm(x_mod0)
  
  # In the second case, an average of sensor values of the same node, for the
  # same variable despite the sensor type.
  x_mod2 <- x_mod1 %>%
    group_by(variable, node, date, hour, minute) %>%
    mutate(imputation = if_else(is.na(measurement) & variable != "radiation",
                                if_else(testNA(measurement),
                                        # If the outcome of the test is NA, there was
                                        # no imputation, otherwise, it's case 2.
                                        "", "case2"), 
                                # If the first test is not the case, leave as is, 
                                # as it may be radiation.
                                imputation),
           measurement = if_else(is.na(measurement) & variable != "radiation",
                                 if_else(testNA(measurement),
                                         -99, mean(measurement, na.rm = TRUE)),
                                 measurement)) %>%
    ungroup() %>%
    mutate(measurement = na_if(measurement, -99),
           imputation = na_if(imputation, "")) 
  rm(x_mod1)
  
  # In the third case, values that were not filled will be filled with data 
  # from the other sensors for the same variable in the other node.
  x_mod3 <- x_mod2 %>%
    group_by(variable, date, hour, minute) %>%
    mutate(imputation = if_else(is.na(measurement) & variable != "radiation",
                                if_else(testNA(measurement),
                                        # If the outcome of the test is NA, there was
                                        # no imputation, otherwise, it's case 3.
                                        "", "case3"), 
                                # If the first test is not the case, leave as is, 
                                # as it may be radiation.
                                imputation),
           measurement = if_else(is.na(measurement) & variable != "radiation",
                                 if_else(testNA(measurement),
                                         -99, mean(measurement, na.rm = TRUE)),
                                 measurement)) %>%
    ungroup() %>%
    mutate(measurement = na_if(measurement, -99),
           imputation = na_if(imputation, "")) 
  rm(x_mod2)
  
  x_mod4 <- x_mod3 %>%
    # Radiation case - different units from both sensors, prioritize not mixing.
    # In this case, the opposite is prioritized. It mixes nodes before
    # mixing sensors.
    group_by(variable, sensor, date, hour, minute) %>%
    mutate(imputation = if_else(is.na(measurement) & variable == "radiation",
                                if_else(testNA(measurement),
                                        # If the outcome of the test is NA, there was
                                        # no imputation, otherwise, it's
                                        # the first radiation case.
                                        "", "case3"), 
                                # If the first test is not the case, leave as is, 
                                # as it may still be filled.
                                imputation),
           measurement = if_else(is.na(measurement) & variable == "radiation",
                                 if_else(testNA(measurement),
                                         -99, mean(measurement, na.rm = TRUE)),
                                 measurement)) %>%
    ungroup() %>%
    mutate(measurement = na_if(measurement, -99),
           imputation = na_if(imputation, "")) 
  rm(x_mod3)
  
  x_mod5 <- x_mod4 %>%
    # Give that up if not possible
    group_by(variable, date, hour, minute) %>%
    mutate(imputation = if_else(is.na(measurement) & variable == "radiation",
                                if_else(testNA(measurement),
                                        # If the outcome of the test is NA, there was
                                        # no imputation, otherwise, it's
                                        # the second radiation case.
                                        "", "case2"), 
                                # If the first test is not the case, leave as is, 
                                # as it may still be filled.
                                imputation),
           measurement = if_else(is.na(measurement) & variable == "radiation",
                                 if_else(is.na(mean(measurement, na.rm = TRUE)) |
                                           is.nan(mean(measurement,
                                                       na.rm = TRUE)),
                                         -99,
                                         mean(measurement, na.rm = TRUE)),
                                 measurement)) %>%
    ungroup() %>%
    mutate(measurement = na_if(measurement, -99),
           imputation = na_if(imputation, ""))
  rm(x_mod4)
    
  # Finally, use external daily data and theoretical distribution through the
  # day to fill other values. This imputation only ascribe an hourly value to
  # the whole hour and applies to temperature and radiation
  proc_theor <- fillNAs_external(x_mod5, cycles_dates)
  
  x_mod6 <- x_mod5 %>%
    left_join(proc_theor,
              c("node", "sensor_id", "variable", 
                "sensor", "date", "cycle", "dat", "hour", 
                "sensor_var", "minute")) %>%
    mutate(measurement = if_else(!is.na(imputation.y) & imputation.y == "case4", 
                                 value, measurement),
           imputation.x = if_else(!is.na(imputation.y) & imputation.y == "case4", 
                                  imputation.y, imputation.x)) %>%
    rename(imputation = imputation.x) %>%
    arrange(dateFull, node) %>%
    select(-imputation.y, -value) %>%
    distinct(.)
  

  return(x_mod6)
  
}

fillNAs_external <- function(x, cycles_dates){
  # Retrieve daily data from NASA Power and interpolate using theoretical
  # functions
  pathfiles <- "./data/weather/unc/"
  full_weather_files <- list.files(pathfiles, recursive = FALSE, 
                                   pattern = "*.csv", full.names = TRUE)
  
  full_file <- grep("cps", full_weather_files, value = TRUE)
  
  weather_all <- read.csv(full_file, skip = 17)
  
  ## Data
  # T2MDEW MERRA2 1/2x1/2 Dew/Frost Point at 2 Meters (C) 
  # T2M_MAX MERRA2 1/2x1/2 Maximum Temperature at 2 Meters (C) 
  # WS2M MERRA2 1/2x1/2 Wind Speed at 2 Meters (m/s) 
  # ALLSKY_SFC_SW_DWN SRB/FLASHFlux 1/2x1/2 All Sky Insolation Incident on 
  # a Horizontal Surface (MJ/m^2/day) 
  # T2M_MIN MERRA2 1/2x1/2 Minimum Temperature at 2 Meters (C) 
  # T2M MERRA2 1/2x1/2 Temperature at 2 Meters (C) 
  # PRECTOT MERRA2 1/2x1/2 Precipitation (mm day-1) 
  # RH2M MERRA2 1/2x1/2 Relative Humidity at 2 Meters (%) 
  
  # Interpolate weather -----------------------------------------------------
  
  df_daily <- weather_all %>%
    rename_all(tolower) %>%
    mutate(date = as.Date(paste(year, doy, sep = "-"), tryFormats = c("%Y-%j"))) %>%
    filter(date %in% as.Date(cycles_dates$date)) %>%
    select(date, lat, lon, year, doy, t2m_max, t2m_min, starts_with("allsky")) %>%
    rename(tmax = t2m_max,
           tmin = t2m_min,
           rad = allsky_sfc_sw_dwn) %>%
    mutate(date = as.character(date)) %>%
    left_join(cycles_dates) %>%
    # Imputation based on previous and following day
    mutate(rad = if_else(rad == -99, 
                         (lead(rad) + lag(rad)) * 0.5, 
                         rad),
           tmax = if_else(tmax == -99, 
                         (lead(tmax) + lag(tmax)) * 0.5, 
                         tmax),
           tmin = if_else(rad == -99, 
                         (lead(tmin) + lag(tmin)) * 0.5, 
                         tmin))
  
  all_hours <- data.frame(hour = rep(seq(1, 24), 366),
                          doy = rep(c(1:366), each = 24))
  
  df_proc <- df_daily %>%
    left_join(all_hours)
  
  df_proc$rad <- mapply(FUN = hourlyRad,
                        srad = df_proc$rad, doy = df_proc$doy, 
                        lat = df_proc$lat[1], hour = df_proc$hour)
  df_proc$rad <- round(df_proc$rad, 4)
  df_proc$tmean <- mapply(FUN = hourlyTemp, 
                          doy = df_proc$doy, 
                          lat = df_proc$lat[1], hs = df_proc$hour, 
                          tmax = df_proc$tmax, tmin = df_proc$tmin) 
  
  df_proc_mod <- df_proc %>%
    mutate(hour = if_else(hour == 24, 0L, hour)) %>%
    #mutate(hour = solar_local_hour(hour, lat, lon, doy)) %>%
    select(-lat, -lon, -tmax, -tmin)
  
  x_fill <- x %>%
    filter(is.na(measurement) & (variable == "temperature" | 
                                   variable == "radiation")) %>%
    left_join(df_proc_mod) %>%
    # Replace only is measurement is missing
    mutate(value = measurement,
           value = if_else(variable == "temperature", 
                           tmean, 
                           # As all imputation is made in MJg and should
                           # return mmolPAR, conversion is the same for
                           # whichever sensor
                           if_else(variable == "radiation", rad*555.6, -99.)),
           value = na_if(value, -99.),
           imputation = "case4") %>%
    select(-rad, -tmean, -year, -doy, -measurement, -dateFull)
  
    return(x_fill)
  
}

agg_daily <- function(x){
  #  No need for imputation of dates as it
  # has been previously done.
  
  x_daily <- x %>%
    arrange(node, date) %>%  
    group_by(cycle, date, node, sensor_var) %>%
    summarise(meas_mean = mean(measurement, na.rm = TRUE),
              meas_min = min(measurement, na.rm = TRUE),
              meas_max = max(measurement, na.rm = TRUE)) %>%
    arrange(node, date) %>%
    ungroup() %>%
    mutate(meas_max = if_else(!is.numeric(meas_max) | abs(meas_max) == Inf, 
                              -99, meas_max),
           meas_max = na_if(meas_max, -99),
           meas_min = if_else(!is.numeric(meas_min) | abs(meas_min) == Inf, 
                              -99, meas_min),
           meas_min = na_if(meas_min, -99),
           meas_mean = if_else(!is.numeric(meas_mean) | abs(meas_mean) == Inf, 
                               -99, meas_mean),
           meas_mean = na_if(meas_mean, -99))
  
  return(x_daily)
  
}

agg_hourly <- function(x){
  # Filtered data is summarized hourly. No need for imputation of dates as it
  # has been previously done.
 
  x_hourly <- x %>%
    distinct(.) %>%
    arrange(node, date, hour) %>%  
    mutate(imputation = if_else(is.na(imputation),"none", imputation),
           flag = if_else(imputation == "none", 0, 1)) %>%
    spread(imputation, flag) %>%
    mutate_at(vars(contains("case")), 
              .funs = ~if_else(is.na(.), 0, .)) %>%
    group_by(cycle, date, dat, hour, node, sensor_var) %>%
    summarise(meas_mean = mean(measurement, na.rm = TRUE),
              meas_min = min(measurement, na.rm = TRUE),
              meas_max = max(measurement, na.rm = TRUE),
              meas_med = median(measurement, na.rm = TRUE),
              flag_case1 = mean(case1),
              flag_case2 = mean(case2),
              flag_case3 = mean(case3),
              flag_case4 = mean(case4)) %>%
    arrange(node, date, hour) %>%
    ungroup() %>%
    mutate(meas_max = if_else(!is.numeric(meas_max) | abs(meas_max) == Inf, 
                              -99, meas_max),
           meas_max = na_if(meas_max, -99),
           meas_min = if_else(!is.numeric(meas_min) | abs(meas_min) == Inf, 
                              -99, meas_min),
           meas_min = na_if(meas_min, -99),
           meas_mean = if_else(!is.numeric(meas_mean) | abs(meas_mean) == Inf, 
                              -99, meas_mean),
           meas_mean = na_if(meas_mean, -99),
           meas_med = if_else(!is.numeric(meas_med) | abs(meas_med) == Inf, 
                              -99, meas_med),
           meas_med = na_if(meas_med, -99)) %>%
    gather("meas_max", "meas_min", "meas_mean", "meas_med",
           key = "stat", value = "value") %>%
    mutate(stat = str_sub(stat, start = 6),
           stat_sensor_var = paste(stat, sensor_var, sep = "_")) %>%
    select(-stat, -sensor_var)
  
  return(x_hourly)
  
}

agg_hourly_mean <- function(x){
  # Filtered data is summarized hourly
  # Values from both nodes are averaged
  
  # Check timezones
  x_mod <- x %>%
    mutate(hour = hour(dateFull))
  
  # NA values should be made explicit so that they are not just "skipped".
  days <- seq(min(x$date), max(x$date), 1)
  sensors <- unique(x$sensor_var)
  
  allHours <- expand.grid(node = c(1, 2), date = days, sensor = sensors,
                          hour = c(0:23)) %>%
    filter(!paste(date, hour, node, 
                  sensor) %in% paste(x_mod$date, x_mod$hour,
                                     x_mod$node, x_mod$sensor_var))
  
  x_hourly <- x_mod %>%
    left_join(allHours) %>%
    select(-node) %>%
    arrange(date, hour) %>%  
    group_by(cycle, date, dat, hour, sensor_var) %>%
    summarise(meas_mean = mean(measurement, na.rm = TRUE),
              meas_min = min(measurement, na.rm = TRUE),
              meas_max = max(measurement, na.rm = TRUE),
              meas_med = median(measurement, na.rm = TRUE)) %>%
    arrange(date, hour) %>%
    ungroup() %>%
    mutate(meas_max = if_else(!is.numeric(meas_max) | abs(meas_max) == Inf, 
                              -99, meas_max),
           meas_max = na_if(meas_max, -99),
           meas_min = if_else(!is.numeric(meas_min) | abs(meas_min) == Inf, 
                              -99, meas_min),
           meas_min = na_if(meas_min, -99),
           meas_mean = if_else(!is.numeric(meas_mean) | abs(meas_mean) == Inf, 
                               -99, meas_mean),
           meas_mean = na_if(meas_mean, -99),
           meas_med = if_else(!is.numeric(meas_med) | abs(meas_med) == Inf, 
                              -99, meas_med),
           meas_med = na_if(meas_med, -99))
  
  return(x_hourly)
  
}
