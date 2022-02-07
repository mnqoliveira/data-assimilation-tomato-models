# Generates weather time-series from historical weather for sensitivity
# analysis

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("tidyverse")
library("lubridate")

# Functions ---------------------------------------------------------------

source("./data_processing/00-additional_functions.R")

# Paths -------------------------------------------------------------------

pathfiles <- "./data/weather/unc/"
full_weather_files <- list.files(pathfiles, recursive = FALSE, 
                                 pattern = "*.csv", full.names = TRUE)

pathout <- "./data/weather/unc/all/"


# Load files --------------------------------------------------------------

unc_scen <- read.csv("./tables/unc_weather_scen.csv")
years <- c(1993:1997, 2000:2009 , 2015:2019)
cities <- expand.grid(city = c("cps", "gnv"), year = years,
                      stringsAsFactors = FALSE)

unc_scen_all <- expand.grid(pdoy = unc_scen$pdate, len = unc_scen$len, 
                            year = years) %>%
  filter(complete.cases(.)) %>%
  mutate(cod = row_number(),
         cod = str_pad(cod, width = 3, side = "left", pad = "0"),
         pdoy = str_pad(pdoy, width = 3, side = "left", pad = "0")) %>%
  full_join(cities) %>%
  arrange(city, cod) %>%
  mutate(pdate = as.Date(paste(year, pdoy, sep = "-"), tryFormats = c("%Y-%j")),
         city_cod = if_else(city == "cps", 1, 2))

write.csv(unc_scen_all, file = "./tables/path_cod_unc.csv")

# Process data ------------------------------------------------------------

it <- 1
for (it in 1:nrow(unc_scen_all)){
  
  test <- unc_scen_all$city[it] != unc_scen_all$city[it - 1]
  if (length(test) == 0){
    
    test <- FALSE
    
  }
  
  if (it == 1 | test){
    
    weather_all <- read.csv(full_weather_files[unc_scen_all$city_cod[it]], 
                            skip = 17)
    
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
    
    last30 <- weather_all %>%
      rename_all(tolower) %>%
      filter(year >= 1990, year <= 2020) %>%
      select(lat, year, doy, t2m_max, t2m_min, starts_with("allsky")) %>%
      rename(tmax = t2m_max,
             tmin = t2m_min,
             rad = allsky_sfc_sw_dwn)
    
    # Interpolate weather -----------------------------------------------------
    
    df_daily <- last30 %>%
      mutate(radiation_unit = "MJg",
             date = as.Date(paste(year, doy, sep = "-"), tryFormats = c("%Y-%j")),
             city = unc_scen_all$city[it],
             co2 = 350,
             rad = na_if(rad, -99),
             tmax = na_if(tmax, -99),
             tmin = na_if(tmin, -99),
             rad = zoo::na.approx(rad),
             tmax = zoo::na.approx(tmax),
             tmin = zoo::na.approx(tmin))
    
    all_hours <- data.frame(hour = rep(seq(1, 24), 366),
                            doy = rep(c(1:366), each = 24))
    
    df_proc <- df_daily %>%
      left_join(all_hours)
    
    df_proc$rad <- mapply(FUN = hourlyRad,
                          srad = df_proc$rad, doy = df_proc$doy, 
                          lat = df_proc$lat[1], hour = df_proc$hour,
                          unit = "MJg")
    df_proc$rad <- round(df_proc$rad, 4)
    df_proc$tmean <- mapply(FUN = hourlyTemp, 
                            doy = df_proc$doy, 
                            lat = df_proc$lat[1], hs = df_proc$hour, 
                            tmax = df_proc$tmax, tmin = df_proc$tmin) 
    df_proc <- df_proc %>%
      mutate(hour = if_else(hour == 24, 0L, hour),
             rad = 0.7*rad)
    
  }
    
  # Separate into different planting dates and lengths ----------------------

  df_filt <- df_proc %>%
    filter(date >= unc_scen_all$pdate[it], 
           date <= (unc_scen_all$pdate[it] + unc_scen_all$len[it] - 1)) %>%
    select(-lat, -tmax, -tmin) %>%
    arrange(date) %>%
    group_by(date) %>%
    mutate(dat = cur_group_id()) %>%
    ungroup()
  
  # write.csv
  filename <- paste0(pathout, "hourly_", unc_scen_all$city[it], "_",
                     unc_scen_all$cod[it], ".csv")
  
  write.csv(df_filt, file = filename, row.names = FALSE)
  
  
  df_filt <- df_daily %>%
    filter(date >= unc_scen_all$pdate[it], 
           date <= (unc_scen_all$pdate[it] + unc_scen_all$len[it] - 1)) %>%
    select(-lat) %>%
    arrange(date) %>%
    group_by(date) %>%
    mutate(dat = cur_group_id()) %>%
    ungroup()
  
  # write.csv
  filename <- paste0(pathout, "daily_", unc_scen_all$city[it], "_",
                     unc_scen_all$cod[it], ".csv")
  
  write.csv(df_filt, file = filename, row.names = FALSE)
  
}
