# Processes wet mass data to included information regarding the plants

# Set configurations ------------------------------------------------------
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(zoo)


# Initial processing ------------------------------------------------------

initialPrep <- function(x, plant_dates, sensor_dates){
  # Before we settled on the methods, there were additional steps 
  # that refer to zeroing the scales everyday 
  # at 10 pm, including filtering and fixing dates. So the dataset
  # is divided to be processed and then put together again.
  # Another difference is that in this period, the zero didn't account
  # for the support, so it had to be summed up to the initial value.
  
  x_mod <- x %>%
    mutate(date = date(dateFull),
           dateFull = ymd_hms(dateFull))
  
  zero_dates <- sensor_dates %>%
    filter(str_detect(event, ("zero"))) %>%
    select(event, date) %>%
    mutate(date = ymd(date))
  
  zero_start <- zero_dates %>%
    filter(event == "zeroing_start") %>%
    mutate(dateFull = paste0(date, "T22:00:00")) %>%
    pull(dateFull)
  
  zero_end <- zero_dates %>%
    filter(event == "zeroing_end") %>%
    mutate(dateFull = paste0(date, "T13:00:00")) %>%
    pull(dateFull)
  
  x1 <- x_mod %>%
    filter(dateFull < ymd_hms(zero_end)) %>%
    modifySupportWeight(zero_start)
  
  zeros <- assignZeros0(x1, 21)
  
  x1_mod <- x1 %>%
    calcMod0(zeros)
  
  x2 <- x_mod %>%
    filter(dateFull >= ymd_hms(zero_end))
  
  x_mod <- rbind(x1_mod, x2) %>%
    arrange(dateFull)
  
  return(x_mod)
  
}

assignZeros0 <- function(x, zero_time){
  # Determine the weight value associated to zeros in each day.
  
  # Since a fixed zero value is required and during this period
  # the filter applied was not enough to account for the noise, averaging is
  # already required at this point.
  
  zeros <- x %>%
    mutate(dateFull = ymd_hms(dateFull),
           hour = hour(dateFull)) %>%
    dplyr::filter(hour == zero_time) %>%
    arrange(node, date) %>%
    group_by(node, date) %>%
    summarise(wm = mean(wm, na.rm = TRUE)) %>%
    mutate(zero = cumsum(wm),
           zero_lag = lag(zero),
           zero_lag = if_else(is.na(zero_lag), 0, zero_lag)) %>%
    ungroup() %>%
    select(-wm)
  
  return(zeros)
  
}

calcMod0 <- function(x, zeros){
  
  # At 10 pm, the scale zeroed the value that was being read. So to have the 
  # total value of the weight, each value should be referred to its proper zero.
  # When the measurement happened immeadiately after 10 pm, it referred to the
  # zero stored in the same day, otherwise, it would refer to the zero of the 
  # previous day (at 10 pm on the previous day, what was the total weight value?)
  # An if was included because the first day of measurement included 
  # the whole system.
  
  x_mod <- x %>%
    left_join(zeros) %>%
    arrange(node, dateFull) %>%
    mutate(zero = zoo::na.locf(zero),
           zero_lag = zoo::na.locf(zero_lag),
           hour = hour(dateFull),
           zero_mod = if_else(date == min(date),
                              if_else(hour < 22, wm, zero),
                              if_else(hour < 22, zero_lag, zero))) %>%
    mutate(wm = if_else(date == min(date) & hour < 22,
                        wm, wm + zero_mod)) %>%
    select(-contains("mod"), -contains("zero"), -hour) %>%
    arrange(node, dateFull)
  
  return(x_mod)
  
}

modifySupportWeight <- function(x, zero_start){
  
  x_mod <- x %>%
    mutate(wm = if_else((node == 1) & (dateFull <= ymd_hms(zero_start)), 
                        wm + (712.15), 
                        if_else((node == 2) & (dateFull <= ymd_hms(zero_start)),
                                wm + (724.56), wm)))
  
  return(x_mod)
  
}

# Overall processing ------------------------------------------------------


includeNAs <- function(x){
  # Include NAs for when the logger was off
  
  x$date <- as.Date(x$date)
  days <- seq(min(x$date), max(x$date), 1)
  
  allDays <- expand.grid(node = c(1, 2), date = days, hour = c(0:23)) %>%
    filter(!paste(date, hour, node) %in% paste(x$date, x$hour, x$node))
  
  x_mod <- x %>%
    mutate(hour = hour(ymd_hms(dateFull))) %>%
    full_join(allDays) %>%
    arrange(node, date, hour)
  
  return(x_mod)
  
}

includeCycle <- function(x, cycle_dates){
  # Cycle is either going to be identified by transplanting date or by the 
  # lower date available in the dataset
  
  x_mod <- x %>%
    left_join(cycle_dates)

  return(x_mod)
  
}

idPlants <- function(x, plant_dates){
  # In order to include plant id, the node number is attributed to the plant
  # and exceptions are recorded as they are recorded in the plant_dates file
  
  change_dates <- plant_dates %>%
    filter(str_detect(event, "change_plant")) %>%
    mutate(date = ymd(date),
           node = as.numeric(node)) %>%
    group_by(node) %>%
    arrange(cycle, node, date) %>%
    mutate(id = row_number(),
           id = if_else(node %% 2 == 1,
                        2*(id) + 1,
                        2*(id) + 2)) %>%
    select(cycle, node, date, id)
  
  x_mod <- x %>%
    mutate(plant = if_else(node == 1, 1, 2)) %>%
    filter(!is.na(node)) %>%
    left_join(change_dates, by = c("node", "date", "cycle")) %>%
    arrange(cycle, node, date, hour) %>%
    group_by(cycle) %>%
    mutate(id = if_else(is.na(id), 
                        if_else(date == min(date), plant, -99),
                        id), 
           id = na_if(id, -99),
           plant = zoo::na.locf(id)) %>%
    select(-id) %>%
    ungroup()
  
  return(x_mod)
  
}

includeStages <- function(x, plant_dates){
  # Code do not account for different stages in the plants on different
  # days
  
  stage_dates <- plant_dates %>%
    filter(event == "stage") %>%
    select(-node) %>%
    mutate(date = as.Date(date),
           dateFull = ymd_hms(paste0(date, "T", "00:00:01")))
  
  x_mod <- x %>%
    mutate(dateFull = if_else(!is.na(dateFull), dateFull,
                              ymd_hms(paste0(date, "T", 
                                             str_pad(hour, 2, pad = "0"), 
                                             "00:01")))) %>%
    full_join(stage_dates) %>%
    arrange(dateFull) %>%
    mutate(comment = zoo::na.locf(comment)) %>%
    rename(stage = comment) %>%
    select(-event)
  
  return(x_mod)
  
}

includeDAP <- function(x, plant_dates){
  
  start_dates <- plant_dates %>%
    filter(event == "planting" | event == "transplanting") %>%
    mutate(date = ymd(date)) %>%
    select(date, event, cycle)
  
  x_mod <- x %>%
    full_join(start_dates, by = c("date", "cycle")) %>%
    # Since the values in the column date_planting and date_transp are being 
    # moved forward, it is important that the dataset is organized accounting 
    # for date and hour
    arrange(date, hour) %>%
    mutate(date = as.character(date)) %>%
    mutate(date_planting = if_else(event == "planting", date, "-99"),
           date_planting = na_if(date_planting, "-99"),
           date_planting = zoo::na.locf(date_planting),
           date_planting = ymd(date_planting)) %>%
    mutate(date_transp = if_else(event == "transplanting", date, 
                                 if_else(ymd(date) == min(ymd(date)), "0", "-99")),
           date_transp = na_if(date_transp, "-99"),
           date_transp = zoo::na.locf(date_transp),
           date_transp = ymd(date_transp)) %>%
    select(-event) %>%
    mutate(date = ymd(date),
           dap = time_length(as.period(date - date_planting, 
                                       unit = "days"), unit = "days"),
           dat = time_length(as.period(date - date_transp, 
                                       unit = "days"), unit = "days")) %>%
    select(-date_planting, -date_transp)
  
  return(x_mod)
  
}

removeRecords <- function(x, plant_dates, sensor_dates){
  # Should remove records in which system mass was perturbed

  # mass_removal - criar var de flag para remover essas do conjunto de dados
  # mass_inclusion - dias de posicionamento das cameras devem ser removidos tb
  
  both <- plant_dates %>%
    filter(node == "both", event == "mass_removal") %>%
    mutate(date = ymd(date))
  
  mass_removal <- plant_dates %>%
    filter(event == "mass_removal") %>%
    mutate(date = ymd(date), node = if_else(node == "both", "1", node)) %>%
    rbind(both) %>%
    mutate(node = if_else(node == "both", "2", node), 
           node = as.numeric(node)) %>%
    select(date, node, event)
  
  mass_inclusion <- sensor_dates %>%
    filter(event == "mass_inclusion", comment == "camera") %>%
    mutate(date = ymd(date), node = as.numeric(node)) %>%
    select(date, node, event)
  
  x_mod <- x %>%
    left_join(mass_removal, by = c("date", "node")) %>%
    left_join(mass_inclusion, by = c("date", "node")) %>%
    mutate(event.x = if_else(is.na(event.x), "-99", event.x), 
           event.y = if_else(is.na(event.y), "-99", event.y)) %>%
    mutate(meanWM = if_else(event.x != "mass_removal" & 
                              event.y != "mass_inclusion",
                            meanWM, -99),
           meanWM = na_if(meanWM, -99)) %>%
    select(-event.x, -event.y)
  
  return(x_mod)
  
}

includeHarvest <- function(x, harvest){
  
  # Data from monitoring do not account for harvesting, so the mass of 
  # harvested fruits should be manually included back in the measurement
  
  fm_mat <- harvest %>% 
    group_by(id, cycle) %>%
    arrange(date) %>%
    mutate(fm_cum = cumsum(fm),
           date = as.Date(date)) %>%
    ungroup() %>%
    mutate(hour = 17) %>%
    select(-dm, -fm)
  
  x_mod <- x %>%
    left_join(fm_mat) %>%
    group_by(cycle, node) %>%
    mutate(temp = row_number(),
           fm_cum = if_else(temp == 1, 0, fm_cum),
           fm_cum = zoo::na.locf(fm_cum),
           wm_mod = wm + fm_cum,
           date = as.Date(date))   %>%
    select(-fm_cum, -temp)
    
  return(x_mod)
  
}