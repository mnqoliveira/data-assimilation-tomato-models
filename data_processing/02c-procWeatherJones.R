# Processes and organize weather data from Jones et al 1999 spreadsheets

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("zoo")
library("readxl")
library("tidyverse")
library("lubridate")

# Process data ------------------------------------------------------------

spreadsheetPath <- "../data/weather/jones/weather_all_jones.xlsx"
nSheets <- length(excel_sheets(spreadsheetPath))
namesSheets <- excel_sheets(spreadsheetPath)
i <- 7

summaries_l <- list()
summaries_l2 <- list()

for (i in 1:nSheets){
  
  dataset <- read_xlsx(spreadsheetPath, sheet = i) 
  city <- strsplit(namesSheets[i], "_")[[1]][1]
  exp <- strsplit(namesSheets[i], "_")[[1]][2]
  dataset_mod <- dataset %>%
    mutate(dap = zoo::na.locf(dap),
           dat = dap,
           hour = rep(1:24, length(unique(dap)))[1:nrow(dataset)],
           hour = if_else(hour == 24, 0L, hour),
           radiation_unit = zoo::na.locf(radiation_unit),
           year = zoo::na.locf(year)) %>%
    rename(tmean = temperature_mean,
           rad = radiation_mean) %>%
    mutate(doy = if_else(nchar(doy) == 1, paste0("00", doy),
                          if_else(nchar(doy) == 2, paste0("0", doy),
                                  as.character(doy))),
           date =  paste(year, doy, sep = "-"),
           date = as.Date(date, tryFormats = c("%Y-%j")),
           date = zoo::na.locf(date) + dap - 1,
           doy = yday(date),
           exp = namesSheets[i],
           co2 = 350) %>%
    group_by(date) %>%
    mutate(flag = if_else(length(unique(hour)) != 24, 1, 0)) %>%
    filter(flag != 1) %>%
    ungroup()

  if (i %in% c(4, 5, 6)){
    temp <- dataset_mod %>%
      mutate(exp = as.factor(exp))
    summaries_l[[paste0('h', i)]] <- as.data.frame(summary(temp))
    summaries_l2[[paste0('h', i)]] <- as.data.frame(summary(temp[temp$rad > 0, ]))
  }
  
  
    # dataset_mod <- dataset_mod %>%
    #   mutate(hour_min = ymd_h(paste0(date, hour, "T")),
    #          hour_min_mod = hour_min - hours(dif),
    #          hour = as.numeric(hour(hour_min_mod)),
    #          date = as.character(date(hour_min_mod)))
    
  #}
  
  write.csv(dataset_mod, file = paste0("../data/weather/hourly_", namesSheets[i], 
                                   ".csv"), row.names=FALSE)
  
  dataset_daily <- dataset_mod %>%
    group_by(date, doy, dap, dat, radiation_unit, year, exp) %>%
    summarise(tmax = max(tmean),
              tmin = min(tmean),
              tmean = mean(tmean),
              rad = sum(rad), 
              co2 = mean(co2))
  
  write.csv(dataset_daily, file = paste0("../data/weather/daily_", namesSheets[i], 
                                       ".csv"), row.names=FALSE)
  if (i %in% c(4, 5, 6)){
    temp <- dataset_daily %>%
      mutate(exp = as.factor(exp))
    summaries_l[[paste0('d', i)]] <- as.data.frame(summary(temp))
  }
}

# summaries <- Reduce(rbind, summaries_l) %>%
#   mutate(Var2 = as.character(Var2),
#          Var2 = trimws(Var2, "both")) %>%
#   filter(Var2 == "tmean" | Var2 == "tmax" | Var2 == "tmin" |
#            Var2 == "rad" | Var2 == "exp") %>%
#   separate(Freq, into = c("stat", "value"), sep = ":") %>%
#   mutate(stat = trimws(stat, "both")) %>%
#   filter(!is.na(stat), stat != "Min.", stat != "1st Qu.",
#          stat != '3rd Qu.', stat != "Max.", stat != "Median",
#          stat != "NA's")
# 
# summaries2 <- Reduce(rbind, summaries_l2) %>%
#   mutate(Var2 = as.character(Var2),
#          Var2 = trimws(Var2, "both")) %>%
#   filter(Var2 == "rad" | Var2 == "exp") %>%
#   separate(Freq, into = c("stat", "value"), sep = ":") %>%
#   mutate(stat = trimws(stat, "both")) %>%
#   filter(!is.na(stat), stat != "Min.", stat != "1st Qu.",
#          stat != '3rd Qu.', stat != "Max.", stat != "Median",
#          stat != "NA's")
# 
# 
# summary_all <- rbind(summaries, summaries2)
# write.csv(summary_all, 
#           file = "../tables/summary_gnv.csv", row.names = FALSE)