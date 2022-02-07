# Generates all the observation models from calibration samples

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(zoo)

library(nlme)

library(ggplot2)


# Load files --------------------------------------------------------------
load("./data/plot_theme_vert.RData")

# Calibration ids
obs_ids <- read.csv("./data/observations/monitoring/obs_exp_all_ids.csv")

# LAI
cover_lat_calib <- read.csv("./data/observations/monitoring/lai/lai_lat_calib_ids.csv")
cover_abv_calib <- read.csv("./data/observations/monitoring/lai/lai_abv_calib_ids.csv")

# W
height_calib <- read.csv("./data/observations/monitoring/dry_mass_aboveground/height_calib_ids.csv")

# Wf
wf_lat_calib <- read.csv("./data/observations/monitoring/dry_mass_fruit/wf_lat_calib_ids.csv")
harvests <- read.csv("./data/observations/monitoring/harvests.csv")
cycle_dates <- read.csv("./data/cycle_dates.csv")


wf_lat <- wf_lat_calib %>%
  left_join(cycle_dates) %>%
  full_join(harvests) %>%
  arrange(cycle, id, date) %>%
  group_by(cycle, id) %>%
  mutate(type_ = if_else(is.na(type), 0, 1),
         flag = if_else(sum(type_) >= 1, 1, 0)) %>%
  filter(flag == 0) %>%
  ungroup() %>%
  select(-dat, -type, -type_,-flag, -fm, -dm) %>%
  distinct()

my_colors <- RColorBrewer::brewer.pal(4, "RdBu")[c(1, 3:4)]
dict_filt <- read.csv("./tables/dictionary_paramsFilter.csv") %>%
  filter(lang == "pt")


# Dataset -----------------------------------------------------------------
dataset <- obs_ids %>%
  filter(!is.na(cycle), cycle != 1) %>%
  full_join(wf_lat) %>%
  full_join(cover_lat_calib) %>%
  full_join(cover_abv_calib) %>%
  full_join(height_calib) %>%
  select(-contains("plant"), -leaf_area, -leaves, -flower) %>%
  mutate(date = as.Date(date)) %>%
  # Multiply because of linear coefficient
  mutate(wf_lat = wf_lat * ro,
         lai_lat = lai_lat * ro,
         lai_abv = lai_abv * ro)

comb <- data.frame(meas_var = c("wf_lat", "w_fm", "lai_lat", "lai_abv"),
                   state_var = c("wf", "w", "lai", "lai")) 

comb_mod <- merge(data.frame(cycle = c(2:4)), comb, by=NULL) %>%
  left_join(dict_filt) %>%
  mutate(slope = NA,
         intercept = NA,
         se_train = NA,
         r2_train = NA,
         mape_val = NA)

it <- 3
for (it in 1:nrow(comb_mod)){
  
  dataset_ <- dataset %>%
    filter(!is.na(get(comb_mod$state_var[it])),
           !is.na(get(comb_mod$meas_var[it])),
           ! ((id == 31 | id == 42) & (cycle == 2 | cycle == 3)), 
           ! ((id == 30 | id == 54) & (cycle == 4)),
           get(comb_mod$state_var[it]) > 0,
           get(comb_mod$meas_var[it]) > 0) %>%
    mutate(pred = NA)
  
  cycle <- comb_mod$cycle[it]

  form <- paste(comb_mod$meas_var[it], "~", comb_mod$state_var[it])
  condition <- dataset_$cycle == cycle
  
  # Model
  dataset_mod <- dataset_[!condition, ]
  
  model <- gls(as.formula(form), 
               data = dataset_mod, weights = varPower())
  
  pred_train <- predict(model, dataset_[!condition, ])
  real_train <- dataset_[!condition, comb_mod$meas_var[it]]
  
  pred <- predict(model, dataset_[condition, ])
  real <- dataset_[condition, comb_mod$meas_var[it]]
  
  # Save model outputs
  comb_mod[it, "slope"] <- model$coefficients[2]
  comb_mod[it, "intercept"] <- model$coefficients[1]
  comb_mod[it, "se_train"] <- model$sigma
  comb_mod[it, "r2_train"] <- (cor(pred_train, real_train))**2
  comb_mod[it, "mape_val"] <- mean(abs(real - pred)/real)
  
}

write.csv(comb_mod, file = "./tables/models_obs.csv", row.names = FALSE)

# # observed versus fitted values by cycle
# plot(model, wf_lat ~ fitted(.) | cycle, abline = c(0,1))
# # box-plots of residuals by cycle
# plot(model, cycle ~ resid(.))
# # standardized residuals versus fitted values by cycle
# plot(model, resid(., type = "p") ~ fitted(.) | cycle,
#      abline = 0)
