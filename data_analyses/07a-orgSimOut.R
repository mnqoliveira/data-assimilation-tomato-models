# Organizes all outputs of simulations

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("tidyverse")
library("lubridate")

# Load data ---------------------------------------------------------------
simul_files_path <- list.files("../tables/results_simul/exp/", 
                               pattern = "simple|tomgro|vanthoor",
                               include.dirs = FALSE, full.names = TRUE)

simul_files <- list.files("../tables/results_simul/exp/", 
                          pattern = "simple|tomgro|vanthoor",
                          include.dirs = FALSE) %>%
  strsplit(split = "-")

error_files_path <- list.files("../tables/results_simul/errors/", 
                               pattern = "simple|tomgro|vanthoor",
                               include.dirs = FALSE, full.names = TRUE)

error_files <- list.files("../tables/results_simul/errors/", 
                          pattern = "simple|tomgro|vanthoor",
                          include.dirs = FALSE) %>%
  strsplit(split = "-")

# Process outputs of models -----------------------------------------------

outputs <- data.frame(matrix(unlist(simul_files),
                             nrow = length(simul_files),
                             byrow = T)) %>%
  rename(model = X1,
         city = X2,
         exp = X3,
         calib = X4) %>%
  mutate(calib = gsub(".csv", "", calib)) %>%
  separate(calib, into=c("nope", "calib")) %>%
  select(-nope) %>%
  unite("city_exp", c("city", "exp"), remove=FALSE)

all_results <- list()
it <- 1
# Organize the results of the simulations
for (it in 1:nrow(outputs)) {
  
  model_output <- read.csv(simul_files_path[it])
  
  if (outputs$model[it] == "tomgro" | outputs$model[it] == "vanthoor"){
    result <- model_output %>%
      mutate(das = das + 1,
             model = outputs$model[it],
             exp = outputs$exp[it],
             calib = outputs$calib[it],
             city = outputs$city[it],
             rad_type = outputs$rad[it]) %>%
      rename_all(tolower)
    
  } else {

    result <- model_output %>%
      select(das, biomass, plant_yield, f_solar) %>%
      mutate(model = "simple",
             exp = outputs$exp[it],
             calib = outputs$calib[it],
             city = outputs$city[it],
             rad_type = outputs$rad[it],
             k = if_else(city == "brd" | city == "gai",
                         0.8, 0.58),
             lai = log(1 - f_solar)/-k) %>%
      rename(w = biomass,
             wm = plant_yield) %>%
      select(-f_solar, -k)

  }
  
  all_results[[it]] <- result
  
}

models <- Reduce(bind_rows, all_results) %>%
  gather(starts_with("w"), "n", starts_with("lai"), c("dw", "pg", "rm"),
         key = "variable", value = "measurement") %>%
  arrange(model, exp, calib, city, variable, das) %>%
  select(das, model, exp, calib, city, variable, measurement)

write.csv(models, "../tables/results_simul/results_simulations_all.csv", 
          row.names=FALSE)

# Errors ------------------------------------------------------------------

outputs <- data.frame(matrix(unlist(error_files),
                             nrow = length(error_files),
                             byrow = T)) %>%
  rename(model = X1,
         city = X2,
         exp = X3,
         calib = X4) %>%
  mutate(calib = gsub(".csv", "", calib)) %>%
  separate(calib, into=c("nope", "calib")) %>%
  select(-nope) %>%
  unite("city_exp", c("city", "exp"), remove=FALSE)

all_results <- list()
it <- 1
# Organize the results of the simulations
for (it in 1:nrow(outputs)) {
  
  temp <- read.csv(error_files_path[it]) %>%
    mutate(das = dat,
           model = outputs$model[it],
           exp = outputs$exp[it],
           calib = outputs$calib[it],
           city = outputs$city[it]) %>%
    rename_all(tolower)

  if (nrow(temp) > 0){
    all_results[[it]] <- temp
  }
  
  
}

all_err <- Reduce(bind_rows, all_results) %>%
  arrange(model, exp, calib, city, variable, das) %>%
  mutate(error = obs - pred,
         abs_error = abs(error),
         sc_rel_error = error/max_obs)

write.csv(all_err, "../tables/results_simul/all_errors.csv", 
          row.names=FALSE)
