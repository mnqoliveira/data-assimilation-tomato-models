# Creates the two types of artificial observations used in assimilation

# Set configurations ------------------------------------------------------
#rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("tidyverse")
library("lubridate")
library("zoo")

# Load data ---------------------------------------------------------------
# vanthoor
obs_vanthoor_path <- list.files("../tables/results_simul/exp/", 
                               pattern = "vanthoor",
                               include.dirs = FALSE, full.names = TRUE)

obs_vanthoor <- list.files("../tables/results_simul/exp/", 
                          pattern = "vanthoor",
                          include.dirs = FALSE) %>%
  strsplit(split = "-")

# tomgro
obs_tomgro <- list.files("../tables/results_simul/exp/", 
                        pattern = "tomgro",
                        include.dirs = FALSE) %>%
  strsplit(split = "-")

obs_tomgro_path <- list.files("../tables/results_simul/exp/", 
                             pattern = "tomgro",
                             include.dirs = FALSE, full.names = TRUE)

# tomgro truth
truth_files_path <- list.files("../tables/results_simul/exp/", 
                             pattern = "tomgro",
                             include.dirs = FALSE, full.names = TRUE)

truth_files <- list.files("../tables/results_simul/exp/", 
                        pattern = "tomgro",
                        include.dirs = FALSE) %>%
  strsplit(split = "-")

s0 <- read.csv("../tables/parameters_inputs/tomgro_s0.csv",
               header = FALSE)
# obs_summ <- read.csv("../data/observations/monitoring/observations_proc.csv") %>%
#   filter(city == "cps", cycle == 4, exp == "n07", node == "calib",
#          variable == "wf" | variable == "wm") %>%
#   select(dat, variable, measurement_sd) %>%
#   spread(variable, measurement_sd) %>%
#   rename(wf_sd_md = wf,
#          wm_sd_md = wm)

errors_simul <- read.csv("../tables/results_simul/all_errors.csv") 



# Errors from simulations -------------------------------------------------
errors_simul_mod <- errors_simul %>%
  filter(variable == "wf" | variable == "wm",
         model == "vanthoor" | model == "tomgro", 
         calib == "cps4" | calib == "gnvMod", exp == "n07") %>%
  group_by(dat, model, exp, city, calib, variable) %>%
  mutate(se = error*error) %>%
  summarise(rmse = sqrt(mean(se))) %>%
  ungroup() %>%
  arrange(dat, model, city, variable, exp) %>%
  spread(variable, rmse) 

rmse_all <- errors_simul %>%
  filter(variable == "wf" | variable == "wm",
         model == "vanthoor" | model == "tomgro", 
         calib == "cps4" | calib == "gnvMod", exp == "n07") %>%
  group_by(model, exp, city, calib, variable) %>%
  mutate(se = error*error) %>%
  summarise(rmse = sqrt(mean(se))) %>%
  ungroup() %>%
  arrange(model, city, variable, exp) %>%
  spread(variable, rmse) 

errors_tomgro <- errors_simul_mod %>%
  filter(model == "tomgro", calib == "gnvMod") %>%
  rename(wf_sd_md = wf,
         wm_sd_md = wm) %>%
  select(-model, -exp, -city, -calib)  %>%
  mutate()

errors_tomgro_calib <- errors_simul_mod %>%
  filter(model == "tomgro", calib == "cps4") %>%
  rename(wf_sd_md = wf,
         wm_sd_md = wm) %>%
  select(-model, -exp, -city, -calib)  %>%
  mutate()

errors_vanthoor <- errors_simul_mod %>%
  filter(model == "vanthoor", calib == "cps4") %>%
  rename(wf_sd = wf,
         wm_sd = wm) %>%
  select(-model, -exp, -city, -calib)


# Artificial observations - Vanthoor --------------------------------------
outputs <- data.frame(matrix(unlist(obs_vanthoor),
                             nrow = length(obs_vanthoor),
                             byrow = T)) %>%
  rename(model = X1,
         city = X2,
         exp = X3,
         calib = X4) %>%
  mutate(calib = gsub(".csv", "", calib)) %>%
  separate(calib, into=c("nope", "calib")) %>%
  select(-nope) %>%
  unite("city_exp", c("city", "exp"), remove=FALSE) %>%
  cbind(path = obs_vanthoor_path) %>%
  filter(exp == "n01" | exp == "n03" | exp == "n05" | exp == "n07",
         calib == "cps4")

all_results <- list()
it <- 1

# Organize the results of the simulations
for (it in 1:nrow(outputs)) {
  
  model_output <- read.csv(outputs$path[it])
  
  result <- model_output %>%
    mutate(dat = das + 1,
           model = outputs$model[it],
           exp = outputs$exp[it],
           calib = outputs$calib[it],
           city = outputs$city[it],
           rad_type = outputs$rad[it]) %>%
    rename_all(tolower)
    

  all_results[[it]] <- result
  
}

simul_vant <- Reduce(bind_rows, all_results) %>%
  group_by(calib, city) %>%
  mutate(stage = if_else(dat == min(dat), 
                         "vegetative",
                         if_else(wf > 0 & wm < 0.1, 
                                 "fruits",
                                 if_else(wm >= 0.1, 
                                         "maturity", 
                                         "NA"))),
         stage = na_if(stage, "NA"),
         stage = zoo::na.locf(stage),
         node = "calib",
         cycle = 1) %>%
  ungroup() %>%
  select(wf, wm, dat, exp, calib, stage, node, cycle) %>%
  # sd_md = tomgro error without calibration (model)
  # sd = vanthoor error calibrated (observation)
  left_join(errors_tomgro) %>%
  left_join(errors_vanthoor) %>%
  mutate(wf_sd_md = if_else(dat == min(dat) | wf_sd_md == 0, 10^-4, wf_sd_md),
         wf_sd_md = zoo::na.locf(wf_sd_md),
         wm_sd_md = if_else(dat == min(dat) | wm_sd_md == 0, 10^-4, wm_sd_md),
         wm_sd_md = zoo::na.locf(wm_sd_md),
         wf_sd = if_else(dat == min(dat) | wf_sd == 0, 10^-4, wf_sd),
         wf_sd = zoo::na.locf(wf_sd),
         wm_sd = if_else(dat == min(dat) | wm_sd == 0, 10^-4, wm_sd),
         wm_sd = zoo::na.locf(wm_sd),
         config = 200,
         city = "cps")

write.csv(simul_vant, "../data/synthetic/obs_vanthoor.csv", 
          row.names=FALSE)



# Artificial observations - Tomgro + Perturbations ------------------------
outputs <- data.frame(matrix(unlist(obs_tomgro),
                             nrow = length(obs_tomgro),
                             byrow = T)) %>%
  rename(model = X1,
         city = X2,
         exp = X3,
         calib = X4) %>%
  mutate(calib = gsub(".csv", "", calib)) %>%
  separate(calib, into=c("nope", "calib")) %>%
  select(-nope) %>%
  unite("city_exp", c("city", "exp"), remove=FALSE) %>%
  cbind(path = obs_tomgro_path) %>%
  filter(exp == "n01" | exp == "n03" | exp == "n05" | exp == "n07",
         calib == "cps4")

all_results <- list()
it <- 1

# Organize the results of the simulations
for (it in 1:nrow(outputs)) {
  
  model_output <- read.csv(outputs$path[it])
  
  result <- model_output %>%
    mutate(dat = das + 1,
           model = outputs$model[it],
           exp = outputs$exp[it],
           calib = outputs$calib[it],
           city = outputs$city[it],
           rad_type = outputs$rad[it]) %>%
    rename_all(tolower)
  
  
  all_results[[it]] <- result
  
}

simul_tomgro <- Reduce(bind_rows, all_results) %>%
  group_by(calib, city) %>%
  mutate(stage = if_else(dat == min(dat), 
                         "vegetative",
                         if_else(wf > 0 & wm < 0.1, 
                                 "fruits",
                                 if_else(wm >= 0.1, 
                                         "maturity", 
                                         "NA"))),
         stage = na_if(stage, "NA"),
         stage = zoo::na.locf(stage),
         node = "calib",
         cycle = 1) %>%
  ungroup() %>%
  select(wf, wm, dat, exp, calib, stage, node, cycle)

cv_list <- rep(c(0.1, 0.3, 0.5), 2)
config <- c(1:length(cv_list))
all_results <- list()
for (it in 1:length(cv_list)){
  
  set.seed(42)
  temp <- simul_tomgro %>%
    mutate(wf = wf + rnorm(nrow(simul_tomgro), sd=cv_list[it]*wf),
           wf = if_else(wf < 0, 0, wf),
           wm = wm + rnorm(nrow(simul_tomgro), sd=cv_list[it]*wm),
           wm = if_else(wm < 0, 0, wm),
           wm_sd = wf*cv_list[it],
           wf_sd = wm*cv_list[it],
           config = config[it],
           city = "cps")
  
  all_results[[it]] <- temp
  
}

tomgro_noisy <- Reduce(bind_rows, all_results) %>%
  group_by(calib, city) %>%
  mutate(stage = if_else(dat == min(dat), 
                         "vegetative",
                         if_else(wf > 0 & wm < 0.1, 
                                 "fruits",
                                 if_else(wm >= 0.1, 
                                         "maturity", 
                                         "NA"))),
         stage = na_if(stage, "NA"),
         stage = zoo::na.locf(stage),
         node = "calib",
         cycle = 1) %>%
  ungroup() %>%
  left_join(errors_tomgro) %>%
  # sd_md = model error without calibration
  # sd = value ascribed from  observation value and desired CV
  mutate(wf_sd_md = if_else(dat == min(dat) | wf_sd_md == 0, 10^-4, wf_sd_md),
         wf_sd_md = zoo::na.locf(wf_sd_md),
         wm_sd_md = if_else(dat == min(dat) | wm_sd_md == 0, 10^-4, wm_sd_md),
         wm_sd_md = zoo::na.locf(wm_sd_md),
         wf_sd = if_else(dat == min(dat) | wf_sd == 0, 10^-4, wf_sd),
         wf_sd = zoo::na.locf(wf_sd),
         wm_sd = if_else(dat == min(dat) | wm_sd == 0, 10^-4, wm_sd),
         wm_sd = zoo::na.locf(wm_sd))

write.csv(tomgro_noisy, "../data/synthetic/obs_tomgro_noisy.csv", 
          row.names=FALSE)

all_obs <- rbind(tomgro_noisy, simul_vant)

write.csv(all_obs, "../data/synthetic/obs_artif_all.csv", 
          row.names=FALSE)


# Artificial truth --------------------------------------------------------

outputs <- data.frame(matrix(unlist(truth_files),
                             nrow = length(truth_files),
                             byrow = T)) %>%
  rename(model = X1,
         city = X2,
         exp = X3,
         calib = X4) %>%
  mutate(calib = gsub(".csv", "", calib)) %>%
  separate(calib, into=c("nope", "calib")) %>%
  select(-nope) %>%
  unite("city_exp", c("city", "exp"), remove=FALSE) %>%
  bind_cols(path = truth_files_path) %>%
  filter(calib == "cps4",
         exp == "n01" | exp == "n03" | exp == "n05" | exp == "n07")

all_results <- list()
it <- 2

# Organize the results of the simulations
for (it in 1:nrow(outputs)) {
  
  model_output <- read.csv(outputs$path[it])
  
  result <- model_output %>%
    mutate(dat = das + 1,
           model = outputs$model[it],
           exp = outputs$exp[it],
           calib = outputs$calib[it],
           city = outputs$city[it],
           rad_type = outputs$rad[it]) %>%
    rename_all(tolower)
  
  
  all_results[[it]] <- result
  
}

truth <- Reduce(bind_rows, all_results) %>%
  group_by(calib, city) %>%
  mutate(stage = if_else(dat == min(dat), 
                         "vegetative",
                         if_else(wf > 0 & wm < 0.1, 
                                 "fruits",
                                 if_else(wm >= 0.1, 
                                         "maturity", 
                                         "NA"))),
         stage = na_if(stage, "NA"),
         stage = zoo::na.locf(stage)) %>%
  ungroup() %>%
  select(wf, wm, dat, exp, calib, stage)

write.csv(truth, "../data/synthetic/truth_tomgro.csv", 
          row.names=FALSE)


