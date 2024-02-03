# Creates the two types of artificial observations used in assimilation

# Set configurations ------------------------------------------------------
#rm(list = ls())
options(stringsAsFactors = FALSE)
options(dplyr.width = Inf)

Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("tidyverse")
library("lubridate")
library("zoo")

# Load data ---------------------------------------------------------------
# vanthoor
obs_vanthoor_path <- list.files("./tables/results_simul/exp/", 
                               pattern = "vanthoor",
                               include.dirs = FALSE, full.names = TRUE)

obs_vanthoor <- list.files("./tables/results_simul/exp/", 
                          pattern = "vanthoor",
                          include.dirs = FALSE) %>%
  strsplit(split = "-")

# tomgro
obs_tomgro <- list.files("./tables/results_simul/exp/", 
                        pattern = "tomgro",
                        include.dirs = FALSE) %>%
  strsplit(split = "-") %>%
  lapply(matrix, nrow=1) %>%
  lapply(data.frame)

obs_tomgro_path <- list.files("./tables/results_simul/exp/", 
                             pattern = "tomgro",
                             include.dirs = FALSE, full.names = TRUE)

# tomgro truth
truth_files_path <- obs_tomgro_path

truth_files <- obs_tomgro

s0 <- read.csv("./tables/parameters_inputs/tomgro_s0.csv",
               header = FALSE)
# obs_summ <- read.csv("./data/observations/monitoring/observations_proc.csv") %>%
#   filter(city == "cps", cycle == 4, exp == "n07", node == "calib",
#          variable == "wf" | variable == "wm") %>%
#   select(dat, variable, measurement_sd) %>%
#   spread(variable, measurement_sd) %>%
#   rename(wf_sd_md = wf,
#          wm_sd_md = wm)

errors_simul <- read.csv("./tables/results_simul/all_errors.csv") 

# Errors from simulations -------------------------------------------------
# Truth values are simulations with parameter cpsIV, i.e., any environments, 
# inputs from cycle 4. Non-calibrated errors come from parameters from
# gnvMod, with inputs from cycle 4, to be comparable.
errors_simul_mod <- errors_simul %>%
  filter(!is.na(error)) %>%
  filter(variable == "wf" | variable == "wm",
         model == "vanthoor" | model == "tomgro", 
         sensor == "A" | is.na(sensor), 
         calib == "cpsIV" | calib == "gnvIV", exp == "n07") %>%
  group_by(dat, model, exp, city, calib, variable) %>%
  mutate(se = error*error,
         obs_ = pmax(max(obs, 0.1))) %>%
  # summarise(rmse = sqrt(mean(se))) %>%
  summarise(rae = abs_error/abs(obs_)) %>%
  ungroup() %>%
  arrange(dat, model, city, variable, exp) %>%
  spread(variable, rae) %>%
  # Remove 0 variance
  mutate(wf = if_else(is.nan(wf) | is.infinite(wf) | wf == 0, 10^-6, wf),
         wm = if_else(is.nan(wm) | is.infinite(wm) | wm == 0, 10^-6, wm))

errors_tomgro <- errors_simul_mod %>%
  filter(model == "tomgro", calib == "gnvIV") %>%
  rename(wf_sd_md = wf,
         wm_sd_md = wm) %>%
  select(-model, -exp, -city, -calib)  

errors_tomgro_calib <- errors_simul_mod %>%
  filter(model == "tomgro", calib == "cpsIV") %>%
  rename(wf_sd_md = wf,
         wm_sd_md = wm) %>%
  select(-model, -exp, -city, -calib)

errors_vanthoor <- errors_simul_mod %>%
  filter(model == "vanthoor", calib == "cpsIV") %>%
  rename(wf_sd = wf,
         wm_sd = wm) %>%
  select(-model, -exp, -city, -calib)

# Artificial observations - Vanthoor --------------------------------------
# Only includes high quality weather observations
outputs <- data.frame(matrix(unlist(obs_vanthoor),
                             nrow = length(obs_vanthoor),
                             byrow = T)) %>%
  rename(model = X1, city = X2, exp = X3, calib = X4, sensor = X5) %>%
  mutate(sensor = gsub(".csv", "", sensor)) %>%
  separate(calib, into=c("nope", "calib")) %>%
  separate(sensor, into=c("nope2", "sensor")) %>%
  select(-nope, -nope2) %>%
  unite("city_exp", c("city", "exp"), remove=FALSE) %>%
  cbind(path = obs_vanthoor_path) %>%
  filter(calib == "cpsIV", sensor == "A")

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
           sensor = outputs$sensor[it],
           city = outputs$city[it],
           rad_type = outputs$rad[it]) %>%
    rename_all(tolower)
    

  all_results[[it]] <- result
  
}

simul_vant <- Reduce(bind_rows, all_results) %>%
  group_by(calib, city, sensor) %>%
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
         cycle = ceiling(as.numeric(gsub("n", "", exp))/2)) %>%
  ungroup() %>%
  select(wf, wm, dat, exp, calib, stage, node, cycle) %>%
  # sd_md = tomgro error without calibration (model)
  # sd = vanthoor error calibrated (observation)
  # relative error applied to observation here, to determine sd value to be used
  # relative observation for the model, left as is, to be applied in the filter
  left_join(errors_tomgro) %>%
  left_join(errors_vanthoor) %>%
  # wx_sd_ and wf_sd_md are relative errors
  mutate(wf_sd_ = if_else(dat == min(dat) | wf_sd == 0, 10^-4, wf_sd),
         wf_sd_ = zoo::na.locf(wf_sd_),
         wm_sd_ = if_else(dat == min(dat) | wm_sd == 0, 10^-4, wm_sd),
         wm_sd_ = zoo::na.locf(wm_sd_),
         wf_sd_md = if_else(dat == min(dat) | wf_sd_md == 0, 10^-4, wf_sd_md),
         wf_sd_md = zoo::na.locf(wf_sd_md),
         wm_sd_md = if_else(dat == min(dat) | wm_sd_md == 0, 10^-4, wm_sd_md),
         wm_sd_md = zoo::na.locf(wm_sd_md),
         wf_sd = abs(wf_sd_*wf),
         wm_sd = abs(wm_sd_*wm),
         config = 200,
         city = "cps") %>%
  # Create delta for the cases in which it is required
  group_by(calib, cycle, city, exp, config) %>%
  mutate(wf_sd_d = abs((wf - lag(wf)) * wf_sd_),
         wm_sd_d = abs((wm - lag(wm)) * wm_sd_),
         wf_sd_d = if_else(is.na(wf_sd_d) | wf_sd_d == 0, 0.001, wf_sd_d),
         wm_sd_d = if_else(is.na(wm_sd_d) | wm_sd_d == 0, 0.001, wm_sd_d))

write.csv(simul_vant, "./data/synthetic/obs_vanthoor.csv", 
          row.names=FALSE)

# Artificial observations - Tomgro + Perturbations ------------------------
# Only includes high quality weather observations
# Uses input from cycle 4 and calib of cycle 4 for all cycles as there likely 
# exists an interaction between them
outputs <- do.call(bind_rows, obs_tomgro)%>%
  rename(model = X1, city = X2, exp = X3, calib = X4, sensor = X5) %>%
  mutate(calib = gsub(".csv", "", calib),
         sensor = gsub(".csv", "", sensor)) %>%
  separate(calib, into=c("nope", "calib")) %>%
  select(-nope) %>%
  separate(sensor, into=c("nope", "sensor")) %>%
  select(-nope) %>%
  unite("city_exp", c("city", "exp"), remove=FALSE) %>%
  cbind(path = obs_tomgro_path) %>%
  filter(calib == "cpsIV", sensor == "A")

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
         cycle = ceiling(as.numeric(gsub("n", "", exp))/2)) %>%
  ungroup() %>%
  select(wf, wm, dat, exp, calib, stage, node, cycle)

cv_list <- rep(c(0.1, 0.3, 0.5), 2)
config <- c(1:length(cv_list))
reps <- 1:20
df <- data.frame(error=cv_list, config = config)

comb <- merge(df, reps) %>%
  rename(rep=y)

all_results <- list()

for (it in 1:nrow(comb)){
  
  comb_it <- comb[it, ]
  set.seed(42 + comb_it$rep)
  temp <- simul_tomgro %>%
    mutate(wf = wf + rnorm(nrow(simul_tomgro), sd=comb$error[it]*wf),
           wf = if_else(wf < 0, 0, wf),
           wm = wm + rnorm(nrow(simul_tomgro), sd=comb$error[it]*wm),
           wm = if_else(wm < 0, 0, wm),
           wf_sd = abs(wf*comb$error[it]),
           wm_sd = abs(wm*comb$error[it]),
           config = comb$config[it],
           city = "cps",
           rep=comb$rep[it])
  
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
         cycle = ceiling(as.numeric(gsub("n", "", exp))/2)) %>%
  ungroup() %>%
  left_join(errors_tomgro) %>%
  # sd_md = model error without calibration
  # sd = value ascribed from  observation value and desired CV
  mutate(wf_sd_md = if_else(dat == min(dat), 10^-6, wf_sd_md),
         wf_sd_md = zoo::na.locf(wf_sd_md),
         wm_sd_md = if_else(dat == min(dat), 10^-6, wm_sd_md),
         wm_sd_md = zoo::na.locf(wm_sd_md),
         wf_sd = if_else(dat == min(dat), 10^-6, wf_sd),
         wf_sd = zoo::na.locf(wf_sd),
         wm_sd = if_else(dat == min(dat), 10^-6, wm_sd),
         wm_sd = zoo::na.locf(wm_sd)) %>%
  # Create delta for the cases in which it is required
  group_by(calib, cycle, city, exp, config, rep) %>%
  mutate(wf_sd_d = abs(wf_sd - lag(wf_sd)),
         wm_sd_d = abs(wm_sd - lag(wm_sd)),
         wf_sd_d = if_else(is.na(wf_sd_d) | wf_sd_d == 0, 0.001, wf_sd_d),
         wm_sd_d = if_else(is.na(wm_sd_d) | wm_sd_d == 0, 0.001, wm_sd_d))

write.csv(tomgro_noisy, "./data/synthetic/obs_tomgro_noisy.csv", 
          row.names=FALSE)

all_obs <- rbind(tomgro_noisy, simul_vant)

write.csv(all_obs, "./data/synthetic/obs_artif_all.csv", 
          row.names=FALSE)


# Artificial truth --------------------------------------------------------
# Only includes high quality weather observations
# Uses input from cycle 4 and calib of cycle 4 for all cycles
outputs <- do.call(bind_rows, truth_files) %>%
  rename(model = X1,
         city = X2,
         exp = X3,
         calib = X4,
         sensor = X5) %>%
  mutate(calib = gsub(".csv", "", calib),
         sensor = gsub(".csv", "", sensor)) %>%
  separate(calib, into=c("nope", "calib")) %>%
  select(-nope) %>%
  separate(sensor, into=c("nope", "sensor")) %>%
  select(-nope) %>%
  unite("city_exp", c("city", "exp"), remove=FALSE) %>%
  bind_cols(path = truth_files_path) %>%
  filter(calib == "cpsIV",
         sensor == "A")

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

write.csv(truth, "./data/synthetic/truth_tomgro.csv", 
          row.names=FALSE)


