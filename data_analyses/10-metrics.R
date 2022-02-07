# Calculates metrics for simulations and assimilation results

# Set configurations ------------------------------------------------------
#rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("ggplot2")
library("tidyverse")
library("lubridate")
library("RColorBrewer")

# Source functions --------------------------------------------------------

# Update results
# source("./data_analyses/07a-orgSimOut.R")
# source("./data_analyses/07b-orgObs_plot.R")
# source("./data_analyses/07c-orgAssim.R")

# Load data ---------------------------------------------------------------
load("./data/plot_theme_horiz.RData")

# Simulations
models_all <- read.csv( "./tables/results_simul/results_simulations_all.csv")

# Measured
obs_summ <- read.csv("./data/observations/monitoring/observations_proc.csv")
obs_ids <- read.csv("./data/observations/monitoring/obs_exp_all_ids.csv")

# Load errors
load("./tables/results_DA/aux_files/all_errors.RData")
errors_filt <- all_err %>%
  mutate(config = as.numeric(config))
  
errors_simul <- read.csv("./tables/results_simul/all_errors.csv")
info_runs <- read.csv("./tables/runs_Filter2.csv") %>%
  rbind(read.csv("./tables/runs_Filter.csv"))

#Load assimilation results
load("./tables/results_DA/aux_files/all_states.RData")
load("./tables/results_DA/aux_files/upd_states.RData")

# Load SI results
all_files_si <- read.csv("./tables/info_Si_org.csv")
outputs_extreme <- read.csv("./tables/results_SA/outputs_SA.csv")

# Errors simul ------------------------------------------------------------
# Calculated with observations as reference value
errors_models <- errors_simul %>%
  filter(variable == "w" | variable == "wf" | variable == "lai" | variable == "wm") %>%
  group_by(model, das, exp, city, calib, variable, sensor) %>%
  #arrange(model, city, variable, calib, exp, sensor, das) 
  mutate(se = error*error) %>%
  ungroup() %>%
  group_by(model, exp, city, calib, variable, sensor) %>%
  summarise(rmse = sqrt(mean(se)),
            me = mean(error),
            mae = mean(abs_error),
            sd_mae = sd(abs_error)
         ) %>%
  ungroup() %>%
  arrange(model, city, variable, calib, exp, sensor) 
# %>%
#   filter(variable == "wm" | variable == "wf", 
#          model == "tomgro" | model == "vanthoor", 
#          calib == "gnvMod" | calib == "cps4",
#          exp == "n07")

# temp <- filter(errors_simul, model == "tomgro", 
#                calib == "gnvMod" | calib == "cpsopt", exp == "n07", variable == "wm")
  
# All
tomgro_gnvMod <- errors_models %>%
  filter(model == "tomgro", calib == "gnvMod", city == "cps") %>%
  gather(rmse, me, mae, sd_mae, key = "metric", value = "measurement") %>%
  spread(exp, measurement)

write.csv(tomgro_gnvMod, file = "./tables/metrics/simul/metrics_gnvMod.csv", 
          row.names = FALSE)


tomgro_calib <- errors_models %>%
  filter(model == "tomgro", city == "cps",
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
            (calib == "cps3" & (exp == "n05" | exp == "n06")) |
            (calib == "cpsopt" & (exp == "n07" | exp == "n08")))) %>%
  select(-calib) %>%
  gather(rmse, me, mae, sd_mae, key = "metric", value = "measurement") %>%
  spread(exp, measurement)

write.csv(tomgro_calib, file = "./tables/metrics/simul/metrics_calib.csv", 
          row.names = FALSE)

# Last
last_gnvMod <- errors_simul %>%
  filter(model == "tomgro", calib == "gnvMod", city == "cps") %>%
  group_by(exp) %>%
  filter(das == max(das)) %>%
  ungroup() %>%
  arrange(exp, variable)

write.csv(last_gnvMod, file = "./tables/metrics/simul/last_gnvMod.csv", 
          row.names = FALSE)

last_calib <- errors_simul %>%
  filter(model == "tomgro", city == "cps",
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
            (calib == "cps3" & (exp == "n05" | exp == "n06")) |
            (calib == "cpsopt" & (exp == "n07" | exp == "n08")))) %>%
  group_by(exp) %>%
  filter(das == max(das)) %>%
  ungroup() %>%
  arrange(exp, variable)

write.csv(last_calib, file = "./tables/metrics/simul/last_calib.csv", 
          row.names = FALSE)

# No zeros
errors_models_nozero <- errors_simul %>%
  filter(variable == "w" | variable == "wf" | variable == "lai" | variable == "wm",
         obs > 0) %>%
  group_by(model, das, exp, city, calib, variable) %>%
  mutate(se = error*error) %>%
  ungroup() %>%
  group_by(model, exp, city, calib, variable, sensor) %>%
  summarise(rmse = sqrt(mean(se)),
            me = mean(error),
            mae = mean(abs_error),
            sd_mae = sd(abs_error)) %>%
  ungroup() %>%
  arrange(model, city, variable, calib, exp) 

tomgro_gnvMod <- errors_models_nozero %>%
  filter(model == "tomgro", calib == "gnvMod", city == "cps") %>%
  gather(rmse, me, mae, sd_mae, key = "metric", value = "measurement") %>%
  spread(exp, measurement)

write.csv(tomgro_gnvMod, file = "./tables/metrics/simul/metrics_gnvMod_nozero.csv", 
          row.names = FALSE)


tomgro_calib <- errors_models_nozero %>%
  filter(model == "tomgro", city == "cps",
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
            (calib == "cps3" & (exp == "n05" | exp == "n06")) |
            (calib == "cpsopt" & (exp == "n07" | exp == "n08")))) %>%
  select(-calib) %>%
  gather(rmse, me, mae, sd_mae, key = "metric", value = "measurement") %>%
  spread(exp, measurement)

write.csv(tomgro_calib, file = "./tables/metrics/simul/metrics_calib_nozero.csv", 
          row.names = FALSE)

# Errors Assim Artif ------------------------------------------------------
# Calculated with simulations as references of truth
simulations <- models_all %>%
  filter(city == "cps", model == "tomgro",
         variable != "dw", variable != "rm", variable != "pg",
         calib == "gnvMod" | (calib == "cps4" & sensor == "A"),
         exp == "n01" | exp == "n03" | exp == "n05" | exp == "n07")

error_notCalib <- simulations %>%
  spread(calib, measurement) %>%
  group_by(das, model, exp, variable) %>%
  mutate(cps4 = mean(cps4, na.rm = TRUE)) %>%
  group_by(das, model, exp, variable, sensor) %>%
  summarise(error = cps4 - gnvMod, 
            abs_error = abs(error),
            rel_err = if_else(cps4 != 0, abs_error/cps4, 0)) %>%
  ungroup() %>%
  mutate(filt = "none",
         calib = "gnvMod",
         id = 0,
         config = 0) %>%
  rename(dat = das)

errors_filt_artif <- errors_filt %>%
  filter(variable == "wm") %>%
  filter(id >= 500, !is.na(config), !is.na(pred)) %>%
  mutate(rel_err = if_else(obs != 0, abs(error)/obs, 0)) %>%
  left_join(info_runs) %>%
  mutate(exp_int = exp,
         exp = paste0("n0", exp)) %>%
  bind_rows(error_notCalib) %>%
  filter(variable == "wm")

metrics <- errors_filt_artif %>%
  group_by(model, calib, variable, filt, exp, frequency, state_var, N, case,
           id, config, Q, R, sensor_type, sensor) %>%
  summarise(me = mean(error),
            rmse = sqrt(mean(error*error)),
            mae = mean(abs_error),
            mean_rel = mean(rel_err[rel_err > 0], na.rm = TRUE),
            med_rel = median(rel_err[rel_err > 0], na.rm = TRUE),
            sd_rel = sd(rel_err[rel_err > 0], na.rm = TRUE)) %>%
  ungroup()

configs <- unique(errors_filt_artif$config)
errors_l <- list()

for (it in configs){
  
  temp <- metrics %>%
    filter(config == it) %>%
    arrange(config, id, filt)
  
  errors_l[[match(it, configs)]] <- temp
  
}

error <- Reduce(rbind, errors_l)

write.csv(error, file = "./tables/metrics/assim/artif/metrics_wm.csv", 
          row.names = FALSE)


# KS Assim Artif Vanthoor -------------------------------------------------
errors_filt_artif <- all_err %>%
  mutate(config = as.numeric(config)) %>%
  left_join(info_runs) %>%
  mutate(rel_err = if_else(obs != 0, abs(error)/obs, 0),
         rel_err = rel_err * 100)

errors_mod <- errors_filt_artif %>%
  select(-obs, -pred, -max_obs, -mean_obs, -error, -abs_error, 
         -sc_rel_error, -city, -N, -P, -Q, -mae, 
         -run, -sensor_type) %>%
  filter(variable == "wm",
         !((filt == "enkf") & (case !=  "case2"))) %>%  
  filter(id >= 1000) %>%
  filter(config > 100 & (config <= 104 | config >= 107 | 
                           ((config == 106))),
         rel_err > 0) %>%
  mutate(filt_ = paste(filt, meas_var, sep = "_"),
         filt_ = factor(filt_,
                        levels = c("enkf_wf", "enkf_wm", "ukf_wf", 
                                   "ukf_wm", "pf_wf", "pf_wm", "none"),
                        labels = c("EnKF\nWf", "EnKF\nWm", "UKF\nWf", 
                                   "UKF\nWm", "PF\nWf", "PF\nWm", "No assim."),
                        ordered = TRUE),
         config_ = factor(config,
                          levels = c(101, 102, 103, 104, 
                                     106: 112),
                          labels = c("Both Fixed",
                                     "Fixed O.E.",
                                     "Fixed M.E.",
                                     "Both var.",
                                     "Both var.",
                                     "Both Fixed",
                                     "Fixed O.E.",
                                     "Fixed M.E.",
                                     "Both Fixed",
                                     "Fixed O.E.",
                                     "Fixed M.E."),
                          ordered = TRUE)) %>%
  filter(filt != "pf") %>%
  unite("id_out", frequency, filt_, exp) %>%
  mutate(id_ins = as.numeric(config_))

combs <- t(combn(1:4, 2))
head(errors_mod)

combinations <- merge(unique(errors_mod$id_out), combs) %>%
  rename(id_out = x,
         g1 = V1,
         g2 = V2) %>%
  mutate(D = NA,
         p = NA)

for (it in 1:nrow(combinations)){
  
  data_filt <- filter(errors_mod, id_out == combinations$id_out[it])
  
  g1 <- filter(data_filt, id_ins == combinations$g1[it])
  g2 <- filter(data_filt, id_ins == combinations$g2[it])
    
  temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
  
  combinations$D[it] <- temp$statistic
  combinations$p[it] <- temp$p.value
    
}

ks_all <- combinations %>%
  separate(id_out, into = c("frequency", "filt_", "exp"), sep = "_") %>%
  separate(filt_, into = c("filt", "meas_var"), sep = "\n")

write.csv(ks_all, file = "./tables/metrics/assim/ks.csv", row.names = FALSE)


# KS Assim Artif Vanthoor - v2 - 50 and 100 -------------------------------
errors_filt_artif <- all_err %>%
  mutate(config = as.numeric(config)) %>%
  left_join(info_runs) %>%
  mutate(rel_err = if_else(obs != 0, abs(error)/obs, 0),
         rel_err = rel_err * 100)

errors_mod <- errors_filt_artif %>%
  select(-obs, -pred, -max_obs, -mean_obs, -error, -abs_error, 
         -sc_rel_error, -city, -N, -P, -Q, -mae, 
         -run, -sensor_type) %>%
  filter(variable == "wm",
         !((filt == "enkf") & (case !=  "case2"))) %>%  
  filter(id >= 1000) %>%
  filter(config > 100 & (config <= 104 | config >= 107 | 
                           ((config == 106))),
         rel_err > 0) %>%
  mutate(filt_ = paste(filt, meas_var, sep = "_"),
         filt_ = factor(filt_,
                        levels = c("enkf_wf", "enkf_wm", "ukf_wf", 
                                   "ukf_wm", "pf_wf", "pf_wm", "none"),
                        labels = c("EnKF\nWf", "EnKF\nWm", "UKF\nWf", 
                                   "UKF\nWm", "PF\nWf", "PF\nWm", "No assim."),
                        ordered = TRUE),
         config_ = factor(config,
                          levels = c(101, 102, 103, 104, 
                                     106: 112),
                          labels = c("Both Fixed",
                                     "Fixed O.E.",
                                     "Fixed M.E.",
                                     "Both var.",
                                     "Both var.",
                                     "Both Fixed",
                                     "Fixed O.E.",
                                     "Fixed M.E.",
                                     "Both Fixed",
                                     "Fixed O.E.",
                                     "Fixed M.E."),
                          ordered = TRUE),
         config2 = as.numeric(config_)) %>%
  filter(filt != "pf") %>%
  unite("id_out", filt_, exp, config2, remove = FALSE) %>%
  #filter(frequency != 0.1) %>%
  mutate(id_ins = frequency)

combs <- t(combn(unique(errors_mod$id_ins), 2))
head(errors_mod)

combinations <- merge(unique(errors_mod$id_out), combs) %>%
  rename(id_out = x,
         g1 = V1,
         g2 = V2) %>%
  mutate(D = NA,
         p = NA)

it <- 55
for (it in 1:nrow(combinations)){
  
  data_filt <- errors_mod[errors_mod$id_out == combinations$id_out[it], ]
  
  g1 <- data_filt[data_filt$id_ins == combinations$g1[it], ]
  g2 <- data_filt[data_filt$id_ins == combinations$g2[it], ]

  temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
  
  combinations$D[it] <- temp$statistic
  combinations$p[it] <- temp$p.value
  
}

ks_all <- combinations %>%
  separate(id_out, into = c("filt_", "exp", "config2"), sep = "_") %>%
  separate(filt_, into = c("filt", "meas_var"), sep = "\n")

write.csv(ks_all, file = "./tables/metrics/assim/ks2.csv", row.names = FALSE)

# KS Assim Artif Controlled -----------------------------------------------
errors_filt_artif <- all_err %>%
  mutate(config = as.numeric(config)) %>%
  left_join(info_runs) %>%
  mutate(rel_err = if_else(obs != 0, abs(error)/obs, 0),
         rel_err = rel_err * 100)

errors_mod <- errors_filt_artif %>%
  select(-obs, -pred, -max_obs, -mean_obs, -error, -abs_error, 
         -sc_rel_error, -city, -N, -P, -Q, -it, -comment, -mae, 
         -run, -sensor_type) %>%
  filter(variable == "wm") %>%
  filter(id >= 500, id <= 1000) %>%
  filter(!(filt == "enkf" & case != "case2")) %>%
  mutate(filt = factor(filt,
                       levels = c("enkf", "ukf", "pf"),
                       labels = c("Ensemble KF", "Unscented KF", 
                                  "Particle Filter"),
                       ordered = TRUE),
         config_ = if_else(config == 1 | config == 4, 10,
                           if_else(config == 2 | config == 5, 30, 
                                   if_else(config == 3 | config == 6, 50, 0)))) %>%
  filter(config_ != 0) %>%
  mutate(R_ = factor(config_,
                     levels = c("10", "30", "50"),
                     labels = c("10%", "30%", "50%"),
                     ordered = TRUE)) %>%
  filter(frequency == 1,
         config <= 6, 
         rel_err > 0, filt != "pf") %>%
  mutate(type = if_else(config <= 3, "Fixed", "Variable")) %>%
  unite("id_out", frequency, filt, meas_var, exp, config_) %>%
  mutate(id_ins = as.numeric(as.factor(type)))

combs <- t(combn(1:2, 2))
head(errors_mod)

combinations <- merge(unique(errors_mod$id_out), combs) %>%
  rename(id_out = x,
         g1 = V1,
         g2 = V2) %>%
  mutate(D = NA,
         p = NA)

for (it in 1:nrow(combinations)){
  
  data_filt <- filter(errors_mod, id_out == combinations$id_out[it])
  
  g1 <- filter(data_filt, id_ins == combinations$g1[it])
  g2 <- filter(data_filt, id_ins == combinations$g2[it])
  
  temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
  
  combinations$D[it] <- temp$statistic
  combinations$p[it] <- temp$p.value
  
}

ks_all <- combinations %>%
  separate(id_out, into = c("frequency", "filt", "meas_var", "exp", "config_"), sep = "_")

write.csv(ks_all, file = "./tables/metrics/assim/ks_controlled.csv", row.names = FALSE)


# KS Assim Artif Controlled 50 vs 100 -------------------------------------
errors_filt_artif <- all_err %>%
  mutate(config = as.numeric(config)) %>%
  left_join(info_runs) %>%
  mutate(rel_err = if_else(obs != 0, abs(error)/obs, 0),
         rel_err = rel_err * 100)

errors_mod <- errors_filt_artif %>%
  select(-obs, -pred, -max_obs, -mean_obs, -error, -abs_error, 
         -sc_rel_error, -city, -N, -P, -Q, -it, -comment, -mae, 
         -run, -sensor_type) %>%
  filter(variable == "wm") %>%
  filter(id >= 500, id <= 1000) %>%
  filter(!(filt == "enkf" & case != "case2")) %>%
  mutate(filt = factor(filt,
                       levels = c("enkf", "ukf", "pf"),
                       labels = c("Ensemble KF", "Unscented KF", 
                                  "Particle Filter"),
                       ordered = TRUE),
         config_ = if_else(config == 8 | config == 4, 10,
                           if_else(config == 9 | config == 5, 30, 
                                   if_else(config == 10 | config == 6, 50, 0)))) %>%
  filter(config_ != 0) %>%
  mutate(R_ = factor(config_,
                     levels = c("10", "30", "50"),
                     labels = c("10%", "30%", "50%"),
                     ordered = TRUE)) %>%
  filter((config >= 4 & config <= 6) | (config >= 8 & config <= 10), 
         rel_err > 0, 
         filt != "Particle Filter") %>%
  unite("id_out", filt, meas_var, exp, config_, remove = FALSE) %>%
  #filter(frequency != 0.1) %>%
  mutate(id_ins = frequency)

combs <- t(combn(unique(errors_mod$id_ins), 2))
head(errors_mod)

combinations <- merge(unique(errors_mod$id_out), combs) %>%
  rename(id_out = x,
         g1 = V1,
         g2 = V2) %>%
  mutate(D = NA,
         p = NA)

it <- 1
for (it in 1:nrow(combinations)){
  
  data_filt <- errors_mod[errors_mod$id_out == combinations$id_out[it], ]
  
  g1 <- data_filt[data_filt$id_ins == combinations$g1[it], ]
  g2 <- data_filt[data_filt$id_ins == combinations$g2[it], ]
  
  temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
  
  combinations$D[it] <- temp$statistic
  combinations$p[it] <- temp$p.value
  
}

ks_all <- combinations %>%
  separate(id_out, into = c("filt", "meas_var", "exp", "config_"), sep = "_")

write.csv(ks_all, file = "./tables/metrics/assim/ks_controlled2.csv", row.names = FALSE)



# Errors - real - state = assim -------------------------------------------
errors_filt_mod <- errors_filt %>%
  filter(id < 500) %>%
  left_join(info_runs) %>%
  select(dat, variable, obs, pred, exp, id, config, filt, state_var, meas_var,
         calib, frequency) %>%
  filter(variable == state_var) %>%
  mutate(error = obs - pred,
         rel_error = (obs - pred)/obs) %>%
  group_by(variable, exp, config, filt, state_var, meas_var,
           calib, frequency) %>%
  summarise(rmse = sqrt(mean(error*error))) %>%
  spread(exp, rmse)

write.csv(errors_filt_mod, file = "./tables/metrics/assim/rmse_real.csv", 
          row.names = FALSE)

error_last <- errors_filt %>%
  filter(id < 500) %>%
  left_join(info_runs) %>%
  select(dat, variable, obs, pred, exp, id, config, 
         filt, state_var, meas_var, frequency, rep, calib) %>%
  mutate(error = obs - pred,
         rel_error = (obs - pred)/obs) %>%
  group_by(exp) %>%
  filter(dat == max(dat), variable == state_var) %>%
  select(variable, state_var, meas_var, exp, error, config, filt, rep,
         calib, frequency) %>%
  spread(exp, error)

write.csv(error_last, file = "./tables/metrics/assim/last_real.csv", 
          row.names = FALSE)

# Errors - real - yield ---------------------------------------------------
rmse <- errors_filt %>%
  filter(id < 500) %>%
  left_join(info_runs) %>%
  select(dat, variable, obs, pred, exp, id, config, rep,
         filt, state_var, meas_var, frequency) %>%
  filter(variable == "wm") %>%
  mutate(error = obs - pred,
         rel_error = (obs - pred)/obs,
         se = error * error) %>%
  group_by(variable, exp, frequency, config, filt, state_var, meas_var, rep) %>%
  summarise(rmse = sqrt(mean(se))) %>%
  ungroup() %>%
  group_by(variable, exp, frequency, config, filt, state_var, meas_var) %>%
  summarise(rmse_max = max(rmse),
            rmse_min = min(rmse)) %>%
  gather(rmse_min, rmse_max, key = "metric", value = "rmse") %>%
  spread(exp, rmse)

write.csv(rmse, file = "./tables/metrics/assim/rmse_real_wm.csv", 
          row.names = FALSE)

metrics_nozero <- errors_filt %>%
  filter(id < 500) %>%
  left_join(info_runs) %>%
  select(dat, variable, obs, pred, exp, id, config, rep,
         filt, state_var, meas_var, frequency) %>%
  filter(variable == "wm", obs > 0) %>%
  mutate(error = obs - pred,
         rel_error = (obs - pred)/obs,
         se = error * error) %>%
  group_by(variable, exp, frequency, config, filt, state_var, meas_var, rep) %>%
  summarise(rmse = sqrt(mean(se)),
            rel_err = rel_error) %>%
  ungroup() %>%
  group_by(variable, exp, frequency, config, filt, state_var, meas_var) %>%
  summarise(rmse_max = max(rmse),
            rmse_min = min(rmse),
            rel_error_max = max(rel_err),
            min_error_max = min(rel_err))

write.csv(metrics_nozero, file = "./tables/metrics/assim/metrics_real_wm_nozero.csv", 
          row.names = FALSE)

# Error consistency index -------------------------------------------------
# ECI = cov(err_var_1, err_var_2) / sqrt(var(err_var_1) * var(err_var_2))
# If ECI is negative, the error is inconsitent. Small negative ECI indicates 
# inconsistent errors and large chance of EnKF to fail.


# Residual / Innovation ---------------------------------------------------

resid <- upd_states_out %>%
  filter(variable == "Resid") %>%
  group_by(id, das) %>%
  summarise(res_mean = mean(measurement),
            res_sd = sd(measurement))

# Normalized innovations

calc <- upd_states_out %>%
  filter(variable == "Resid" | variable == "P_upd_m" | variable == "R") %>%
  spread(variable, measurement) %>%
  rename(P = P_upd_m) %>%
  mutate(ni = Resid / sqrt(P + R))

calc_summ <- calc %>%
  group_by(id) %>%
  summarise(mean_ni = mean(ni),
            sd_ni = sd(ni))

# ggplot() +
#   facet_wrap("id") +
#   geom_histogram(data=calc, aes(ni))

# Gain --------------------------------------------------------------------

gain <- upd_states_out %>%
  filter(variable == "Gain") %>%
  group_by(id, das) %>%
  summarise(gain_mean = mean(measurement),
            gain_sd = sd(measurement))


# SA Case 1 ---------------------------------------------------------------
# 
# all_Si_Filt <- all_files_si %>%
#   filter(case == 1)
# 
# all_si_l <- list()
# 
# for (it in 1:nrow(all_Si_Filt)){
# 
# 
#   si <- read.csv(all_Si_Filt$path[it]) %>%
#     mutate(path = all_Si_Filt$path[it])
# 
#   if (all_Si_Filt$model[it] == "simple"){
#     si <- si %>%
#       rename(w = tt_sum,
#              wm = plant_yield,
#              lai = f_solar)
#   }
# 
#   si <- si %>%
#     left_join(all_Si_Filt[it, ]) %>%
#     mutate(cod = str_extract(str_extract(path, "path[:digit:]{3}"),
#                              "[:digit:]{3}"),
#            cod = as.numeric(cod)) %>%
#     select(-path)
# 
#   all_si_l[[it]] <- si
# 
# 
# }
# 
# all_si <- rbindlist(all_si_l, fill = TRUE)
# save(all_si, file = "./tables/results_SA/all_si.RData")

load("./tables/results_SA/all_si.RData")

all_si_avg <- all_si %>%
  filter(index == "ST", n_samples == 5000) %>%
  filter(dat > 40, wm < 10) %>%
  gather(w, lai, biomass, wm, n, wf,
         key = "variable", value = "index") %>%
  group_by(factors, model, city, variable) %>%
  summarise(index_avg = mean(index)) %>%
  arrange(model, desc(index_avg)) %>%
  ungroup() %>%
  group_by(model, city, variable) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 2)
  
write.csv(all_si_avg, file = "./tables/results_SA/case1_highest_indices.csv")


# SA Case 0 ---------------------------------------------------------------
# factors_filt <- outputs_extreme %>%
#   filter(grepl("paramsMask", path_fac), case == 0) %>%
#   select(-path_out) %>%
#   distinct()
# 
# all_results_plot_l <- list()
# for (it in 1:nrow(factors_filt)){
# 
#   # Carrego o arquivo e insiro o numero de linha referente a cada teste de
#   # parametro. Mais do que isso, faco o mod
#   # pra achar a pasta e monto o nome da pasta em que estao os resultados
#   # no df de param filtrado e arrumado. Dai eu carrego numa lista e depois dou
#   # um rbind? para todos os resultados que eu quero plotar, desde que uma coluna
#   # tenha o id dos parametros.
# 
#   id_run <- factors_filt[it, "id"]
#   factors_values <- read.csv(factors_filt$path_fac[it]) %>%
#     mutate(id = id_run,
#            it_fac = row_number()-1) %>%
#     gather(-id, -it_fac, key = "param", value = "value")
# 
#   outputs_filt <- outputs_extreme %>%
#     filter(outputs_extreme$id == id_run, grepl("paramsMask", path_fac)) %>%
#     mutate(it_fac = str_extract(path_out, "it[[:digit:]]+"),
#            it_fac = as.numeric(gsub("it", "", it_fac))) %>%
#     arrange(it_fac) %>%
#     select(-path_fac, -run, -n_samples, -n_splits, -sensor_type,
#            -cont, -config, -comment)
# 
#   for (it2 in 1:nrow(outputs_filt)){
# 
#     temp <- read.csv(outputs_filt$path_out[it2])
# 
#     temp_ <- temp %>%
#       mutate(das = row_number(),
#              it_fac = outputs_filt$it_[it2],
#              id = factors_filt$id[it]) %>%
#       left_join(outputs_filt[it2, ]) %>%
#       left_join(factors_values) %>%
#       select(-path_out)
# 
#     if (factors_filt$model[it] == "simple"){
# 
#       temp_ <- temp_ %>%
#         mutate(k = 0.58,
#                lai = log(1 - f_solar)/-k) %>%
#         rename(w = biomass,
#                wm = plant_yield) %>%
#         select(-f_solar, -k, -tt_sum)
# 
#     }
# 
#     id_full <- paste(it, it2, sep = "_")
#     all_results_plot_l[[id_full]] <- temp_
# 
#   }
# 
# }
# 
# all_results_plot_ext <- Reduce(bind_rows, all_results_plot_l)
# save(all_results_plot_ext, file = "./tables/results_SA/all_curves_weather.RData")

load("./tables/results_SA/all_curves_weather.RData")

ranges_unc_weather <- all_results_plot_ext %>%
  gather(-c("das", "it_fac", "id",
            "case", "model", "city", "exp",
            "param", "value"),
         key = "variable", value = "measurement") %>%
  group_by(param, variable, model, city, exp) %>%
  mutate(flag = if_else(value >= quantile(value, 0.9), "max", 
                        if_else(value <= quantile(value, 0.1), "min", "none"))) %>%
  ungroup() %>%
  filter(flag != "none", !is.na(measurement)) %>%
  group_by(param, variable, model, city, flag, exp) %>%
  filter(das == max(das)) %>%
  summarise(avg_var = mean(measurement)) %>%
  spread(flag, avg_var) %>%
  mutate(dif = max - min) %>%
  filter(variable == "wm")

write.csv(ranges_unc_weather,
          file = "./tables/results_SA/ranges_unc_weather.csv",
          row.names = FALSE)
