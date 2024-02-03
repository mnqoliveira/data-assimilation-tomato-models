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
            rrmse = rmse/mean(obs),
            me = mean(error),
            mae = mean(abs_error),
            mape = mean(r_abs_error),
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
  gather(rmse, me, mae, sd_mae, mape, rrmse, key = "metric", value = "measurement") %>%
  spread(exp, measurement)

write.csv(tomgro_gnvMod, file = "./tables/metrics/simul/metrics_gnvMod.csv", 
          row.names = FALSE)

tomgro_calib <- errors_models %>%
  filter(model == "tomgro" | model == "vanthoor", city == "cps",
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
            (calib == "cps3" & (exp == "n05" | exp == "n06")) |
            (calib == "cps4" & (exp == "n07" | exp == "n08")))) %>%
  select(-calib) %>%
  gather(rmse, me, mae, sd_mae, mape, rrmse, key = "metric", value = "measurement") %>%
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
            (calib == "cps4" & (exp == "n07" | exp == "n08")))) %>%
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
            rrmse = rmse/mean(obs),
            me = mean(error),
            mae = mean(abs_error),
            mape = mean(r_abs_error),
            rmae = mae/mean(obs),
            sd_mae = sd(abs_error)) %>%
  ungroup() %>%
  arrange(model, city, variable, calib, exp) 

tomgro_gnvMod <- errors_models_nozero %>%
  filter(model == "tomgro", calib == "gnvIV", city == "cps") %>%
  gather(rmse, me, mae, sd_mae, mape, rrmse, rmae, key = "metric", value = "measurement") %>%
  spread(exp, measurement)

write.csv(tomgro_gnvMod, file = "./tables/metrics/simul/metrics_gnvMod_nozero.csv", 
          row.names = FALSE)

tomgro_calib <- errors_models_nozero %>%
  filter(model == "tomgro" | model == "vanthoor", city == "cps",
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
            (calib == "cps3" & (exp == "n05" | exp == "n06")) |
            (calib == "cps4" & (exp == "n07" | exp == "n08")))) %>%
  select(-calib) %>%
  gather(rmse, me, mae, sd_mae, rmae, mape, rrmse, key = "metric", value = "measurement") %>%
  spread(exp, measurement)

write.csv(tomgro_calib, file = "./tables/metrics/simul/metrics_calib_nozero.csv", 
          row.names = FALSE)

# Errors Assim Artif ------------------------------------------------------
# Calculated with simulations as references of truth
simulations <- models_all %>%
  filter(city == "cps", model == "tomgro",
         variable == "wm",
         # variable != "dw", variable != "rm", variable != "pg",
         calib == "gnvIV" | (calib == "cpsIV" & sensor == "A"),
         exp == "n03" | exp == "n05" | exp == "n07")

error_notCalib <- simulations %>%
  spread(calib, measurement) %>%
  group_by(das, model, exp, variable) %>%
  mutate(cpsIV = mean(cpsIV, na.rm = TRUE)) %>%
  group_by(das, model, exp, variable, sensor) %>%
  summarise(error = gnvIV - cpsIV, 
            abs_error = abs(error),
            rel_err = if_else(cpsIV != 0, abs_error/cpsIV, 0)) %>%
  ungroup() %>%
  mutate(filt = "none",
         calib = "gnvIV",
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

error <- Reduce(rbind, errors_l) %>%
  mutate(selected = case_when(filt == "none" | filt == "ukf" ~ 1,
                              filt == "enkf" & case == "case2" ~ 1,
                              TRUE ~ 0))

write.csv(error, file = "./tables/metrics/assim/artif/metrics_wm.csv", 
          row.names = FALSE)

# Errors - real - state = assim -------------------------------------------
errors_filt_mod <- errors_filt %>%
  filter(id < 500) %>%
  left_join(info_runs) %>%
  select(dat, variable, obs, pred, exp, id, config, filt, state_var, meas_var,
         calib, frequency) %>%
  filter(variable == state_var) %>%
  mutate(error = pred - obs,
         rel_error = (pred - obs)/obs) %>%
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
  mutate(error = pred - obs,
         rel_error = (pred - obs)/obs) %>%
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
  select(dat, variable, obs, pred, exp, id, config, rep, calib,
         filt, state_var, meas_var, frequency) %>%
  filter(variable == "wm") %>%
  mutate(error = pred - obs,
         rel_error = (pred - obs)/obs,
         se = error * error) %>%
  group_by(variable, exp, frequency, config, calib, filt, state_var, meas_var, rep) %>%
  summarise(rmse = sqrt(mean(se))) %>%
  ungroup() %>%
  group_by(variable, exp, frequency, calib, config, filt, state_var, meas_var) %>%
  summarise(rmse_max = max(rmse),
            rmse_min = min(rmse),
            rmse_mean = mean(rmse)) %>%
  gather(rmse_min, rmse_max, rmse_mean, key = "metric", value = "rmse") %>%
  spread(exp, rmse)

write.csv(rmse, file = "./tables/metrics/assim/rmse_real_wm.csv", 
          row.names = FALSE)

metrics_nozero <- errors_filt %>%
  filter(id < 500) %>%
  left_join(info_runs) %>%
  select(dat, variable, obs, pred, exp, id, config, rep,
         filt, state_var, meas_var, frequency) %>%
  filter(variable == "wm", obs > 0) %>%
  mutate(error = pred - obs,
         rel_error = (pred - obs)/obs,
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

resid <- upd_assim %>%
  filter(variable == "Resid") %>%
  group_by(id, das) %>%
  summarise(res_mean = mean(measurement),
            res_sd = sd(measurement))

# Normalized innovations

calc <- upd_assim %>%
  filter(variable == "Resid" | variable == "P_upd_m" | variable == "R") %>%
  spread(variable, measurement) %>%
  rename(P = P_upd_m) %>%
  mutate(ni = Resid / sqrt(P + R))

calc_summ <- upd_assim %>%
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
