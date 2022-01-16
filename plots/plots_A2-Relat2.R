# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("ggplot2")
library(tidyverse)
library("lubridate")
library("RColorBrewer")

#install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
# 
library(patchwork)

# Assimilation ------------------------------------------------------------
# Update results
source("./07a-orgSimOut.R")
source("./07b-orgObs_plot.R")

# Memory is not cleared after this line and some variables remain
source("./07c-orgAssim.R")

# Load data 
load("../data/plot_theme.RData")

models_all <- read.csv( "../tables/results_simul/results_simulations_all.csv")

obs <- read.csv("../data/observations_proc.csv")
obs_last <- read.csv( "../data/observations_last.csv")

assim_exp <- read.csv("../tables/results_assim/all/assim_exp.csv", )
all_states_out <- read.csv("../tables/results_assim/all/all_states.csv")
upd_states_out <- read.csv("../tables/results_assim/all/upd_states.csv")

# Load dictionary of measurement
dict <- read.csv("../tables/dictionary_paramsFilter.csv")

codes_exp <- read.csv("../tables/codes_exp.csv")

# Observations
obs_sum <- read.csv("../data/observations/full_set_obs.csv") %>%
  filter(city == "cps")

# Plot variables

# Extended names
plot_mods <- c(tomgro =  "Calibrated", ukf = "UKF assimilation",
               GNVMod = "Not calibrated")

codes_plants <- c("c01" = "Cycle 1", "c02" = "Cycle 2", "c03" = "Cycle 3",
                  "c04" = "Cycle 4",
                  "n01" = "Cycle 1, Plant 1", "n02" = "Cycle 1, Plant 2", 
                  "n03" = "Cycle 2, Plant 1", "n04" = "Cycle 2, Plant 2", 
                  "n04" = "Cycle 3, Plant 1", "n06" = "Cycle 3, Plant 2")
codes_plants_df <- data.frame(exp = names(codes_plants),
                              plant_id = codes_plants,
                              row.names = NULL)

dict_mod <- dict %>%
  mutate(name_unit = paste(name_plot, unit),
         name_unit2 = paste0(name_plot, "\n", unit),
         name_unit_f = fct_reorder(name_unit, order)) %>%
  select(-R, -P, -Q)

# Process data

simulations <- models_all %>%
  filter(city == "cps", exp == "n03" | exp == "n04")

allStates <- all_states_out   %>%
  filter(city == "cps", exp == "n03" | exp == "n04")

updState <- upd_states_out   %>%
  filter(city == "cps", exp == "n03" | exp == "n04")
# %>%
#   left_join(dict_mod)

obs_mod <- obs %>%
  filter(city == "cps", exp == "n03" | exp == "n04")

obs_vline <- obs_sum %>%
  filter(!is.na(cycle), cycle != 1) %>%
  select(cycle, dat, lai, lai_abv, lai_lat, node) %>%
  gather("lai", "lai_abv", "lai_lat", 
         key = "variable",
         value = "measurement") %>%
  filter(cycle == 2, node != "calib", 
         variable == "lai_lat", !is.na(measurement)) %>%
  mutate(exp = if_else(node == 1, "n03", "n04"),
         meas_mod = (measurement - 0.0128991)/0.0006926,
         variable = "w") %>%
  rename(das = dat) %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx])

exps <- assim_exp %>%
  select(-var_out) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(dict_mod) 

# Assimilated vs non-assimilated
it <- 1

#for (it in 1:nrow(exps)) {

upd <- allStates %>%
  filter(das != 0) %>%
  filter(model == exps$model[it],
         meas_var == exps$meas_var[it],
         var_est == exps$variable[it],
         city == exps$city[it],
         calib == exps$calib[it]) %>%
  mutate(model = tolower(filt))

simul_calib <- simulations %>%
  filter(model == exps$model[it],
         # Calibrated model
         calib == "cps1",
         city == exps$city[it]) %>%
  select(-calib, -city)

simul_NotCalib <- simulations %>%
  filter(model == exps$model[it],
         # Not calibrated model
         calib == "gnvMod",
         city == exps$city[it]) %>%
  mutate(model = "GNVMod") %>%
  select(-calib, -city)

assim <- bind_rows(simul_calib, simul_NotCalib, upd) %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df)

obs_plot <- obs_mod %>%
  filter(variable == "lai" | variable == "n" | variable == "w" | 
           variable == "wf" | variable == "wm") %>%
  select(-model) %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  filter(dat != max(dat))

obs_last_i <- obs_last %>%
  #filter(cycle == allStates$cycle[it]) %>%
  rename(das = dat) %>%
  filter(variable == "lai" | variable == "n" | variable == "w" | 
           variable == "wf" | variable == "wm") %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx],
         exp = if_else(city_exp == "cps_n03", "n03", "n04"))

if (exps$calib[it] == exps$city[it]){
  title <-paste0("Assimilation of ", exps$name_plot[it], 
                 " for ", exps$var_plot[it],
                 "\nCalibrated model")
} else {
  title <-paste0("Assimilation of ", exps$name_plot[it], 
                 " for ", exps$var_plot[it],
                 "\nCalibrated vs Non-calibrated model vs UKF Non-Calibrated")
}

ggplot() +
  facet_grid(variable2 ~ exp, scales = "free",
             labeller = labeller(variable2 = label_wrap_gen(18),
                                 exp = codes_plants)) + 
  geom_line(data = assim, aes(das, measurement, 
                              colour = model), 
            size = 0.5) +
  geom_point(data = obs_last_i, aes(das, measurement),
             colour = "#e41a1c",
             size = 1.5) +
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 1) + 
  geom_errorbar(data = obs_plot,
                aes(dat, 
                    ymin=measurement-measurement_sd, 
                    ymax=measurement+measurement_sd), 
                width = 0.05) +
  geom_point(data = obs_vline, 
             mapping = aes(das, meas_mod), 
             shape = 4, colour = "red") +
  labs(title = "",
       x = "Days after simulation started", 
       y = "",
       color = "") + 
  mytheme   +
  scale_colour_manual(name="Model",
                      breaks=c("GNVMod", "tomgro", "ukf"),
                      labels=c("RT - Not calibrated", 
                               "RT - Calibrated",
                               "RT - Not Calibrated + UKF"),
                      values = c("#4daf4a", "#377eb8", "#984ea3")) +
  guides(colour = guide_legend(override.aes = list(size=5)))

exp <- paste(allStates[1, "var_out"],
             exps[it, "variable"],
             exps[it, "city"],
             exps[it, "calib"],
             exps[it, "filter"],
             exps[it, "model"],
             exps[it, "meas_var"],
             sep = "-")

plot_file_name <- paste0('../figures/relat2/example_assim.png')
ggsave(plot_file_name,
       width = 18.0, height = 20.0, units = "cm",
       family = "serif")
# }



# Example Simul -----------------------------------------------------------

# Update results
source("./07a-orgSimOut.R")
source("./07b-orgObs_plot.R")

# Load data
load("../data/plot_theme.RData")

models_all <- read.csv("../tables/results_simul/results_simulations_all.csv")
obs <- read.csv("../data/observations_proc.csv")
obs_last <- read.csv("../data/observations_last.csv")

photo_all <- read.csv("../tables/results_simul/results_photo_all.csv")

# Plot variables
plot_states <- c("lai" = "LAI\n[m² leaves/m² soil]", 
                 "n" = "N\n[number of nodes/\nm² soil]", 
                 "w" = "Aboveground\n biomass\n[g D.M./m² soil]", 
                 "wf" = "Fruit weight\n[g D.M./m² soil]", 
                 "wm" = "Mature fruit weight\n[g D.M./m² soil]")
plot_cities <- c("cps" = "Campinas",
                 "arA" = "Campinas")
plot_treats_cpA <- c("n01" = "2013 - T1", "n06" = "2013 - T2")

codes_plants <- c("c01" = "Cycle 1", "c02" = "Cycle 2", "c03" = "Cycle 3",
                  "c04" = "Cycle 4",
                  "n01" = "Plant 1", "n02" = "Plant 2", 
                  "n03" = "Plant 1, Cycle 2", "n04" = "Plant 2, Cycle 2", 
                  "n05" = "Plant 2, Cycle 3", "n06" = "Plant 2, Cycle 3")
codes_plants_df <- data.frame(exp = names(codes_plants),
                              exp_name = codes_plants,
                              row.names = NULL)

# Process data
models <- models_all %>%
  filter(city == "cps") %>%
  mutate(city_exp = paste(city, exp, sep = "_"),
         exp_full = paste(model, city_exp, sep = "_")) %>%
  # filter(exp == "n03" | exp == "n04" | das <= 53)
  filter(exp == "n03" | exp == "n04" | das <= 75)

photo <- photo_all %>%
  filter(city == "cps") %>%
  mutate(city_exp = paste(city, exp, sep = "_"),
         exp_full = paste(model, city_exp, sep = "_"))

obs_mod <- obs %>%
  filter(city == "cps", 
         variable %in% names(plot_states))

# Plot 
model_plot <- models %>%
  filter(model == "tomgro", city == "cps")

obs_plot <- obs_mod

ggplot() +
  facet_grid(variable ~ exp,
             labeller = labeller(variable = plot_states,
                                 exp = codes_plants),
             scales = "free",
             space = "free_x") +
  geom_line(data = model_plot, aes(das, measurement, col = calib),
            size = 0.5) +
  geom_point(data = obs_plot, aes(das, measurement),
             size = 0.5, shape = 15) +
  labs(x = "Days after simulation started",
       y = "") +
  mytheme +
  scale_colour_manual(name="Calibration",
                      breaks=c("cps1", "cps2", "cps3", "gnvMod"),
                      labels=c("Calibrated - Cycle 2", 
                               "Calibrated - Cycle 3",
                               "Calibrated- Both",
                               "Not calibrated"),
                      values = c("#4daf4a", "#377eb8", "#e41a1c",
                                 "#984ea3")) +
  guides(colour = guide_legend(override.aes = list(size=5)))

# namefile <- "../figures/example_simul.png"
# ggsave(namefile,
#        width = 17.0, height = 15.0, units = "cm",
#        family = "serif", dpi = 320)

