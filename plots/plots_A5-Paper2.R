rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")
Sys.setlocale("LC_TIME", "Portuguese")

# Libraries ---------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(lubridate)
#library(GGally)

library(paletteer)
library(patchwork)

# Load theme --------------------------------------------------------------
load("../data/plot_theme_vert.RData")


# Load data ---------------------------------------------------------------
# Dates
cycle_dates <- read.csv("../data/cycle_dates.csv")
codes_exp <- read.csv("../tables/codes_exp.csv")

# Harvests
harvests <- read.csv("../data/observations/monitoring/harvests.csv")

# Cover lat data
cover_lat_calib <- read.csv("../data/observations/monitoring/lai/lai_lat_calib_ids.csv")
cover_abv_calib <- read.csv("../data/observations/monitoring/lai/lai_abv_calib_ids.csv")
height_calib <- read.csv("../data/observations/monitoring/dry_mass_aboveground/height_calib_ids.csv")
wf_lat_calib <- read.csv("../data/observations/monitoring/dry_mass_fruit/wf_lat_calib_ids.csv")
wm_lat_calib <- read.csv("../data/observations/monitoring/dry_mass_mature_fruit/wm_lat_calib_ids.csv")

# Load dictionary of measurement
dict <- read.csv("../tables/dictionary_paramsFilter.csv")

# From the first harvest on, the data will be ignored
wm_lat <- wm_lat_calib %>%
  left_join(cycle_dates) %>%
  full_join(harvests) %>%
  arrange(cycle, id, date) %>%
  group_by(cycle, id) %>%
  mutate(type_ = if_else(is.na(type), 0, 1),
         flag = if_else(sum(type_) >= 1, 1, 0)) %>%
  filter(flag == 0) %>%
  ungroup() %>%
  select(-dat, -type, -type_, -flag, -fm, -dm) %>%
  distinct()

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

# Assimilation
info_runs <- read.csv("../tables/runs_Filter.csv") %>%
  mutate(exp_int = exp,
         exp = paste0("n0", exp_int)) %>%
  select(-it, -comment)

# Assimilation results
load("../tables/results_DA/aux_files/all_states.RData")
#load("../tables/results_DA/aux_files/upd_states.RData")

# Simulations
models_all <- read.csv( "../tables/results_simul/results_simulations_all.csv")

# Observations
obs <- read.csv("../data/observations/monitoring/observations_proc.csv")
obs_last <- read.csv( "../data/observations/monitoring/observations_last.csv")
# Calibration ids
obs_ids <- read.csv("../data/observations/monitoring/obs_exp_all_ids.csv")
# Full monitoring and experiment
obs_all <- read.csv("../data/observations/full_set_obs.csv") %>%
  filter(city == "cps")

# Labels ------------------------------------------------------------------

plot_vars = c("leaf_area" = "Leaf area [m² leaves/plant]",
              "lai_lat" = "Green cover area \n(side view) [m²/plant]",
              "lai_abv" = "Green cover area \n(above view) [m²/plant]",
              "height" = "Height [m]",
              "wf_lat" = "Total area of fruits \n[m²/plant]",
              "w_fm_full" = "System wet mass [g]",
              "wf_plant" = "Fruit dry mass\n[g DM/plant]",
              "w_plant" = "Aboveground\n dry mass\n[g D.M./plant]",
              "w_plant_fm" = "Aboveground\n fresh mass\n[g F.M./plant]",
              "wf" = "Fruit dry mass\n[g DM/m² soil]",
              "w" = "Aboveground dry mass\n[g D.M./m² soil]",
              "wm" = "Mature fruit dry mass\n[g D.M./m² soil]")

plot_vars_unitless = c("leaf_area" = "Leaf area",
              "lai_lat" = "Green cover area \n(side view)",
              "lai_abv" = "Green cover area \n(above view)",
              "height" = "Height",
              "wf_lat" = "Total area of fruits",
              "w_fm_full" = "System wet mass",
              "wf_plant" = "Fruit dry mass",
              "w_plant" = "Aboveground\n dry mass",
              "w_plant_fm" = "Aboveground\n fresh mass",
              "wf" = "Fruit dry mass",
              "w" = "Aboveground dry mass",
              "wm" = "Mature fruit dry mass")


plot_vars_1l = c("leaf_area" = "Leaf area [m² leaves/plant]",
              "lai_lat" = "Green cover area (side view) [m²/plant]",
              "lai_abv" = "Green cover area (above view) [m²/plant]",
              "height" = "Height [m]",
              "wf_lat" = "Total area of fruits [m²/plant]",
              "w_fm_full" = "System wet mass [g]",
              "wf_plant" = "Fruit dry mass [g DM/plant]",
              "w_plant" = "Aboveground dry mass [g D.M./plant]",
              "w_plant_fm" = "Aboveground fresh mass [g F.M./plant]",
              "wf" = "Fruit dry mass [g DM/m² soil]",
              "w" = "Aboveground dry mass [g D.M./m² soil]",
              "wm" = "Mature fruit dry mass [g D.M./m² soil]")


plot_cities <- c("cps" = "Campinas")

# Extended names
plot_mods <- c(tomgro =  "Calibrated", ukf = "UKF assimilation",
               enkf = "EnKF assimilation",
               GNVMod = "Not calibrated")

codes_plants <- c("n03" = "Cycle 1, Plant 1", "n04" = "Cycle 1, Plant 2", 
                  "n05" = "Cycle 2, Plant 1", "n06" = "Cycle 2, Plant 2",
                  "n07" = "Cycle 3, Plant 1", "n08" = "Cycle 3, Plant 2")
codes_plants_df <- data.frame(exp = names(codes_plants),
                              plant_id = codes_plants,
                              row.names = NULL)

models_transf <- read.csv("../tables/models_obs.csv") %>%
  select(-name_plot, -var_plot, -order, -unit, -lang) %>%
  left_join(select(codes_exp, cycle))

# Org vars ----------------------------------------------------------------

dict_mod <- dict %>%
  filter(lang == "en") %>%
  mutate(name_unit = paste(name_plot, unit),
         name_unit2 = paste0(name_plot, "\n", unit),
         name_unit_f = fct_reorder(name_unit, order))

allStates <- all_assim %>%
  filter(id < 500)
  right_join(info_runs) 

# updState <- upd_states_out %>%
#   right_join(info_runs)

simulations <- models_all %>%
  filter(city == "cps", exp != "n01", exp != "n02",
         variable != "dw", variable != "rm", variable != "pg")

exps <- info_runs %>%
  rename(id_filt = id) %>%
  distinct(.keep_all = TRUE) %>%
  left_join(dict_mod) %>%
  unite(col = "city_exp", c("city", "exp"), sep = "_", remove = FALSE) %>%
  select(-filt, -city_exp, -exp, -id_filt, -R, -N, -P, -Q, -exp_int,
         -run, -case,
         -starts_with("X")) %>%
  distinct()

obs_mod <- obs %>%
  filter(city == "cps")


# Figure 1 - Growth monit and calib ---------------------------------------
calib <- obs_ids %>%
  full_join(cover_lat_calib) %>%
  full_join(cover_abv_calib) %>%
  full_join(height_calib) %>%
  full_join(wf_lat) %>%
  full_join(wm_lat) %>%
  mutate(w_fm_full = w_plant_fm) %>%
  gather("lai_abv", "lai_lat", "wf_lat", "height", "w_fm_full",
         key = "variable",
         value = "measurement") %>%
  mutate(node = "calib")
# %>%
#   group_by(cycle, date, variable, dat) %>%
#   summarise(meas_mean = mean(measurement, na.rm = T),
#             meas_sd = sd(measurement, na.rm = T))

dataset_plot <- obs_all %>%
  filter(!is.na(cycle), cycle != 1) %>%
  select(cycle, dat, lai_abv, lai_lat, wf_lat, height, w_fm_full, node) %>%
  gather("lai_abv", "lai_lat", "wf_lat", "height", "w_fm_full",
         key = "variable",
         value = "measurement") %>%
  filter(!is.na(measurement), node != "calib")

ggplot() +
  facet_grid(variable ~ cycle,  scales = "free",
             space = "free_x",
             labeller = labeller(variable = plot_vars,
                                 cycle = c("2" = "Cycle 1",
                                           "3" = "Cycle 2",
                                           "4" = "Cycle 3"))) +
  geom_point(data = dataset_plot,
             aes(x = dat, y = measurement,
                 fill = as.factor(node)), 
             size = 1.5, shape=21, colour="black", stroke=0.2) +
  geom_point(data = calib,
             aes(x = dat, y = measurement,
                 fill = as.factor(node)), 
             size = 1.5, shape=21, colour="black", stroke=0.2) +
  labs(x = "Days after transplanting",
       y = "") +
  scale_fill_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3),
                    name = "Plant",
                    breaks=c("calib", "1", "2"),
                    labels=c("Calibration samples", "Plant 1", "Plant 2")) +
  scale_x_continuous(breaks=seq(0,110,15)) +
  theme_vert  +
  theme(panel.background = element_rect(fill = "grey99"))

namefile <- "../paper/paper2-DAaplic/figures/Fig1_plant_monit.png"
ggsave(namefile,
       width = 15, height = 20, units = "cm", 
       family = "serif", dpi = 320)


# Figure 3 - Scatterplots -------------------------------------------------
dataset <- obs_ids %>%
  filter(!is.na(cycle), cycle != 1) %>%
  full_join(cover_abv_calib) %>%
  full_join(cover_lat_calib) %>%
  full_join(wf_lat) %>%
  full_join(wm_lat) %>%
  mutate(cycle_mod = factor(cycle,
                            levels = c(2, 3, 4),
                            labels = c("Cycle 1", "Cycle 2", "Cycle 3"))) %>%
  filter(!(((cycle == 2) | (cycle == 3)) & ((id == "42") | (id == "31"))),
         !((cycle == 4) & ((id == "54") | (id == "30"))))


comb <- data.frame(state = c("leaf_area", "leaf_area", "w_plant", "wf_plant"),
                   meas_var = c("lai_lat",  "lai_abv", "w_plant_fm", "wf_lat"))

# Scatter Lai_lat
plot2a <- ggplot() +
  geom_point(data = dataset,
             aes(x = lai_lat, y = leaf_area, fill = cycle_mod),
             shape=21, size=2) +
  labs(x = plot_vars_1l[match("lai_lat", names(plot_vars_1l))],
       y = plot_vars_1l[match("leaf_area", names(plot_vars_1l))]) +
  scale_discrete_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3), 
                        aesthetics = c("fill"),
                        name = "Cycle") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99")) +
  theme(legend.position = "none")

# Scatter Lai_abv
plot2b <- ggplot() +
  geom_point(data = dataset,
             aes(x = lai_abv, y = leaf_area, fill = cycle_mod),
             shape=21, size=2) +
  labs(x = plot_vars_1l[match("lai_abv", names(plot_vars_1l))],
       y = plot_vars_1l[match("leaf_area", names(plot_vars_1l))]) +
  scale_discrete_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3), 
                        aesthetics = c("fill"),
                        name = "Cycle") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99")) +
  theme(legend.position = "none")

# Scatter W
plot3a <- ggplot() +
  geom_point(data = dataset,
             aes(x = w_plant_fm, y = w_plant, fill = cycle_mod),
             shape=21, size=2) +
  labs(x = plot_vars_1l[match("w_plant_fm", names(plot_vars_1l))],
       y = plot_vars_1l[match("w_plant", names(plot_vars_1l))]) +
  scale_discrete_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3), 
                        aesthetics = c("fill"),
                        name = "Cycle") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))  +
  theme(legend.position = "none")

# Scatter Wf
plot3b <- ggplot() +
  geom_point(data = dataset,
             aes(x = wf_lat, y = wf_plant, fill = cycle_mod),
             shape=21, size=2) +
  labs(x = plot_vars_1l[match("wf_lat", names(plot_vars_1l))],
       y = plot_vars_1l[match("wf_plant", names(plot_vars_1l))]) +
  scale_discrete_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3), 
                        aesthetics = c("fill"),
                        name = "Cycle") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99")) +
  theme(legend.position = "none")

plot_all <- ((plot2a + plot2b) / (plot3a + plot3b))  +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

plot_name <- paste0("Figure2_pairs_all")
plot_file_name <- paste0('../paper/paper2-DAaplic/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 15, height = 15, units = "cm", 
       family = "serif", dpi = 320)

# Figure 4 - Assim Wf -----------------------------------------------------
upd <- allStates %>%
  filter(das != 0) %>%
  filter(meas_var == "wf_lat",
         frequency == 1,
         calib == "gnvMod") %>%
  mutate(model = filt) %>%
  filter(filt != "pf")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         ((calib == "cps3" & (exp == "n05" | exp == "n06")) |
            (calib == "cps4" & (exp == "n07" | exp == "n08"))),
         city == "cps") %>%
  select(-calib, -city)

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvMod",
         city == "cps") %>%
  mutate(model = "gnvMod") %>%
  select(-calib, -city)

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable != "n") %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df) %>%
  filter(variable == "wf" | variable == "wm") %>%
  filter(exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

assim <- upd %>%
  filter(variable != "n") %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df) %>%
  filter(variable == "wf" | variable == "wm") %>%
  filter(exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

obs_plot <- obs_mod %>%
  filter(variable == "wm" | variable == "wf") %>%
  select(-model) %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  filter(exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

obs_last_i <- obs_last %>%
  separate(city_exp, into = c("city", "exp")) %>%
  rename(das = dat) %>%
  filter(variable == "wm" | variable == "wf") %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  filter(exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

ggplot() +
  facet_grid(variable ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(variable = plot_vars,
                                 exp = codes_plants,
             model = plot_mods)) + 
  geom_point(data = assim, aes(das, measurement, 
                               fill = model), 
             size = 0.9, shape=21, colour="black", stroke=0.2) +
  geom_point(data = simuls, aes(das, measurement, 
                               colour = model), 
             size = 0.7, alpha = 0.9) +
  geom_point(data = obs_last_i, aes(das, measurement),
             size = 1.5) +
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.5) +
  geom_errorbar(data = obs_plot,
                aes(dat,
                    ymin=measurement-measurement_sd,
                    ymax=measurement+measurement_sd),
                width = 0.05) +
  labs(x = "Days after simulation started", 
       y = "") +
  theme_vert +
  scale_fill_manual(name="Approach",
                    breaks=c("tomgro", "gnvMod", "ukf", "enkf"),
                    labels=c("Open loop calibrated",
                             "Open loop not calibrated",
                             "UKF not calibrated",
                             "EnKF not calibrated"),
                        values = paletteer_d("RColorBrewer::Set1")[c(1, 4)],
                    drop = FALSE) +
  scale_colour_manual(name="",
                      breaks=c("tomgro", "gnvMod", "ukf", "enkf"),
                      labels=c("Open loop calibrated",
                               "Open loop not calibrated",
                               "UKF not calibrated",
                               "EnKF not calibrated"),
                      values = paletteer_d("RColorBrewer::Set1")[c(3, 2)],
                      drop = FALSE) +
  guides(fill=guide_legend(override.aes = list(size = 2), order=1,
                           title.theme = element_text(face = "bold", size=9)),
         colour=guide_legend(override.aes = list(size = 2), order=2,
                             title.theme = element_text(face = "bold", size=9))) +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- paste0("Figure4_wf_assim")
plot_file_name <- paste0('../paper/paper2-DAaplic/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 18, height = 10, units = "cm",
       family = "serif")


# Figure 5 - Assim W ------------------------------------------------------
upd <- allStates %>%
  filter(das != 0) %>%
  filter(meas_var == "w_fm_full",
         calib == "gnvMod",
         frequency == 1) %>%
  mutate(model = filt) %>%
  filter(filt != "pf")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         ((calib == "cps4" & (exp == "n07" | exp == "n08"))),
         city == "cps") %>%
  select(-calib, -city)

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvMod",
         city == "cps") %>%
  mutate(model = "gnvMod") %>%
  select(-calib, -city)

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable != "n") %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df) %>%
  filter(exp == "n07" | exp == "n08") %>%
  filter(variable == "w" | variable == "wm" | variable == "wf")

assim <- upd %>%
  filter(variable != "n") %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df) %>%
  filter(exp == "n07" | exp == "n08") %>%
  filter(variable == "w" | variable == "wm" | variable == "wf")

obs_plot <- obs_mod %>%
  filter(variable == "w" | variable == "wm" | variable == "wf") %>%
  select(-model) %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  filter(exp == "n07" | exp == "n08")

obs_last_i <- obs_last %>%
  separate(city_exp, into = c("city", "exp")) %>%
  rename(das = dat) %>%
  filter(variable == "w" | variable == "wm" | variable == "wf") %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  filter(exp == "n07" | exp == "n08")

ggplot() +
  facet_grid(variable ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(variable = plot_vars,
                                 exp = codes_plants,
                                 model = plot_mods)) + 
  geom_point(data = assim, aes(das, measurement, 
                               fill = model), 
             size = 0.9, shape=21, colour="black", stroke=0.2) +
  geom_point(data = simuls, aes(das, measurement, 
                                colour = model), 
             size = 0.7, alpha = 0.9) +
  geom_point(data = obs_last_i, aes(das, measurement),
             size = 1.5) +
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.7) +
  geom_errorbar(data = obs_plot,
                aes(dat,
                    ymin=measurement-measurement_sd,
                    ymax=measurement+measurement_sd),
                width = 0.05) +
  labs(x = "Days after simulation started", 
       y = "",
       color = "Plant") +
  theme_vert +
  scale_fill_manual(name="Approach",
                    breaks=c("tomgro", "gnvMod", "ukf", "enkf"),
                    labels=c("Open loop calibrated",
                             "Open loop not calibrated",
                             "UKF not calibrated",
                             "EnKF not calibrated"),
                    values = paletteer_d("RColorBrewer::Set1")[c(1, 4)],
                    drop = FALSE) +
  scale_colour_manual(name="",
                      breaks=c("tomgro", "gnvMod", "ukf", "enkf"),
                      labels=c("Open loop calibrated",
                               "Open loop not calibrated",
                               "UKF not calibrated",
                               "EnKF not calibrated"),
                      values = paletteer_d("RColorBrewer::Set1")[c(3, 2)],
                      drop = FALSE) +
  guides(fill=guide_legend(override.aes = list(size = 2), order=1,
                           title.theme = element_text(face = "bold", size=9),
                           nrow = 2),
         colour=guide_legend(override.aes = list(size = 2), order=2,
                             title.theme = element_text(face = "bold", size=9),
                             nrow = 2)) +
  theme(panel.background = element_rect(fill = "gray99"))
  
plot_name <- paste0("Figure5_w_fm_full_assim")
plot_file_name <- paste0('../paper/paper2-DAaplic/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 10, height = 15, units = "cm",
       family = "serif")

# Figure 6 - Different calibrations ---------------------------------------
upd <- allStates %>%
  filter(das != 0) %>%
  filter(meas_var == "wf_lat",
         frequency == 1,
         calib == "cps4") %>%
  mutate(model = filt) %>%
  filter(filt != "pf")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         ((calib == "cps3" & (exp == "n05" | exp == "n06"))),
         city == "cps") %>%
  select(-calib, -city)

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "cps4",
         city == "cps") %>%
  mutate(model = "gnvMod") %>%
  select(-calib, -city)

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable != "n") %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df) %>%
  filter(variable == "wf" | variable == "wm") %>%
  filter(exp == "n05" | exp == "n06")

assim <- upd %>%
  filter(variable != "n") %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df) %>%
  filter(variable == "wf" | variable == "wm") %>%
  filter(exp == "n05" | exp == "n06")

obs_plot <- obs_mod %>%
  filter(variable == "wm" | variable == "wf") %>%
  select(-model) %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  filter(exp == "n05" | exp == "n06")

obs_last_i <- obs_last %>%
  separate(city_exp, into = c("city", "exp")) %>%
  rename(das = dat) %>%
  filter(variable == "wm" | variable == "wf") %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  filter(exp == "n05" | exp == "n06")

ggplot() +
  facet_grid(variable ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(variable = plot_vars,
                                 exp = codes_plants,
                                 model = plot_mods)) + 
  geom_point(data = assim, aes(das, measurement, 
                               fill = model), 
             size = 0.7, shape=21, colour="black", stroke=0.1) +
  geom_point(data = simuls, aes(das, measurement, 
                                colour = model), 
             size = 0.5, alpha = 0.9) +
  geom_point(data = obs_last_i, aes(das, measurement),
             size = 1.5) +
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.5) +
  geom_errorbar(data = obs_plot,
                aes(dat,
                    ymin=measurement-measurement_sd,
                    ymax=measurement+measurement_sd),
                width = 0.05) +
  labs(x = "Days after simulation started", 
       y = "") +
  theme_vert +
  scale_fill_manual(name="Approach",
                    breaks=c("tomgro", "gnvMod", "ukf", "enkf"),
                    labels=c("Open loop calibrated",
                             "Open loop not calibrated",
                             "UKF not calibrated",
                             "EnKF not calibrated"),
                    values = paletteer_d("RColorBrewer::Set1")[c(1, 4)],
                    drop = FALSE) +
  scale_colour_manual(name="",
                      breaks=c("tomgro", "gnvMod", "ukf", "enkf"),
                      labels=c("Open loop calibrated",
                               "Open loop not calibrated",
                               "UKF not calibrated",
                               "EnKF not calibrated"),
                      values = paletteer_d("RColorBrewer::Set1")[c(3, 2)],
                      drop = FALSE) +
  guides(fill=guide_legend(override.aes = list(size = 1.5), order=1,
                           title.theme = element_text(face = "bold", size=9),
                           nrow = 2),
         colour=guide_legend(override.aes = list(size = 1.5), order=2,
                             title.theme = element_text(face = "bold", size=9),
                             nrow = 2)) +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- paste0("Figure6_wf_assim_cps4")
plot_file_name <- paste0('../paper/paper2-DAaplic/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 10, height = 10, units = "cm",
       family = "serif")
