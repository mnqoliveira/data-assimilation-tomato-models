"""

Visualization used in

@inproceedings{sbiagro,
 author = {Monique Oliveira and Luiz Rodrigues},
 title = {Monitoramento de plantas em casas de vegetação para assimilação de dados},
 booktitle = {Anais do XIII Congresso Brasileiro de Agroinformática},
 location = {Evento Online},
 year = {2021},
 keywords = {},
 issn = {0000-0000},
 pages = {215--224},
 publisher = {SBC},
 address = {Porto Alegre, RS, Brasil},
 url = {https://sol.sbc.org.br/index.php/sbiagro/article/view/18393}
}

"""

rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")
Sys.setlocale("LC_TIME", "Portuguese")

# Libraries ---------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(lubridate)
library(GGally)

# Load theme --------------------------------------------------------------
load("./data/plot_theme_vert.RData")


# Load data ---------------------------------------------------------------
# Dates
cycle_dates <- read.csv("./data/cycle_dates.csv")
codes_exp <- read.csv("./tables/codes_exp.csv")

# Harvests
harvests <- read.csv("./data/observations/monitoring/harvests.csv")

# Calibration ids
obs_ids <- read.csv("./data/observations/monitoring/obs_exp_all_ids.csv")

# Full monitoring and experiment
obs_all <- read.csv("./data/observations/full_set_obs.csv") %>%
  filter(city == "cps")

# Cover lat data
cover_lat_calib <- read.csv("./data/observations/monitoring/lai/lai_lat_calib_ids.csv")
cover_abv_calib <- read.csv("./data/observations/monitoring/lai/lai_abv_calib_ids.csv")
height_calib <- read.csv("./data/observations/monitoring/dry_mass_aboveground/height_calib_ids.csv")
wf_lat_calib <- read.csv("./data/observations/monitoring/dry_mass_fruit/wf_lat_calib_ids.csv")
wm_lat_calib <- read.csv("./data/observations/monitoring/dry_mass_mature_fruit/wm_lat_calib_ids.csv")


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


# Weather: no missing values, no outliers, hourly
weather_hourly <- read.csv("./data/weather/monitoring/weather_hourly.csv")

# External radiation
rad_power <- read.csv("./data/weather/unc/cps_POWER_SinglePoint_Daily_19870101_20210103_22d82S_47d60W.csv",
                      skip = 17)

my_colors <- RColorBrewer::brewer.pal(11, "RdYlBu")[c(1, 3, 11)]
my_colors4 <- RColorBrewer::brewer.pal(11, "RdYlBu")[c(1, 3, 10, 11)]
my_colors2 <- RColorBrewer::brewer.pal(4, "RdBu")[c(1, 3:4)]

# Assimilation results
all_states_out <- read.csv("./tables/results_DA/aux_files/all_states.csv")
assim_out <- read.csv("./tables/results_DA/aux_files/upd_states.csv")

# Assimilation
info_runs <- read.csv("./tables/runs_Filter.csv") %>%
  mutate(exp_int = exp,
         exp = paste0("n0", exp_int)) %>%
  select(-it, -comment)

# Load dictionary of measurement
dict <- read.csv("./tables/dictionary_paramsFilter.csv")

# Simulations
models_all <- read.csv( "./tables/results_simul/results_simulations_all.csv")

# Observations
obs <- read.csv("./data/observations/monitoring/observations_proc.csv")
obs_last <- read.csv( "./data/observations/monitoring/observations_last.csv")


# SBIAgro - Figura 1 ------------------------------------------------------

dataset <- obs_ids %>%
  filter(!is.na(cycle), cycle != 1) %>%
  mutate(cycle_mod = factor(cycle,
                            levels = c(2, 3, 4),
                            labels = c("Ciclo 1", "Ciclo 2", "Ciclo 3")))

myhist <- function(data = data, mapping = mapping, ...){
  
  ggplot(data) +
    geom_histogram(mapping, ...)
  
}

dataset_ <- dataset %>%
  full_join(cover_lat_calib) %>%
  full_join(height_calib) %>%
  filter(!is.na(lai_lat), 
         id != 31, id != 42)

variables <- c("w_plant", "w_plant_fm", "lai_lat",  "height")

plot_vars = c("lai_lat" = "Área verde \n(visada lateral)\n[m²/planta]",
              "w_plant" = "Massa seca\nda parte aérea\n[g/planta]",
              "height" = "Altura [m]",
              "w_plant_fm" = "Massa fresca\n da parte aérea\n[g/planta]",
              "w_fm_full" = "Massa fresca\n do sistema\n[g/planta]")

ggpairs(dataset_,
        columns = variables,
        columnLabels = plot_vars[match(variables, names(plot_vars))],
        mapping = ggplot2::aes(fill = cycle_mod, color=cycle_mod),
        diag = list(continuous = wrap(myhist, bins = 10)),
        lower = list(continuous = wrap(ggally_cor, 
                                       size = 2.8, 
                                       fontface = 2)),
        upper = list(continuous = wrap(ggally_points,
                                       size = 2, shape=21, 
                                       colour="black", stroke=0.2))) +
  scale_fill_manual(values = my_colors) +
  scale_colour_manual(values = my_colors) +
  theme_vert

namefile <- "../figures/relat3/relationships_w_pt.png"
ggsave(namefile,
       width = 15, height = 15, units = "cm",
       family = "serif", dpi = 320)  

# SBIAgro - Figura 2 ------------------------------------------------------
codes_plants <- c("n03" = "Ciclo 1, Planta 1", "n04" = "Ciclo 1, Planta 2", 
                  "n05" = "Ciclo 2, Planta 1", "n06" = "Ciclo 2, Planta 2",
                  "n07" = "Ciclo 3, Planta 1", "n08" = "Ciclo 3, Planta 2")
codes_plants_df <- data.frame(exp = names(codes_plants),
                              plant_id = codes_plants,
                              row.names = NULL)


plot_vars = c("lai_lat" = "Área verde \n(visada lateral)",
              "w_plant" = "Massa seca\nda parte aérea",
              "height" = "Altura",
              "w_plant_fm" = "Massa fresca\n da parte aérea",
              "w_fm_full" = "Massa fresca\n do sistema")

plot_mods <- c(tomgro =  "RT calibrado", ukf = "UKF",
               GNVMod = "RT sem calibrar")

dict_mod <- dict %>%
  filter(lang == "pt") %>%
  mutate(name_unit = paste(name_plot, unit),
         name_unit2 = paste0(name_plot, "\n", unit),
         name_unit_f = fct_reorder(name_unit, order))

allStates <- all_states_out %>%
  right_join(info_runs) 

simulations <- models_all %>%
  filter(city == "cps", exp != "n01", exp != "n02",
         variable != "dw", variable != "rm", variable != "pg")

obs_mod <- obs %>%
  filter(city == "cps")

upd <- allStates %>%
  filter(das != 0) %>%
  filter(state_var == "w",
         frequency == 1) %>%
  mutate(model = filt) %>%
  filter(filt != "enkf")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) | 
         ((calib == "cps3" & (exp == "n05" | exp == "n06")) |
            (calib == "cps4" & (exp == "n07" | exp == "n08")))),
         city == "cps") %>%
  select(-calib, -city)

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvMod",
         city == "cps") %>%
  mutate(model = "gnvMod") %>%
  select(-calib, -city)

simuls <- simul_NotCalib %>%
  bind_rows(simul_calib) %>%
  filter(variable == "w") %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df) %>%
  filter(exp == "n03" | exp == "n04" | 
           exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

assim <- upd %>%
  filter(variable == "w") %>%
  # idx is an auxiliary variable to find the proper
  # name of the variable
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  left_join(codes_plants_df) %>%
  filter(exp == "n03" | exp == "n04" | 
           exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

obs_plot <- obs_mod %>%
  filter(variable == "w") %>%
  #select(-model) %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx]) %>%
  filter(exp == "n03" | exp == "n04" | 
           exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

obs_last_i <- obs_last %>%
  separate(city_exp, into = c("city", "exp")) %>%
  rename(das = dat) %>%
  filter(variable == "w") %>%
  mutate(idx = match(variable, dict_mod$meas_var),
         variable2 = dict_mod$name_unit_f[idx],
         model = "obs") %>%
  filter(exp == "n03" | exp == "n04" | 
           exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

ggplot() +
  # facet_wrap("exp",
  #            scales = "free",
  #            drop = TRUE,
  #            labeller = labeller(meas_var = plot_vars,
  #                                exp = codes_plants,
  #                                model = plot_mods),
  #            nrow = 3) + 
  facet_grid(meas_var ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(meas_var = plot_vars,
                                 exp = codes_plants,
                                 model = plot_mods)) +
  geom_point(data = assim, aes(das, measurement,
                               colour = model),
             size = 0.6) +
  geom_point(data = simuls, aes(das, measurement, 
                               colour = model), 
             size = 0.6) +
  geom_point(data = obs_last_i, aes(das, measurement),
             size = 1.5) +
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 1) +
  geom_errorbar(data = obs_plot,
                aes(dat,
                    ymin=measurement-measurement_sd,
                    ymax=measurement+measurement_sd),
                width = 0.05) +
  labs(x = "Dias após início da simulação", 
       y = "",
       color = "") +
  theme_vert +
  scale_colour_manual(name="Técnica",
                      breaks=c("gnvMod", "tomgro", "ukf", "enkf"),
                      labels=c("RT sem calibrar",
                               "RT calibrado",
                               "UKF",
                               "EnKF not calibrated"),
                      values = my_colors)

plot_file_name <- paste0('../figures/relat3/w_assim.png')
ggsave(plot_file_name,
       width = 18, height = 11, units = "cm",
       family = "serif")


# SBIAgro - Plot incerteza ------------------------------------------------
transf_unit <- function(x){

  x["meas_conv"] <- NA
  
  mask_var <- x$meas_var == "lai_lat"
  mask_cycle <- (x$exp == "n03" | x$exp == "n04")
  mask_both <- mask_var & mask_cycle
  x[mask_both, "meas_conv"] <- 0.000721489301534449 * x$measurement[mask_both] + 0.0342958768986944
  mask_cycle <- (x$exp == "n05" | x$exp == "n06")
  mask_both <- mask_var & mask_cycle
  x[mask_both, "meas_conv"] <- 0.000721489301534449 * x$measurement[mask_both] + 0.0342958768986944
  mask_cycle <- (x$exp == "n07" | x$exp == "n08")
  mask_both <- mask_var & mask_cycle  
  x[mask_both, "meas_conv"] <- 0.00132247905034675 * x$measurement[mask_both] + 0.00798881571198673
  
  mask_var <- x$meas_var == "height"
  mask_cycle <- (x$exp == "n03" | x$exp == "n04")
  mask_both <- mask_var & mask_cycle
  x[mask_both, "meas_conv"] <- 0.0041288518609504 * x$measurement[mask_both] + 0.291874831697662
  mask_cycle <- (x$exp == "n05" | x$exp == "n06")
  mask_both <- mask_var & mask_cycle
  x[mask_both, "meas_conv"] <- 0.00354675263026496 * x$measurement[mask_both] + 0.485804524050112
  mask_cycle <- (x$exp == "n07" | x$exp == "n08")
  mask_both <- mask_var & mask_cycle  
  x[mask_both, "meas_conv"] <- 0.00423744432241557 * x$measurement[mask_both] + 0.414647824361298
  
  mask_var <- x$meas_var == "w_fm_full"
  mask_cycle <- (x$exp == "n03" | x$exp == "n04")
  mask_both <- mask_var & mask_cycle
  x[mask_both, "meas_conv"] <- 8.32327522333884 * x$measurement[mask_both] + 0.473530592923878
  mask_cycle <- (x$exp == "n05" | x$exp == "n06")
  mask_both <- mask_var & mask_cycle
  x[mask_both, "meas_conv"] <- 7.80892263589377 * x$measurement[mask_both] -0.52637360053634
  mask_cycle <- (x$exp == "n07" | x$exp == "n08")
  mask_both <- mask_var & mask_cycle  
  x[mask_both, "meas_conv"] <- 6.89608162488282 * x$measurement[mask_both] + 0.678711728361538
 

  return(x)
  
}

assim_out_mod <- assim_out %>%
  filter(variable == "observations") %>%
  left_join(info_runs) %>%
  left_join(codes_plants_df) %>%
  filter(exp == "n03" | exp == "n04" | 
           exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

comb <- simul_NotCalib %>%
  filter(variable == "w") %>%
  expand(., das, exp, variable, meas_var = c("lai_lat", "height", "w_fm_full"))

simul_mod <- simul_NotCalib %>%
  filter(variable == "w") %>%
  left_join(comb) %>%
  transf_unit(.)

assim_mod <- assim %>%
  filter(variable == "w") %>%
  transf_unit(.)

ggplot() +
  facet_grid(meas_var ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(meas_var = plot_vars,
                                 exp = codes_plants,
                                 model = plot_mods)) + 
  geom_point(data = assim_out_mod, aes(das, measurement, 
                               colour = model), 
             size = 0.6) +
  geom_point(data = simul_mod, aes(das, meas_conv, colour = model), 
             size = 0.6) +
  geom_point(data = assim_mod, aes(das, meas_conv, 
                               colour = model), 
             size = 0.6) +
  labs(x = "Dias após início da simulação", 
       y = "",
       color = "") +
  theme_vert +
  scale_colour_manual(name="Técnica",
                      breaks=c("gnvMod", "enkf", "tomgro", "ukf"),
                      labels=c("RT sem calibrar",
                               "RT calibrado",
                               "Observações",
                               "UKF"),
                      values = my_colors4)

plot_file_name <- paste0('../figures/relat3/w_eval_meas.png')
ggsave(plot_file_name,
       width = 18, height = 11, units = "cm",
       family = "serif")

