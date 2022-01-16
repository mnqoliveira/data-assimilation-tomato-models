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
load("../data/plot_theme_vert.RData")


# Load data ---------------------------------------------------------------
# Dates
cycle_dates <- read.csv("../data/cycle_dates.csv")
codes_exp <- read.csv("../tables/codes_exp.csv")

# Harvests
harvests <- read.csv("../data/observations/monitoring/harvests.csv")

# Calibration ids
obs_ids <- read.csv("../data/observations/monitoring/obs_exp_all_ids.csv")

# Full monitoring and experiment
obs_all <- read.csv("../data/observations/full_set_obs.csv") %>%
  filter(city == "cps")

# Cover lat data
cover_lat_calib <- read.csv("../data/observations/monitoring/lai/lai_lat_calib_ids.csv")
cover_abv_calib <- read.csv("../data/observations/monitoring/lai/lai_abv_calib_ids.csv")
height_calib <- read.csv("../data/observations/monitoring/dry_mass_aboveground/height_calib_ids.csv")
wf_lat_calib <- read.csv("../data/observations/monitoring/dry_mass_fruit/wf_lat_calib_ids.csv")
wm_lat_calib <- read.csv("../data/observations/monitoring/dry_mass_mature_fruit/wm_lat_calib_ids.csv")


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
weather_hourly <- read.csv("../data/weather/monitoring/weather_hourly.csv")

# External radiation
rad_power <- read.csv("../data/weather/unc/cps_POWER_SinglePoint_Daily_19870101_20210103_22d82S_47d60W.csv",
                      skip = 17)

my_colors <- RColorBrewer::brewer.pal(11, "RdYlBu")[c(1, 3, 11)]
my_colors4 <- RColorBrewer::brewer.pal(11, "RdYlBu")[c(1, 3, 10, 11)]
my_colors2 <- RColorBrewer::brewer.pal(4, "RdBu")[c(1, 3:4)]

# Assimilation results
all_states_out <- read.csv("../tables/results_DA/aux_files/all_states.csv")
assim_out <- read.csv("../tables/results_DA/aux_files/upd_states.csv")

# Assimilation
info_runs <- read.csv("../tables/runs_Filter.csv") %>%
  mutate(exp_int = exp,
         exp = paste0("n0", exp_int)) %>%
  select(-it, -comment)

# Load dictionary of measurement
dict <- read.csv("../tables/dictionary_paramsFilter.csv")

# Simulations
models_all <- read.csv( "../tables/results_simul/results_simulations_all.csv")

# Observations
obs <- read.csv("../data/observations/monitoring/observations_proc.csv")
obs_last <- read.csv( "../data/observations/monitoring/observations_last.csv")


# Weather: no NAs, no outliers --------------------------------------------
# Aggregated
weather_mod <- weather_hourly %>%
  separate(stat_sensor_var, sep = "_", into = c("stat", "sensor", "var")) %>%
  filter(var != "humidity", !is.na(cycle), value < 3000, 
         sensor != "pi", node == 1, cycle != 1) %>%
  mutate(stat_var = str_c(var, stat, sep = "_"), date = as.Date(date)) %>%
  filter((stat == "mean" & var == "radiation") | 
           (var == "temperature" & stat != "med")) %>%
  select(-stat, -var, -sensor, -starts_with("flag")) %>%
  spread(stat_var, value) %>%
  group_by(cycle, date, node) %>%
  summarise(rad = sum(radiation_mean, na.rm = TRUE),
            tmax = max(temperature_max, na.rm = TRUE),
            tmin = min(temperature_min, na.rm = TRUE),
            tmean = mean(temperature_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(c("rad", "tmax", "tmin", "tmean"), 
         key = "variable", value = "measurement") %>%
  mutate(var = if_else(variable == "rad", "radiation", "temperature"),
         stat = if_else(variable == "rad", "sum", 
                        substring(variable, 2, nchar(variable))))

rad_ext <- rad_power %>% 
  rename_all(tolower) %>%
  mutate(date = as.Date(paste(year, doy, sep = "-"), tryFormats = c("%Y-%j"))) %>%
  rename(rad_ext_d = allsky_sfc_sw_dwn) %>%
  select(date, rad_ext_d) %>%
  mutate(date = as.character(date)) %>%
  right_join(cycle_dates) %>%
  mutate(rad = rad_ext_d * 555.6,
         date = as.Date(date),
         var = "radiation",
         stat = "sum") %>%
  filter(cycle != 1, rad > 0)

ggplot() +
  facet_grid(var ~ cycle, scales = "free",
             labeller = labeller(cycle = c("1" = "Ciclo 1",
                                           "2" = "Ciclo 2",
                                           "3" = "Ciclo 3",
                                           "4" = "Ciclo 4"),
                                 var = c("radiation" = "Radiação Solar\n [micromol RFA dia-1]",
                                         "temperature" = "Temperatura\n [oC]")),
             space = "free_x") +
  geom_line(data=weather_mod, 
            aes(x = date, y = measurement,  
                colour = stat), size = 0.5) +
  geom_line(data=rad_ext, aes(x = date, y = rad,
                              colour = stat), 
            size = 0.5, linetype = 2) +
  labs(x = "Dias após o transplantio",
       y = "") +
  scale_colour_brewer(palette = "RdBu",
                      name = "Agregação",
                      breaks=c("max", "mean", "min", "sum"),
                      labels=c("Máximo diário", 
                               "Média diária", 
                               "Mínimo diário",
                               "Soma diária")) + 
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(override.aes = list(size = 1))) +
  theme_vert

namefile <- "../figures/relat3/weather_final.png"
ggsave(namefile,
       width = 17.0, height = 12.0, units = "cm", 
       family = "serif", dpi = 320)

# Map flags ---------------------------------------------------------------

cycles <- c("1" = "Ciclo 1",
            "2" = "Ciclo 2",
            "3" = "Ciclo 3",
            "4" = "Ciclo 4")

sensor_vars <- c("li1400_radiation" = "RFA\nPiranômetro",
                 "pi_radiation" = "Lux\nLuxímetro (Pi)",
                 "radio_temperature" = "Temperatura\nSHT75",
                 "pi_temperature" = "Temperatura\nDHT22 (Pi)",
                 "radio_humidity" = "Umidade Relativa\nSHT75",
                 "pi_humidity" = "Umidade Relativa\nDHT22 (Pi)")

flags <- weather_hourly %>%
  separate(stat_sensor_var, into = c("stat", "sensor", "var")) %>%
  filter(stat == "mean", var != "humidity") %>%
  select(-value) %>%
  mutate(date = as.Date(date)) %>%
  gather(starts_with("flag"), key = "flag", value = "value") %>%
  unite(sensor_var, c(sensor, var), sep = "_") %>%
  mutate(flag_mod = if_else(value > 0, flag, "none"),
         flag_mod = na_if(flag_mod, "none"),
         sensor_var = factor(sensor_var,
                             levels = names(sensor_vars),
                             labels = sensor_vars,
                             ordered = TRUE))

# Node 1
flags_mod <- filter(flags, node == 1)
ggplot(flags_mod) +
  geom_tile(aes(x = date, y = hour, fill = flag_mod, alpha = value)) +
  facet_grid(sensor_var ~ cycle, scales = "free",
             labeller = labeller(cycle = cycles)) +
  labs(x = "Data",
       y = "Hora") +
  scale_x_date(date_labels = "%d %b %y") +
  scale_fill_brewer(palette = "RdBu",
                    name="Flags",
                    breaks=c("flag_case1", "flag_case2", "flag_case3",
                             "flag_case4"),
                    labels=c("Redundância", "Sensores diferentes", 
                             "Nós diferentes", "Externo")) +
  scale_alpha_continuous(name="Percentual imputado",
                         breaks=c(0.00, 0.50, 1.00),
                         labels=c("0%", "50%", "100%")) +
  theme_vert

namefile <- "../figures/relat3/weather_imputation.png"
ggsave(namefile,
       width = 20, height = 14, units = "cm", 
       family = "serif", dpi = 320)

# LAI destructive analysis ------------------------------------------------
dataset <- obs_ids

# Lai and lai_lat in separate scatterplots
plot_vars = c("lai" = "IAF [m² folhas/m² solo]",
              "leaf_area" = "Área foliar [m² folhas/planta]")

dataset_plot <- dataset %>%
  gather("lai", "leaf_area", key = variable,
         value = measurement)

ggplot(data = dataset_plot) +
  facet_wrap("variable", scales = "free",
             labeller = labeller(variable = plot_vars)) +
  geom_point(aes(dat, measurement, fill = as.factor(cycle)), 
             size = 2, shape=21, colour="black", stroke=0.2) +
  # geom_text(aes(x=dat, y=measurement, label=id),
  #           hjust=-0.4, vjust=0, size=3) +
  labs(x = "Dias após o transplantio",
       y = "",
       colour = "Ciclo") +
  scale_fill_manual(values = my_colors,
                    name="Ciclo") +
  theme_vert

namefile <- "../figures/relat3/plant_lai.png"
ggsave(namefile,
       width = 15, height = 6, units = "cm", 
       family = "serif", dpi = 320)


# Images non-dest ---------------------------------------------------------
dataset <- obs_ids %>%
  full_join(cover_lat_calib) %>%
  full_join(cover_abv_calib) %>%
  full_join(height_calib) %>%
  full_join(wf_lat) %>%
  full_join(wm_lat)

# Lai and lai_lat in separate scatterplots
plot_vars = c("lai_lat" = "Área verde \n(visada lateral) [m²/planta]",
              "lai_abv" = "Área verde \n(visada superior) [m²/planta]",
              "height" = "Altura [m]",
              "wf_lat" = "Área de frutos total \n [m²/planta]",
              "wm_lat" = "Área de frutos maduros \n [m²/planta]")

dataset_plot <- dataset %>%
  gather("lai_lat", "lai_abv", "height", "wf_lat", "wm_lat", key = variable,
         value = measurement)

ggplot(data = dataset_plot) +
  facet_wrap("variable", scales = "free", nrow = 2,
             labeller = labeller(variable = plot_vars)) +
  geom_point(aes(dat, measurement, fill = as.factor(cycle)), 
             size = 1.5, shape=21, colour="black", stroke=0.2) +
  # geom_text(aes(x=dat, y=measurement, label=id),
  #           hjust=-0.4, vjust=0, size=3) +
  labs(x = "Dias após o transplantio",
       y = "",
       colour = "Ciclo") +
  scale_fill_manual(values = my_colors,
                    name="Ciclo") +
  theme_vert

namefile <- "../figures/relat3/plant_areas.png"
ggsave(namefile,
       width = 15, height = 10, units = "cm", 
       family = "serif", dpi = 320)



# Monitoring data vs calibration for green cover --------------------------
calib <- obs_ids %>%
  full_join(cover_lat_calib) %>%
  full_join(cover_abv_calib) %>%
  full_join(height_calib) %>%
  full_join(wf_lat) %>%
  full_join(wm_lat) %>%
  gather("lai_abv", "lai_lat", "wf_lat", "height",
         key = "variable",
         value = "measurement") %>%
  group_by(cycle, date, variable, dat) %>%
  summarise(meas_mean = mean(measurement, na.rm = T),
            meas_sd = sd(measurement, na.rm = T))

dataset_plot <- obs_all %>%
  filter(!is.na(cycle), cycle != 1) %>%
  select(cycle, dat, lai_abv, lai_lat, wf_lat, height, node) %>%
  gather("lai_abv", "lai_lat", "wf_lat", "height",
         key = "variable",
         value = "measurement") %>%
  filter(!is.na(measurement))

plot_vars = c("lai_lat" = "Área verde \n(visada lateral)\n[m²/planta]",
              "lai_abv" = "Área verde \n(visada superior)\n[m²/planta]",
              "height" = "Altura [m]",
              "wf_lat" = "Área de \n frutos total \n [m²/planta]",
              "wm_lat" = "Área de frutos maduros \n [m²/planta]")

ggplot() +
  facet_grid(variable ~ cycle,  scales = "free",
             space = "free_x",
             labeller = labeller(variable = plot_vars,
                                 cycle = c("2" = "Ciclo 2",
                                           "3" = "Ciclo 3",
                                           "4" = "Ciclo 4"))) +
  geom_point(data = dataset_plot,
             aes(x = dat, y = measurement,
                 fill = as.factor(node)), 
             size = 1.5, shape=21, colour="black", stroke=0.2) +
  geom_errorbar(data = calib, 
                aes(x = dat,
                    ymin=meas_mean-meas_sd,
                    ymax=meas_mean+meas_sd), width=.1, colour=my_colors[1]) +
  labs(x = "Dias após o transplantio",
       y = "") +
  scale_fill_manual(values = my_colors,
                    name = "Plant",
                    breaks=c("calib", "1", "2"),
                    labels=c("Dados de calibração", "Planta 1", "Planta 2")) +
  scale_x_continuous(breaks=seq(0,110,15)) +
  theme_vert +
  theme(strip.text.y = element_text(size = 7))

namefile <- "../figures/relat3/plant_monit_areas.png"
ggsave(namefile,
       width = 20, height = 10, units = "cm", 
       family = "serif", dpi = 320)


# Biomass destructive -----------------------------------------------------

# Biomass only
dataset <- obs_ids %>%
  filter(!is.na(cycle), cycle != 1) %>%
  mutate(date = as.Date(date))

dataset_mod <- dataset %>%
  gather(starts_with("w"),
         key = variable,
         value = measurement) %>%
  mutate(type = if_else(grepl("fm", variable), "fm", "dm"),
         variable_mod = variable,
         variable_mod = gsub("_fm", "", variable_mod)) %>%
  filter(variable_mod == "w_plant" | 
           variable_mod == "wf_plant" |
           variable_mod == "wm_plant")


plot_part = c("w_plant" = "Massa da parte aérea\n[g/planta]", 
              "wf_plant" = "Massa total de frutos\n[g/planta]", 
              "wm_plant" = "Massa de frutos maduros\n[g/planta]")

plot_cond = c("dm" = "Massa seca\n[g MS/planta]",
              "fm" = "Massa fresca\n[g MF/planta]")

ggplot(data = dataset_mod) +
  facet_grid(type ~ variable_mod, scales = "free",
             labeller = labeller(type = plot_cond,
                                 variable_mod = plot_part)) +
  geom_point(aes(x = dat, y = measurement,
                 fill = as.factor(cycle)), 
             size = 1.5, shape=21, colour="black", stroke=0.2) +
  labs(x = "Dias após o transplantio",
       y = "",
       colour = "Ciclo") +
  scale_fill_manual(values = my_colors,
                    name = "Ciclo") +
  theme_vert

namefile <- "../figures/relat3/plant_biomass.png"
ggsave(namefile,
       width = 15, height = 10, units = "cm",
       family = "serif", dpi = 320)


# Processed vs experiment mass --------------------------------------------

plot_vars = c("mean_mv" = "Moisture [mV]",
              "w_fm_full" = "Massa total do sistema \nplanta-vaso-água [g]")

dataset_plot <- obs_all  %>%
  filter(!is.na(cycle)) %>%
  gather(w_fm_full, mean_mv, 
         key = "variable", 
         value = "measurement") %>%
  filter(!(variable == "mean_mv" & cycle == 4),
         !(variable == "mean_mv"), dat <= 110, 
         abs(measurement) < 6000)

dataset_mod <- obs_ids %>%
  select(dat, cycle, w_plant_fm) %>%
  mutate(variable = "w_fm_full",
         node = "calib") %>%
  filter(dat <= 110)

ggplot() +
  facet_wrap("cycle",
             labeller = labeller(cycle = c("1" = "Ciclo 1",
                                           "2" = "Ciclo 2",
                                           "3" = "Ciclo 3",
                                           "4" = "Ciclo 4"),
                                 variable = plot_vars)) +
  # System
  geom_point(data = dataset_plot,
             aes(x = dat, y = measurement,
                 fill = node), 
             size = 1.5, shape=21, colour="black", stroke=0.2) +
  # Experiments
  geom_point(data = dataset_mod,
             aes(x = dat, y = w_plant_fm,
                 fill = node), 
             size = 1.5, shape=21, colour="black", stroke=0.2) +
  labs(x = "Dias após o transplantio",
       y = "Massa fresca\n[g M.F./planta]") +
  scale_fill_manual(values = my_colors,
                    name = "Plant",
                    breaks=c("calib", "1", "2"),
                    labels=c("Dados de calibração", "Planta 1", "Planta 2")) +
  scale_x_continuous(breaks=seq(0,110,15)) +
  theme_vert

namefile <- "../figures/relat3/plant_systemBiomass.png"
ggsave(namefile,
       width = 15, height = 15, units = "cm",
       family = "serif", dpi = 320)



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

