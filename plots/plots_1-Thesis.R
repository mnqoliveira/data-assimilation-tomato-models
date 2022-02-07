#rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")
#Sys.setlocale("LC_TIME", "Portuguese")

# Libraries ---------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(lubridate)
library(data.table)

library(patchwork)
library(paletteer)

# Source functions --------------------------------------------------------
source("./data_analyses/07a-orgSimOut.R")
source("./data_analyses/07b-orgObs_plot.R")

# Load theme --------------------------------------------------------------
load("./data/plot_theme_vert.RData")

theme_vert <- theme_vert +
  theme(panel.background = element_rect(fill = "grey99"))

# Load data ---------------------------------------------------------------
# Dates
cycle_dates <- read.csv("./data/cycle_dates.csv")
codes_exp <- read.csv("./tables/codes_exp.csv")
plant_dates <- read.csv("./tables/relevant_dates_plants.csv")
colnames(plant_dates)[1] <- "cycle"

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

# Simulations
models_all <- read.csv( "./tables/results_simul/results_simulations_all.csv")

# Observations
obs <- read.csv("./data/observations/monitoring/observations_proc.csv")
obs_last <- read.csv( "./data/observations/monitoring/observations_last.csv")
obs_artif <- read.csv("./data/synthetic/obs_artif_all.csv")

# Scales semi-proc
scales_proc_full <- read.csv("./data/observations/monitoring/scales_proc_full.csv")

# Uncertainty outputs
load("./tables/results_SA/all_curves_params.RData")
load("./tables/results_SA/all_curves_weather.RData")

load("./tables/results_SA/all_si_weather.RData")
all_si_weather <- all_si
load("./tables/results_SA/all_si.RData")

case1_hi <- read.csv("./tables/results_SA/case1_highest_indices.csv")

# Assimilation results
load("./tables/results_DA/aux_files/all_states.RData")
#load("./tables/results_DA/aux_files/upd_states.RData")

# Assimilation
info_runs <- read.csv("./tables/runs_Filter.csv") %>% 
  rbind(read.csv("./tables/runs_Filter2.csv")) %>%
  mutate(exp_int = exp,
         exp = paste0("n0", exp_int)) %>%
  select(-it, -comment)

# Load dictionary of measurement
dict <- read.csv("./tables/dictionary_paramsFilter.csv")

# Additional variables ----------------------------------------------------

path_figures <- "../../../../Doutorado/Tese/figures/"

cycles <- c("1" = "Cycle 0",
            "2" = "Cycle 1",
            "3" = "Cycle 2",
            "4" = "Cycle 3")

sensor_names <- c("li1400_radiation" = "PAR\nQuantum sensor",
                 "pi_radiation" = "Lux\nLuxmeter",
                 "radio_temperature" = "Temperature\nSHT75",
                 "pi_temperature" = "Temperature\nDHT22",
                 "radio_humidity" = "Humidity\nSHT75",
                 "pi_humidity" = "Humidity\nDHT22")

plot_states <- c("lai" = "LAI\n[m² leaves/m² soil]", 
                 "n" = "N\n[number of nodes/\nm² soil]", 
                 "w" = "Aboveground\n biomass\n[g D.M./m² soil]", 
                 "wf" = "Fruit weight\n[g D.M./m² soil]", 
                 "wm" = "Mature fruit weight\n[g D.M./m² soil]")

plot_states_1L <- c("lai" = "LAI", 
                 "n" = "N", 
                 "w" = "Aboveground biomass", 
                 "wf" = "Fruit weight", 
                 "wm" = "Mature fruit weight")


plot_vars = c("leaf_area" = "Leaf area [m² leaves/plant]",
              "lai_lat" = "Green cover area \n(lateral view) [m²/plant]",
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

plot_vars_1l = c("leaf_area" = "Leaf area [m² leaves/plant]",
                 "lai_lat" = "Green cover area (lateral view) [m²/plant]",
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

factors_weather <- c("co2" = "Carbon Dioxide",
                 "rad" = "Solar Radiation",
                 "tmax" = "Maximum Temperature",
                 "tmean" = "Average Temperature",
                 "tmin" = "Minimum Temperature")

codes_plants <- c("n03" = "Cycle 1, Plant 1", "n04" = "Cycle 1, Plant 2", 
                  "n05" = "Cycle 2, Plant 1", "n06" = "Cycle 2, Plant 2",
                  "n07" = "Cycle 3, Plant 1", "n08" = "Cycle 3, Plant 2")


# Org variables -----------------------------------------------------------

models <- models_all %>%
  mutate(city_exp = paste(city, exp, sep = "_"),
         exp_full = paste(model, city_exp, sep = "_")) 

allStates <- all_assim %>%
  mutate(id = as.integer(id)) %>%
  filter(id < 500) %>%
  right_join(info_runs) 

updState <- upd_states_out %>%
  right_join(info_runs)

simulations <- models_all %>%
  filter(city == "cps", exp != "n01", exp != "n02",
         variable != "dw", variable != "rm", variable != "pg")

simulations_artif <- models_all %>%
  filter(city == "cps", 
         exp != "n02", exp != "n04", exp != "n06", exp != "n08",
         variable != "dw", variable != "rm", variable != "pg")

assim_artif <- all_assim %>%
  mutate(id = as.integer(id)) %>%
  filter(id > 500) %>%
  right_join(info_runs)

# Fig 07 - Jones 1999 - Gainesville ---------------------------------------
obs_mod <- obs %>%
  filter(variable %in% names(plot_states))

model_plot <- models %>%
  filter(city == "gnv", calib == "gnv") %>%
  filter(model == "tomgro") %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm")) %>%
  mutate(year = if_else(exp == "n01" | exp == "n02" | exp == "n03", 1993, 1994),
         type = if_else(exp == "n01" | exp == "n04", "Cool",
                        if_else(exp == "n02" | exp == "n05", "Warm", "Hot")),
         type_ = factor(type,
                        levels = c("Cool", "Warm", "Hot"),
                        ordered = TRUE))

obs_plot <- obs_mod %>%
  filter(city == "gnv") %>%
  select(-model) %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm")) %>%
  mutate(year = if_else(exp == "n01" | exp == "n02" | exp == "n03", 1993, 1994),
         type = if_else(exp == "n01" | exp == "n04", "Cool",
                        if_else(exp == "n02" | exp == "n05", "Warm", "Hot")),
         type_ = factor(type,
                        levels = c("Cool", "Warm", "Hot"),
                        ordered = TRUE))

ggplot() +
  facet_grid(variable ~ year,
             labeller = labeller(variable = plot_states),
             scales = "free",
             space = "free_x") +
  geom_line(data = model_plot, aes(das, measurement, col = type_),
            size = 0.8) +
  geom_point(data = obs_plot, aes(das, measurement, col = type_),
             size = 1.5, shape = 15) +
  labs(x = "Days after simulation started",
       y = "",
       colour = "Experiment") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")[c(2, 3, 1)]) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig07_tomgro-gnv.png')
ggsave(plot_file_name,
       width = 15, height = 18, units = "cm",
       family = "serif")

# Fig 08 - Jones 1999 - Avignon -------------------------------------------
obs_mod <- obs %>%
  filter(variable %in% names(plot_states))

model_plot <- models %>%
  filter(city == "av", calib == "av" | calib == "gnv") %>%
  filter(model == "tomgro") %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm"))

obs_plot <- obs_mod %>%
  filter(city == "av") %>%
  select(-model) %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm"))

ggplot() +
  facet_grid(variable ~ calib,
             labeller = labeller(variable = plot_states,
                                 calib = c("gnv" = "Not Calibrated",
                                           "av" = "Calibrated")),
             scales = "free") +
  geom_line(data = model_plot, aes(das, measurement),
            size = 0.8) +
  geom_point(data = obs_plot, aes(das, measurement),
             size = 1.5, shape = 15) +
  labs(x = "Days after simulation started",
       y = "") +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig08_tomgro-av.png')
ggsave(plot_file_name,
       width = 15, height = 18, units = "cm",
       family = "serif")

# Fig 09 - Jones 1999 - Lake City -----------------------------------------
obs_mod <- obs %>%
  filter(variable %in% names(plot_states))

model_plot <- models %>%
  filter(city == "lc", calib == "lc" | calib == "gnv") %>%
  filter(model == "tomgro") %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm"))

obs_plot <- obs_mod %>%
  filter(city == "lc") %>%
  select(-model) %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm"))

ggplot() +
  facet_grid(variable ~ calib,
             labeller = labeller(variable = plot_states,
                                 calib = c("gnv" = "Not Calibrated",
                                           "lc" = "Calibrated")),
             scales = "free") +
  geom_line(data = model_plot, aes(das, measurement),
            size = 0.8) +
  geom_point(data = obs_plot, aes(das, measurement),
             size = 1.5, shape = 15) +
  labs(x = "Days after simulation started",
       y = "") +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig09_tomgro-lc.png')
ggsave(plot_file_name,
       width = 15, height = 18, units = "cm",
       family = "serif")


# Fig 10 - Weather after processing ---------------------------------------

# Aggregated
weather_mod <- weather_hourly %>%
  separate(stat_sensor_var, sep = "_", into = c("stat", "sensor", "var")) %>%
  filter(!is.na(cycle), value < 3000, 
         sensor != "pi", cycle != 1) %>%
  mutate(stat_var = str_c(var, stat, sep = "_"), date = as.Date(date)) %>%
  filter((stat == "mean" & var == "radiation") | 
           (var == "temperature" & stat != "med") |
           (var == "humidity" & stat != "med")) %>%
  select(-stat, -var, -sensor, -starts_with("flag")) %>%
  spread(stat_var, value) %>%
  group_by(cycle, date, node) %>%
  summarise(rad = sum(radiation_mean, na.rm = TRUE),
            tmax = max(temperature_max, na.rm = TRUE),
            tmin = min(temperature_min, na.rm = TRUE),
            tmean = mean(temperature_mean, na.rm = TRUE),
            hmean = mean(humidity_mean, na.rm = TRUE),
            hmax = max(humidity_max, na.rm = TRUE),
            hmin = min(humidity_min, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(c("rad", "tmax", "tmin", "tmean", "hmean", "hmax", "hmin"), 
         key = "variable", value = "measurement") %>%
  mutate(var = if_else(variable == "rad", "radiation", 
                       if_else(variable == "hmean" | variable == "hmax" | variable == "hmin", 
                               "humidity",
                               "temperature")),
         stat = if_else(variable == "rad", "sum", 
                        substring(variable, 2, nchar(variable))),
         measurement = if_else(variable == "rad", measurement*0.5/555.6, measurement))

rad_ext <- rad_power %>% 
  rename_all(tolower) %>%
  mutate(date = as.Date(paste(year, doy, sep = "-"), tryFormats = c("%Y-%j"))) %>%
  rename(rad_ext_d = allsky_sfc_sw_dwn) %>%
  select(date, rad_ext_d) %>%
  mutate(date = as.character(date)) %>%
  right_join(cycle_dates) %>%
  mutate(#rad = rad_ext_d * 555.6,
    rad = rad_ext_d*0.5,
         date = as.Date(date),
         var = "radiation",
         stat = "sum") %>%
  filter(cycle != 1, rad > 0)

weather_mod <- weather_mod %>%
  mutate(var_ = factor(var,
                       levels = c("radiation", "temperature", "humidity"),
                       labels = c("PAR~(MJ~m^{-2}~day^{-1})",
                                  "Temperature~('°C')", "Humidity~('%')")),
         cycle_ = factor(cycle,
                         levels = names(cycles),
                         labels = cycles))

rad_ext <- rad_ext %>%
  mutate(var_ = factor(var,
                       levels = c("radiation", "temperature", "humidity"),
                       labels = c("PAR~(MJ~m^{-2}~day^{-1})",
                                  "Temperature~('°C')", "Humidity~('%')")),
         cycle_ = factor(cycle,
                         levels = names(cycles),
                         labels = cycles))

plant_dates <- plant_dates %>%
  filter(event == "stage", comment != "vegetative", comment != "harvest")

# "Temperature~('°C')", 
# "Light~(µmol~m^{-2}~s^{-1})")

ggplot() +
  facet_grid(var_ ~ cycle_, scales = "free",
             labeller = labeller(cycle_ = label_value, var_ = label_parsed),
             space = "free_x") +
  geom_line(data=weather_mod, 
            aes(x = date, y = measurement,  
                colour = stat, lty = as.factor(node)), size = 0.5) +
  geom_point(data=rad_ext, aes(x = date, y = rad,
                              colour = stat), 
            size = 0.5) +
  geom_vline(data=plant_dates, aes(xintercept=as.Date(date))) + 
  labs(x = "",
       y = "",
       lty = "Node") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")[c(1, 3, 2, 4)],
                      name = "Summary",
                      breaks=c("max", "min", "mean", "sum"),
                      labels=c("Daily maximum", 
                               "Daily minimum", 
                               "Daily average",
                               "Daily sum")) + 
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%d %b %y") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(override.aes = list(size = 1))) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig10_weather-final.png')
ggsave(plot_file_name,
       width = 18, height = 16, units = "cm", 
       family = "serif", dpi = 320)


# Fig 11 and Fig 12 - Map imputations -------------------------------------
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
                             levels = names(sensor_names),
                             labels = sensor_names,
                             ordered = TRUE))

# Node 1
flags_mod <- filter(flags, node == 1)
ggplot(flags_mod) +
  geom_tile(aes(x = date, y = hour, fill = flag_mod, alpha = value)) +
  facet_grid(sensor_var ~ cycle, scales = "free",
             labeller = labeller(cycle = cycles)) +
  labs(x = "Date",
       y = "Hour") +
  scale_x_date(date_labels = "%d %b %y") +
  scale_fill_brewer(palette = "Set1",
                    name="Flags",
                    breaks=c("flag_case1", "flag_case2", "flag_case3",
                             "flag_case4"),
                    labels=c("Redundancy", "Different sensors", 
                             "Different nodes", "External")) +
  scale_alpha_continuous(name="Percentage imputated",
                         breaks=c(0.00, 0.50, 1.00),
                         labels=c("0%", "50%", "100%")) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig11_weather-imputations1.png')
ggsave(plot_file_name,
       width = 20, height = 14, units = "cm", 
       family = "serif", dpi = 320)

# Node 2
flags_mod <- filter(flags, node == 2)
ggplot(flags_mod) +
  geom_tile(aes(x = date, y = hour, fill = flag_mod, alpha = value)) +
  facet_grid(sensor_var ~ cycle, scales = "free",
             labeller = labeller(cycle = cycles)) +
  labs(x = "Date",
       y = "Hour") +
  scale_x_date(date_labels = "%d %b %y") +
  scale_fill_brewer(palette = "Set1",
                    name="Flags",
                    breaks=c("flag_case1", "flag_case2", "flag_case3",
                             "flag_case4"),
                    labels=c("Redundancy", "Different sensors", 
                             "Different nodes", "External")) +
  scale_alpha_continuous(name="Percentage imputated",
                         breaks=c(0.00, 0.50, 1.00),
                         labels=c("0%", "50%", "100%")) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig12_weather-imputations2.png')
ggsave(plot_file_name,
       width = 20, height = 14, units = "cm", 
       family = "serif", dpi = 320)


# Fig 13 - Correlation - Sensor Rad ---------------------------------------
dataset <- weather_hourly %>%
  filter(flag_case2 == 0, flag_case4 == 0) %>%
  select(-starts_with("flag")) %>%
  separate(stat_sensor_var, into = c("stat", "sensor", "var")) %>%
  filter(stat == "mean", var == "radiation") %>%
  spread(sensor, value) %>%
  filter(li1400 <= 1500)%>%
  filter(complete.cases(.))

cors <- dataset %>%
  group_by(cycle) %>%
  summarise(cor = round(cor(li1400, pi), 2))

ggplot(dataset, na.rm = TRUE) +
  geom_point(aes(x = li1400, y = pi, colour = as.factor(node)), 
             size = 0.5, alpha = 0.5) +
  geom_abline(slope = 1) +
  facet_wrap("cycle", drop = TRUE, 
             labeller = labeller(cycle = cycles)) +
  labs(x = expression("LI190SA (µmol PAR m"^-2*"s"^-1*")"),
       y = expression("BH1750 (µmol PAR m"^-2*"s"^-1*")"),
       colour = "Sensor node") +
  scale_color_brewer(palette = "Set1") +
  guides(colour=guide_legend(override.aes = list(size = 2),
                             title.theme = element_text(face = "bold", size=9))) +
  coord_fixed() +
  geom_text(data=cors, 
            aes(label=paste("r=", cor, sep="")), 
            x=1200, y=55) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig13_weather-correl_rad.png')
ggsave(plot_file_name,
       width = 17, height = 8, units = "cm", 
       family = "serif", dpi = 320)

# Fig 14 - Correlation - Temperature --------------------------------------

dataset <- weather_hourly %>%
  filter(flag_case2 == 0, flag_case3 == 0, flag_case4 == 0) %>%
  select(-starts_with("flag")) %>%
  separate(stat_sensor_var, into = c("stat", "sensor", "var")) %>%
  filter(stat == "mean", var == "temperature") %>%
  spread(sensor, value) %>%
  filter(complete.cases(.))

cors <- dataset %>%
  group_by(cycle) %>%
  summarise(cor = round(cor(radio, pi), 2))

ggplot(dataset, na.rm = TRUE) +
  geom_point(aes(x = radio, y = pi, colour = as.factor(node)), 
             alpha = 0.5, size = 0.5) +
  geom_abline(slope = 1) +
  facet_wrap("cycle", drop = TRUE, 
             labeller = labeller(cycle = cycles)) +
  labs(x = "SHT75 (°C)",
       y = "DHT22 (°C)",
       colour = "Sensor node") +
  scale_color_brewer(palette = "Set1") +
  guides(colour=guide_legend(override.aes = list(size = 2),
                             title.theme = element_text(face = "bold", size=9))) +
  coord_fixed() +
  geom_text(data=cors, 
            aes(label=paste("r = ", cor, sep="")), 
            x=35, y=12) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig14_weather-correl_temp.png')
ggsave(plot_file_name,
       width = 18, height = 8, units = "cm", 
       family = "serif", dpi = 320)

# Fig 16 - Biomass - Different hours of the day ---------------------------
scales <- scales_proc_full %>%
  mutate(dateFull = ymd_hms(dateFull),
         date = date(dateFull)) %>%
  filter(!is.na(cycle)) %>%
  group_by(cycle, date, dat, hour, node) %>%
  summarise(wm = mean(w_fm_full, na.rm = TRUE)) %>%
  filter(hour == 4 | hour == 12 | hour == 17) %>%
  ungroup() 

ggplot(scales) +
  facet_grid(cycle ~ node,
             labeller = labeller(cycle = cycles,
                                 node = c("1" = "Node 1",
                                          "2" = "Node 2")),
             scales = "free") + 
  geom_point(aes(x = dat, y = wm,
                 colour = as.factor(hour)), size = 0.8) +
  labs(x = "Days after transplanting",
       y = expression("System biomass (g plant"^-1*")")) +
  scale_colour_brewer(name="Hour",
                      breaks=c("4", "12", "17"),
                      labels=c("4 am", "Noon",
                               "5 pm"),
                      palette = "Set1", type = "qual") +
  
  
  scale_x_continuous(breaks=seq(0,110,15)) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig16_biomass-hours.png')
ggsave(plot_file_name,
       width = 12, height = 18, units = "cm",
       family = "serif", dpi = 320)


# Fig 17 - Green area - Calibration ---------------------------------------
dataset <- obs_ids %>%
  full_join(cover_lat_calib) %>%
  full_join(cover_abv_calib)

# Lai and lai_lat in separate scatterplots
plot_vars = c("lai" = "LAI [m² leaves/m² soil]",
              "leaf_area" = "Leaf area [m² leaves/plant]",
              "lai_lat" = "Green cover area (lateral view) [m²/plant]",
              "lai_abv" = "Green cover area (above view) [m²/plant]")

dataset_plot <- dataset %>%
  gather("lai", "lai_lat", "lai_abv", "leaf_area", key = variable,
         value = measurement)

ggplot(data = dataset_plot) +
  facet_wrap("variable", scales = "free",
             labeller = labeller(variable = plot_vars)) +
  geom_point(aes(dat, measurement, colour = as.factor(cycle)), size = 1.5) +
  #geom_text(aes(x=dat, y=measurement, label=id),hjust=-0.4, vjust=0, size=3) +
  labs(x = "Days after transplanting",
       y = "",
       colour = "Cycle") +
  theme_vert +
  scale_colour_brewer(name="Cycle",
                      breaks=c("2", "3", "4"),
                      labels=c("1",
                               "2",
                               "3"),
                      palette = "Set1", type = "qual")

plot_file_name <- paste0(path_figures,
                         'Fig17_calib-LAI.png')
ggsave(plot_file_name,
       width = 18, height = 14, units = "cm",
       family = "serif")


# Fig 18 - Biomass - Calibration ------------------------------------------

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

plot_part = c("w_plant" = "Aboveground biomass [g/plant]", 
              "wf_plant" = "Fruit weight [g/plant]", 
              "wm_plant" = "Mature fruit weight [g/plant]")

plot_cond = c("dm" = "Dry mass [g DM/plant]",
              "fm" = "Fresh mass [g FM/plant]")

ggplot(data = dataset_mod) +
  facet_grid(type ~ variable_mod, scales = "free",
             labeller = labeller(type = plot_cond
                                 #variable_mod = plot_part
                                 )) +
  geom_point(aes(x = dat, y = measurement,
                 colour = as.factor(cycle)), size = 1) +
  labs(x = "Days after transplanting",
       y = "",
       colour = "Cycle") +
  theme_vert +
  scale_colour_brewer(breaks=c("2", "3", "4"),
                      labels=c("1",
                               "2",
                               "3"),
                      palette = "Set1", type = "qual")

plot_file_name <- paste0(path_figures,
                         'Fig18_calib-biomass.png')
ggsave(plot_file_name,
       width = 25.0, height = 10.0, units = "cm",
       family = "serif", dpi = 320)

# Fig 19 - Green area - Monitoring ----------------------------------------
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
                                 cycle = cycles)) +
  geom_point(data = dataset_plot,
             aes(x = dat, y = measurement,
                 colour = as.factor(node)), 
             size = 1.5) +
  geom_point(data = calib,
             aes(x = dat, y = measurement,
                 colour = as.factor(node)), 
             size = 1.5) +
  labs(x = "Days after transplanting",
       y = "") +
  scale_colour_brewer(name="Plant",
                      breaks=c("1", "2", "calib"),
                      labels=c("1",
                               "2",
                               "Calib."),
                      palette = "Set1", type = "qual") +
  scale_x_continuous(breaks=seq(0,110,15)) +
  theme_vert
plot_file_name <- paste0(path_figures,
                         'Fig19_monit.png')
ggsave(plot_file_name,
       width = 18, height = 21, units = "cm",
       family = "serif")


# Fig 20 - SI - Params ----------------------------------------------------
dataset_plot <- all_si %>%
  filter(city == "cps", model == "tomgro") %>%
  rename(type = index) %>%
  filter(type == "ST") %>%
  gather(n, lai, w, wf, wm,
         key = "variable", value = index) %>%
  mutate(index = if_else(is.na(index) | is.nan(index), 0., index)) %>%
  filter(index > 0.0, index <= 10,
         !((dat < 40) & (variable == "wm" | variable == "wf"))
         # factors != "N_b", factors != "N_FF", factors != "N_max",
         # factors != "LAI_max", factors != "alpha_F",
         # factors != "T_crit"
  ) %>%
  group_by(factors, variable, type, dat, city, model) %>%
  mutate(index_avg = mean(index, na.rm = TRUE),
         index_min = quantile(index, 0.1, na.rm = TRUE),
         index_max = quantile(index, 0.9, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(index_avg > 0.05) %>%
  arrange(variable, factors) %>%
  mutate(test1 = index_min + index_avg,
         test2 = index_max - index_avg)

ggplot(dataset_plot) +
  facet_wrap("variable", nrow = 3,
             labeller = labeller(variable = plot_states_1L)) +
  geom_line(data = dataset_plot,
            aes(x=dat, y=index_avg,
                col = factors),
            size = 1)   +
  geom_point(data = dataset_plot,
             aes(x=dat, y=index, color = factors), alpha = 0.5) +
  labs(x = "Days after transplanting",
       y = "ST",
       colour = "Parameters") +
  ylim(0, 1.2) +
  scale_discrete_manual(values = paletteer_d("ggthemes::stata_s1color"),
                        aesthetics = c("colour", "fill"),
                        name = "Parameters",
                        guide=guide_legend(nrow = 2,
                                           keywidth = 0.7,
                                           keyheight = 0.7)) +
  labs(x = "Days after transplanting",
       y = "") +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig20_si-param.png')
ggsave(plot_file_name,
       width = 15, height = 19, units = "cm",
       family = "serif")

# Fig 21 - Unc - Params ---------------------------------------------------
dataset_plot <- all_curves_params %>%
  gather(-c("das", "it_fac", "id",
            "case", "model", "city", "exp",
            "param", "value", "flag"),
         key = "variable", value = "measurement") %>%
  filter(model == "tomgro", city == "cps")

plots_all <- c("n", "lai", "w", "wf", "wm")
plots_names <- c("Number of nodes", "LAI", "Aboveground biomass", 
                 "Fruit biomass", "Mature fruit biomass")

plots <- list()
it <- 1
for (it in 1:length(plots_all)){
  
  case1_hi_filt <- case1_hi %>%
    filter(variable == plots_all[it],
           model == "tomgro", city == "cps")
  
  plot_filt <- dataset_plot %>%
    filter(param %in% case1_hi_filt$factors,
           variable == plots_all[it])
  
  if (it < 5){
    lab_X <- ""
  } else {
    lab_X <- "Days after simulation started"
  }
  
  lab_X <- "Days after simulation started"
  
  plots[[it]] <- ggplot() +
    facet_wrap("param") +
    geom_line(data = plot_filt, 
              aes(das, measurement, group = it_fac, colour = flag)) +
    labs(x = lab_X, y = "", title = plots_names[it]) +
    scale_colour_manual(values = c("firebrick3", "dodgerblue2"),
                        breaks=c("max", "min"),
                        labels=c("Largest values", "Smallest values"),
                        name = "") +
    theme_vert +
    theme(plot.title = element_text(face = "bold"))
  
}

plot <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) / (plots[[5]] + plot_spacer()) 
plot + plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

plot_file_name <- paste0(path_figures,
                         'Fig21_unc-param.png')
ggsave(plot_file_name,
       width = 21, height = 18, units = "cm",
       family = "serif")

# Fig 22 - SI - Weather ---------------------------------------------------
all_si_plot <- all_si_weather %>%
  mutate(wm = na_if(wm, -99)) %>%
  filter(index == "ST", city == "cps")

tomgro <- all_si_plot %>%
  filter(model == "tomgro") %>%
  gather(lai, w, wf, wm, n,
         key = "variable", value = "index") %>%
  mutate(index = if_else(is.na(index) | is.nan(index), 0., index)) %>%
  filter(index > 0)

ylim_ <- 1.2

ggplot() +
  facet_wrap("variable", nrow = 3, labeller = labeller(variable = plot_states_1L)) +
  geom_area(data = tomgro,
            aes(x=dat, y=index,
                fill = factors),
            size = 1, linetype = 1, colour = "black") +
  ylim(0, ylim_) +
  labs(x = "Days after simulation started", y = "") +
  scale_fill_brewer(name="Factors",
                      breaks=c("co2", "rad", "tmean"),
                      labels=c("Carbon dioxide",
                               "Solar radiation",
                               "Average temperature"),
                      palette = "Set1", type = "qual") +
  theme_vert +
  theme(panel.background = element_rect(fill = "grey99"))

plot_file_name <- paste0(path_figures,
                         'Fig22_si-weather.png')
ggsave(plot_file_name,
       width = 15, height = 19, units = "cm",
       family = "serif")

# Fig 23 - Unc - Weather --------------------------------------------------
dataset_plot <- all_results_plot_ext %>%
  filter(model == "tomgro",
         city == "cps") %>%
  gather(-c("das", "it_fac", "id",
            "case", "model", "city", "exp",
            "param", "value"),
         key = "variable", value = "measurement") %>%
  group_by(das, param, variable) %>%
  mutate(flag = if_else(value >= quantile(value, 0.9), "max", 
                        if_else(value <= quantile(value, 0.1), "min", "none"))) %>%
  ungroup() %>%
  filter(flag != "none", !is.na(measurement))

plots_all <- c("n", "lai", "w", "wf", "wm")
plots_names <- c("Number of nodes", "LAI", "Aboveground biomass", 
                 "Fruit biomass", "Mature fruit biomass")

plots <- list()
it <- 1
lab_X <- "Days after simulation started"

ggplot() +
  facet_grid(variable ~ param, scales = "free",
             labeller = labeller(variable = plot_states,
                                 param = factors_weather)) +
  geom_line(data = dataset_plot, 
            aes(das, measurement, group = it_fac, col = flag)) +
  labs(x = lab_X, y = "") +
  scale_colour_manual(values = c("firebrick3", "dodgerblue2"),
                      breaks=c("max", "min"),
                      labels=c("Largest values", "Smallest values"),
                      name = "") +
  theme_vert +
  theme(plot.title = element_text(face = "bold"))


plot_file_name <- paste0(path_figures,
                         'Fig23_unc-param.png')
ggsave(plot_file_name,
       width = 21, height = 21, units = "cm",
       family = "serif")

# Fig 24 - Calibs - Gnv ---------------------------------------------------

obs_mod <- obs %>%
  filter(variable %in% names(plot_states))

model_plot <- models %>%
  filter(city == "gnv", calib == "gnv" | calib == "gnvopt",
         exp == "n04" | exp == "n05" | exp == "n06") %>%
  filter(model == "tomgro") %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm")) %>%
  mutate(type = if_else(exp == "n01" | exp == "n04", "Cool",
                        if_else(exp == "n02" | exp == "n05", "Warm", "Hot")),
         type_ = factor(type,
                        levels = c("Cool", "Warm", "Hot"),
                        ordered = TRUE),
         calib_ = if_else(calib == "gnvopt", "Optimization", "Original"))

obs_plot <- obs_mod %>%
  filter(city == "gnv",
         exp == "n04" | exp == "n05" | exp == "n06") %>%
  select(-model) %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm")) %>%
  mutate(year = if_else(exp == "n01" | exp == "n02" | exp == "n03", 1993, 1994),
         type = if_else(exp == "n01" | exp == "n04", "Cool",
                        if_else(exp == "n02" | exp == "n05", "Warm", "Hot")),
         type_ = factor(type,
                        levels = c("Cool", "Warm", "Hot"),
                        ordered = TRUE))

ggplot() +
  facet_grid(variable ~ type_,
             labeller = labeller(variable = plot_states),
             scales = "free",
             space = "free_x") +
  geom_line(data = model_plot, aes(das, measurement, col = calib_),
            size = 0.8) +
  geom_point(data = obs_plot, aes(das, measurement),
             size = 1.5, shape = 15) +
  labs(x = "Days after simulation started",
       y = "",
       colour = "Calibration") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")[c(2, 3, 1)]) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig24_tomgro-gnv-opt.png')
ggsave(plot_file_name,
       width = 15, height = 18, units = "cm",
       family = "serif")


# Fig 25 - Calibs - Cps ---------------------------------------------------

obs_mod <- obs %>%
  filter(variable %in% names(plot_states))

model_plot <- models %>%
  filter(city == "cps", calib == "cpsman" | calib == "cpsopt",
         exp == "n08") %>%
  filter(model == "tomgro") %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm")) %>%
  mutate(calib_ = if_else(calib == "cpsopt", "Optimization", "Manual"))

obs_plot <- obs_mod %>%
  filter(city == "cps",
         exp == "n08") %>%
  select(-model) %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm"))

ggplot() +
  facet_wrap("variable", nrow = 3,
             labeller = labeller(variable = plot_states),
             scales = "free") +
  geom_line(data = model_plot, aes(das, measurement, col = calib_),
            size = 0.8) +
  geom_point(data = obs_plot, aes(das, measurement),
             size = 1.5, shape = 15) +
  labs(x = "Days after simulation started",
       y = "",
       colour = "Calibration") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")[c(3, 2, 1)]) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig25_tomgro-cps-opt.png')
ggsave(plot_file_name,
       width = 15, height = 18, units = "cm",
       family = "serif")


# Fig 26 - Calib Opt All - Cps --------------------------------------------

obs_mod <- obs %>%
  filter(variable %in% names(plot_states))

model_plot <- models %>%
  filter(city == "cps", 
         (((calib == "cps3" | calib == "gnvMod") & (exp == "n05" | exp == "n06")) |
            ((calib == "cps4" | calib == "gnvMod") & (exp == "n07" | exp == "n08")) | 
            ((calib == "cps2" | calib == "gnvMod") & (exp == "n03" | exp == "n04")) ),
         model == "tomgro") %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm")) %>%
  left_join(codes_exp) %>%
  mutate(cycle = as.factor(cycle),
         node = as.factor(node),
         calib_ = if_else(calib == "gnvMod", "Not Calibrated", "Calibrated"))

obs_plot <- obs_mod %>%
  filter(city == "cps",
         exp == "n03" | exp == "n04" | exp == "n05" |
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  select(-model) %>%
  filter(variable %in% c("lai", 'n', "w", "wf", "wm"))

ggplot() +
  facet_grid(variable ~ cycle,
             labeller = labeller(variable = plot_states,
                                 cycle = cycles),
             scales = "free") +
  geom_line(data = model_plot, aes(das, measurement, 
                                   col = calib_, lty = node),
            size = 0.8) +
  geom_point(data = obs_plot, aes(das, measurement),
             size = 1, shape = 15) +
  labs(x = "Days after simulation started",
       y = "",
       colour = "Calibration",
       lty = "Plant") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")[c(2, 3, 1)]) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig26_tomgro-cps-opt.png')
ggsave(plot_file_name,
       width = 15, height = 18, units = "cm",
       family = "serif")


# Fig 27 - Scatterplots - Obs model ---------------------------------------

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
  scale_discrete_manual(values = paletteer_d("RColorBrewer::Set1"), 
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
  scale_discrete_manual(values = paletteer_d("RColorBrewer::Set1"), 
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
  scale_discrete_manual(values = paletteer_d("RColorBrewer::Set1"), 
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
  scale_discrete_manual(values = paletteer_d("RColorBrewer::Set1"), 
                        aesthetics = c("fill"),
                        name = "Cycle") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99")) +
  theme(legend.position = "none")

plot_all <- ((plot2a + plot2b) / (plot3a + plot3b))  +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

plot_file_name <- paste0(path_figures,
                         'Fig27_scatterplots_obs_calib.png')
ggsave(plot_file_name,
       width = 15, height = 16, units = "cm",
       family = "serif")

# Fig 28 - Assim - Wf_lat -------------------------------------------------

upd <- allStates %>%
  filter(das != 0) %>%
  filter(meas_var == "wf_lat", sensor_type == "A",
         frequency == 1,
         calib == "gnvMod") %>%
  mutate(model = filt) %>%
  filter(filt != "pf")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
            (calib == "cps3" & (exp == "n05" | exp == "n06")) |
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
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"),
         sensor == "A")

assim <- upd %>%
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

obs_plot <- obs %>%
  filter(city == "cps") %>%
  select(-model) %>%
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

obs_last_i <- obs_last %>%
  separate(city_exp, into = c("city", "exp")) %>%
  rename(das = dat) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  filter(exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

ggplot() +
  facet_grid(variable ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(variable = plot_states,
                                 exp = codes_plants)) +
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

plot_file_name <- paste0(path_figures,
                         'Fig28_assim_wflat.png')
ggsave(plot_file_name,
       width = 25, height = 18, units = "cm",
       family = "serif")


# Fig 29 - Assim - Lai_lat ------------------------------------------------

upd <- allStates %>%
  filter(das != 0) %>%
  filter(meas_var == "lai_lat",
         frequency == 1, sensor_type == "A",
         calib == "gnvMod") %>%
  mutate(model = filt) %>%
  filter(filt != "pf")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
           (calib == "cps3" & (exp == "n05" | exp == "n06")) |
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
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"),
         sensor == "A")

assim <- upd %>%
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

obs_plot <- obs %>%
  filter(city == "cps") %>%
  select(-model) %>%
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

obs_last_i <- obs_last %>%
  separate(city_exp, into = c("city", "exp")) %>%
  rename(das = dat) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  filter(exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

ggplot() +
  facet_grid(variable ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(variable = plot_states,
                                 exp = codes_plants)) +
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

plot_file_name <- paste0(path_figures,
                         'Fig29_assim_lailat.png')
ggsave(plot_file_name,
       width = 25, height = 18, units = "cm",
       family = "serif")


# Fig 30 - Assim - Lai_abv ------------------------------------------------

upd <- allStates %>%
  filter(das != 0) %>%
  filter(meas_var == "lai_abv", sensor_type == "A",
         frequency == 1,
         calib == "gnvMod") %>%
  mutate(model = filt) %>%
  filter(filt != "pf")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
            (calib == "cps3" & (exp == "n05" | exp == "n06")) |
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
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"), sensor == "A")

assim <- upd %>%
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

obs_plot <- obs %>%
  filter(city == "cps") %>%
  select(-model) %>%
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

obs_last_i <- obs_last %>%
  separate(city_exp, into = c("city", "exp")) %>%
  rename(das = dat) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  filter(exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

ggplot() +
  facet_grid(variable ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(variable = plot_states,
                                 exp = codes_plants)) +
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

plot_file_name <- paste0(path_figures,
                         'Fig30_assim_laiabv.png')
ggsave(plot_file_name,
       width = 25, height = 18, units = "cm",
       family = "serif")


# Fig 31 - Assim - W_fm_full ----------------------------------------------
upd <- allStates %>%
  filter(das != 0) %>%
  filter(meas_var == "w_fm_full", sensor_type == "A",
         frequency == 1,
         calib == "gnvMod") %>%
  mutate(model = filt) %>%
  filter(filt != "pf")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         ((calib == "cps2" & (exp == "n03" | exp == "n04")) |
            (calib == "cps3" & (exp == "n05" | exp == "n06")) |
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
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"), sensor == "A")

assim <- upd %>%
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

obs_plot <- obs %>%
  filter(city == "cps") %>%
  select(-model) %>%
  filter(exp == "n03" | exp == "n04" | exp == "n05" | 
           exp == "n06" | exp == "n07" | exp == "n08") %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

obs_last_i <- obs_last %>%
  separate(city_exp, into = c("city", "exp")) %>%
  rename(das = dat) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  filter(exp == "n05" | exp == "n06" | exp == "n07" | exp == "n08")

ggplot() +
  facet_grid(variable ~ exp,
             scales = "free",
             space = "free_x",
             drop = TRUE,
             labeller = labeller(variable = plot_states,
                                 exp = codes_plants)) +
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

plot_file_name <- paste0(path_figures,
                         'Fig31_assim_w_fm_full.png')
ggsave(plot_file_name,
       width = 25, height = 18, units = "cm",
       family = "serif")

# Fig 32 - Assim - Low-cost Wf --------------------------------------------
Pdata <- upd_assim %>%
  filter(as.numeric(id) >= 916, as.numeric(id) <= 931) %>%
  mutate(measurement = as.numeric(measurement),
         das = as.numeric(das)) %>%
  filter(!grepl("_s", variable),
         !grepl("wm", variable),
         !grepl("Resid", variable),
         !grepl("Cov_pred", variable),
         variable == "Gain" | 
           variable == "R" | variable == "P_m") %>%
  mutate(var_est = if_else(((as.numeric(id) >= 916) & (as.numeric(id) <= 919)) |  
                             ((as.numeric(id) >= 924) & (as.numeric(id) <= 927)),
                           "wm", "wf"),
         noise =  if_else((as.numeric(id) >= 916) & (as.numeric(id) <= 923),
                          "10 %", "30 %"))

data_wf <- Pdata %>%
  filter(var_est == "wf")

ggplot() +
  facet_grid(variable ~ exp, scales = "free",
             labeller = labeller(variable = c("Gain" = "Gain",
                                              "R" = "Observation covariance",
                                              "P_m" = "State covariance"),
                                 exp = c("n01" = "Cycle 0",
                                         "n03" = "Cycle 1",
                                         "n05" = "Cycle 2",
                                         "n07" = "Cycle 3"))) + 
  geom_point(data = data_wf, 
             aes(das, measurement,
                 group = rep, col = as.factor(noise)),
             alpha = 0.5) +
  labs(x = "Days after simulation started", 
       y = "",
       colour = "Noise level") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")) +
  theme_vert

plot_file_name <- paste0(path_figures,
                         'Fig32_assim_lowcost_wf.png')
ggsave(plot_file_name,
       width = 18, height = 15, units = "cm",
       family = "serif")

# Fig 33 - Assim - Low-cost Wm --------------------------------------------

Pdata <- upd_assim %>%
  filter(as.numeric(id) >= 916, as.numeric(id) <= 931) %>%
  mutate(measurement = as.numeric(measurement),
         das = as.numeric(das)) %>%
  filter(!grepl("_s", variable),
         !grepl("wm", variable),
         !grepl("Resid", variable),
         !grepl("Cov_pred", variable),
         variable == "Gain" | 
           variable == "R" | variable == "P_m") %>%
  mutate(var_est = if_else(((as.numeric(id) >= 916) & (as.numeric(id) <= 919)) |  
                             ((as.numeric(id) >= 924) & (as.numeric(id) <= 927)),
                           "wm", "wf"),
         noise =  if_else((as.numeric(id) >= 916) & (as.numeric(id) <= 923),
                          "10 %", "30 %"))

data_wm <- Pdata %>%
  filter(var_est == "wm")

ggplot() +
  facet_grid(variable ~ exp, scales = "free",
             labeller = labeller(variable = c("Gain" = "Gain",
                                              "R" = "Observation covariance",
                                              "P_m" = "State covariance"),
                                 exp = c("n01" = "Cycle 0",
                                         "n03" = "Cycle 1",
                                         "n05" = "Cycle 2",
                                         "n07" = "Cycle 3"))) + 
  geom_point(data = data_wm, 
             aes(das, measurement,
                 group = rep, col = as.factor(noise)),
             alpha = 0.5) +
  labs(x = "Days after simulation started", 
       y = "",
       colour = "Noise level") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")) +
  theme_vert


plot_file_name <- paste0(path_figures,
                         'Fig33_assim_lowcost_wm.png')
ggsave(plot_file_name,
       width = 18, height = 15, units = "cm",
       family = "serif")
