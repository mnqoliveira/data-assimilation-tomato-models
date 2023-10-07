# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("ggplot2")
library("tidyverse")
library("lubridate")
library("RColorBrewer")

library("paletteer")
library("scales")
library("facetscales")

# Source functions --------------------------------------------------------

# Update results
# source("./07a-orgSimOut.R")
# source("./07c-orgAssim.R")

# Load theme --------------------------------------------------------------
load("../data/plot_theme_vert.RData")

theme_vert <- theme_vert +
  theme(panel.background = element_rect(fill = "grey99"),
        strip.text.x = element_text(size = 9,
                              margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1,
                                              unit = "cm")),
        strip.text.y = element_text(size = 9))

# Load data ---------------------------------------------------------------
# Dates
cycle_dates <- read.csv("../data/cycle_dates.csv")
codes_exp <- read.csv("../tables/codes_exp.csv")
plant_dates <- read.csv("../tables/relevant_dates_plants.csv")
colnames(plant_dates)[1] <- "cycle"

# Weather: no missing values, no outliers, hourly
weather_hourly <- read.csv("../data/weather/monitoring/weather_hourly.csv")

# External radiation
rad_power <- read.csv("../tests/assess_sensors/POWER_Point_Daily_19870101_20220415_022d8186S_047d0609W_LST.csv",
                      skip = 18)

# Simulations
models_all <- read.csv( "../tables/results_simul/results_simulations_all.csv")

# Uncertainty outputs
load("../tables/results_SA/all_curves_weather.RData")

load("../tables/results_SA/all_si_weather.RData")
all_si_weather <- all_si

load("../tables/results_SA/all_si.RData")
case1_hi <- read.csv("../tables/results_SA/case1_highest_indices.csv")

# Assimilation results
load("../tables/results_DA/aux_files/all_states.RData")
load("../tables/results_DA/aux_files/upd_states.RData")

# Assimilation
info_runs <- rbind(read.csv("../tables/runs_Filter2.csv")) %>%
  mutate(exp_int = exp,
         exp = paste0("n0", exp_int)) %>%
  select(-it, -comment)

# Observations
obs <- read.csv("../data/synthetic/obs_artif_all.csv")
obs_real <- read.csv("../data/observations/full_set_obs.csv")
obs_ids <- read.csv("../data/observations/monitoring/obs_exp_all_ids.csv")

# Load dictionary of measurement
dict <- read.csv("../tables/dictionary_paramsFilter.csv")

# Additional variables ----------------------------------------------------

path_figures <- "../../modeling/paper/paper4-lowCost/figures/"

cycles <- c("2" = "Cycle 1",
            "3" = "Cycle 2",
            "4" = "Cycle 3")

exps <- c("n04" = "Cycle 1",
          "n06" = "Cycle 2",
          "n08" = "Cycle 3")

sensor_names <- c("li1400_radiation" = "PAR\nQuantum sensor",
                  "pi_radiation" = "Lux\nLuxmeter",
                  "radio_temperature" = "Temperature\nSHT75",
                  "pi_temperature" = "Temperature\nDHT22",
                  "radio_humidity" = "Humidity\nSHT75",
                  "pi_humidity" = "Humidity\nDHT22")

plot_states <- c("lai" = "LAI\n[m² leaves/\nm² soil]", 
                 "n" = "N\n[number of nodes/\nm² soil]", 
                 "w" = "Aboveground\n biomass\n[g D.M./m² soil]", 
                 "wf" = "Fruit dry mass\n[g D.M./m² soil]", 
                 "wm" = "Mature fruit\ndry mass\n[g D.M./m² soil]")

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


# Org variables -----------------------------------------------------------

models <- models_all %>%
  mutate(city_exp = paste(city, exp, sep = "_"),
         exp_full = paste(model, city_exp, sep = "_")) %>%
  filter(exp == "n04" | exp == "n06" | exp == "n08", 
         model == "tomgro", city == "cps")

simulations <- models_all %>%
  filter(city == "cps", exp != "n01", exp != "n02",
         variable != "dw", variable != "rm", variable != "pg") %>%
  filter(exp == "n04" | exp == "n06" | exp == "n08", 
         model == "tomgro")

assim_artif <- all_assim %>%
  mutate(id = as.integer(id)) %>%
  filter(id > 500) %>%
  right_join(info_runs)


upd_assim_ <- upd_assim %>%
  mutate(id = as.integer(id)) %>%
  filter(as.numeric(id) >= 916, as.numeric(id) <= 939) %>%
  right_join(info_runs)

obs_mod <- obs %>%
  gather(wf, wm, key = "variable", value = "measurement")

obs_real_ <- obs_real %>%
  gather(names(plot_states), key = "variable", value = "measurement") %>%
  filter(city == "cps", node == "calib") %>%
  select(cycle, dat, date, exp, variable, measurement)
  
# Fig 1 - Weather ---------------------------------------------------------

weather_mod <- weather_hourly %>%
  filter(!is.na(cycle), value < 3000, 
         cycle != 1, 
         node == 2) %>%
  separate(stat_sensor_var, sep = "_", into = c("stat", "sensor", "var")) %>%
  mutate(stat_var = str_c(var, stat, sep = "_"), date = as.Date(date)) %>%
  filter((stat == "mean" & var == "radiation") | 
           (var == "temperature" & stat != "med")) %>%
  select(-stat, -var, -starts_with("flag")) %>%
  spread(stat_var, value) %>%
  group_by(cycle, date, node, sensor) %>%
  summarise(rad = sum(radiation_mean),
            tmax = max(temperature_max),
            tmin = min(temperature_min),
            tmean = mean(temperature_mean)) %>%
  ungroup() %>%
  gather(c("rad", "tmax", "tmin", "tmean"), 
         key = "variable", value = "measurement") %>%
  filter(!(variable == "rad" & sensor == "radio") & 
           !(variable != "rad" & (sensor == "li1400" | sensor == "none")) & 
           !(variable == "rad" & sensor == "none")) %>%
  mutate(var = if_else(variable == "rad", "radiation", "temperature"),
         stat = if_else(variable == "rad", "sum", 
                        substring(variable, 2, nchar(variable))),
         measurement = if_else(variable == "rad", 
                               # Convert to global
                               measurement/555.6, 
                               measurement),
         sensor_ = factor(sensor,
                          levels = c("li1400", "pi", "radio", "none"),
                          labels = c("SG", "LC", "SG", "External")))

weather_mod_ <- weather_mod %>%
  mutate(var_ = factor(var,
                       levels = c("radiation", "temperature"),
                       labels = c("PAR~(MJ~m^{-2}~day^{-1})",
                                  "Temperature~('°C')")),
         cycle_ = factor(cycle,
                         levels = names(cycles),
                         labels = cycles))


# rad_ext <- rad_power %>%
#   rename_all(tolower) %>%
#   mutate(date = as.Date(paste(year, doy, sep = "-"), tryFormats = c("%Y-%j"))) %>%
#   rename(rad_ext_d = allsky_sfc_sw_dwn) %>%
#   select(date, rad_ext_d) %>%
#   mutate(date = as.character(date)) %>%
#   right_join(cycle_dates) %>%
#   mutate(#rad = rad_ext_d * 555.6,
#     rad = rad_ext_d*0.7,
#     date = as.Date(date),
#     var = "radiation",
#     stat = "sum") %>%
#   filter(cycle != 1, rad > 0)
# 
# rad_ext <- rad_ext %>%
#   mutate(var_ = factor(var,
#                        levels = c("radiation", "temperature"),
#                        labels = c("PAR~(MJ~m^{-2}~day^{-1})",
#                                   "Temperature~('°C')")),
#          cycle_ = factor(cycle,
#                          levels = names(cycles),
#                          labels = cycles))

plant_dates <- plant_dates %>%
  filter(event == "stage", comment != "vegetative", comment != "harvest")

# "Temperature~('°C')", 
# "Light~(µmol~m^{-2}~s^{-1})")

ggplot() +
  facet_grid(var_ ~ cycle_, scales = "free",
             labeller = labeller(cycle_ = label_value, 
                                 var_ = label_parsed),
             space = "free_x",
             switch = "y") +
  geom_line(data=weather_mod_, 
            aes(x = date, y = measurement,  
                colour = stat, lty = sensor_), size = 0.5) +
  # geom_point(data=rad_ext, aes(x = date, y = rad,
  #                              ), colour = "black",
  #            #size = 0.5
  #            ) +
  labs(x = "",
       y = "",
       lty = "Sensor") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")[c(1, 3, 2, 4)],
                      name = "Summary",
                      breaks=c("max", "min", "mean", "sum"),
                      labels=c("Daily maximum", 
                               "Daily minimum", 
                               "Daily average",
                               "Daily sum")) + 
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%d %b %y") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(override.aes = list(size = 1), nrow = 2)) +
  theme_vert

plot_name <- "Fig1-rad_meas"
plot_file_name <- paste0('../paper/paper4-lowCost/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 18, height = 12, units = "cm",
       family = "serif")


# Fig 2 - Pairs Radiation -------------------------------------------------
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

ks <- dataset %>%
  group_by(cycle) %>%
  summarise(p = as.numeric(ks.test(li1400, pi, alternative='two.sided')["p.value"]),
            ks = as.numeric(ks.test(li1400, pi, alternative='two.sided')["statistic"])) %>%
  mutate(ks = round(ks, 2))

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
  geom_text(data=ks, 
            aes(label=paste("D=", ks, sep="")), 
            x=1200, y=200) +
  theme_vert

plot_name <- "Fig2-correlations"
plot_file_name <- paste0('../paper/paper4-lowCost/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 16, height = 8, units = "cm",
       family = "serif")


# Fig 3 - Simulations -----------------------------------------------------
simul_calib <- models_all %>%
  filter(city == "cps", exp != "n01", exp != "n02",
         variable != "dw",
         # variable != "lai",
         # variable != "n",
         variable != "rm", 
         sensor != "C",
         #variable != "pg"
         ) %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cpsIV") %>%
  mutate(sensor_ = factor(sensor,
                          levels = c("A", "B", "C"),
                          labels = c("Scientific grade",
                                     "Low-cost", "External")),
         city_exp = paste(city, exp, sep = "_")) %>%
  left_join(codes_exp) %>%
  filter(node == 2)

plot_states_ = c(plot_states,
                "pg" = "Gross \nphotosynthesis\n[g CH2O/plant/day]",
                "rm" = "Respiration\n[g CH2O/plant/day]")

# obs_plot <- obs_real_ %>%
#   rename(das = dat) %>%
#   filter(cycle == 4)

obs_plot <- obs_ids %>%
  gather(names(plot_states), key = "variable", value = "measurement") %>%
  rename(das = dat) %>%
  filter(cycle == 4)

ggplot() +
  facet_grid(variable ~ cycle,
             labeller = labeller(variable = plot_states_,
                                 cycle = cycles),
             scales = "free",
             space = "free_x",
             switch = "y") +
  geom_line(data = simul_calib, aes(das, measurement, colour = sensor_),
            size = 0.8) +
  # geom_point(data = obs_plot, aes(das, measurement)) +
  labs(x = "Days after simulation started",
       y = "",
       colour = "Measurement source") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")) +
  theme_vert

plot_name <- "Fig3-curves-simul"
plot_file_name <- paste0('../paper/paper4-lowCost/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 18, height = 20, units = "cm",
       family = "serif")


# Fig 4 - Assim Wf --------------------------------------------------------

upd <- assim_artif %>%
  filter((config >= 11 & config <= 13),
         (filt == "enkf" & case == "case3"),
         calib == "cpsIV") %>%
  mutate(filt_ = factor(filt,
                        levels = c("enkf", "simul_B_gnvMod", 
                                   "simul_A_gnvMod", "cpsIVA", "cpsIVB"),
                        labels = c("Lux meter, assim.", "Lux meter, not calib.", 
                                   "QS, not calib.", "Sim. truth", "Lux meter, calib.")),
         config_ = if_else(config == 11 | config == 4, "10 %",
                           if_else(config == 12 | config == 5, "30 %", 
                                   if_else(config == 13 | config == 6, "50 %", "0")))) %>%
  filter(exp == "n04" | exp == "n06" | exp == "n08")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cpsIV", sensor != "C") %>%
  select(-calib) %>%
  mutate(filt_ = factor(if_else(sensor == "A", "cpsIVA", "cpsIVB"),
                        levels = c("enkf", "simul_B_gnvMod", 
                                   "simul_A_gnvMod", "cpsIVA", "cpsIVB"),
                        labels = c("Lux meter, assim.", "Lux meter, not calib.", 
                                   "QS, not calib.", "Sim. truth", "Lux meter, calib.")))

simuls <- simul_calib %>%
    filter(variable == "wm") 

assim <- upd %>%
  filter(meas_var == "wf", variable == "wm", sensor_type == "B")

ggplot() +
  facet_grid(config_ ~ exp, scales = "free",
             labeller = labeller(exp = exps)) + 
  # Simulations
  geom_line(data = simuls, aes(das, measurement,
                                colour = filt_),
            size = 1) +
  # Assimilation
  geom_line(data = assim, aes(das, measurement, group = rep, 
                               colour = filt_),
            alpha = 0.25,
            size = 1) +
  labs(x = "Days after simulation started", 
       y = "Mature fruit biomass [g/m²]",
       colour = "") +
  scale_colour_manual(values = paletteer_dynamic("ggthemes_solarized::yellow",
                                                 5)) +
  guides(colour=guide_legend(override.aes = list(size = 2),
                             title.theme = element_text(face = "bold", size=9))) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- "Fig4-curves-assim_noises"
plot_file_name <- paste0('../paper/paper4-lowCost/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 20, height = 16, units = "cm",
       family = "serif")

# Fig 5 - Hyperparameters -------------------------------------------------

Pdata <- upd_assim_ %>%
  filter(calib == "cpsIV") %>%
  mutate(measurement = as.numeric(measurement),
         das = as.numeric(das)) %>%
  filter(!grepl("_s", variable),
         !grepl("wm", variable),
         !grepl("Resid", variable),
         !grepl("Cov_pred", variable),
         variable == "Gain" | 
           variable == "R" | variable == "P_m" | variable == "P_upd_m") %>%
  mutate(var_est = if_else(((as.numeric(id) >= 916) & (as.numeric(id) <= 919)) |  
                             ((as.numeric(id) >= 924) & (as.numeric(id) <= 927)) |
                                ((as.numeric(id) >= 932) & (as.numeric(id) <= 935)),
                           "wm", "wf"),
         noise =  if_else((as.numeric(id) >= 916) & (as.numeric(id) <= 923),
                          "10 %", 
                          if_else((as.numeric(id) >= 924) & (as.numeric(id) <= 931),
                                  "30 %", "50 %")))

Pdata1 <- Pdata %>%
  filter(variable == "R" | variable == "Gain")

Pdata2 <- Pdata %>%
  filter(variable != "R", variable != "Gain") %>%
  pivot_wider(names_from = variable, values_from = measurement) %>%
  mutate(dif = P_m - P_upd_m) %>%
  pivot_longer(c("P_upd_m", "P_m", "dif"),
               names_to = "variable", values_to = "measurement") %>%
  filter(variable != "P_m", variable != "P_upd_m")

data_wf <- rbind(Pdata1, Pdata2) %>%
  filter(var_est == "wf") 
# %>%
#   filter(variable != "P_upd_m")

scales_y <- list(
  Gain = scale_y_continuous(),
  R = scale_y_log10(),
  dif = scale_y_continuous()
)

ggplot() +
  # facet_grid(variable ~ exp, 
  #            switch = "y",
  #            scales = "free") + 
  facet_grid_sc(rows = vars(variable),
                cols = vars(exp),
                scales = list(y = scales_y),
                labeller = labeller(
                  variable = c("Gain" = "Gain",
                               "R" = "Observation covariance\n[g/m²]²",
                               "dif" = "Difference in\n state covariance[g/m²]²"),
                                    exp = exps),
                switch = "y") +
  geom_point(data = data_wf, 
             aes(x=das, y=measurement,
                 group = rep, col = as.factor(noise)),
             alpha = 0.8) +
  labs(x = "Days after simulation started", 
       y = "",
       colour = "Noise level") +
  scale_colour_manual(values = paletteer_d("RColorBrewer::Set1")) +
  theme_vert

plot_name <- "Fig5-curves-covariances"
plot_file_name <- paste0('../paper/paper4-lowCost/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 23, height = 16, units = "cm",
       family = "serif")


# Metrics sensors ---------------------------------------------------------

dataset <- weather_hourly %>%
  filter(flag_case2 == 0, flag_case4 == 0) %>%
  select(-starts_with("flag")) %>%
  separate(stat_sensor_var, into = c("stat", "sensor", "var")) %>%
  filter(stat == "mean", var == "radiation") %>%
  spread(sensor, value) %>%
  filter(li1400 <= 1500)%>%
  filter(complete.cases(.))

temp1 <- dataset %>%
  mutate(ratio = li1400/pi) %>%
  filter(is.numeric(ratio), ratio != Inf, ratio <= 5) %>%
  group_by(node, cycle) %>%
  summarise(ratio_mean = mean(li1400/pi),
            ratio_sd = sd(li1400/pi)) %>%
  filter(node == 2)

temp2 <- dataset %>%
  mutate(ratio = li1400/pi) %>%
  filter(is.numeric(ratio), ratio != Inf, ratio <= 5)

ggplot(data = temp2) +
  facet_wrap("cycle") +
  geom_histogram(aes(ratio, fill = node)) 
# +
#   geom_histogram(aes(li1400), fill = "red") +
#   geom_histogram(aes(pi), fill = "blue")
  



# Fig XX - Assim Wf in Wf -------------------------------------------------

upd <- assim_artif %>%
  filter((config >= 11 & config <= 13),
         (filt == "enkf" & case == "case3"),
         calib == "cpsIV") %>%
  mutate(filt_ = factor(filt,
                        levels = c("enkf", "simul_B_gnvMod", 
                                   "simul_A_gnvMod", "cpsIVA", "cpsIVB"),
                        labels = c("Lux meter, assim.", "Lux meter, not calib.", 
                                   "QS, not calib.", "Sim. truth", "Lux meter, calib.")),
         config_ = if_else(config == 11 | config == 4, "10 %",
                           if_else(config == 12 | config == 5, "30 %", 
                                   if_else(config == 13 | config == 6, "50 %", "0")))) %>%
  filter(exp == "n04" | exp == "n06" | exp == "n08")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cpsIV") %>%
  select(-calib) %>%
  mutate(filt_ = factor(if_else(sensor == "A", "cpsIVA", "cpsIVB"),
                        levels = c("enkf", "simul_B_gnvMod", 
                                   "simul_A_gnvMod", "cpsIVA", "cpsIVB"),
                        labels = c("Lux meter, assim.", "Lux meter, not calib.", 
                                   "QS, not calib.", "Sim. truth", "Lux meter, calib.")))

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvMod") %>%
  mutate(model = "gnvMod") %>%
  select(-calib) %>%
  mutate(filt_ = factor(if_else(sensor == "A", "simul_A_gnvMod", "simul_B_gnvMod"),
                        levels = c("enkf", "simul_B_gnvMod", 
                                   "simul_A_gnvMod", "cpsIVA", "cpsIVB"),
                        labels = c("Lux meter, assim.", "Lux meter, not calib.", 
                                   "QS, not calib.", "Sim. truth", "Lux meter, calib."))) %>%
  filter(variable == "wm") 

#simuls <- simul_calib %>%
simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable == "wf") 

assim <- upd %>%
  filter(meas_var == "wf", variable == "wf", sensor_type == "B")

obs_plot <- obs_mod %>%
  filter(variable == "wf",
         (config >= 4 & config <= 6) | (config >= 11 & config <= 13)) %>%
  mutate(config_ = if_else(config == 11 | config == 4, "10 %",
                           if_else(config == 12 | config == 5, "30 %", 
                                   if_else(config == 13 | config == 6, "50 %", "0"))),
         meas_var = variable)

ggplot() +
  facet_grid(config_ ~ exp, scales = "free",
             labeller = labeller(exp = exps),
             switch = "y") + 
  # Simulations
  geom_line(data = simuls, aes(das, measurement,
                               colour = filt_),
            size = 1) +
  # Assimilation
  geom_line(data = assim, aes(das, measurement, group = rep, 
                              colour = filt_),
            alpha = 0.3,
            size = 1) +
  # Observations
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.5) +
  labs(x = "Days after simulation started", 
       y = "",
       colour = "") +
  scale_colour_manual(values = paletteer_dynamic("ggthemes_solarized::yellow",
                                                 5)) +
  guides(colour=guide_legend(override.aes = list(size = 2),
                             title.theme = element_text(face = "bold", size=9))) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

# plot_name <- "Fig4-curves-assim_noises"
# plot_file_name <- paste0('../paper/paper4-lowCost/figures/', plot_name,'.png')
# ggsave(plot_file_name,
#        width = 20, height = 16, units = "cm",
#        family = "serif")
