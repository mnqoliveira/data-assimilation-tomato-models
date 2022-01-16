# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("ggplot2")
library("tidyverse")
library("lubridate")
library("RColorBrewer")

library(paletteer)


# Source functions --------------------------------------------------------

# # Update results
# source("./07a-orgSimOut.R")
# source("./07b-orgObs_plot.R")
# source("./07c-orgAssim.R")

# Load data ---------------------------------------------------------------
load("../data/plot_theme_vert.RData")

# Dates
cycle_dates <- read.csv("../data/cycle_dates.csv")
codes_exp <- read.csv("../tables/codes_exp.csv")

models_all <- read.csv( "../tables/results_simul/results_simulations_all.csv")

# Measured
#errors_filt <- read.csv("../tables/results_DA/aux_files/all_errors.csv")
load("../tables/results_DA/aux_files/all_errors.RData")

errors_simul <- read.csv("../tables/results_simul/all_errors.csv")

# Assimilation
info_runs <- read.csv("../tables/runs_Filter2.csv") %>%
  mutate(exp_int = exp,
         exp = paste0("n0", exp_int)) %>%
  select(-it, -comment)

# Assimilation results
# all_states_out <- read.csv("../tables/results_DA/aux_files/all_states.csv")
# upd_states_out <- read.csv("../tables/results_DA/aux_files/upd_states.csv")

load("../tables/results_DA/aux_files/all_states.RData")
load("../tables/results_DA/aux_files/upd_states.RData")

# Simulations
models_all <- read.csv( "../tables/results_simul/results_simulations_all.csv")

# Observations
obs <- read.csv("../data/synthetic/obs_artif_all.csv")

# Weather: no missing values, no outliers, hourly
weather_hourly <- read.csv("../data/weather/monitoring/weather_hourly.csv")

# External radiation
rad_power <- read.csv("../data/weather/unc/cps_POWER_SinglePoint_Daily_19870101_20210103_22d82S_47d60W.csv",
                      skip = 17)

# Ensembles
#ensembles <- read.csv("../tables/results_DA/aux_files/all_ensembles.csv")

# General variables -------------------------------------------------------
simulations <- models_all %>%
  filter(city == "cps", model == "tomgro",
         variable != "dw", variable != "rm", variable != "pg",
         calib == "gnvMod" | calib == "cps4",
         exp == "n01" | exp == "n03" | exp == "n05" | exp == "n07")

allStates <- all_assim %>%
  mutate(id = as.numeric(id)) %>%
  filter(id >= 500) %>%
  left_join(info_runs)

error_notCalib <- simulations %>%
  spread(calib, measurement) %>%
  group_by(das, model, exp, variable) %>%
  summarise(error = cps4 - gnvMod,
            abs_error = abs(error),
            rel_err = if_else(cps4 != 0, abs_error/cps4, 0)) %>%
  ungroup() %>%
  mutate(filt = "none",
         calib = "gnvMod",
         id = 0,
         config = 0) %>%
  rename(dat = das) %>%
  filter(variable == "wm")

errors_filt_artif <- all_err %>%
  mutate(config = as.numeric(config)) %>%
  left_join(info_runs) %>%
  mutate(rel_err = if_else(obs != 0, abs(error)/obs, 0),
         rel_err = rel_err * 100)

obs_mod <- obs %>%
  gather(wf, wm, key = "variable", value = "measurement")

plots_meas_short <- c("wm" = "Meas: Wm",
                      "wf" = "Meas: Wf")
plots_st_unit <- c("wm" = "Mature Fruit Dry Weight\n[g D.M./m² soil]",
                   "wf" = "Fruit Dry Weight\n[g D.M./m² soil]")
plots_st_1L <- c("wm" = "Mature Fruit Dry Weight",
                 "wf" = "Fruit Dry Weight")
plots_st_2L <- c("wm" = "Mature Fruit Dry\n Weight",
                 "wf" = "Fruit Dry Weight")
plots_exps <- c("n01" = "Exp 1",
                "n03" = "Exp 2",
                "n05" = "Exp 3",
                "n07" = "Exp 4")

plots_config <- c("1" = "Fixed error",
                  "2" = "Variable model\n error",
                  "3" = "Variable observation\n error",
                  "4" = "Both errors\n variable")

plots_freq <- c("0.1" = "10%",
                "0.5" = "50%",
                "1" = "100%")



# Fig 1 - Weather ---------------------------------------------------------

weather_mod <- weather_hourly %>%
  separate(stat_sensor_var, sep = "_", into = c("stat", "sensor", "var")) %>%
  filter(var != "humidity", !is.na(cycle), value < 3000, 
         node == 1, cycle != 1) %>%
  mutate(stat_var = str_c(var, stat, sep = "_"), date = as.Date(date)) %>%
  filter((stat == "mean" & var == "radiation")) %>%
  select(-stat, -var, -starts_with("flag")) %>%
  spread(stat_var, value) %>%
  group_by(cycle, date, node, sensor) %>%
  summarise(rad = sum(radiation_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  gather(c("rad"), 
         key = "variable", value = "measurement") %>%
  mutate(var = if_else(variable == "rad", "radiation", "temperature"),
         stat = if_else(variable == "rad", "sum", 
                        substring(variable, 2, nchar(variable))),
         sensor = factor(sensor,
                         levels = c("li1400", "pi"),
                         labels = c("Quantum", "Luximeter"))) %>%
  select(-node, -variable)

rad_ext <- rad_power %>% 
  rename_all(tolower) %>%
  mutate(date = as.Date(paste(year, doy, sep = "-"), tryFormats = c("%Y-%j"))) %>%
  rename(rad_ext_d = allsky_sfc_sw_dwn) %>%
  select(date, rad_ext_d) %>%
  mutate(date = as.character(date)) %>%
  right_join(cycle_dates) %>%
  mutate(rad = rad_ext_d * 555.6,
         date = as.Date(date),
         stat = "sum",
         sensor = "External",
         var = "radiation") %>%
  filter(cycle != 1, rad > 0) %>%
  rename(measurement = rad) %>%
  select(-dat, -rad_ext_d)

data_plot <- rbind(weather_mod, rad_ext)

ggplot() +
  facet_wrap("cycle", scales = "free",
             labeller = labeller(cycle = c("2" = "Exp 1",
                                           "3" = "Exp 2",
                                           "4" = "Exp 3"))) +
  geom_line(data=data_plot, 
            aes(x = date, y = measurement, colour = sensor), size = 0.5) +
  labs(x = "Days after transplanting",
       y = "Solar radiation [MJ m-2 day-1]",
       colour = "Data source") +
  scale_colour_manual(values = paletteer_dynamic("ggthemes_solarized::yellow",
                                                 3)) +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%b %d") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- "Fig1-rad_meas"
plot_file_name <- paste0('../paper/paper6-lowCost/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 16, height = 6, units = "cm",
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

ggplot(dataset, na.rm = TRUE) +
  geom_point(aes(x = li1400, y = pi, colour = as.factor(node)), 
             size = 0.5, alpha = 0.5) +
  facet_wrap("cycle", drop = TRUE, 
             labeller = labeller(cycle = c("2" = "Exp 1",
                                           "3" = "Exp 2",
                                           "4" = "Exp 3"))) +
  labs(x = "LI190SA [mmol PAR m-2 s-1]",
       y = "BH1750 \n[mmol PAR m-2 s-1]\n (converted from lux)",
       colour = "Sensor node") +
  coord_fixed() +
  geom_text(data=cors, 
            aes(label=paste("r=", cor, sep="")), 
            x=1200, y=55) +
  scale_colour_manual(values = paletteer_dynamic("ggthemes_solarized::magenta",
                                                 2)) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- "Fig2-correlations"
plot_file_name <- paste0('../paper/paper6-lowCost/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 16, height = 8, units = "cm",
       family = "serif")


# Fig 3 -------------------------------------------------------------------
upd <- allStates %>%
  filter((config >= 4 & config <= 6) | (config >= 11 & config <= 13),
         (filt == "enkf" & case == "case3")) %>%
  unite("filt_sensor", filt, sensor_type, remove=FALSE) %>%
  mutate(filt_ = factor(filt_sensor,
                        levels = c("enkf_A", "enkf_B", "gnvMod", "cps4"),
                        labels = c("Quantum sensor", "Luximeter", 
                                   "OL, not calib.", "Sim. truth")),
         config_ = if_else(config == 11 | config == 4, 10,
                           if_else(config == 12 | config == 5, 30, 
                                   if_else(config == 13 | config == 6, 50, 0))))

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cps4") %>%
  select(-calib) %>%
  mutate(filt_ = factor("cps4",
                        levels = c("enkf_A", "enkf_B", "gnvMod", "cps4"),
                        labels = c("Quantum sensor", "Luximeter", 
                                   "OL, not calib.", "Sim. truth")))

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvMod") %>%
  mutate(model = "gnvMod") %>%
  select(-calib) %>%
  mutate(filt_ = factor("gnvMod",
                        levels = c("enkf_A", "enkf_B", "gnvMod", "cps4"),
                        labels = c("Quantum sensor", "Luximeter", 
                                   "OL, not calib.", "Sim. truth")))

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable == "wm") 

assim <- upd %>%
  filter(variable == "wm") 

obs_plot <- obs_mod %>%
  filter(variable == "wm",
         (config >= 4 & config <= 6) | (config >= 11 & config <= 13)) %>%
  mutate(config_ = if_else(config == 11 | config == 4, 10,
                           if_else(config == 12 | config == 5, 30, 
                                   if_else(config == 13 | config == 6, 50, 0))),
         meas_var = variable)

ggplot() +
  facet_grid(meas_var + config_ ~ exp, scales = "free",
             labeller = labeller(meas_var = plots_meas_short,
                                 exp = plots_exps)) + 
  # # Simulations
  geom_point(data = simuls, aes(das, measurement,
                                colour = filt_),
             size = 1) +
  # Assimilation
  geom_point(data = assim, aes(das, measurement, group = rep, 
                               colour = filt_), shape = 21,
             size = 1) +
  # # Observations
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.5) +
  labs(x = "Days after simulation started", 
       y = "",
       colour = "") +
  scale_colour_manual(values = paletteer_dynamic("ggthemes_solarized::yellow",
                                                 4)) +
  guides(colour=guide_legend(override.aes = list(size = 2),
                             title.theme = element_text(face = "bold", size=9))) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- "Fig3-curves"
plot_file_name <- paste0('../paper/paper6-lowCost/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 23, height = 16, units = "cm",
       family = "serif")

