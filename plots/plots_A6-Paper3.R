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
#load("../tables/results_DA/aux_files/upd_states.RData")

# Simulations
models_all <- read.csv( "../tables/results_simul/results_simulations_all.csv")

# Observations
obs <- read.csv("../data/synthetic/obs_artif_all.csv")

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

plots_filt_short <- c("ukf" = "UKF",
                      "enkf" = "EnKF",
                      "pf" = "PF")

plots_filt_long <- c("enkf" = "Ensemble KF",
                     "ukf" = "Unscented KF",
                     "pf" = "Particle Filter")

# Fig 1 - Basic Config - Vanthoor -----------------------------------------
errors_mod <- errors_filt_artif %>%
  filter(variable == "wm",
         !((filt == "enkf") & (case !=  "case2"))) %>%
  filter(id >= 1000, !is.na(pred)) %>%
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
         order = as.numeric(filt),
         freq_ = factor(frequency,
                        levels = c("0.1", "0.5", "1"),
                        labels = c("10%", "50%", "100%"))) %>%
  arrange(order)

errors_null <- error_notCalib %>%
  filter(rel_err > 0, config == 0, variable == "wm") %>%
  select(-filt) %>%
  group_by(model, exp) %>%
  mutate(rel_err = rel_err * 100) %>%
  summarise(med_err = median(rel_err),
            err_25 = quantile(rel_err, 0.25),
            err_75 = quantile(rel_err, 0.75))

ggplot() +
  facet_grid(filt + meas_var ~ exp,
             labeller = labeller(exp = plots_exps,
                                 filt = plots_filt_long,
                                 meas_var = plots_meas_short)) +
  # Assimilation
  geom_boxplot(aes(x = freq_,
                   y =  rel_err, fill = config_),
               data = errors_mod,
               outlier.size = 1) +
  # Null error
  geom_hline(data=errors_null, aes(yintercept = err_25), 
             lty = "dashed", colour = "#ff7f00", size = 1) + 
  geom_hline(data=errors_null, aes(yintercept = err_75), 
             lty = "dashed", colour = "#ff7f00", size = 1) + 
  geom_hline(data=errors_null, aes(yintercept = med_err),
             colour = "#ff7f00", size = 1) + 
  labs(x = "Percentage of observations used",
       y = "Relative Error [%]",
       fill = "Uncertainty config.") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_brewer(palette = "Set1", type = "") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 1,
                                   colour = "black",
                                   size = 8))

plot_name <- "Fig1-vanthoor-rel_error_configs_101to104"
plot_file_name <- paste0('../paper/paper3-DAteor/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 23, height = 16, units = "cm",
       family = "serif")

# Fig 2 - Basic Config - Controlled error ---------------------------------
errors_mod <- errors_filt_artif %>%
  filter(variable == "wm",
         !((filt == "enkf") & (case !=  "case2"))) %>%
  filter(id >= 500, id <= 1000, !is.na(pred)) %>%
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
         rel_err > 0) %>%
  mutate(type = if_else(config <= 3, "Fixed", "Variable"))

errors_null <- error_notCalib %>%
  filter(rel_err > 0, config == 0, variable == "wm") %>%
  select(-filt) %>%
  group_by(model, exp) %>%
  mutate(rel_err = rel_err * 100) %>%
  summarise(med_err = median(rel_err),
            err_25 = quantile(rel_err, 0.25),
            err_75 = quantile(rel_err, 0.75))

ggplot() +
  facet_grid(filt + meas_var ~ exp,
             labeller = labeller(exp = plots_exps,
                                 meas_var = plots_meas_short)) +
  geom_boxplot(aes(x = R_, y =  rel_err, fill = type),
               data = errors_mod) +
  geom_hline(data=errors_null, aes(yintercept = err_25), 
             lty = "dashed", colour = "#ff7f00", size = 1) + 
  geom_hline(data=errors_null, aes(yintercept = err_75), 
             lty = "dashed", colour = "#ff7f00", size = 1) + 
  geom_hline(data=errors_null, aes(yintercept = med_err),
             colour = "#ff7f00", size = 1) + 
  labs(y = "Relative Error [%]",
       fill = "Model error",
       x = "Noise level") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_brewer(palette = "Set1", type = "") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 1,
                                   colour = "black",
                                   size = 8))

plot_name <- "Fig2-controlled-rel_error_configs_1to6"
plot_file_name <- paste0('../paper/paper3-DAteor/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 18, height = 25, units = "cm",
       family = "serif")

# Fig 3 - Frequency - Controlled error ------------------------------------
errors_mod <- errors_filt_artif %>%
  filter(variable == "wm",
         !((filt == "enkf") & (case !=  "case2"))) %>%  
  filter(id >= 500, id <= 1000, !is.na(pred),
         config == 4 | config == 5 | config == 6 | 
           config == 8 | config == 9 | config == 10) %>%
  filter(config != 0, rel_err > 0) %>%
  filter(filt != "pf", !(filt == "enkf" & case != "case2")) %>%
  mutate(filt_ = paste(filt, meas_var, sep = "_"),
         filt_ = factor(filt_,
                        levels = c("enkf_wf", "enkf_wm", "ukf_wf", 
                                   "ukf_wm", "none"),
                        labels = c("EnKF\nWf", "EnKF\nWm", "UKF\nWf", 
                                   "UKF\nWm", "No assim."),
                        ordered = TRUE),
         config_ = if_else(config == 4 | config == 8, 10,
                           if_else(config == 5 | config == 9, 30, 
                                   if_else(config == 6 | config == 10, 50, 0))),
         R_ = factor(config_,
                     levels = c("10", "30", "50", "0"),
                     labels = c("10%", "30%", "50%", "Other"),
                     ordered = TRUE),
         frequency = factor(frequency,
                            levels=c(0.1, 0.5, 1),
                            labels=c("10%", "50%", "100%")))

errors_null <- error_notCalib %>%
  filter(rel_err > 0, config == 0, variable == "wm") %>%
  select(-filt) %>%
  group_by(model, exp) %>%
  mutate(rel_err = rel_err * 100) %>%
  summarise(med_err = median(rel_err),
            err_25 = quantile(rel_err, 0.25),
            err_75 = quantile(rel_err, 0.75))

ggplot() +
  facet_grid(filt + meas_var ~ exp,
             labeller = labeller(exp = plots_exps,
                                 filt = plots_filt_long,
                                 meas_var = plots_meas_short)) +
  # Assimilation
  geom_boxplot(aes(x = R_, y =  rel_err, fill = frequency),
               data = errors_mod,
               outlier.size = 1) +
  # Null error
  geom_hline(data=errors_null, aes(yintercept = err_25),
             lty = "dashed", colour = "#ff7f00", size = 1) +
  geom_hline(data=errors_null, aes(yintercept = err_75),
             lty = "dashed", colour = "#ff7f00", size = 1) +
  geom_hline(data=errors_null, aes(yintercept = med_err),
             colour = "#ff7f00", size = 1) +
  labs(y = "Relative Error [%]",
       fill = "Sampling Frequency",
       x = "Noise level") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_brewer(palette = "Set1", type = "") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 1,
                                   colour = "black",
                                   size = 8))

plot_name <- "Fig3-controlled-subsample"
plot_file_name <- paste0('../paper/paper3-DAteor/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 15, height = 16, units = "cm",
       family = "serif")

# Fig 4 - Curve example - Vanthoor ----------------------------------------
upd <- allStates %>%
  filter(config == 104,
         filt != "pf",
         !(filt == "enkf" & case != "case2")) %>%
    mutate(filt_ = factor(filt,
                          levels = c("ukf", "gnvMod", "cps4", "enkf"),
                          labels = c("UKF", "OL, not calib.", "Sim. truth",
                                     "EnKF")))

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cps4") %>%
  select(-calib) %>%
  mutate(filt_ = factor("cps4",
                        levels = c("ukf", "gnvMod", "cps4"),
                        labels = c("UKF", "OL, not calib.", "Sim. truth")))

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvMod") %>%
  mutate(model = "gnvMod") %>%
  select(-calib) %>%
  mutate(filt_ = factor("gnvMod",
                      levels = c("ukf", "gnvMod", "cps4"),
                      labels = c("UKF", "OL, not calib.", "Sim. truth")))

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable == "wf" | variable == "wm") 

assim <- upd %>%
  filter(variable == "wf" | variable == "wm") 

obs_plot <- obs_mod %>%
  filter(variable == "wf" | variable == "wm",
         config == 200) %>%
  mutate(meas_var = variable)

ggplot() +
  facet_grid(meas_var + variable ~ exp, scales = "free",
             labeller = labeller(meas_var = plots_meas_short,
                                 exp = plots_exps,
                                 variable = plots_st_1L)) + 
  # Simulations
  geom_point(data = simuls, aes(das, measurement, 
                                colour = filt_),
             size = 1.5) +
  # Assimilation
  geom_point(data = assim, aes(das, measurement, group = rep, 
                               colour = filt_),
             size = 1.5) +
  # Observations
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.5) +
  labs(x = "Days after simulation started", 
       y = "",
       colour = "") +
  scale_colour_brewer(palette = "Set1", type = "") +
  guides(colour=guide_legend(override.aes = list(size = 2),
                           title.theme = element_text(face = "bold", size=9))) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- "Fig4-vanthoor-curves"
plot_file_name <- paste0('../paper/paper3-DAteor/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 23, height = 16, units = "cm",
       family = "serif")


# Fig 5 - Ensemble Type - Controlled --------------------------------------
upd <- allStates %>%
  #filter(das != 0) %>%
  filter(config == 5)  %>%
  filter(filt == "enkf") %>%
  mutate(model = tolower(filt)) %>%
  mutate(case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvMod", "cps4"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth"))) %>%
  filter(variable == "wf" | variable == "wm")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cps4") %>%
  select(-calib) %>%
  mutate(case = "cps4",
         case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvMod", "cps4"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth")))

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvMod") %>%
  mutate(model = "gnvMod") %>%
  select(-calib) %>%
  mutate(case = "gnvMod",
         case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvMod", "cps4"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth")))

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable == "wf" | variable == "wm") 

obs_plot <- obs_mod %>%
  filter(variable == "wf" | variable == "wm",
         calib == "cps4", config == 5)  %>%
  mutate(meas_var = variable)

ggplot() +
  facet_grid(variable + meas_var ~ exp, scales = "free",
             labeller = labeller(variable = plots_st_1L,
                                 meas_var = plots_meas_short,
                                 exp = plots_exps)) + 
  # Simulations
  geom_point(data = simuls, aes(das, measurement, 
                               colour = case), 
             size = 0.8) +
  # Assimilation
  geom_point(data = upd, aes(das, measurement, 
                             colour = case), 
             size = 0.9) +
  # Observations
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.5) +
  labs(x = "Days after simulation started", 
       y = "",
       colour = "Source of uncertainty") +
  scale_colour_manual(values = paletteer_dynamic("ggthemes_solarized::magenta",
                                               6)) +
  guides(colour=guide_legend(override.aes = list(size = 2),
                           title.theme = element_text(face = "bold", size=9))) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- "Fig5-controlled-ensembles"
plot_file_name <- paste0('../paper/paper3-DAteor/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 18, height = 16, units = "cm",
       family = "serif")



# Sup Mat - Fig 6 - Ensemble Size - Controlled ----------------------------
upd <- allStates %>%
  #filter(das != 0) %>%
  filter(config == 5)  %>%
  filter(filt == "enkf") %>%
  mutate(model = tolower(filt)) %>%
  mutate(case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvMod", "cps4"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth"))) %>%
  filter(variable == "wf" | variable == "wm")

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cps4") %>%
  select(-calib) %>%
  mutate(case = "cps4",
         case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvMod", "cps4"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth")))

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvMod") %>%
  mutate(model = "gnvMod") %>%
  select(-calib) %>%
  mutate(case = "gnvMod",
         case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvMod", "cps4"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth")))

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable == "wf" | variable == "wm") 

obs_plot <- obs_mod %>%
  filter(variable == "wf" | variable == "wm",
         calib == "cps4", config == 5)  %>%
  mutate(meas_var = variable)

ggplot() +
  facet_grid(variable + meas_var ~ exp, scales = "free",
             labeller = labeller(variable = plots_st_1L,
                                 meas_var = plots_meas_short,
                                 exp = plots_exps)) + 
  # Simulations
  geom_point(data = simuls, aes(das, measurement, 
                                colour = case), 
             size = 0.8) +
  # Assimilation
  geom_point(data = upd, aes(das, measurement, 
                             colour = case), 
             size = 0.9) +
  # Observations
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.5) +
  labs(x = "Days after simulation started", 
       y = "",
       colour = "Source of uncertainty") +
  scale_colour_manual(values = paletteer_dynamic("ggthemes_solarized::magenta",
                                                 6)) +
  guides(colour=guide_legend(override.aes = list(size = 2),
                             title.theme = element_text(face = "bold", size=9))) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

plot_name <- "Fig5-controlled-ensembles"
plot_file_name <- paste0('../paper/paper3-DAteor/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 18, height = 16, units = "cm",
       family = "serif")


