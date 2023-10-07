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
library("patchwork")
library("scales")
# library("facetscales")


# Source functions --------------------------------------------------------

# # Update results
# source("./07a-orgSimOut.R")
# source("./07b-orgObs_plot.R")
# source("./07c-orgAssim.R")

# Load data ---------------------------------------------------------------
load("../data/plot_theme_vert2.RData")

models_all <- read.csv( "../tables/results_simul/results_simulations_all.csv")

# Measured
#errors_filt <- read.csv("../tables/results_DA/aux_files/all_errors.csv")
load("../tables/results_DA/aux_files/all_errors.RData")

# truth <- read.csv("../data/synthetic/truth_tomgro.csv") %>%
#   rename(obs = wm, das = dat) %>%
#   select(exp, das, obs)
# assim_results <- allStates %>%
#   filter(variable == "wm") %>%
#   select(id, rep, exp, das, measurement, variable) %>%
#   rename(pred = measurement)
# all_err <- assim_results %>%
#   left_join(truth) %>%
#   arrange(exp, id, das) %>%
#   mutate(error = pred - obs) %>%
#   filter(das > 0)

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

# Ensembles
#ensembles <- read.csv("../tables/results_DA/aux_files/all_ensembles.csv")

# General variables -------------------------------------------------------
simulations <- models_all %>%
  filter(city == "cps", model == "tomgro",
         variable != "dw", variable != "rm", variable != "pg",
         calib == "gnvIV" | calib == "cpsIV",
         exp == "n03" | exp == "n05" | exp == "n07",
         sensor == "A")

allStates <- all_assim %>%
  mutate(id = as.numeric(id)) %>%
  filter(id >= 500) %>%
  left_join(info_runs) %>%
  filter(sensor_type == "A", exp == "n03" | exp == "n05" | exp == "n07") %>%
# Exps used in the study
  filter(filt == "ukf", id < 1000, 
         (config >= 4 & config <= 6) | (config >= 8 & config <= 10))

error_notCalib <- simulations %>%
  spread(calib, measurement) %>%
  group_by(das, model, exp, variable) %>%
  summarise(error = cpsIV - gnvIV,
            abs_error = abs(error),
            rel_err = if_else(cpsIV != 0, abs_error/cpsIV, 0)) %>%
  ungroup() %>%
  mutate(filt = "none", calib = "gnvIV", id = 0, config = 0) %>%
  rename(dat = das) %>%
  filter(variable == "wm", model == "tomgro")

errors_filt_artif <- all_err %>%
  mutate(config = as.numeric(config)) %>%
  left_join(info_runs) %>%
  filter(sensor_type == "A") %>%
  mutate(rel_err = if_else(obs != 0, abs(error)/obs, 0),
         rel_err = rel_err * 100) %>%
  # Exps used in the study
  filter(filt == "ukf", id < 1000, 
         (config >= 4 & config <= 6) | (config >= 8 & config <= 10))

obs_mod <- obs %>%
  gather(wf, wm, key = "variable", value = "measurement") %>%
  filter(exp == "n03" | exp == "n05" | exp == "n07")

plots_meas_short <- c("wm" = "Meas: Wm",
                "wf" = "Meas: Wf")
plots_st_unit <- c("wm" = "Mature Fruit Dry Weight\n[g D.M./m² soil]",
                   "wf" = "Fruit Dry Weight\n[g D.M./m² soil]")
plots_st_1L <- c("wm" = "Mature Fruit Dry Weight",
              "wf" = "Fruit Dry Weight")
plots_st_2L <- c("wm" = "Mature Fruit Dry\n Weight",
                 "wf" = "Fruit Dry Weight")
plots_exps <- c("n01" = "Exp 0",
                "n03" = "Exp 1",
                "n05" = "Exp 2",
                "n07" = "Exp 3")

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

# Fig - Frequency - Controlled error ------------------------------------
errors_mod <- errors_filt_artif %>%
  filter(variable == "wm", filt == "ukf") %>%  
  filter(id >= 500, id <= 1000, !is.na(pred),
         config == 4 | config == 5 | config == 6 | 
           config == 8 | config == 9 | config == 10) %>%
  filter(config != 0, rel_err > 0) %>%
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
  facet_grid(meas_var ~ exp,
             labeller = labeller(exp = plots_exps,
                                 filt = plots_filt_long,
                                 meas_var = plots_meas_short)) +
  # Assimilation
  geom_boxplot(aes(x = frequency, y =  rel_err, fill = R_),
               data = errors_mod,
               outlier.size = 1) +
  # Null error
  geom_hline(data=errors_null, aes(yintercept = err_25),
             lty = "dashed", colour = "#ff7f00", size = 1) +
  geom_hline(data=errors_null, aes(yintercept = err_75),
             lty = "dashed", colour = "#ff7f00", size = 1) +
  geom_hline(data=errors_null, aes(yintercept = med_err),
             colour = "#ff7f00", size = 1) +
  labs(y = "Absolute relative error [%]",
       fill = "Noise level",
       x = "Sampling Frequency") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_fill_brewer(palette = "Set1", type = "") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 1,
                                   colour = "black",
                                   size = 8))

plot_name <- "Fig1-controlled-subsample"
plot_file_name <- paste0('../paper/paperB-sbiagro2023/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 14, height = 8, units = "cm",
       family = "serif")

# Fig - Curves ------------------------------------------------------------

upd <- allStates %>%
  filter(config == 4 | config == 5 | config == 6 | 
           config == 8 | config == 9 | config == 10, filt == "ukf") %>%
  mutate(filt_ = factor(filt,
                        levels = c("ukf", "gnvIV", "cpsIV", "enkf"),
                        labels = c("UKF", "OL, not calib.", "Sim. truth",
                                   "EnKF")),
         config_ = if_else(config == 4 | config == 8, 10,
                           if_else(config == 5 | config == 9, 30, 
                                   if_else(config == 6 | config == 10, 50, 0))),
         R_ = factor(config_,
                     levels = c("10", "30", "50", "0"),
                     labels = c("10%", "30%", "50%", "Other"),
                     ordered = TRUE),
         variable_ = factor(variable,
                            levels = c("wf", "wm"),
                            labels = c("Wf", "Wm")))

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cpsIV") %>%
  select(-calib) %>%
  mutate(filt_ = factor("cpsIV",
                        levels = c("ukf", "gnvIV", "cpsIV"),
                        labels = c("UKF", "OL, not calib.", "Sim. truth")))

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvIV") %>%
  mutate(model = "gnvIV") %>%
  select(-calib) %>%
  mutate(filt_ = factor("gnvIV",
                        levels = c("ukf", "gnvIV", "cpsIV"),
                        labels = c("UKF", "OL, not calib.", "Sim. truth")))

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable == "wf" | variable == "wm", das >= 30) %>%
  mutate(variable_ = factor(variable,
                        levels = c("wf", "wm"),
                        labels = c("Wf", "Wm")))

assim <- upd %>%
  filter(variable == "wf" | variable == "wm", frequency == 1, das >= 30)

assim_sum <- assim %>%
  group_by(exp, meas_var, R_, filt_, variable_, das) %>%
  summarise(measurement = mean(measurement)) %>%
  filter(variable_ == "Wm", das >= 30)

obs_plot <- obs_mod %>%
  filter(variable == "wf" | variable == "wm",
         config == 4 | config == 5 | config == 6) %>%
  mutate(meas_var = variable,
         config_ = if_else(config == 4 | config == 8, 10,
                           if_else(config == 5 | config == 9, 30, 
                                   if_else(config == 6 | config == 10, 50, 0))),
         R_ = factor(config_,
                     levels = c("10", "30", "50", "0"),
                     labels = c("10%", "30%", "50%", "Other"),
                     ordered = TRUE),
         variable_ = factor(variable,
                            levels = c("wf", "wm"),
                            labels = c("Wf", "Wm"))) %>%
  filter(!(variable == "wf" & measurement > 800), dat >= 30)

ggplot() +
  facet_grid(meas_var + R_  ~ exp, scales = "free",
             labeller = labeller(meas_var = plots_meas_short,
                                 exp = plots_exps,
                                 variable = plots_st_1L)) + 
  # Observations
  geom_point(data = obs_plot, aes(dat, measurement),
             size = 0.5, alpha = 0.1) +
  # Assimilation
  geom_line(data = assim, aes(das, measurement, 
                              group = interaction(rep, variable_),
                              linetype = variable_),
             size = 0.5, alpha = 0.2, colour = "tomato1") +
  # Simulations
  geom_line(data = simuls, aes(das, measurement,
                               colour = filt_, linetype = variable_),
            size = 1.2) +
  # Average upd Wm
  geom_line(data = assim_sum, aes(das, measurement, linetype = variable_,
                                  colour = filt_),
            size = 1.2) +
  labs(x = "Days after simulation started", 
       y = "Mature fruit dry mass [g D.M./m² soil]",
       colour = "") +
  # scale_colour_brewer(palette = "Set1") +
  scale_colour_manual(values = c("red", "dodgerblue3", "springgreen4"),
                      breaks = c("UKF", "OL, not calib.", "Sim. truth")) +
  guides(linetype=guide_legend(title = "")) +
  theme_vert +
  theme(panel.background = element_blank())

plot_name <- "v2_Fig-curves"
plot_file_name <- paste0('../paper/paperB-sbiagro2023/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 20, height = 25, units = "cm", family = "serif")

# # Fig - Basic Config - Controlled error ---------------------------------
# errors_mod <- errors_filt_artif %>%
#   filter(variable == "wm", filt == "ukf") %>%
#   filter(id >= 500, id <= 1000, !is.na(pred), (config >= 4 & config <= 6)) %>%
#   # filter(!(filt == "enkf" & case != "case2")) %>%
#   mutate(filt = factor(filt,
#                        levels = c("enkf", "ukf", "pf"),
#                        labels = c("Ensemble KF", "Unscented KF", 
#                                   "Particle Filter"),
#                        ordered = TRUE),
#          config_ = if_else(config == 1 | config == 4, 10,
#                            if_else(config == 2 | config == 5, 30, 
#                                    if_else(config == 3 | config == 6, 50, 0)))) %>%
#   filter(config_ != 0) %>%
#   mutate(R_ = factor(config_,
#                      levels = c("10", "30", "50"),
#                      labels = c("10%", "30%", "50%"),
#                      ordered = TRUE)) %>%
#   filter(frequency == 1,
#          config <= 6, 
#          rel_err > 0) %>%
#   mutate(type = if_else(config <= 3, "Fixed", "Variable")) %>%
#   filter(exp == "n03" | exp == "n05" | exp == "n07")
# 
# errors_null <- error_notCalib %>%
#   filter(rel_err > 0, config == 0) %>%
#   select(-filt) %>%
#   mutate(rel_err = rel_err * 100) %>%
#   group_by(exp) %>%
#   summarise(med_err = median(rel_err),
#             err_25 = quantile(rel_err, 0.25),
#             err_75 = quantile(rel_err, 0.75))
# 
# ggplot() +
#   facet_grid(meas_var ~ exp,
#              labeller = labeller(exp = plots_exps,
#                                  meas_var = plots_meas_short)) +
#   geom_boxplot(aes(x = R_, y =  rel_err),
#                data = errors_mod) +
#   geom_hline(data=errors_null, aes(yintercept = err_25), 
#              lty = "dashed", colour = "#ff7f00", size = 1) + 
#   geom_hline(data=errors_null, aes(yintercept = err_75), 
#              lty = "dashed", colour = "#ff7f00", size = 1) + 
#   geom_hline(data=errors_null, aes(yintercept = med_err),
#              colour = "#ff7f00", size = 1) + 
#   labs(y = "Absolute relative error [%]",
#        fill = "Model error",
#        x = "Noise level") +
#   coord_cartesian(ylim = c(0, 100)) +
#   scale_fill_brewer(palette = "Set1", type = "") +
#   theme_vert +
#   theme(panel.background = element_rect(fill = "gray99"),
#         axis.text.x = element_text(angle = 0,
#                                    hjust = 1,
#                                    colour = "black",
#                                    size = 8))
# 
# plot_name <- "v2_Fig1-controlled-rel_error_fixvsvar"
# plot_file_name <- paste0('../paper/paperB-sbiagro2023/figures/', plot_name,'.png')
# ggsave(plot_file_name,
#        width = 14, height = 8, units = "cm",
#        family = "serif")
# 
# # Curve examples ----------------------------------------------------------
# config_it <- 1
# for (config_it in 1:3){
# 
#   upd <- allStates %>%
#     filter(config == config_it, filt == "ukf") %>%
#     mutate(filt_ = factor(filt,
#                           levels = c("ukf", "gnvIV", "cpsIV", "enkf"),
#                           labels = c("UKF", "OL, not calib.", "Sim. truth",
#                                      "EnKF")))
#   
#   simul_calib <- simulations %>%
#     filter(model == "tomgro",
#            # Calibrated model
#            calib == "cpsIV") %>%
#     select(-calib) %>%
#     mutate(filt_ = factor("cpsIV",
#                           levels = c("ukf", "gnvIV", "cpsIV"),
#                           labels = c("UKF", "OL, not calib.", "Sim. truth")))
#   
#   simul_NotCalib <- simulations %>%
#     filter(model == "tomgro",
#            # Not calibrated model
#            calib == "gnvIV") %>%
#     mutate(model = "gnvIV") %>%
#     select(-calib) %>%
#     mutate(filt_ = factor("gnvIV",
#                           levels = c("ukf", "gnvIV", "cpsIV"),
#                           labels = c("UKF", "OL, not calib.", "Sim. truth")))
#   
#   simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
#     filter(variable == "wf" | variable == "wm") 
#   
#   assim <- upd %>%
#     filter(variable == "wf" | variable == "wm") 
#   
#   obs_plot <- obs_mod %>%
#     filter(variable == "wf" | variable == "wm",
#            config == config_it) %>%
#     mutate(meas_var = variable)
#   
#   ggplot() +
#     facet_grid(meas_var + variable ~ exp, scales = "free_x",
#                labeller = labeller(meas_var = plots_meas_short,
#                                    exp = plots_exps,
#                                    variable = plots_st_1L)) + 
#     # Simulations
#     geom_point(data = simuls, aes(das, measurement, 
#                                   colour = filt_),
#                size = 1.5) +
#     # Assimilation
#     geom_point(data = assim, aes(das, measurement, group = rep, 
#                                  colour = filt_),
#                size = 1.5) +
#     # Observations
#     geom_point(data = obs_plot, aes(dat, measurement),
#                size = 0.5) +
#     labs(x = "Days after simulation started", 
#          y = "",
#          colour = "") +
#     scale_colour_brewer(palette = "Set1", type = "") +
#     guides(colour=guide_legend(override.aes = list(size = 2),
#                                title.theme = element_text(face = "bold", size=9))) +
#     theme_vert +
#     theme(panel.background = element_rect(fill = "gray99"))
#   
#   plot_name <- paste0("v2_Rplot", config_it)
#   plot_file_name <- paste0('../paper/paperB-sbiagro2023/figures/', plot_name,'.png')
#   ggsave(plot_file_name,
#          width = 23, height = 16, units = "cm", family = "serif")
#   
#     
# }
# 
# # Eval params filt --------------------------------------------------------
# 
# filt_params <- upd_assim %>%
#   mutate(across(.cols = c("das", "rep", "measurement", "id"), .fns = as.numeric)) %>%
#   left_join(info_runs) %>%
#   filter(id >= 500, id <= 1000) %>%
#   filter(filt == "ukf", frequency == 1) %>%
#   mutate(filt_ = paste(filt, meas_var, sep = "_"),
#          filt_ = factor(filt_,
#                         levels = c("enkf_wf", "enkf_wm", "ukf_wf",
#                                    "ukf_wm", "none"),
#                         labels = c("EnKF\nWf", "EnKF\nWm", "UKF\nWf",
#                                    "UKF\nWm", "No assim."),
#                         ordered = TRUE),
#          config_ = if_else(config == 4 | config == 8 | config == 1, 10,
#                            if_else(config == 5 | config == 9 | config == 2, 30,
#                                    if_else(config == 6 | config == 10 | config == 3, 50, 0))),
#          R_ = factor(config_,
#                      levels = c("10", "30", "50", "0"),
#                      labels = c("10%", "30%", "50%", "Other"),
#                      ordered = TRUE),
#          frequency = factor(frequency,
#                             levels=c(0.1, 0.5, 1),
#                             labels=c("10%", "50%", "100%"))) %>%
#   mutate(moderr = if_else(id <= 643, "fixed", "variable"))
# 
# 
# v1 <- filt_params %>%
#   mutate(measurement_ = log(measurement)) %>%
#   # filter(variable ==  "P_m" | variable == "R", exp != "n01")
#   filter(variable == "Gain", exp != "n01")
# 
# ggplot() +
#   facet_grid(meas_var ~ exp + moderr) +
#   geom_point(aes(x = das, y =  measurement,
#                  colour = interaction(R_)),
#              alpha = 0.5, position = "jitter",
#              data = v1) +
#   scale_fill_brewer(palette = "Set1", type = "")
# 
# plot_name <- "v2_v1"
# plot_file_name <- paste0('../paper/paperB-sbiagro2023/figures/', plot_name,'.png')
# ggsave(plot_file_name,
#        width = 30, height = 16, units = "cm", family = "serif")
# 
# 
# v2 <- filt_params  %>%
#   filter(variable ==  "P_m" | variable == "R", exp != "n01") %>%
#   mutate(measurement_ = log(measurement)) %>%
#   select(variable, measurement, id, rep, exp, meas_var, das, moderr) %>%
#   pivot_wider(names_from = "variable", values_from = "measurement") %>%
#   mutate(dif = R/P_m)
# 
# ggplot(v2) +
#   facet_grid(meas_var ~ exp + moderr) +
#   geom_histogram(aes(dif))
# 
# plot_name <- "v2_v2"
# plot_file_name <- paste0('../paper/paperB-sbiagro2023/figures/', plot_name,'.png')
# ggsave(plot_file_name,
#        width = 30, height = 16, units = "cm", family = "serif")
# 
# 
# v3 <- filt_params %>%
#   filter(moderr == "variable") %>%
#   filter(variable ==  "wm_est_s" | variable == "wm_upd_s", exp != "n01") %>%
#   select(variable, measurement, id, rep, exp, meas_var, das, R_) %>%
#   filter(rep < 5)
# 
# ggplot(v3) +
#   facet_grid(meas_var ~ exp + R_) +
#   geom_point(aes(x = das, y =  measurement, colour = variable),
#              alpha = 0.5, position = "jitter",
#              data = v3) +
#   scale_fill_brewer(palette = "Set1", type = "")
# 
# plot_name <- "v2_v3"
# plot_file_name <- paste0('../paper/paperB-sbiagro2023/figures/', plot_name,'.png')
# ggsave(plot_file_name,
#        width = 30, height = 16, units = "cm", family = "serif")
# 
# 
# v4 <- filt_params %>%
#   filter(moderr == "variable") %>%
#   filter(exp != "n01") %>%
#   select(variable, measurement, id, rep, exp, meas_var, das, R_) %>%
#   filter(rep == 5) %>%
#   pivot_wider(names_from = "variable", values_from = "measurement")
# 
# ggplot(v4) +
#   facet_grid(meas_var ~ exp + R_) +
#   geom_point(aes(x = das, y =  Gain),
#              alpha = 0.5, position = "jitter",
#              data = v4) +
#   scale_fill_brewer(palette = "Set1", type = "")
# 
# plot_name <- "v2_v4"
# plot_file_name <- paste0('../paper/paperB-sbiagro2023/figures/', plot_name,'.png')
# ggsave(plot_file_name,
#        width = 30, height = 16, units = "cm", family = "serif")
# 
