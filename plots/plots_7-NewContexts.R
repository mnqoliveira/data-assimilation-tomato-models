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
library("ggsci")

# Path save ---------------------------------------------------------------

path_png <- "./paper/paper3-DAteor/figures/sub1/"
path_eps <- "./paper/paper3-DAteor/sub1/figs/"

# Source functions --------------------------------------------------------

# # Update results
# source("./07a-orgSimOut.R")
# source("./07b-orgObs_plot.R")
# source("./07c-orgAssim.R")

# Load data ---------------------------------------------------------------
load("./data/plot_theme_vert2.RData")

models_all <- read.csv( "./tables/results_simul/results_simulations_all.csv")

# Measured
#errors_filt <- read.csv("./tables/results_DA/aux_files/all_errors.csv")
load("./tables/results_DA/aux_files/all_errors.RData")

errors_simul <- read.csv("./tables/results_simul/all_errors.csv")

# Assimilation
info_runs <- read.csv("./tables/runs_Filter2.csv") %>%
  mutate(exp_int = exp,
         exp = paste0("n0", exp_int)) %>%
  select(-it, -comment)

# Assimilation results
# all_states_out <- read.csv("./tables/results_DA/aux_files/all_states.csv")
# upd_states_out <- read.csv("./tables/results_DA/aux_files/upd_states.csv")

load("./tables/results_DA/aux_files/all_states.RData")
# load("./tables/results_DA/aux_files/upd_states.RData")

# Simulations
models_all <- read.csv( "./tables/results_simul/results_simulations_all.csv")

# Observations
obs <- read.csv("./data/synthetic/obs_artif_all.csv")

# Ensembles
#ensembles <- read.csv("./tables/results_DA/aux_files/all_ensembles.csv")

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
  filter(sensor_type == "A", exp == "n03" | exp == "n05" | exp == "n07")

error_notCalib <- simulations %>%
  spread(calib, measurement) %>%
  group_by(das, model, exp, variable) %>%
  summarise(error = cpsIV - gnvIV,
            abs_error = abs(error),
            rel_err = if_else(cpsIV != 0, abs_error/cpsIV, 0)) %>%
  ungroup() %>%
  mutate(filt = "none",
         calib = "gnvIV",
         id = 0,
         config = 0) %>%
  rename(dat = das) %>%
  filter(variable == "wm")

cols_remove <- c("obs", "pred", "max_obs", "mean_obs", "error",
                 "abs_error", "sc_rel_error", "run",
                 "model", "sensor_type", "city",
                 "state_var", "P", "Q", "comment2", "mae", "ref")

# truth <- read.csv("./data/synthetic/truth_tomgro.csv") %>%
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

errors_filt_artif <- all_err %>%
  mutate(config = as.numeric(config)) %>%
  left_join(info_runs) %>%
  filter(sensor_type == "A", exp == "n03" | exp == "n05" | exp == "n07") %>%
  mutate(rel_err = if_else(obs != 0, abs(error)/obs, 0),
         rel_err = rel_err * 100) %>%
  select(-any_of(cols_remove))

obs_mod <- obs %>%
  gather(wf, wm, key = "variable", value = "measurement") %>%
  filter(exp == "n03" | exp == "n05" | exp == "n07")

plots_meas_short <- c("wm" = "Meas: Wm",
                "wf" = "Meas: Wf")
plots_st_unit <- c("wm" = "Mature Fruit Dry Weight\n[g D.M./m² soil]",
                   "wf" = "Fruit Dry Weight\n[g D.M./m² soil]")
plots_st_1L <- c("wm" = "Mature Fruit Dry Weight",
              "wf" = "Fruit Dry Weight")
plots_st_1L_short <- c("wm" = "Mature Fruit",
                       "wf" = "Fruit")

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

# palette.colors(8, "Set1")
# abc <- palette(value = c("#E41A1C","#377EB8", "#984EA3", "#4DAF4A",
#                          "#FF7F00","#F781BF"))
# abc <- palette(value = c("red", "magenta", "dodgerblue3", "springgreen4"))

# Fig 1 - Curves Controlled -----------------------------------------------

upd <- allStates %>%
  filter(as.numeric(config) <= 10, !(filt == "enkf" & case != "case2")) %>%
  mutate(filt_ = factor(filt,
                        levels = c("ukf", "enkf", "gnvIV", "cpsIV"),
                        labels = c("UKF", "EnKF", "OL, not calib.", "Sim. truth"),
                        ordered = TRUE),
         config_ = if_else(config %in% c(1, 4, 8), 10,
                           if_else(config %in% c(2, 5, 9), 30, 
                                   if_else(config %in% c(3, 6, 10), 50, 0))),
         R_ = factor(config_,
                     levels = c("10", "30", "50", "0"),
                     labels = c("10%", "30%", "50%", "Other"),
                     ordered = TRUE),
         variable_ = factor(variable,
                            levels = c("wf", "wm"),
                            labels = c("Wf", "Wm"))) %>%
  filter(as.numeric(config) %in% c(4, 5, 6)) %>%
  arrange(filt_)

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cpsIV") %>%
  select(-calib) %>%
  mutate(filt_ = factor("cpsIV",
                        levels = c("ukf", "enkf", "gnvIV", "cpsIV"),
                        labels = c("UKF", "EnKF", "OL, not calib.", "Sim. truth"),
                        ordered = TRUE)) %>%
  arrange(filt_)

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvIV") %>%
  mutate(model = "gnvIV") %>%
  select(-calib) %>%
  mutate(filt_ = factor("gnvIV",
                        levels = c("ukf", "enkf", "gnvIV", "cpsIV"),
                        labels = c("UKF", "EnKF", "OL, not calib.", "Sim. truth"),
                        ordered = TRUE)) %>%
  arrange(filt_)

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable == "wf" | variable == "wm", das >= 30) %>%
  mutate(variable_ = factor(variable,
                            levels = c("wf", "wm"),
                            labels = c("Wf", "Wm")))

assim <- upd %>%
  filter(variable == "wf" | variable == "wm", frequency == 1, das >= 30)

assim_1 <- filter(assim, filt == "ukf")
assim_2 <- filter(assim, filt == "enkf")

assim_sum <- assim %>%
  group_by(exp, meas_var, R_, filt_, variable_, das, config) %>%
  summarise(measurement = mean(measurement)) %>%
  filter(variable_ == "Wm" | variable_ == "Wf", das >= 30) %>%
  arrange(filt_)

obs_plot <- obs_mod %>%
  filter(variable == "wf" | variable == "wm",
         config == 4 | config == 5 | config == 6) %>%
  mutate(meas_var = variable,
         config_ = if_else(config %in% c(1, 4, 8), 10,
                           if_else(config %in% c(2, 5, 9), 30, 
                                   if_else(config %in% c(3, 6, 10), 50, 0))),
         R_ = factor(config_,
                     levels = c("10", "30", "50", "0"),
                     labels = c("10%", "30%", "50%", "Other"),
                     ordered = TRUE),
         variable_ = factor(variable,
                            levels = c("wf", "wm"),
                            labels = c("Wf", "Wm"))) %>%
  filter(!(variable == "wf"), dat >= 30)

obs_plot_1 <- filter(obs_plot, variable == "wf")
assim_1_1 <- filter(assim_1, meas_var == "wf")
assim_2_1 <- filter(assim_2, meas_var == "wf")
assim_sum_1 <- filter(assim_sum, meas_var == "wf")

p1 <- ggplot() +
  facet_grid(meas_var + R_  ~ exp, scales = "free",
             labeller = labeller(meas_var = plots_meas_short,
                                 exp = plots_exps,
                                 variable = plots_st_1L)) + 
  # Observations
  geom_point(data = obs_plot_1, aes(dat, measurement),
             size = 0.5, alpha = 0.2, color = "gray50") +
  # Assimilation
  geom_line(data = assim_1_1, aes(das, measurement,
                              group = interaction(rep, variable_, case, filt_),
                              colour = filt_,
                              linetype = variable_),
            size = 0.5, alpha = 0.1) +
  geom_line(data = assim_2_1, aes(das, measurement,
                                group = interaction(rep, variable_, case, filt_),
                                colour = filt_,
                                linetype = variable_),
            size = 0.5, alpha = 0.2) +
  # Simulations
  geom_line(data = simuls, aes(das, measurement,
                               colour = filt_, linetype = variable_),
            size = 1.2) +
  # Average upd Wm
  geom_line(data = assim_sum_1, aes(das, measurement, linetype = variable_,
                                  colour = filt_),
            size = 1.2, position = "jitter") +
  labs(x = "", 
       y = "Dry biomass [g D.M./m² soil]",
       linetype = "Output variable",
       colour = "Method") +
  ylim(c(0, 600)) +
  scale_color_manual(values = paletteer_d("ggsci::nrc_npg")[c(1, 2, 4, 3)],
                     breaks = c("UKF", "EnKF", "OL, not calib.", "Sim. truth")) +
  guides(linetype=guide_legend(nrow = 2,
                               title.theme = element_text(face = "bold")),
         color=guide_legend(nrow = 2,
                            title.theme = element_text(face = "bold"))
         ) +
  theme_vert +
  theme(panel.background = element_blank(),
        legend.box="horizontal")

obs_plot_2 <- filter(obs_plot, variable == "wm")
assim_1_2 <- filter(assim_1, meas_var == "wm", variable == "wm")
assim_2_2 <- filter(assim_2, meas_var == "wm", variable == "wm")
assim_sum_2 <- filter(assim_sum, meas_var == "wm", variable_ == "Wm")
simuls_2 <- filter(simuls, variable == "wm")

p2 <- ggplot() +
  facet_grid(meas_var + R_  ~ exp, scales = "free",
             labeller = labeller(meas_var = plots_meas_short,
                                 exp = plots_exps,
                                 variable = plots_st_1L)) + 
  # Observations
  geom_point(data = obs_plot_2, aes(dat, measurement),
             size = 0.5, alpha = 0.2, color = "gray50", show.legend = FALSE) +
  # Assimilation
  geom_line(data = assim_1_2, aes(das, measurement,
                                  group = interaction(rep, variable_, case, filt_),
                                  colour = filt_,
                                  # linetype = variable_
                                  ),
            size = 0.5, alpha = 0.1, linetype = "dashed", show.legend = FALSE) +
  geom_line(data = assim_2_2, aes(das, measurement,
                                  group = interaction(rep, variable_, case, filt_),
                                  colour = filt_,
                                  # linetype = variable_
                                  ),
            size = 0.5, alpha = 0.2, linetype = "dashed", show.legend = FALSE) +
  # Simulations
  geom_line(data = simuls_2, aes(das, measurement,
                               colour = filt_, 
                               # linetype = variable_
                               ),
            size = 1.2, linetype = "dashed", show.legend = FALSE) +
  # Average upd Wm
  geom_line(data = assim_sum_2, aes(das, measurement, 
                                    # linetype = variable_,
                                    colour = filt_),
            size = 1.2, position = "jitter", linetype = "dashed", show.legend = FALSE) +
  labs(x = "Days after simulation started", 
       y = "Mature fruit dry mass [g D.M./m² soil]",
       linetype = "Output variable",
       colour = "Method") +
  ylim(c(0, 300)) +
  scale_color_manual(values = paletteer_d("ggsci::nrc_npg")[c(1, 2, 4, 3)],
                     breaks = c("UKF", "EnKF", "OL, not calib.", "Sim. truth")) +
  # guides(linetype=guide_legend(nrow = 2,
  #                              title.theme = element_text(face = "bold")),
  #        color=guide_legend(nrow = 2,
  #                           title.theme = element_text(face = "bold"))) +
  theme_vert +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank())

layout <- "
A
B
"
(p1 / p2) +
  plot_layout(design = layout, guides = 'collect') &
  theme(legend.position='bottom')


plot_name <- "Fig1-curves"
namefile <- paste0(path_png, plot_name, ".png")
ggsave(namefile,
       width = 20, height = 25, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = "png"
)

namefile <- paste0(path_eps, plot_name, ".eps")
ggsave(namefile,
       width = 20, height = 25, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = "eps"
)

# Fig 2 - Ensemble Type - Controlled --------------------------------------
upd <- allStates %>%
  filter(config == 4 | config == 5 | config == 6) %>%
  filter(filt == "enkf") %>%
  mutate(model = tolower(filt)) %>%
  mutate(case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvIV", "cpsIV"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth")),
         config_ = if_else(config %in% c(1, 4, 8), 10,
                         if_else(config %in% c(2, 5, 9), 30, 
                                 if_else(config %in% c(3, 6, 10), 50, 0))),
         R_ = factor(config_,
                   levels = c("10", "30", "50", "0"),
                   labels = c("10%", "30%", "50%", "Other"),
                   ordered = TRUE),
         variable_ = factor(variable,
                          levels = c("wf", "wm"),
                          labels = c("Wf", "Wm"))) %>%
  filter(variable == "wm", config == 5, das >= 30)

simul_calib <- simulations %>%
  filter(model == "tomgro",
         # Calibrated model
         calib == "cpsIV") %>%
  select(-calib) %>%
  mutate(case = "cpsIV",
         case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvIV", "cpsIV"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth"),
                       ordered= TRUE))

simul_NotCalib <- simulations %>%
  filter(model == "tomgro",
         # Not calibrated model
         calib == "gnvIV") %>%
  mutate(model = "gnvIV") %>%
  select(-calib) %>%
  mutate(case = "gnvIV",
         case = factor(case,
                       levels = c("case0", "case1", 
                                  "case2", "case3",
                                  "gnvIV", "cpsIV"),
                       labels = c("Init. States", "States", 
                                  "Parameter", "Input",
                                  "OL, not calib.", "Sim. truth"),
                       ordered= TRUE))

simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
  filter(variable == "wm", das >= 30) 

assim_sum <- upd %>%
  group_by(exp, meas_var, variable, das, config, case) %>%
  summarise(measurement = mean(measurement)) %>%
  filter(variable == "wm", das >= 30)

upd1 <- filter(upd, variable == "wm") %>%
  arrange(case)

ggplot() +
  facet_grid(meas_var ~ exp, scales = "free",
             labeller = labeller(variable = plots_st_1L,
                                 meas_var = plots_meas_short,
                                 exp = plots_exps)) + 
  # Simulations
  geom_point(data = simuls, aes(das, measurement, fill = case), 
             shape = 21, size = 0.9, colour = "transparent") +
  # Assimilation
  geom_line(data = upd1, aes(das, measurement, colour = case,
                             group = interaction(rep, case)), 
             size = 0.5, alpha = 0.3, position = "jitter") +
  # Assim_avg
  # geom_line(data = assim_sum, aes(das, measurement, 
  #                               group = interaction(variable, case),
  #                               colour = case),
  #           size = 1, position = "jitter") +
  
  # Observations
  # geom_point(data = obs_plot, aes(dat, measurement),
  #            size = 0.5, alpha = 0.2) +
  labs(x = "Days after simulation started", 
       y = "Mature fruit dry mass [g D.M./m² soil]",
       colour = "Approach",
       fill = "Simulations") +
  scale_color_manual(values = paletteer_d("ggsci::nrc_npg")[c(1, 5, 6, 2)]) +
  scale_fill_manual(values = paletteer_d("ggsci::nrc_npg")[c(4, 3)]) +
  # scale_colour_manual(values = abc) +
  # scale_fill_manual(values = abc[5:6]) +
  theme_vert +
  ylim(c(0, 400)) +
  theme(panel.background = element_rect(fill = "gray99")) +
  guides(fill=guide_legend(nrow = 2,
                           title.theme = element_text(face = "bold"),
                           override.aes = list(size = 2, 
                                               linetype = c(rep("blank", 2))) 
                           ),
         color=guide_legend(nrow = 2,
                            title.theme = element_text(face = "bold"),
                            override.aes = list(size = 2, alpha = 1))
         )

plot_name <- "Fig2-controlled-ensembles"
namefile <- paste0(path_png, plot_name, ".png")
ggsave(namefile,
       width = 15, height = 10, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = "png"
)

namefile <- paste0(path_eps, plot_name, ".eps")
ggsave(namefile,
       width = 15, height = 10, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = "eps"
)

# Fig 3 - Basic Config - Controlled error ---------------------------------
errors_mod <- errors_filt_artif %>%
  # filter(frequency == 1, config == 6 | config == 3, rel_err > 0) %>%
  # filter(variable == "wm", filt == "enkf", case == "case2") %>%
  filter(variable == "wm", !((filt == "enkf") & (case !=  "case2"))) %>%
  filter(id >= 500, id <= 1000) %>%
  filter(frequency == 1, config <= 6, rel_err > 0) %>%
  mutate(config = as.numeric(config),
         filt = factor(filt,
                       levels = c("enkf", "ukf"),
                       labels = c("Ensemble KF", "Unscented KF"),
                       ordered = TRUE),
         config_ = if_else(config %in% c(1, 4, 8), 10,
                           if_else(config %in% c(2, 5, 9), 30, 
                                   if_else(config %in% c(3, 6, 10), 50, 0)))) %>%
  filter(config_ != 0) %>%
  mutate(R_ = factor(config_,
                     levels = c("10", "30", "50"),
                     labels = c("10%", "30%", "50%"),
                     ordered = TRUE)) %>%
  mutate(type = if_else(config <= 3, "Fixed", "Variable")) %>%
  filter(exp == "n03" | exp == "n05" | exp == "n07") %>%
  group_by(frequency, filt, meas_var, exp, config_) %>%
  mutate(id_out = cur_group_id()) %>%
  ungroup() %>%
  mutate(D_ = NA, p_ = NA) %>%
  as.data.frame()

it = 1

for (it in unique(errors_mod$id_out)){

  data_filt <- filter(errors_mod, id_out == it) %>%
    as.data.frame()

  g1 <- filter(data_filt, type == "Fixed")
  g2 <- filter(data_filt, type == "Variable")

  temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
  print(temp)
  
  errors_mod[errors_mod$id_out == it, "D_"] <- temp$statistic
  errors_mod[errors_mod$id_out == it, "p_"] <- temp$p.value
  
  plot(ecdf(g1$rel_err))
  lines(ecdf(g2$rel_err), col = "red")
  
  # ggplot() +
  #   facet_wrap("rep") +
  #   geom_line(data = g1, aes(x = dat, y = rel_err)) +
  #   geom_line(data = g2, aes(x = dat, y = rel_err), col = "red")
  #   
  # stat_ecdf(data = g1, aes(rel_err), geom = "step") +
  #   stat_ecdf(data = g2, aes(rel_err), geom = "step", col = "red")

}

ks_all <- errors_mod %>%
  select(frequency, filt, meas_var, exp, config_, config,
         starts_with("D_"), starts_with("p_"),
         id_out) %>%
  distinct()

write.csv(ks_all, file = "./tables/metrics/assim/ks_fixed_variable.csv", row.names = FALSE)

errors_null <- error_notCalib %>%
  filter(rel_err > 0, config == 0, variable == "wm") %>%
  select(-filt) %>%
  group_by(model, exp) %>%
  mutate(rel_err = rel_err * 100) %>%
  summarise(med_err = median(rel_err),
            err_25 = quantile(rel_err, 0.25),
            err_75 = quantile(rel_err, 0.75))

errors_mod_sum <- errors_mod %>%
  group_by(frequency, filt, meas_var, exp, type, R_) %>%
  summarise(error_pos = quantile(rel_err, probs = 0.25),
            error_pos = 0)

# Different marked
errors_mod_ <- errors_mod %>%
  left_join(errors_mod_sum) %>%
  group_by(frequency, filt, meas_var, exp, R_) %>%
  mutate(label_ = case_when(((p_ < 0.05) & (row_number() == 1)) ~ "*",
                           TRUE ~ ""),
         label_ = na_if(label_, ""))

ggplot() +
  facet_grid(meas_var + filt ~ exp,
             labeller = labeller(exp = plots_exps,
                                 meas_var = plots_meas_short)) +
  geom_boxplot(aes(x = R_, y =  rel_err, fill = type),
               data = errors_mod_) +
  geom_text(data = errors_mod_, 
            aes(x = R_, y = error_pos, label = label_, group = type),
            size = 6) +
  geom_hline(data=errors_null, aes(yintercept = err_25), 
             lty = "dashed", colour = "#ff7f00", size = 1) + 
  geom_hline(data=errors_null, aes(yintercept = err_75), 
             lty = "dashed", colour = "#ff7f00", size = 1) + 
  geom_hline(data=errors_null, aes(yintercept = med_err),
             colour = "#ff7f00", size = 1) + 
  labs(y = "Relative Error [%]",
       fill = "Model covariance",
       x = "Noise level") +
  coord_cartesian(ylim = c(0, 100)) +
  # scale_fill_brewer(palette = "Set1", type = "") +
  scale_fill_npg() +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 1,
                                   colour = "black",
                                   size = 8))

plot_name <- "Fig3-basic_controlled"
namefile <- paste0(path_png, plot_name, ".png")
ggsave(namefile,
       width = 15, height = 12, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = "png"
)


namefile <- paste0(path_eps, plot_name, ".eps")
ggsave(namefile,
       width = 15, height = 12, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = "eps"
)

# Fig 4 - Frequency - Controlled error ------------------------------------
# Variable error
errors_mod <- errors_filt_artif %>%
  filter(variable == "wm",
         !((filt == "enkf") & (case !=  "case2"))) %>%  
  filter(id >= 500, id <= 1000, config >= 4, config <= 10, config != 7) %>%
  filter(config != 0, rel_err > 0) %>%
  mutate(filt_ = paste(filt, meas_var, sep = "_"),
         filt_ = factor(filt_,
                        levels = c("enkf_wf", "enkf_wm", "ukf_wf", 
                                   "ukf_wm", "none"),
                        labels = c("EnKF\nWf", "EnKF\nWm", "UKF\nWf", 
                                   "UKF\nWm", "No assim."),
                        ordered = TRUE),
         config_ = if_else(config %in% c(1, 4, 8), 10,
                           if_else(config %in% c(2, 5, 9), 30, 
                                   if_else(config %in% c(3, 6, 10), 50, 0))),
         R_ = factor(config_,
                     levels = c("10", "30", "50", "0"),
                     labels = c("10%", "30%", "50%", "Other"),
                     ordered = TRUE),
         frequency = factor(frequency,
                            levels=c(0.1, 0.5, 1),
                            labels=c("10%", "50%", "100%"))) %>%
  mutate(type = if_else(frequency == "100%", "Reference", "Test")) %>%
  group_by(filt, meas_var, exp, config_) %>%
  mutate(id_out = cur_group_id()) %>%
  ungroup() %>%
  mutate(D_ = NA, p_ = NA) %>%
  as.data.frame()

it = 2
for (it in unique(errors_mod$id_out)){

  data_filt <- filter(errors_mod, id_out == it) %>%
    as.data.frame()

  g1 <- filter(data_filt, frequency == "100%")
  g2 <- filter(data_filt, frequency == "10%")

  temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
  print(temp)

  condition <- ((errors_mod$id_out == it) & (errors_mod$frequency == "10%"))

  errors_mod[condition, "D_"] <- temp$statistic
  errors_mod[condition, "p_"] <- temp$p.value

  # plot(ecdf(g1$rel_err))
  # lines(ecdf(g2$rel_err), col = "red")

  g2 <- filter(data_filt, frequency == "50%")

  temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
  print(temp)

  condition <- ((errors_mod$id_out == it) & (errors_mod$frequency == "50%"))

  errors_mod[condition, "D_"] <- temp$statistic
  errors_mod[condition, "p_"] <- temp$p.value
  
  # plot(ecdf(g1$rel_err))
  # lines(ecdf(g2$rel_err), col = "red")

}

ks_all <- errors_mod %>%
  select(frequency, filt, meas_var, exp, config_, config,
         starts_with("D_"), starts_with("p_"),
         id_out) %>%
  distinct()

write.csv(ks_all, file = "./tables/metrics/assim/ks_frequency.csv", row.names = FALSE)

errors_null <- error_notCalib %>%
  filter(rel_err > 0, config == 0, variable == "wm") %>%
  select(-filt) %>%
  group_by(model, exp) %>%
  mutate(rel_err = rel_err * 100) %>%
  summarise(med_err = median(rel_err),
            err_25 = quantile(rel_err, 0.25),
            err_75 = quantile(rel_err, 0.75))

# Different marked
errors_mod_ <- errors_mod %>%
  select(frequency, filt, meas_var, exp, R_, config, p_, type) %>%
  distinct() %>%
  group_by(frequency, filt, meas_var, exp, R_, config, type) %>%
  mutate(label_ = case_when(((p_ < 0.05)) ~ "*",
                            TRUE ~ ""),
         label_ = na_if(label_, ""),
         error_pos = 0)

ggplot() +
  facet_grid(meas_var + filt ~ exp,
             labeller = labeller(exp = plots_exps,
                                 filt = plots_filt_long,
                                 meas_var = plots_meas_short)) +
  # Assimilation
  geom_boxplot(aes(x = R_, y =  rel_err, fill = frequency),
               data = errors_mod,
               outlier.size = 1) +
  geom_text(data = errors_mod_, 
            aes(x = R_, y = error_pos, label = label_, color = frequency),
            size = 6, position = position_dodge(width = 0.75)) +
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
  # scale_fill_brewer(palette = "Set1", type = "") +
  # scale_color_brewer(palette = "Set1", type = "") +
  scale_fill_npg() +
  scale_color_npg() +
  guides(color = "none") +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"),
        axis.text.x = element_text(angle = 0,
                                   hjust = 1,
                                   colour = "black",
                                   size = 8)) 

plot_name <- "Fig4-controlled-subsample"
namefile <- paste0(path_png, plot_name, ".png")
ggsave(namefile,
       width = 15, height = 15, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = "png"
)


namefile <- paste0(path_eps, plot_name, ".eps")
ggsave(namefile,
       width = 15, height = 15, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = "eps"
)

# # Fig 5 - Curve example - Vanthoor ----------------------------------------
upd <- allStates %>%
  filter(config == 106 | config == 111,
         (filt == "enkf" & case == "case2"),
         frequency == 0.1) %>%
  mutate(filt_ = factor(filt,
                        levels = c("ukf", "gnvIV", "cpsIV", "enkf"),
                        labels = c("UKF", "OL, not calib.", "Sim. truth",
                                   "EnKF")),
         type_ = if_else(config == 111, "Fixed", "Variable"),
         type_ = as.factor(type_))

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

simuls <- simul_NotCalib %>%
  bind_rows(simul_calib) %>%
  filter(variable == "wm" | variable == "wf", das >= 40,
         measurement <= 300)

assim <- upd %>%
  filter(variable == "wm" | variable == "wf", das >= 40) %>%
  mutate(variable_ = factor(variable,
                            levels = c("wf", "wm"), labels = c("Wf", "Wm")))

obs_plot <- obs_mod %>%
  filter(variable == "wm" | variable == "wf",
         config == 200, dat >= 40) %>%
  mutate(meas_var = variable) %>%
  mutate(variable_ = factor(variable,
                            levels = c("wf", "wm"), labels = c("Wf", "Wm")))

assim1 <- assim %>%
  filter(meas_var == "wf")

obs1 <- obs_plot %>%
  filter(meas_var == "wf")

cov_changes_ <- c(52, 66, 90)
cov_changes <- data.frame(das = cov_changes_, 
                          measurement = rep(0, length(cov_changes_)), 
                          variable = rep("wf", length(cov_changes_)))

p1 <- ggplot() +
  facet_grid(meas_var + variable ~ exp, scales = "free",
             labeller = labeller(meas_var = plots_meas_short,
                                 exp = plots_exps,
                                 variable = plots_st_1L_short)) +
  # Simulations
  geom_point(data = simuls, aes(das, measurement, fill = filt_),
             size = 1, shape = 21, colour = "transparent") +
  # Assimilation
  geom_line(data = assim1, aes(das, measurement, colour = type_,
                               group = interaction(rep, variable, type_)
                               ),
             size = 1, alpha = 0.3) +
  # # Observations
  geom_point(data = obs1, aes(dat, measurement),
             size = 0.5, alpha = 0.5, color = "black") +
  # # Changes covs
  geom_point(data = cov_changes, aes(das, measurement),
             size = 1, shape = 4, colour = "black") +
  labs(x = "",
       y = "Dry weight [g D.M./m² soil]",
       linetype = "Variable",
       colour = "Observation covariance",
       fill = "Simulation")   +
  # scale_colour_brewer(palette = "Set1", type = "") +
  scale_colour_manual(values = paletteer_d("ggsci::nrc_npg")[c(1, 2)]) +
  scale_fill_manual(values = paletteer_d("ggsci::nrc_npg")[c(3, 4)]) +
  guides(linetype=guide_legend(title.theme = element_text(face = "bold")),
         color=guide_legend(title.theme = element_text(face = "bold")),
         fill=guide_legend(title.theme = element_text(face = "bold"))) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99"))

simul2 <- simuls %>%
  filter(variable == "wm", measurement <= 150)
  
assim2 <- assim %>% 
  filter(meas_var == "wm", variable == "wm")

obs2 <- obs_plot %>%
  filter(variable == "wm")

cov_changes <- data.frame(das = cov_changes_, 
                          measurement = rep(0, length(cov_changes)), 
                          variable = rep("wm", length(cov_changes)))

p2 <- ggplot() +
  facet_grid(meas_var + variable ~ exp, scales = "free",
             labeller = labeller(meas_var = plots_meas_short,
                                 exp = plots_exps,
                                 variable = plots_st_1L_short)) +
  # Simulations
  geom_point(data = simul2, aes(das, measurement, fill = filt_),
             size = 1, shape = 21, colour = "transparent") +
  # Assimilation
  geom_line(data = assim2, aes(das, measurement, colour = type_,
                               group = interaction(rep, variable, type_)), 
            alpha = 0.3, size = 1) +
  # # Observations
  geom_point(data = obs2, aes(dat, measurement),
             size = 0.5, alpha = 0.5, color = "black") +
  # # Changes covs
  geom_point(data = cov_changes, aes(das, measurement),
             size = 1, shape = 4, colour = "black") +
  labs(x = "",
       y = "Dry weight [g D.M./m² soil]",
       linetype = "Variable",
       colour = "Observation covariance",
       fill = "Simulation")   +
  # scale_colour_brewer(palette = "Set1", type = "") +
  scale_colour_manual(values = paletteer_d("ggsci::nrc_npg")[c(1, 2)]) +
  scale_fill_manual(values = paletteer_d("ggsci::nrc_npg")[c(3, 4)]) +
  guides(linetype=guide_legend(title.theme = element_text(face = "bold")),
         color=guide_legend(title.theme = element_text(face = "bold")),
         fill=guide_legend(title.theme = element_text(face = "bold"))) +
  theme_vert +
  theme(panel.background = element_rect(fill = "gray99")) +
  theme(strip.background.x = element_blank(),
        strip.text.x = element_blank())

p2

layout <- "
AAAA
AAAA
BBBB
"
(p1 / p2) +
  plot_layout(design = layout, guides = 'collect') &
  theme(legend.position='bottom', legend.box="vertical")

plot_name <- "Fig5-vanthoor-curves2"
namefile <- paste0(path_png, plot_name, ".png")
ggsave(namefile,
       width = 15, height = 15, units = "cm",
       family = "serif",
       dpi = 320,
       device = "png"
)

namefile <- paste0(path_eps, plot_name, ".eps")
ggsave(namefile,
       width = 15, height = 15, units = "cm",
       family = "serif",
       dpi = 320,
       device = "eps"
)

# # Sup Mat - Fig 6 - Ensemble Size - Controlled ----------------------------
# errors_mod <- errors_filt_artif %>%
#   filter(variable == "wm", !((filt == "enkf") & (case !=  "case2")),
#          filt == "enkf") %>%
#   filter(id >= 500, id <= 1000) %>%
#   mutate(config = as.numeric(config),
#          filt = factor(filt,
#                        levels = c("enkf", "ukf"),
#                        labels = c("Ensemble KF", "Unscented KF"),
#                        ordered = TRUE),
#          config_ = if_else(config %in% c(1, 4, 8), 10,
#                            if_else(config %in% c(2, 5, 7, 9), 30, 
#                                    if_else(config %in% c(3, 6, 10), 50, 0)))) %>%
#   filter(config_ != 0) %>%
#   mutate(R_ = factor(config_,
#                      levels = c("10", "30", "50"),
#                      labels = c("10%", "30%", "50%"),
#                      ordered = TRUE)) %>%
#   filter(frequency == 1, (config == 5) | (config == 7), rel_err > 0) %>%
#   mutate(type = case_when((id >= 788 & id <= 791) |  (id >= 804 & id <= 807) ~ "500",
#                           (id >= 792 & id <= 795) ~ "750",
#                           (id >= 796 & id <= 803) ~ "100",
#                           TRUE ~ "250"),
#          type_ = factor(N, levels = c(100, 250, 500, 750),
#                         ordered = TRUE)) %>%
#   filter(exp == "n03" | exp == "n05" | exp == "n07") %>%
#   group_by(frequency, filt, meas_var, exp, config_) %>%
#   mutate(id_out = cur_group_id()) %>%
#   ungroup() %>%
#   mutate(D_ = NA, p_ = NA) %>%
#   as.data.frame()
# 
# it = 2
# for (it in unique(errors_mod$id_out)){
# 
#   data_filt <- filter(errors_mod, id_out == it) %>%
#     as.data.frame()
# 
#   g1 <- filter(data_filt, N == "250")
#   g2 <- filter(data_filt, N == "100")
# 
#   temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
#   print(temp)
# 
#   condition <- ((errors_mod$id_out == it) & (errors_mod$N == 100))
# 
#   errors_mod[condition, "D_"] <- temp$statistic
#   errors_mod[condition, "p_"] <- temp$p.value
# 
#   # plot(ecdf(g1$rel_err))
#   # lines(ecdf(g2$rel_err), col = "red")
# 
#   g2 <- filter(data_filt, type == "500")
# 
#   temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
#   print(temp)
# 
#   condition <- ((errors_mod$id_out == it) & (errors_mod$N == 500))
# 
#   errors_mod[condition, "D_"] <- temp$statistic
#   errors_mod[condition, "p_"] <- temp$p.value
# 
#   #
#   g2 <- filter(data_filt, type == "750")
# 
#   temp <- ks.test(g1$rel_err, g2$rel_err, alternative='two.sided')
#   print(temp)
# 
#   condition <- ((errors_mod$id_out == it) & (errors_mod$N == 750))
# 
#   errors_mod[condition, "D_"] <- temp$statistic
#   errors_mod[condition, "p_"] <- temp$p.value
# 
# }
# 
# ks_all <- errors_mod %>%
#   select(frequency, filt, meas_var, N, exp, config_, config,
#          starts_with("D_"), starts_with("p_"),
#          id_out) %>%
#   distinct()
# 
# write.csv(ks_all, file = "./tables/metrics/assim/ks_ens_size.csv", row.names = FALSE)
# 
# errors_null <- error_notCalib %>%
#   filter(rel_err > 0, config == 0, variable == "wm") %>%
#   select(-filt) %>%
#   group_by(model, exp) %>%
#   mutate(rel_err = rel_err * 100) %>%
#   summarise(med_err = median(rel_err),
#             err_25 = quantile(rel_err, 0.25),
#             err_75 = quantile(rel_err, 0.75))
# 
# # Different marked
# errors_mod_ <- errors_mod %>%
#   select(type_, N, filt, meas_var, exp, R_, config, p_, type) %>%
#   group_by(N, filt, meas_var, exp, R_) %>%
#   mutate(label_ = case_when(((p_ < 0.05) & (row_number() == 1)) ~ "*",
#                             TRUE ~ ""),
#          label_ = na_if(label_, ""),
#          error_pos = 0)
# 
# ggplot() +
#   facet_grid(meas_var ~ exp,
#              labeller = labeller(exp = plots_exps,
#                                  meas_var = plots_meas_short)) +
#   # Assimilation
#   geom_boxplot(aes(x = R_, y =  rel_err, fill = type_),
#                data = errors_mod) +
#   geom_text(data = errors_mod_, 
#             aes(x = R_, y = error_pos, label = label_, color = type_),
#             size = 6, position = position_dodge(width = 0.75)) +
#   # Null error
#   geom_hline(data=errors_null, aes(yintercept = err_25), 
#              lty = "dashed", colour = "#ff7f00", size = 1) + 
#   geom_hline(data=errors_null, aes(yintercept = err_75), 
#              lty = "dashed", colour = "#ff7f00", size = 1) + 
#   geom_hline(data=errors_null, aes(yintercept = med_err),
#              colour = "#ff7f00", size = 1) + 
#   labs(y = "Relative Error [%]",
#        fill = "Ensemble size",
#        x = "Noise level") +
#   # coord_cartesian(ylim = c(0, 100)) +
#   # scale_fill_brewer(palette = "Set1", type = "") +
#   # scale_color_brewer(palette = "Set1", type = "") +
#   scale_fill_npg() +
#   scale_color_npg() +
#   guides(color = "none") +
#   theme_vert +
#   theme(panel.background = element_rect(fill = "gray99"),
#         axis.text.x = element_text(angle = 0,
#                                    hjust = 1,
#                                    colour = "black",
#                                    size = 8)) 
# 
# plot_name <- "Fig6-ensemblesize"
# namefile <- paste0(path_png, plot_name, ".png")
# ggsave(namefile,
#        width = 15, height = 12, units = "cm", 
#        family = "serif", 
#        dpi = 320,
#        device = "png"
# )
# 
# 
# namefile <- paste0(path_eps, plot_name, ".eps")
# ggsave(namefile,
#        width = 15, height = 12, units = "cm", 
#        family = "serif", 
#        dpi = 320,
#        device = "eps"
# )
# 
# # Eval params filt --------------------------------------------------------

filt_params <- upd_assim %>%
  mutate(across(.cols = c("das", "rep", "measurement", "id"), .fns = as.numeric)) %>%
  left_join(info_runs) %>%
  filter(id >= 1000,
         config == 106|
           config == 111, frequency == 0.1,
         ) %>%
  filter(filt == "enkf", case == "case2") %>%
  mutate(filt_ = paste(filt, meas_var, sep = "_"),
         filt_ = factor(filt_,
                        levels = c("enkf_wf", "enkf_wm", "ukf_wf",
                                   "ukf_wm", "none"),
                        labels = c("EnKF\nWf", "EnKF\nWm", "UKF\nWf",
                                   "UKF\nWm", "No assim."),
                        ordered = TRUE)) %>%
  mutate(obserr = if_else(config == 106, "variable", "fixed"))


cov_changes_ <- c(52, 66, 90)
cov_changes <- data.frame(das = cov_changes_, 
                          measurement = rep(0, length(cov_changes_)), 
                          variable = rep("wf", length(cov_changes_)))


v1 <- filt_params %>%
  # mutate(measurement_ = log(measurement)) %>%
  # filter(variable ==  "P_m" | variable == "R" | variable == "Gain", exp != "n01") %>%
  # mutate(measurement = if_else(variable == "P_m", measurement*measurement, measurement))
  filter(variable == "Gain", exp != "n01", das >= 40)

ggplot() +
  facet_grid(meas_var ~ exp) +
  geom_point(aes(x = das, y =  measurement, color = obserr),
             alpha = 0.5, data = v1) +
  # Changes covs
  geom_point(data = cov_changes, aes(das, measurement),
             size = 1, shape = 4, colour = "black") +
  scale_fill_brewer(palette = "Set1", type = "")
# 
# 
# v2 <- filt_params %>%
#   # mutate(measurement_ = log(measurement)) %>%
#   filter(variable ==  "P_m" | variable == "R", exp != "n01",
#          obserr == "variable") %>%
#   mutate(measurement = if_else(variable == "P_m", measurement*measurement, measurement))
#   # filter(variable == "Gain", exp != "n01")
# 
# ggplot() +
#   facet_grid(meas_var ~ exp, scales = "free") +
#   geom_point(aes(x = das, y =  measurement, color = variable),
#              alpha = 0.5, data = v2) +
#   ylim(c(0, 100)) +
#   scale_fill_brewer(palette = "Set1", type = "")
# 
# 
# # Curves frequency --------------------------------------------------------
# 
# upd <- allStates %>%
#   filter(as.numeric(config) <= 10, !(filt == "enkf" & case != "case2")) %>%
#   mutate(filt_ = factor(filt,
#                         levels = c("ukf", "enkf", "gnvIV", "cpsIV"),
#                         labels = c("UKF", "EnKF", "OL, not calib.", "Sim. truth"),
#                         ordered = TRUE),
#          config_ = if_else(config %in% c(1, 4, 8), 10,
#                            if_else(config %in% c(2, 5, 9), 30, 
#                                    if_else(config %in% c(3, 6, 10), 50, 0))),
#          R_ = factor(config_,
#                      levels = c("10", "30", "50"),
#                      labels = c("10%", "30%", "50%"),
#                      ordered = TRUE),
#          variable_ = factor(variable,
#                             levels = c("wf", "wm"),
#                             labels = c("Wf", "Wm"))) %>%
#   arrange(filt_) %>%
#   filter(config_ == 30)
# 
# simul_calib <- simulations %>%
#   filter(model == "tomgro",
#          # Calibrated model
#          calib == "cpsIV") %>%
#   select(-calib) %>%
#   mutate(filt_ = factor("cpsIV",
#                         levels = c("ukf", "enkf", "gnvIV", "cpsIV"),
#                         labels = c("UKF", "EnKF", "OL, not calib.", "Sim. truth"),
#                         ordered = TRUE)) %>%
#   arrange(filt_)
# 
# simul_NotCalib <- simulations %>%
#   filter(model == "tomgro",
#          # Not calibrated model
#          calib == "gnvIV") %>%
#   mutate(model = "gnvIV") %>%
#   select(-calib) %>%
#   mutate(filt_ = factor("gnvIV",
#                         levels = c("ukf", "enkf", "gnvIV", "cpsIV"),
#                         labels = c("UKF", "EnKF", "OL, not calib.", "Sim. truth"),
#                         ordered = TRUE)) %>%
#   arrange(filt_)
# 
# simuls <- bind_rows(simul_calib, simul_NotCalib) %>%
#   filter(variable == "wf" | variable == "wm", das >= 30) %>%
#   mutate(variable_ = factor(variable,
#                             levels = c("wf", "wm"),
#                             labels = c("Wf", "Wm")))
# 
# assim <- upd %>%
#   filter(filt == "enkf") %>%
#   filter(meas_var == "wf", das >= 30, 
#          variable == "wf" | variable == "wm", is.na(Q))
# 
# assim_1 <- filter(assim, filt == "ukf")
# assim_2 <- filter(assim, filt == "enkf")
# 
# assim_sum <- assim %>%
#   group_by(exp, meas_var, R_, filt_, frequency, variable_, das, config) %>%
#   summarise(measurement = mean(measurement)) %>%
#   filter(variable_ == "Wm" | variable_ == "Wf", das >= 30) %>%
#   arrange(filt_)
# 
# obs_plot <- obs_mod %>%
#   filter(variable == "wf",
#          config == 4 | config == 5 | config == 6) %>%
#   mutate(meas_var = variable,
#          config_ = if_else(config %in% c(1, 4, 8), 10,
#                            if_else(config %in% c(2, 5, 9), 30, 
#                                    if_else(config %in% c(3, 6, 10), 50, 0))),
#          R_ = factor(config_,
#                      levels = c("10", "30", "50", "0"),
#                      labels = c("10%", "30%", "50%", "Other"),
#                      ordered = TRUE),
#          variable_ = factor(variable,
#                             levels = c("wf", "wm"),
#                             labels = c("Wf", "Wm"))) %>%
#   filter(dat >= 30) %>%
#   filter(config_ == 30)
# 
# obs_plot_1 <- filter(obs_plot, variable == "wf")
# assim_1_1 <- filter(assim_1, meas_var == "wf")
# assim_2_1 <- filter(assim_2, meas_var == "wf")
# assim_sum_1 <- filter(assim_sum, meas_var == "wf")
# 
# ggplot() +
#   facet_grid(frequency ~ exp, scales = "free", drop = TRUE,
#              labeller = labeller(meas_var = plots_meas_short,
#                                  exp = plots_exps,
#                                  variable = plots_st_1L)) + 
#   # Observations
#   geom_point(data = obs_plot_1, aes(dat, measurement),
#              size = 0.5, alpha = 0.2, color = "gray50") +
#   # Assimilation
#   geom_line(data = assim_1_1, aes(das, measurement,
#                                   group = interaction(rep, variable_, case, filt_),
#                                   colour = filt_,
#                                   linetype = variable_),
#             size = 0.5, alpha = 0.1) +
#   geom_line(data = assim_2_1, aes(das, measurement,
#                                   group = interaction(rep, variable_, case, filt_),
#                                   colour = filt_,
#                                   linetype = variable_),
#             size = 0.5, alpha = 0.2) +
#   # Simulations
#   geom_line(data = simuls, aes(das, measurement,
#                                colour = filt_, linetype = variable_),
#             size = 1.2) +
#   # Average upd Wm
#   geom_line(data = assim_sum_1, aes(das, measurement, linetype = variable_,
#                                     colour = filt_),
#             size = 1.2) +
#   labs(x = "", 
#        y = "Dry biomass [g D.M./m² soil]",
#        linetype = "Output variable",
#        colour = "Method") +
#   ylim(c(0, 600)) +
#   scale_color_manual(values = paletteer_d("ggsci::nrc_npg")[c(1, 2, 4, 3)],
#                      breaks = c("UKF", "EnKF", "OL, not calib.", "Sim. truth")) +
#   guides(linetype=guide_legend(nrow = 2,
#                                title.theme = element_text(face = "bold")),
#          color=guide_legend(nrow = 2,
#                             title.theme = element_text(face = "bold"))
#   ) +
#   theme_vert +
#   theme(panel.background = element_blank(),
#         legend.box="horizontal")
# 
