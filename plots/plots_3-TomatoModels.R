# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(ggplot2)
library(tidyverse)
library(zoo)
library(lubridate)
library("data.table")

#install.packages("devtools")
# devtools::install_github("thomasp85/patchwork")
# 
library(patchwork)
library(paletteer)
library(RColorBrewer)

# Source functions --------------------------------------------------------

# Update results
source("./data_analyses/07a-orgSimOut.R")
source("./data_analyses/07b-orgObs_plot.R")
source("./data_analyses/07d-orgSA.R")


# Load data ---------------------------------------------------------------
load("./data/plot_theme_horiz.RData")

outputs_extreme <- read.csv("./tables/results_SA/outputs_SA.csv")
all_files_si <- read.csv("./tables/info_Si_org.csv")
case1_hi <- read.csv("./tables/results_SA/case1_highest_indices.csv")

models_all <- read.csv("./tables/results_simul/results_simulations_all.csv")
obs <- read.csv("./data/observations/monitoring/observations_proc.csv")
obs_last <- read.csv("./data/observations/monitoring/observations_last.csv")

# Auxiliary info ----------------------------------------------------------
plot_params <- c("co2" = "Carbon Dioxide",
                 "rad" = "Solar Radiation",
                 "tmax" = "Maximum Temperature",
                 "tmean" = "Average Temperature",
                 "tmin" = "Minimum Temperature")

out_simple <- c("w", "wm", "lai")
out_tomgro <- c("n", "lai", "w", "wf", "wm")

plot_states <- c("lai" = "Leaf area index\n[m² leaves/m² soil]", 
                 "n" = "Number of nodes\n[number of nodes/\nm² soil]", 
                 "w" = "Aboveground\n biomass\n[g D.M./m² soil]", 
                 "wf" = "Fruit mass\n[g D.M./m² soil]", 
                 "wm" = "Mature fruit mass\n[g D.M./m² soil]")

plot_states_unitless <- c("lai" = "LAI", 
                          "n" = "Number of nodes", 
                          "w" = "Aboveground dry\n biomass", 
                          "wf" = "Fruit dry mass", 
                          "wm" = "Mature dry fruit mass")

codes_gnv <- c("gnv_n01" = "Gainesville - 1993 - Cool", 
               "gnv_n02" = "Gainesville - 1993 - Warm", 
               "gnv_n03" = "Gainesville - 1993 - Hot", 
               "gnv_n04" = "Gainesville - 1994 - Cool", 
               "gnv_n05" = "Gainesville - 1994 - Warm", 
               "gnv_n06" = "Gainesville - 1994 - Hot")

codes_cps <- c("cps_n07" = "Campinas",
               "cps_n08" = "Campinas")

codes_exps_int <- c("7" = "Campinas",
                    "4" = "Gainesville - 1994 - Cool", 
                    "5" = "Gainesville - 1994 - Warm", 
                    "6" = "Gainesville - 1994 - Hot")

plot_model_name <- c("tomgro" = "Reduced Tomgro",
                     "vanthoor" = "Vanthoor",
                     "simple" = "Simple")

plot_cities <- c("gnv" = "Gainesville",
                 "cps" = "Campinas",
                 "arA" = "Campinas",
                 "lc" = "Lake City", codes_gnv, codes_cps)

cod_unc <- read.csv("./tables/path_cod_unc.csv") %>%
  mutate(season = if_else(city == "cps",
                          factor(pdoy,
                                 levels = c(80, 172, 265, 356),
                                 labels = c("Aut", "Win", "Spr", "Sum")),
                          factor(pdoy,
                                 levels = c(80, 172, 265, 356),
                                 labels = c("Spr", "Sum", "Aut", "Win"))),
         cod = cod - 1) %>%
  select(cod, city, season)

# my_colors11 <- RColorBrewer::brewer.pal(11, "RdYlBu")
# my_colors4 <- RColorBrewer::brewer.pal(11, "RdYlBu")[c(1, 3, 10, 11)]
# my_colors3 <- RColorBrewer::brewer.pal(4, "RdBu")[c(1, 3:4)]
my_colors3 <- paletteer_d("ggthemes::stata_s1color")[c(1, 2, 13)]
my_colors2 <- paletteer_d("ggthemes::stata_s1color")[c(1, 2, 13)]

# Process data ------------------------------------------------------------
models <- models_all %>%
  mutate(city_exp = paste(city, exp, sep = "_")) 

models_ens <- models %>%
  group_by(das, exp, calib, city, variable, city_exp) %>%
  summarise(measurement = mean(measurement, na.rm = TRUE)) %>%
  mutate(model = "ensemble") %>%
  filter((city_exp == "cps_n07" & (calib == "cpsopt")) |
           ((city_exp == "gnv_n04" | city_exp == "gnv_n05" | 
               city_exp == "gnv_n06") & 
              ((calib == "gnvopt")))) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  mutate(type_calib = if_else(calib == "cpsopt" | calib == "gnvopt",
                              "opt", "man")) %>%
  filter(type_calib == "opt") %>%
  select(-type_calib) %>%
  ungroup() %>%
  group_by(city_exp) %>%
  filter(das < max(das)-1) %>%
  ungroup

obs_mod <- obs %>%
  filter(variable %in% names(plot_states))

# Figure 1: Model comparison ----------------------------------------------

model_plot <- models %>%
  filter((city_exp == "cps_n07" & ((calib == "cps4") | (calib == "cpsopt"))) |
           ((city_exp == "gnv_n04" | city_exp == "gnv_n05" | 
               city_exp == "gnv_n06") & 
              ((calib == "gnv") | (calib == "gnvopt")))) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  mutate(type_calib = if_else(calib == "cpsopt" | calib == "gnvopt",
                              "opt", "man")) %>%
  filter(type_calib == "opt") %>%
  select(-type_calib) %>%
  rbind(models_ens)

obs_plot <- obs_mod %>%
  filter(city_exp == "cps_n07" | city_exp == "gnv_n04" | city_exp == "gnv_n05" | 
           city_exp == "gnv_n06") %>%
  select(-model) %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

ggplot() +
  facet_grid(variable ~ city_exp,
             labeller = labeller(variable = plot_states,
                                 city_exp = plot_cities),
             scales = "free",
             space = "free_x") +
  geom_line(data = model_plot, aes(das, measurement, 
                                   col = model),
            size = 0.8) +
  geom_point(data = obs_plot, aes(das, measurement),
             size = 2) +
  labs(x = "Days after simulation started",
       y = "",
       colour = "Model") +
  scale_colour_brewer(palette = "Set1", type = "div",
                      breaks=c("simple", "tomgro", "vanthoor", "ensemble"),
                      labels=c("Simple", "Reduced Tomgro", 
                               "Vanthoor", "Ensemble")) +
  theme_horiz +
  theme(panel.background = element_rect(fill = "grey99"))

plot_file_name <- paste0('./paper/paper1-model/figures/fig1_modelComparison.png')
ggsave(plot_file_name,
       width = 20.0, height = 20.0, units = "cm", family = "serif")

# Figures 2 to 4: SIs params Wm -------------------------------------------

all_Si_Filt <- all_files_si %>%
  filter(case == 1, n_samples == 5000)

all_si_l <- list()

for (it in 1:nrow(all_Si_Filt)){
  
  
  si <- read.csv(all_Si_Filt$path[it]) %>%
    mutate(path = all_Si_Filt$path[it])
  
  if (all_Si_Filt$model[it] == "simple"){
    si <- si %>%
      rename(w = biomass,
             wm = plant_yield,
             lai = f_solar)
  }
  
  si <- si %>%
    left_join(all_Si_Filt[it, ]) %>%
    mutate(cod = str_extract(str_extract(path, "path[:digit:]{3}"), 
                             "[:digit:]{3}"),
           cod = as.numeric(cod)) %>%
    select(-path)
  
  all_si_l[[it]] <- si
  
  
}

all_si1 <- rbindlist(all_si_l, fill = TRUE)


all_si_plot <- all_si1 %>%
  select(-n_splits, -sensor_type, -cont, 
         -config, -comment, -run, -var_out, -n_samples,
         -case, -exp) %>%
  filter(!grepl("conf", index)) %>%
  rename(type = index) %>%
  gather(lai, w, wf, wm,
         key = "variable", value = "index") %>%
  filter(type == "ST")  %>%
  left_join(cod_unc) 

# PLOT
wm_si_plot <- all_si_plot %>%
  filter(variable == "wm") %>%
  group_by(model, type, variable) %>%
  mutate(id_plot = cur_group_id(),
         city = factor(city, 
                       levels = c("gnv", "cps"),
                       labels = c("Gainesville", 
                                  "Campinas"))) %>%
  filter(type == "ST") %>%
  ungroup()

plots <- wm_si_plot %>%
  select(model, id_plot) %>%
  distinct

it <- 1

for (it in 1:nrow(plots)){
  
  dataset_plot <- wm_si_plot %>%
    filter(id_plot == plots$id_plot[it]) %>%
    mutate(index = if_else(is.na(index) | is.nan(index), 0., index)) %>%
    filter(index >= 0.0, index <= 10) %>%
    group_by(factors, dat, city) %>%
    summarise(index_avg = mean(index, na.rm = TRUE)) %>%
    ungroup() %>%
    filter(index_avg > 0.02) %>%
    complete(factors, dat, city,
             fill = list(index_avg = 0)) %>%
    filter(dat > 40) %>%
    mutate(type = "Total sensitivity",
           lab = if_else(city == "Gainesville", "C", "D"))
  
  dataset_sd <- wm_si_plot %>%
    filter(id_plot == plots$id_plot[it]) %>%
    mutate(index = if_else(is.na(index) | is.nan(index), 0., index)) %>%
    filter(index >= 0.0, index <= 10) %>%
    group_by(factors, dat, city) %>%
    summarise(index_sd = sd(index, na.rm = TRUE)) %>%
    ungroup() %>%
    complete(factors, dat, city,
             fill = list(index_sd = 0)) %>%
    filter(dat > 40) %>%
    mutate(type = "Standard deviation",
           lab = if_else(city == "Gainesville", "A", "B")) %>%
    filter(factors %in% unique(dataset_plot$factors))
  
  ggplot() +
    facet_grid(type ~ city,
               labeller = labeller(variable = plot_states),
               scales = "free_y") +
    geom_area(data = dataset_plot,
              aes(x=dat, y=index_avg,
                  fill = factors),
              size = 1, linetype = 1, colour = "black") +
    geom_point(data = dataset_sd,
               aes(x=dat, y=index_sd,
                   fill = factors),
               size = 3, colour = "black", shape=21) +
    geom_text(data = dataset_sd,
              mapping = aes(x = Inf, y = Inf, label = lab),
              vjust = "inward", hjust = "inward", size = 5) + 
    geom_text(data = dataset_plot,
              mapping = aes(x = Inf, y = Inf, label = lab),
              vjust = "inward", hjust = "inward", size = 5) + 
    scale_discrete_manual(values = paletteer_d("ggthemes::stata_s1color"),
                          aesthetics = c("fill"),
                          name = "Parameters",
                          guide=guide_legend(nrow = 1,
                                             keywidth = 0.7,
                                             keyheight = 0.7,
                                             override.aes = list(colour=NA))) +
    labs(x = "Days after transplanting",
         y = "") +
    theme_horiz +
    theme(panel.background = element_rect(fill = "grey99"))
  
  plot_name <- paste0("figxx",
                      "_case1_wm_", 
                      plots$model[it])
  plot_file_name <- paste0('./paper/paper1-model/figures/', plot_name,'.png')
  ggsave(plot_file_name,
         width = 23.0, height = 20, units = "cm",
         family = "serif")  
  
}


# Figure 5 - Unc Yield Param ----------------------------------------------
load("./tables/results_SA/all_curves_params.RData")

# Plot each exp separately
dataset_plot <- filtered_curves %>%
  gather(-c("das", "it_fac", "id",
            "case", "model", "city", "exp",
            "param", "value", "flag"),
         key = "variable", value = "measurement")

combinations <- expand.grid(city = c("gnv" , "cps"),
                            model = c("simple", "vanthoor", "tomgro"),
                            stringsAsFactors = FALSE) %>%
  mutate(color = if_else(city == "gnv", "gray99", "gray93"))

plots <- list()
for (it in 1:nrow(combinations)){
  
  case1_hi_filt <- case1_hi %>%
    filter(model == combinations$model[it], 
           city == combinations$city[it],
           variable == "wm")
  
  plot_filt <- dataset_plot  %>%
    filter(param %in% case1_hi_filt$factors,
           model == combinations$model[it],
           city == combinations$city[it]) %>%
    mutate(city = factor(city,
                         levels = c("gnv", "cps"),
                         labels = c("Gainesville",
                                    "Campinas")))
  if (it < 5){
    lab_X <- ""
  } else {
    lab_X <- "Days after simulation started"
  }

  plots[[it]] <- ggplot() +
    facet_wrap("param", scales = "free") +
    geom_line(data = plot_filt, 
              aes(das, measurement, group = it_fac, colour = flag)) +
    labs(x = lab_X, y = "") +
    scale_colour_manual(values = c("firebrick3", "dodgerblue2"),
                        breaks=c("max", "min"),
                        labels=c("Largest values", "Smallest values"),
                        name = "") +
    theme_horiz +
    theme(panel.background = element_rect(fill = combinations$color[it]))

}

plot <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) / (plots[[5]] + plots[[6]]) 
plot + plot_annotation(tag_levels = c('A')) +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

plot_name <- paste0("FigureXX_case1_wm_curves", 
                    plots$model[it])
plot_file_name <- paste0('./paper/paper1-model/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 23.0, height = 20, units = "cm",
       family = "serif")  

# Figures 6 and 7 - SIs Weather -------------------------------------------

load("./tables/results_SA/all_si_weather.RData")

all_si_plot <- all_si %>%
  mutate(wm = na_if(wm, -99)) %>%
  filter(index == "ST")

tomgro <- all_si_plot %>%
  filter(model == "tomgro") %>%
  gather(lai, w, wf, wm,
         key = "variable", value = "index") %>%
  mutate(index = if_else(is.na(index) | is.nan(index), 0., index)) %>%
  filter(index > 0)
  
ylim_ <- 1.2

ggplot() +
  facet_grid(variable ~ exp,
             labeller = labeller(variable = plot_states_unitless,
                                 exp = codes_exps_int)) +
  geom_area(data = tomgro,
            aes(x=dat, y=index,
                fill = factors),
            size = 1, linetype = 1, colour = "black") +
  ylim(0, ylim_) +
  labs(x = "Days after simulation started", y = "") +
  scale_discrete_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3),
                        aesthetics = c("fill"),
                        name = "Factors",
                        breaks = names(plot_params),
                        labels = plot_params) +
  theme_horiz +
  theme(panel.background = element_rect(fill = "grey99"))

plot_name <- paste0("FigureXX_case0_si_tomgro")
plot_file_name <- paste0('./paper/paper1-model/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 23.0, height = 20, units = "cm",
       family = "serif")  


vanthoor <- all_si_plot %>%
  filter(model == "vanthoor") %>%
  gather(lai, w, wf, wm,
         key = "variable", value = "index") %>%
  mutate(index = if_else(is.na(index) | is.nan(index), 0., index)) %>%
  filter(index > 0)

ylim_ <- 1.2

ggplot() +
  facet_grid(variable ~ exp,
             labeller = labeller(variable = plot_states_unitless,
                                 exp = codes_exps_int)) +
  geom_area(data = vanthoor,
            aes(x=dat, y=index,
                fill = factors),
            size = 1, linetype = 1, colour = "black") +
  ylim(0, ylim_) +
  labs(x = "Days after simulation started", y = "") +
  scale_discrete_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3),
                        aesthetics = c("fill"),
                        name = "Factors",
                        breaks = names(plot_params),
                        labels = plot_params) +
  theme_horiz +
  theme(panel.background = element_rect(fill = "grey99"))

  
plot_name <- paste0("FigureXX_case0_si_vanthoor")
plot_file_name <- paste0('./paper/paper1-model/figures/', plot_name,'.png')
ggsave(plot_file_name,
       width = 23.0, height = 20, units = "cm",
       family = "serif")  


# Sup mat: SIs - All variables --------------------------------------------

vars_si_plot <- all_si_plot %>%
  group_by(model, type) %>%
  mutate(id_plot = cur_group_id(),
         city = factor(city,
                       levels = c("gnv", "cps"),
                       labels = c("Gainesville",
                                  "Campinas"))) %>%
  ungroup() %>%
  filter(type == "ST")

plots <- vars_si_plot %>%
  select(model, id_plot) %>%
  distinct

it <- 2
for (it in 1:nrow(plots)){

  dataset_plot <- vars_si_plot %>%
    filter(id_plot == plots$id_plot[it]) %>%
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
    facet_grid(variable ~ city,
               labeller = labeller(variable = plot_states)) +
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
    theme_horiz +
    theme(panel.background = element_rect(fill = "grey99"))

  plot_name <- paste0("FigureXX_case1_sup_",
                      dataset_plot$model[1])
  plot_file_name <- paste0('./paper/paper1-model/figures/', plot_name,'.png')
  ggsave(plot_file_name,
         width = 22.0, height = 20, units = "cm",
         family = "serif")

}




# NOT - Sup mat - Curves case 0 -------------------------------------------
load("./tables/results_SA/all_curves_weather.RData")

# PLOT
# Plot each exp separately
it <- 1

curves_plot <- all_results_plot_ext %>%
  mutate(city = factor(city, 
                       levels = c("gnv", "cps"),
                       labels = c("Gainesville", 
                                  "Campinas")))

plots <- curves_plot %>%
  select(model, id, city) %>%
  distinct

it <- 1
for (it in 1:nrow(plots)){
  
  dataset_plot <- curves_plot %>%
    filter(model == plots$model[it],
           city == plots$city[it],
           id == plots$id[it]) %>%
    gather(-c("das", "it_fac", "id",
              "case", "model", "city", "exp",
              "param", "value"),
           key = "variable", value = "measurement") %>%
    group_by(das, param, variable) %>%
    mutate(flag = if_else(value >= quantile(value, 0.9), "max", 
                          if_else(value <= quantile(value, 0.1), "min", "none"))) %>%
    ungroup() %>%
    filter(flag != "none", !is.na(measurement)) 
  
  ggplot() +
    facet_grid(variable ~ param,
               scales = "free",
               labeller = labeller(param = plot_params,
                                   variable = plot_states),
               drop = TRUE) +
    geom_line(data = dataset_plot, 
              aes(das, measurement, group = it_fac, 
                  colour = flag)) +
    labs(x = "Days after simulation started",
         y = "") +
    scale_colour_manual(values = c("firebrick3", "dodgerblue2"),
                        breaks=c("max", "min"),
                        labels=c("Largest values", "Smallest values"),
                        name = "") +
    theme_horiz +
    theme(panel.background = element_rect(fill = "gray99"))
  
  plot_name <- paste0("FigureXX_case0_", 
                      plots$model[it],
                      plots$city[it])
  plot_file_name <- paste0('./paper/paper1-model/figures/', plot_name,'.png')
  ggsave(plot_file_name,
         width = 23.0, height = 20, units = "cm",
         family = "serif")  
  
}





# NOT - Model comparison - Processes --------------------------------------
# 
# model_plot <- models %>%
#   filter(city_exp == "cps_n07"| city_exp == "gnv_n04", 
#          calib == "gnvopt" | calib == "cpsopt",
#          variable %in% c("lai", "pg", "rm"),
#          model != "simple")
# 
# plot_states <- c(plot_states,
#                  pg = "Daily photosynthesis",
#                  rm = "Daily total respiration")
# 
# ggplot() +
#   facet_grid(variable ~ city_exp,
#              labeller = labeller(variable = plot_states,
#                                  city_exp = plot_cities),
#              scales = "free", space = "free_x") +
#   geom_line(data = model_plot, 
#             aes(das, measurement, colour = model),
#             size = 1) +
#   labs(x = "Days after simulation started",
#        y = "",
#        colour = "Model") +
#   scale_colour_manual(values = my_colors3,
#                       breaks=c("simple", "tomgro", "vanthoor"),
#                       labels=c("Simple", "Reduced Tomgro", 
#                                "Vanthoor")) +
#   theme_horiz
# +
#   scale_colour_manual(name="Model",
#                       breaks=c("tomgro", "simple"),
#                       labels=c("Reduced TOMGRO", "Simple"),
#                       values = c("#4daf4a", "#e41a1c")) +
#   guides(colour = guide_legend(override.aes = list(size=5)))

# plot_file_name <- paste0('./paper/paper1/figures/simul/other_results_tomgro.png')
# ggsave(plot_file_name,
#        width = 20.0, height = 20.0, units = "cm", family = "serif")

