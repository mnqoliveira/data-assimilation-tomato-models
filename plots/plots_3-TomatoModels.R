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
library(lemon)

# Source functions --------------------------------------------------------

# Update results
# source("./data_analyses/07a-orgSimOut.R")
# source("./data_analyses/07b-orgObs_plot.R")
# source("./data_analyses/07d-orgSA.R")

# Path save ---------------------------------------------------------------

path_fig <- "./"

# Load data ---------------------------------------------------------------
load("./data/plot_theme_horiz.RData")

outputs_extreme <- read.csv("./tables/results_SA/outputs_SA.csv")
all_files_si <- read.csv("./tables/info_Si_org.csv")
case1_hi <- read.csv("./tables/results_SA/case1_highest_indices.csv")

models_all <- read.csv("./tables/results_simul/results_simulations_all.csv")
obs <- read.csv("./data/observations/monitoring/observations_proc.csv")
obs_last <- read.csv("./data/observations/monitoring/observations_last.csv")

weather_files <- c("./data/weather/daily_cpsA_n07.csv",
                   "./data/weather/daily_gnv_n04.csv",
                   "./data/weather/daily_gnv_n05.csv",
                   "./data/weather/daily_gnv_n06.csv",
                   list.files("./data/weather/unc/all/", full.names = TRUE,
                              pattern = "daily"))

# Auxiliary info ----------------------------------------------------------
plot_params <- c("co2" = "Carbon Dioxide",
                 "rad" = "Solar Radiation",
                 "tmax" = "Maximum Temperature",
                 "tmean" = "Average Temperature",
                 "tmin" = "Minimum Temperature")

plot_params_full <- c("rad" = "Solar Radiation [MJ/(m² day)]",
                      "tmax" = "Maximum Temperature [ºC]",
                      "tmin" = "Minimum Temperature [ºC]")

out_simple <- c("w", "wm", "lai")
out_tomgro <- c("n", "lai", "w", "wf", "wm")

plot_states <- c("lai" = "Leaf area index\n[m² leaves/m² soil]", 
                 "n" = "Number of nodes\n[number of nodes/\nm² soil]", 
                 "w" = "Aboveground biomass\n[g D.M./m² soil]", 
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

codes_gnv_1994 <- c("gnv_n04" = "Gainesville - Cool", 
                    "gnv_n05" = "Gainesville - Warm", 
                    "gnv_n06" = "Gainesville - Hot")

codes_cps <- c("cps_n07" = "Campinas",
               "cps_n08" = "Campinas")

codes_exps_int <- c("7" = "Campinas",
                    "0" = "Campinas",
                    "4" = "Gainesville - Cool", 
                    "5" = "Gainesville - Warm", 
                    "6" = "Gainesville - Hot")

plot_model_name <- c("tomgro" = "Reduced Tomgro",
                     "vanthoor" = "Vanthoor",
                     "simple" = "Simple")

plot_cities <- c("gnv" = "Gainesville",
                 "cps" = "Campinas",
                 "arA" = "Campinas",
                 "lc" = "Lake City", codes_gnv_1994, codes_cps)

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
  mutate(city_exp = paste(city, exp, sep = "_")) %>%
  filter(sensor == "A")

models_ens <- models %>%
  # mutate(calib = if_else(model == "vanthoor" & calib == "cps4", "cpsopt", calib)) %>%
  filter(city_exp %in% c("cps_n07", "gnv_n04", "gnv_n05", "gnv_n06" ),
         (calib %in% c("gnv", "cps4") & model != "simple") | 
           (calib %in% c("gnvopt", "cpsopt") & model == "simple")) %>%
  group_by(das, exp, city, variable, city_exp) %>%
  summarise(measurement = mean(measurement, na.rm = TRUE)) %>%
  mutate(model = "ensemble", calib = "ensemble") %>%
  # filter((city_exp == "cps_n07" & ((calib == "cps4") | (calib == "cpsopt"))) |
  #          ((city_exp == "gnv_n04" | city_exp == "gnv_n05" | 
  #              city_exp == "gnv_n06") & 
  #             ((calib == "gnv") | (calib == "gnvopt")))) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  # mutate(type_calib = if_else(calib == "cpsopt" | calib == "gnvopt" | 
  #                               (model == "vanthoor" & calib == "cps4"),
  #                             "opt", "man")) %>%
  # filter(type_calib == "opt") %>%
  # select(-type_calib) %>%
  ungroup() %>%
  group_by(city_exp) %>%
  filter(das < max(das)-1) %>%
  ungroup %>%
  mutate(linetype = "dashed")

models_ens2 <- models %>%
  filter(model != "simple") %>%
  filter(city_exp %in% c("cps_n07", "gnv_n04", "gnv_n05", "gnv_n06" ),
         calib %in% c("gnv", "cps4")) %>%
  # mutate(calib = if_else(model == "vanthoor" & calib == "cps4", "cpsopt", calib)) %>%
  group_by(das, exp, city, variable, city_exp) %>%
  summarise(measurement = mean(measurement, na.rm = TRUE)) %>%
  mutate(model = "ensemble", calib = "ensemble") %>%
  # filter((city_exp == "cps_n07" & ((calib == "cps4") | (calib == "cpsopt"))) |
  #          ((city_exp == "gnv_n04" | city_exp == "gnv_n05" | 
  #              city_exp == "gnv_n06") & 
  #             ((calib == "gnv") | (calib == "gnvopt")))) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  # mutate(type_calib = if_else(calib == "cpsopt" | calib == "gnvopt" | 
  #                               (model == "vanthoor" & calib == "cps4"),
  #                             "opt", "man")) %>%
  # filter(type_calib == "opt") %>%
  # select(-type_calib) %>%
  ungroup() %>%
  group_by(city_exp) %>%
  filter(das < max(das)-1) %>%
  ungroup %>%
  mutate(linetype = "solid")

obs_mod <- obs %>%
  filter(variable %in% names(plot_states))

# Figures 2 to 4: SIs params Wm -------------------------------------------

# all_Si_Filt <- all_files_si %>%
#   filter(case == 1, n_samples == 5000)
# 
# all_si_l <- list()
# 
# for (it in 1:nrow(all_Si_Filt)){
#   
#   
#   si <- read.csv(all_Si_Filt$path[it]) %>%
#     mutate(path = all_Si_Filt$path[it])
#   
#   if (all_Si_Filt$model[it] == "simple"){
#     si <- si %>%
#       rename(w = biomass,
#              wm = plant_yield,
#              lai = f_solar)
#   }
#   
#   si <- si %>%
#     left_join(all_Si_Filt[it, ]) %>%
#     mutate(cod = str_extract(str_extract(path, "path[:digit:]{3}"), 
#                              "[:digit:]{3}"),
#            cod = as.numeric(cod)) %>%
#     select(-path)
#   
#   all_si_l[[it]] <- si
#   
#   
# }
# 
# all_si1 <- rbindlist(all_si_l, fill = TRUE)

load("./tables/results_SA/all_si.RData")
all_si1 <- all_si

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
                       levels = c("cps", "gnv"),
                       labels = c("Campinas", 
                                  "Gainesville"))) %>%
  filter(type == "ST") %>%
  ungroup()

plots <- wm_si_plot %>%
  select(model, id_plot) %>%
  distinct %>%
  mutate(id_plot_ = c(3, 1, 2)) %>%
  arrange(id_plot_)

it <- 1

for (it in plots$id_plot_){
  
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
    mutate(type = "Total sensitivity [-]",
           lab = if_else(city == "Gainesville", "D", "C"))
  
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
    mutate(type = "Standard deviation [-]",
           lab = if_else(city == "Gainesville", "B", "A")) %>%
    filter(factors %in% unique(dataset_plot$factors))

  dataset_sd_ <- dataset_sd %>%
    select(type, city, lab) %>%
    distinct()
  
  dataset_plot_ <- dataset_plot %>%
    select(type, city, lab) %>%
    distinct()
  
  ggplot() +
    facet_rep_grid(type ~ city,
               labeller = labeller(variable = plot_states),
               scales = "free_y",
               switch = "y") +
    geom_area(data = dataset_plot, aes(x=dat, y=index_avg, fill = factors)
              # ,
              # size = 1, linetype = 1, colour = "black"
                ) +
    geom_point(data = dataset_sd, aes(x=dat, y=index_sd,
                   fill = factors), size = 3, colour = "black", shape=21) +
    geom_text(data = dataset_sd_, mapping = aes(x = Inf, y = Inf, label = lab),
              vjust = "inward", hjust = "inward", size = 5, family = "serif") + 
    geom_text(data = dataset_plot_, mapping = aes(x = Inf, y = Inf, label = lab),
              vjust = "inward", hjust = "inward", size = 5, family = "serif") + 
    scale_discrete_manual(values = paletteer_d("ggthemes::stata_s1color"),
                          aesthetics = c("fill"),
                          name = "Parameters",
                          guide=guide_legend(nrow = 2,
                                             keywidth = 0.7,
                                             keyheight = 0.7,
                                             override.aes = list(colour=NA))) +
    labs(x = "Days after transplanting [days]",
         y = "") +
    theme_horiz
  
  num_plot <- 1 + plots$id_plot_[it]
  plot_name <- paste0("fig", num_plot, "_case1_wm_", plots$model[it])
  
  namefile <- paste0(path_fig, plot_name,'.png')
  ggsave(namefile,
         width = 18, height = 16, units = "cm", dpi = 320,
         device = "png"
  )
  
  namefile <- paste0(path_fig, plot_name,'.eps')
  ggsave(namefile,
         width = 18, height = 16, units = "cm", family = "serif", 
         dpi = 320, device = cairo_ps
  )
  
}

# Figure 5 - Unc Yield Param ----------------------------------------------
load("./tables/results_SA/all_curves_params.RData")

# Plot each exp separately
dataset_plot <- all_curves_params %>%
  gather(-c("das", "it_fac", "id",
            "case", "model", "city", "exp",
            "param", "value", "flag"),
         key = "variable", value = "measurement")

combinations <- expand.grid(city = c("cps" , "gnv"),
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
           city == combinations$city[it],variable == "wm") %>%
    mutate(city = factor(city,
                         levels = c("cps", "gnv"),
                         labels = c("Campinas",
                                    "Gainesville"))) 
  
  labels <- plot_filt %>%
    group_by(param, model, city) %>%
    summarise(x_ = 5, y_ = max(measurement)) %>%
    mutate(label_ = LETTERS[it])
  
  if (it < 5){
    lab_X <- ""
  } else {
    lab_X <- "Days after simulation started [days]"
  }
  
  if (it == 1){
    lab_y <- "Yield [kg/m²]"
  } else {
    lab_y <- ""
  }

  plots[[it]] <- ggplot() +
    facet_rep_wrap("param", scales = "free") +
    geom_line(data = plot_filt, 
              aes(das, measurement, group = it_fac, colour = flag)) +
    geom_text(data = labels, aes(x_, y_, label=label_), family = "serif") +
    labs(x = lab_X, y = lab_y) +
    scale_colour_manual(values = c("firebrick3", "dodgerblue2"),
                        breaks=c("max", "min"),
                        labels=c("Largest values", "Smallest values"),
                        name = "") +
    theme_horiz +
    theme(axis.title.y = element_text(angle = 0,
                                      vjust = 1.2,
                                      hjust = 1,
                                      margin = margin(r = -25, l = 10)))

}

plot <- (plots[[1]] + plots[[2]]) / (plots[[3]] + plots[[4]]) / (plots[[5]] + plots[[6]]) 
plot + 
  # plot_annotation(tag_levels = c('A')) +
  plot_layout(guides = 'collect') &
  theme(legend.position='bottom')

plot_name <- paste0("fig5_case1_wm_curves")

namefile <- paste0(path_fig, plot_name,'.png')
ggsave(namefile,
       width = 20, height = 15, units = "cm", dpi = 320,
       device = "png"
)

namefile <- paste0(path_fig, plot_name,'.eps')
ggsave(namefile,
       width = 20, height = 15, units = "cm", family = "serif", 
       dpi = 320, device = cairo_ps
)

# Figures 6 and 7 - SIs Weather -------------------------------------------

load("./tables/results_SA/all_si_weather.RData")

all_si_plot_case0 <- all_si %>%
  mutate(wm = na_if(wm, -99),
         exp = if_else(exp == 7, 0L, exp)) %>%
  filter(index == "ST") %>%
  mutate(city = fct_relevel(factor(city),
                            c("cps", "gnv")))

tomgro <- all_si_plot_case0 %>%
  filter(model == "tomgro") %>%
  gather(lai, w, wf, wm,
         key = "variable", value = "index") %>%
  mutate(index = if_else(is.na(index) | is.nan(index), 0., index)) %>%
  filter(index > 0)
  
ylim_ <- 1.05

panel_IDs <- tomgro %>%
  group_by(variable) %>%
  mutate(x = 10, y = 1) %>%
  ungroup() %>%
  mutate(variable_ = LETTERS[as.numeric(as.factor(variable))],
         city_exp_ = as.numeric(as.factor(exp)),
         label_ = paste0(variable_, city_exp_),
         x_ = x + 5,
         y_ = y - 0.1 * y) %>%
  select(variable, exp, x_, y_, label_) %>%
  distinct()

ggplot() +
  facet_rep_grid(variable ~ exp,
             labeller = labeller(variable = plot_states_unitless,
                                 exp = codes_exps_int),
             switch = "y") +
  geom_area(data = tomgro,
            aes(x=dat, y=index, fill = factors),
            # size = 1, linetype = 1, colour = "black"
              ) +
  geom_text(data=panel_IDs, aes(x=x_, y=y_, label=label_),
            family = "serif") +
  ylim(0, ylim_) +
  labs(x = "Days after simulation started [days]", 
       y = "ST [-]") +
  scale_discrete_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3),
                        aesthetics = c("fill"),
                        name = "Factors",
                        breaks = names(plot_params),
                        labels = plot_params) +
  theme_horiz

plot_name <- paste0("fig6_case0_si_tomgro")
namefile <- paste0(path_fig, plot_name,'.png')
ggsave(namefile,
       width = 20, height = 18, units = "cm", dpi = 320,
       device = "png"
)

namefile <- paste0(path_fig, plot_name,'.eps')
ggsave(namefile,
       width = 20, height = 18, units = "cm", family = "serif", 
       dpi = 320, device = cairo_ps
)


vanthoor <- all_si_plot_case0 %>%
  filter(model == "vanthoor") %>%
  gather(lai, w, wf, wm,
         key = "variable", value = "index") %>%
  mutate(index = if_else(is.na(index) | is.nan(index), 0., index)) %>%
  filter(index > 0)

ylim_ <- 1.05

ggplot() +
  facet_rep_grid(variable ~ exp,
             labeller = labeller(variable = plot_states_unitless,
                                 exp = codes_exps_int),
             switch = "y") +
  geom_area(data = vanthoor, aes(x=dat, y=index,
                fill = factors),
            # size = 1, linetype = 1, colour = "black"
              ) +
  geom_text(data=panel_IDs, aes(x=x_, y=y_, label=label_),
            family = "serif") +
  ylim(0, ylim_) +
  labs(x = "Days after simulation started [days]", y = "ST [-]") +
  scale_discrete_manual(values = paletteer_dynamic("ggthemes_solarized::magenta", 3),
                        aesthetics = c("fill"),
                        name = "Factors",
                        breaks = names(plot_params),
                        labels = plot_params) +
  theme_horiz

  
plot_name <- paste0("fig7_case0_si_vanthoor")
namefile <- paste0(path_fig, plot_name,'.png')
ggsave(namefile,
       width = 20, height = 18, units = "cm", dpi = 320,
       device = "png"
)

namefile <- paste0(path_fig, plot_name,'.eps')
ggsave(namefile,
       width = 20, height = 18, units = "cm", family = "serif", 
       dpi = 320, device = cairo_ps
)

# Figure 8: Model comparison ----------------------------------------------

model_plot <- models %>%
  # filter((city_exp == "cps_n07" & ((calib == "cps4") | (calib == "cpsopt"))) |
  #          ((city_exp == "gnv_n04" | city_exp == "gnv_n05" |
  #              city_exp == "gnv_n06") &
  #             ((calib == "gnv") | (calib == "gnvopt")))) %>%
  filter(city_exp %in% c("cps_n07", "gnv_n04", "gnv_n05", "gnv_n06" ),
         (calib %in% c("gnv", "cps4") & model != "simple") | 
           (calib %in% c("gnvopt", "cpsopt") & model == "simple")) %>%
  filter(variable %in% c("lai", "w", "wf", "wm")) %>%
  # mutate(type_calib = if_else(calib == "cpsopt" | calib == "gnvopt" |
  #                               (model == "vanthoor" & calib == "cps4"),
  #                             "opt", "man")) %>%
  # filter(type_calib == "opt", sensor == "A") %>%
  # select(-type_calib, -sensor) %>%
  select(-sensor) %>%
  mutate(linetype = "solid") %>%
  rbind(models_ens) %>%
  rbind(models_ens2)

obs_plot <- obs_mod %>%
  filter(city_exp == "cps_n07" | city_exp == "gnv_n04" | city_exp == "gnv_n05" | 
           city_exp == "gnv_n06") %>%
  select(-model) %>%
  filter(variable %in% c("lai", "w", "wf", "wm"))

panel_IDs <- obs_mod %>%
  filter(variable %in% c("lai", "w", "wf", "wm"),
         city_exp %in% model_plot$city_exp) %>%
  select(variable, city_exp, das, measurement) %>%
  group_by(variable) %>%
  mutate(x = min(das, na.rm=TRUE),
         y = max(measurement, na.rm = TRUE)) %>%
  ungroup() %>%
  select(-das, -measurement) %>%
  mutate(variable_ = LETTERS[as.numeric(as.factor(variable))],
         city_exp_ = as.numeric(as.factor(city_exp)),
         label_ = paste0(variable_, city_exp_),
         x_ = x + 5,
         y_ = y - 0.1 * y) %>%
  distinct()

ggplot() +
  facet_rep_grid(variable ~ city_exp,
                 labeller = labeller(variable = plot_states,
                                     city_exp = plot_cities),
                 scales = "free",
                 space = "free_x",
                 switch = "y") +
  geom_text(data=panel_IDs, aes(x=x_, y=y_, label=label_),
            family = "serif") +
  geom_line(data = model_plot, aes(das, measurement, 
                                   col = model, linetype = linetype),
            size = 0.8) +
  geom_point(data = obs_plot, aes(das, measurement),
             size = 2) +
  labs(x = "Days after simulation started [days]",
       y = "",
       colour = "Model") +
  scale_colour_brewer(palette = "Set1", type = "div",
                      breaks=c("simple", "tomgro", "vanthoor", "ensemble"),
                      labels=c("Simple", "Reduced Tomgro", 
                               "Vanthoor", "Ensemble")) +
  scale_linetype_identity(breaks=c("solid", "dashed"),
                          labels=c("Full", "Partial")) +
  theme_horiz


namefile <- paste0(path_fig, "fig8_modelComparison.png")
ggsave(namefile,
       width = 20, height = 20, units = "cm", 
       dpi = 320,
       device = "png"
)


namefile <- paste0(path_fig, "fig8_modelComparison.eps")
ggsave(namefile,
       width = 20, height = 20, units = "cm", 
       family = "serif", 
       dpi = 320,
       device = cairo_ps
)


# Sup mat - Weather -------------------------------------------------------

weather_all_ <- list()

for (weather_it in weather_files){
  
  temp <- read.csv(weather_it) %>%
    mutate(city = if_else(grepl("cps", weather_it), "cps", "gnv"),
           code = if_else(grepl("n0[1-9]", weather_it), 
                          str_extract(weather_it, "n0[1-9]"), ""),
           case = if_else(grepl("unc", weather_it), "unc", "calib"),
           id = weather_it) %>%
    select(city, case, doy, tmax, tmin, rad, radiation_unit, id, code)
  
  rad_unit <- temp$radiation_unit[1]
  
  if(rad_unit == "mmolPAR"){
    temp$rad <- temp$rad / 555.6
  }
  
  doy_init <- temp$doy[1]
  
  if(doy_init > 180){
    
    temp <- temp %>%
      mutate(id = if_else(doy < 180, paste0(id, "a"), paste0(id, "b")))
    
  }
  
  weather_all_[[weather_it]] <- temp
  
}

weather_all <- rbindlist(weather_all_) %>%
  mutate(id = as.numeric(as.factor(id))) %>%
  pivot_longer(c("tmax", "tmin", "rad"), 
               names_to = "variable", values_to = "value")

unc <- filter(weather_all, case == "unc")

calib <- filter(weather_all, case == "calib") %>%
  mutate(code = paste0(city, "_", code))

colors_ <- paletteer_dynamic("ggthemes_solarized::magenta", 4)[c(1, 3, 4, 2)]

ggplot() +
  facet_grid(variable ~ city, labeller=labeller(variable = plot_params_full, 
                                                city = plot_cities),
             scales = "free_y",
             switch = "y") +
  geom_line(data=unc, aes(x=doy, y=value, group = id), color = "gray70") +
  geom_line(data=calib, aes(x=doy, y=value, group = id, color = code)) +
  labs(x = "Day of the year", y = "", color = "") +
  scale_color_manual(values = colors_,
                     name = "Experiments",
                     breaks = names(plot_cities),
                     labels = plot_cities) +
  theme_horiz

namefile <- paste0(path_fig, "fig-a1_weather.png")
ggsave(namefile,
       width = 20, height = 20, units = "cm",
       dpi = 320,
       device = "png"
)

namefile <- paste0(path_fig, "fig-a1_weather.eps")
ggsave(namefile,
       width = 20, height = 20, units = "cm",
       family = "serif",
       dpi = 320,
       device = cairo_ps
)

# Sup mat: SIs - All variables --------------------------------------------
vars_si_plot <- all_si_plot %>%
  group_by(model, type) %>%
  mutate(id_plot = cur_group_id(),
         city = factor(city,
                       levels = c("cps", "gnv"),
                       labels = c("Campinas",
                                  "Gainesville"))) %>%
  ungroup() %>%
  filter(type == "ST")

plots <- vars_si_plot %>%
  select(model, id_plot) %>%
  distinct %>%
  mutate(id_plot_ = c(3, 1, 2)) %>%
  arrange(id_plot_)

it <- 1
for (it in plots$id_plot_){

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
  
  labels <- dataset_plot %>%
    group_by(variable, city) %>%
    summarise(x_ = 5, y_ = 1.0) %>%
    ungroup() %>%
    mutate(variable_ = LETTERS[as.numeric(as.factor(variable))],
           city_exp_ = as.numeric(as.factor(city)),
           label_ = paste0(variable_, city_exp_))
  
  ggplot(dataset_plot) +
    facet_rep_grid(variable ~ city,
               labeller = labeller(variable = plot_states_unitless),
               switch = "y") +
    geom_line(data = dataset_plot,
              aes(x=dat, y=index_avg,
                  col = factors),
              size = 1)   +
    geom_point(data = dataset_plot,
               aes(x=dat, y=index, color = factors), alpha = 0.5) +
    geom_text(data=labels, aes(x_, y_, label = label_), family = "serif") +
    labs(x = "Days after transplanting [days]",
         y = "ST [-]",
         colour = "Parameters") +
    ylim(0, 1.05) +
    scale_discrete_manual(values = paletteer_d("ggthemes::stata_s1color"),
                          aesthetics = c("colour", "fill"),
                          name = "Parameters",
                          guide=guide_legend(nrow = 2,
                                             keywidth = 0.7,
                                             keyheight = 0.7)) +
    theme_horiz

  num_plot <- plots$id_plot_[it]
  plot_name <- paste0("fig-c", num_plot,"-case1_sup_",
                      dataset_plot$model[1])
  
  namefile <- paste0(path_fig, plot_name,'.png')
  ggsave(namefile,
         width = 20, height = 20, units = "cm", dpi = 320,
         device = "png"
  )
  
  namefile <- paste0(path_fig, plot_name,'.eps')
  ggsave(namefile,
         width = 20, height = 20, units = "cm", family = "serif", 
         dpi = 320, device = cairo_ps
  )

}


# Sup mat: S1 - All variables ---------------------------------------------

load("./tables/results_SA/all_si.RData")

all_si_plot <- all_si %>%
  select(-n_splits, -sensor_type, -cont, 
         -config, -comment, -run, -var_out, -n_samples,
         -case, -exp) %>%
  filter(!grepl("conf", index)) %>%
  rename(type = index) %>%
  gather(lai, w, wf, wm,
         key = "variable", value = "index") %>%
  filter(type == "S1")  %>%
  left_join(cod_unc)

vars_si_plot <- all_si_plot %>%
  group_by(model, type) %>%
  mutate(id_plot = cur_group_id(),
         city = factor(city,
                       levels = c("cps", "gnv"),
                       labels = c("Campinas",
                                  "Gainesville"))) %>%
  ungroup() %>%
  filter(type == "S1")

plots <- vars_si_plot %>%
  select(model, id_plot) %>%
  distinct %>%
  mutate(id_plot_ = c(3, 1, 2)) %>%
  arrange(id_plot_)

it <- 1
for (it in plots$id_plot_){
  
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
  
  labels <- dataset_plot %>%
    group_by(variable, city) %>%
    summarise(x_ = 5, y_ = 1.0) %>%
    ungroup() %>%
    mutate(variable_ = LETTERS[as.numeric(as.factor(variable))],
           city_exp_ = as.numeric(as.factor(city)),
           label_ = paste0(variable_, city_exp_))
  
  ggplot(dataset_plot) +
    facet_rep_grid(variable ~ city,
                   labeller = labeller(variable = plot_states_unitless),
                   switch = "y") +
    geom_line(data = dataset_plot,
              aes(x=dat, y=index_avg,
                  col = factors),
              size = 1)   +
    geom_point(data = dataset_plot,
               aes(x=dat, y=index, color = factors), alpha = 0.5) +
    geom_text(data=labels, aes(x_, y_, label = label_), family = "serif") +
    labs(x = "Days after transplanting [days]",
         y = "S1 [-]",
         colour = "Parameters") +
    ylim(0, 1) +
    scale_discrete_manual(values = paletteer_d("ggthemes::stata_s1color"),
                          aesthetics = c("colour", "fill"),
                          name = "Parameters",
                          guide=guide_legend(nrow = 2,
                                             keywidth = 0.7,
                                             keyheight = 0.7)) +
    theme_horiz
  
  num_plot <- 3 + plots$id_plot_[it]
  plot_name <- paste0("fig-c", num_plot, "-case1_sup_",
                      dataset_plot$model[1])
  
  namefile <- paste0(path_fig, plot_name,'.png')
  ggsave(namefile,
         width = 20, height = 20, units = "cm", dpi = 320,
         device = "png"
  )
  
  namefile <- paste0(path_fig, plot_name,'.eps')
  ggsave(namefile,
         width = 20, height = 20, units = "cm", family = "serif", 
         dpi = 320, device = cairo_ps
  )
  
}