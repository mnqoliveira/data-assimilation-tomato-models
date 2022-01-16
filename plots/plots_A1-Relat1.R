# Plot datasets

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

# .libPaths(c("C:/Users/monique/Documents/R/win-library/3.4", 
#             "C:/Program Files/Microsoft/R Open/R-3.4.4/library"))
library("ggplot2")
library(tidyverse)
library("lubridate")
library("RColorBrewer")

# Load data ---------------------------------------------------------------

upd <- read.csv("../data/assim/relat1/simulacao_All_calibGainesville.csv")
simul <- read.csv("../data/assim/relat1/simulacao_calibGainesville.csv")

w_values <- read.csv("../data/assim/relat1/simulacaoEFiltro_W_calibGainesville.csv")

head(simul)
head(upd)
head(w_values)

# Simulado vs UKF ---------------------------------------------------------

simul$Modelo <- "tomgro"
upd$Modelo <- "ukf"

assim <- rbind(simul, upd) %>%
  gather("N", "LAI", "W", "Wf", "Wm", key = "Estado", value = "Estimativa")

plot_mods <- c(tomgro =  "Sem assimilação", ukf = "Com assimilação")
plot_states <- c("LAI" = "IAF\n[m² folha/m² solo]", 
                 "N" = "N\n[nº. de nós]", 
                 "W" = "Biomassa aérea\n[g M.S./m² solo]", 
                 "Wf" = "Massa de frutos\n[g M.S./m² solo]", 
                 "Wm" = "Massa de frutos\n maduros\n[g M.S./m² solo]")

ggplot(assim, aes(dap, Estimativa)) +
  facet_grid(Estado ~ Modelo, scales = "free",
             labeller = labeller(Estado = plot_states, 
                                       Modelo = plot_mods)) + 
  geom_line(size = 1)+
  labs(x = "Dias após o plantio", 
       y = "") + 
  theme(text = element_text(size = 6, colour = "black"),        
        axis.text.x = element_text(angle = 90, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.1, 
                                                    b = 0, l = 0,
                                                    unit = "cm")),
        legend.key.size = unit(0.2, "cm"),
        legend.text.align = 1,
        legend.margin = margin(t = 0.1, r = 0, b = 0.1, l = 0.0, "cm"),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        strip.text.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        panel.grid.major = element_line(colour = "grey60", size = 0.1, 
                                        linetype = 1))

ggsave('../figures/relat1/simulado_vs_ukf.png',
       width = 14.0, height = 8.0, units = "cm", family = "serif")


# Filtro ------------------------------------------------------------------

w_values2 <- w_values %>%
  gather("W_est_transf", "W_upd_transf", "W_meas" , 
         "W_est", "W_upd",
         key = "Variavel", value = "Valor") %>%
  mutate(Tipo = if_else(str_detect(Variavel, "transf") | 
                          str_detect(Variavel, "W_meas"),
                        "MU", "MS"),
         Variavel = if_else(str_detect(Variavel, "W_est_transf"),
                            "W_est", 
                            if_else(str_detect(Variavel, "W_upd_transf"),
                                    "W_upd", Variavel))) %>%
  filter(Tipo == "MU")

var_names <- c(W_meas = "Biomassa medida", 
               W_est = "Biomassa estimada pelo modelo (após conversão)", 
               W_upd = "Biomassa estimada com assimilação da medida (após conversão)")

ggplot(w_values2, aes(dap, Valor)) +
  geom_line() +
  facet_wrap("Variavel", 
             labeller = labeller(Variavel = var_names), nrow = 3) +
  labs(x = "Dias após o plantio", 
       y = "Biomassa úmida da parte aérea [g M.U./m² solo]") + 
  theme(text = element_text(size = 7, colour = "black"),        
        axis.text.x = element_text(angle = 90, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.1, 
                                                    b = 0, l = 0,
                                                    unit = "cm")),
        legend.key.size = unit(0.2, "cm"),
        legend.text.align = 1,
        legend.margin = margin(t = 0.05, r = 0, b = 0.1, l = 0.0, "cm"),
        legend.position = "bottom",
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        strip.text.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        panel.grid.major = element_line(colour = "grey60", size = 0.1, 
                                        linetype = 1))

ggsave('../figures/relat1/filtered.png',
       width = 14.0, height = 8.0, units = "cm", family = "serif")

# Mass SD - First cycle ---------------------------------------------------
end_date <- ymd("2019-04-19")

weight <- dataset_hourly %>%
  filter(date <= end_date) %>%
  mutate(plant = if_else(plant == 1, 3, plant)) %>%
  mutate(fullTime = ymd_h(paste0(date, "T", hour))) %>%
  # filter((sensor_name == "t19" & variable == "temperature") | 
  #          (sensor_name == "h19" & variable == "humidity") |
  #          (sensor_name == "bh1750" & variable == "rad"), 
  #        stat == "mean") %>%
  filter(sdWM < 400) %>%
  #filter(hour < 7 | hour > 18)
  filter(hour == 2 | hour ==  5) 
#%>%
#select(-sensor_var, -sensor_name, -stat) %>%
# spread(variable, stat_value)

ggplot(weight, aes(as.Date(fullTime), sdWM)) +
  facet_wrap("plant") + 
  #geom_point(aes(colour = as.factor(hour)), size = 5)+
  geom_bar(aes(fill = as.factor(hour)), position = "dodge", stat="identity")+
  geom_vline(xintercept = c(ymd("2019-02-15"), ymd("2019-03-10"))) +
  scale_x_date(date_breaks = "7 days", date_labels = "%d-%m-%y") +
  labs(x = "Data", 
       y = "Desvio-padrão horário das medições realizadas a cada minuto [g]",
       fill = "Hora") + 
  theme(text = element_text(size = 7, colour = "black"),        
        axis.text.x = element_text(angle = 90, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.1, 
                                                    b = 0, l = 0,
                                                    unit = "cm")),
        legend.key.size = unit(0.2, "cm"),
        legend.text.align = 1,
        legend.margin = margin(t = 0.1, r = 0, b = 0.1, l = 0.0, "cm"),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        strip.text.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        panel.grid.major = element_line(colour = "grey60", size = 0.1, 
                                        linetype = 1))
ggsave('../figures/relat1/sdWM_cycle0.png',
       width = 14.0, height = 8.0, units = "cm", family = "serif")


# Moisture - First cycle --------------------------------------------------
water <- dataset_hourly %>%
  filter(sensor == "ec05") %>%
  #filter(plant == 1) %>%
  mutate(fullTime = ymd_h(paste0(dmy, "T", hour))) %>%
  filter(fullTime > ymd_h(paste0("2019-02-22", "T", 9)))  %>%
  filter(hour %% 4 == 0)
#%>%
#filter(mean_mv <= 4400) %>%
# mutate(test = ifelse(hour == 5, T, F)) %>%
# mutate(mean_mv = ifelse(sensor == "graphite",
#                         mean_mv * -0.2, mean_mv))

png(filename = "../figures/water.png", width = 1304, height = 768)
ggplot(water, aes(fullTime, mean_mv)) +
  facet_wrap("node") + 
  geom_point(aes(colour = as.factor(hour)), size = 1)+
  geom_errorbar(aes(ymin=mean_mv-sd_mv, ymax=mean_mv+sd_mv), 
                width = 0.05) +
  labs(x = "", y = "Tensão registrada no sensor de umidade [mV]",
       colour = "Hora") + 
  scale_x_datetime(date_breaks = "7 days", 
                   date_labels = "%d-%m-%y") +
  theme(text = element_text(size = 7, colour = "black"),        
        axis.text.x = element_text(angle = 90, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.1, 
                                                    b = 0, l = 0,
                                                    unit = "cm")),
        legend.key.size = unit(0.2, "cm"),
        legend.text.align = 1,
        legend.margin = margin(t = 0.1, r = 0, b = 0.1, l = 0.0, "cm"),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        strip.text.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        panel.grid.major = element_line(colour = "grey60", size = 0.1, 
                                        linetype = 1))
ggsave('../figures/relat1/moisture_cycle0.png',
       width = 14.0, height = 8.0, units = "cm", family = "serif")

dev.off()

# par(mfrow = c(2,1))  
# plot(water$fullTime[water$plant == 1 & water$sensor == "graphite"], 
#      water$mean_mv[water$plant == 1 & water$sensor == "graphite"])
# plot(water$fullTime[water$plant == 1 & water$sensor == "ec05"], 
#      water$mean_mv[water$plant == 1 & water$sensor == "ec05"])



# Generic mass plots ------------------------------------------------------

weight2 <- weight %>%
  filter(hour == 2)

png(filename = "../figures/mass.png", width = 1304, height = 768)
#p <- ggplot(weight, aes(fullTime, meanWM)) +
ggplot(weight2, aes(as.Date(fullTime), mass_plant)) +
  geom_errorbar(aes(ymin=mass_plant-sdWM, ymax=mass_plant+sdWM), width=.1, 
                colour="red") +
  facet_wrap("plant") + 
  geom_point(size = 3) + 
  #geom_smooth() +
  scale_x_date(date_breaks = "7 days", date_labels = "%y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

weight2_A <- filter(weight, node == 1)
weight2_B <- filter(weight, node == 2)

#ggsave(filename = "../figures/mass.png", plot = p, device = png)

png(filename = "../figures/dif.png", width = 1304, height = 768)
ggplot(weight, aes(as.Date(dmy), dif)) +
  #facet_wrap("plant") + 
  geom_point(aes(colour = as.factor(plant)), size = 5) +
  geom_errorbar(aes(ymin=dif-sdWM, ymax=dif+sdWM), width=.1) +
  geom_hline(yintercept = 0) +
  #geom_smooth() +
  scale_x_date(date_breaks = "7 days", date_labels = "%y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()



# Generic weather plots ---------------------------------------------------
weather <- dataset_hourly %>%
  #filter(sensor_temp == "ti2c") %>%
  #filter(plant == 1) %>%
  mutate(fullTime = ymd_h(paste0(dmy, "T", hour))) %>%
  filter(fullTime > ymd_h(paste0("2019-02-10", "T", 9)))  %>%
  filter(!is.na(sensor_hum))

png(filename = "../figures/hum.png", width = 1304, height = 768)
ggplot(weather, aes(as.Date(fullTime), humidity_mean)) +
  facet_grid(sensor_hum ~ node) + 
  geom_point(aes(colour = as.factor(hour)), size = 5) +
  scale_x_date(date_breaks = "7 days", date_labels = "%y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

png(filename = "../figures/temp.png", width = 1304, height = 768)
ggplot(weather, aes(as.Date(fullTime), temperature_mean)) +
  facet_grid(sensor_temp ~ node) + 
  geom_point(aes(colour = as.factor(hour)), size = 5) +
  scale_x_date(date_breaks = "7 days", date_labels = "%y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

png(filename = "../figures/hum_max.png", width = 1304, height = 768)
ggplot(weather, aes(as.Date(fullTime), humidity_max)) +
  facet_grid(sensor_hum ~ node) + 
  geom_point(aes(colour = as.factor(hour)), size = 5) +
  scale_x_date(date_breaks = "7 days", date_labels = "%y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

png(filename = "../figures/temp_max.png", width = 1304, height = 768)
ggplot(weather, aes(as.Date(fullTime), temperature_max)) +
  facet_grid(sensor_temp ~ node) + 
  geom_point(aes(colour = as.factor(hour)), size = 5) +
  scale_x_date(date_breaks = "7 days", date_labels = "%y-%m-%d") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()



# Fixed weight - wrong data -----------------------------------------------
calibration_date <- sensor_dates %>%
  # filter(str_detect(event, "calibration_start"), 
  #        str_detect(comment, "standard"))  %>%
  filter(str_detect(event, "calibration_start"),
         str_detect(comment, "bucket")) %>%
  filter(!is.na(dmy(date))) %>%
  select(date) %>%
  mutate(date = dmy(date))

calibration_date2 <- calibration_date + 7
calibration_date3 <- calibration_date + 17

wrong_data <- dataset_hourly %>%
  filter(dmy >= calibration_date2, dmy < calibration_date3) %>%
  filter(if_else(dmy == min(dmy), hour >= 16, hour >= 0)) %>%
  mutate(node = as.numeric(node)) %>%
  filter((sensor_name == "t19" & variable == "temperature") | 
           (sensor_name == "h19" & variable == "humidity") |
           (sensor_name == "bh1750" & variable == "rad"), 
         stat == "mean") %>%
  filter(meanWM > -100) %>%
  spread(variable, stat_value) %>%
  group_by(cycle, dmy, hour, node, plant) %>%
  summarise_at(c("humidity", "rad", "temperature", "meanWM"), 
               funs(mean), na.rm = TRUE) %>%
  gather('meanWM', "temperature", 
         key = 'var', value = 'measurement') %>%
  mutate(id = ymd_h(paste(dmy, hour)))

plot_vars <- c(meanWM =  "Massa [g]", temperature = "Temperatura [ºC]")
plot_nodes <- c("1" = "Nó 1", "2" = "Nó 2")

ggplot(wrong_data, aes(id, measurement)) +
  geom_point(size = 0.5) +
  facet_wrap(var ~ node, scales = "free",
             labeller = labeller(var = plot_vars, node = plot_nodes)) +
  labs(x = "Dia (Dia:Hora)", 
       y = "") + 
  scale_x_datetime(date_breaks = "8 hours",
                   date_labels = "%d:%H") +
  theme(text = element_text(size = 7, colour = "black"),        
        axis.text.x = element_text(angle = 90, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.1, 
                                                    b = 0, l = 0,
                                                    unit = "cm")),
        legend.key.size = unit(0.2, "cm"),
        legend.text.align = 1,
        legend.margin = margin(t = 0.1, r = 0, b = 0.1, l = 0.0, "cm"),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        strip.text.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        panel.grid.major = element_line(colour = "grey60", size = 0.1, 
                                        linetype = 1))

ggsave('../figures/relat1/weight_empty0.png',
       width = 14.0, height = 8.0, units = "cm", family = "serif")


# Fixed weight - proper data ----------------------------------------------
fixed_weight <- dataset_hourly %>%
  filter(dmy >= calibration_date3) %>%
  filter(if_else(dmy == min(dmy), hour >= 16, hour >= 0)) %>%
  mutate(node = as.numeric(node)) %>%
  filter((sensor_name == "radio" & variable == "temperature") | 
           (sensor_name == "radio" & variable == "humidity") |
           (sensor_name == "bh1750" & variable == "rad"), 
         stat == "mean") %>%
  filter((node == 1 & meanWM > -15 | 
            node == 2 & meanWM > 1990)) %>%
  spread(variable, stat_value) %>%
  group_by(cycle, dmy, hour, node, plant) %>%
  summarise_at(c("humidity", "rad", "temperature", "meanWM"), 
               funs(mean), na.rm = TRUE) %>%
  gather('meanWM', "temperature", 
         key = 'var', value = 'measurement') %>%
  mutate(id = ymd_h(paste(dmy, hour)))

plot_vars <- c(meanWM =  "Massa [g]", temperature = "Temperatura [ºC]")
plot_nodes <- c("1" = "Nó 1", "2" = "Nó 2")

ggplot(fixed_weight, aes(id, measurement)) +
  geom_point(size = 0.5) +
  facet_wrap(var ~ node, scales = "free",
             labeller = labeller(var = plot_vars, node = plot_nodes)) +
  labs(x = "Dia (Dia:Hora)", 
       y = "") + 
  scale_x_datetime(date_breaks = "8 hours",
                   date_labels = "%d:%H") +
  theme(text = element_text(size = 7, colour = "black"),        
        axis.text.x = element_text(angle = 90, hjust = 1, colour = "black"),
        axis.text.y = element_text(colour = "black"),
        axis.title.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        axis.title.y = element_text(margin = margin(t = 0, r = 0.1, 
                                                    b = 0, l = 0,
                                                    unit = "cm")),
        legend.key.size = unit(0.2, "cm"),
        legend.text.align = 1,
        legend.margin = margin(t = 0.1, r = 0, b = 0.1, l = 0.0, "cm"),
        panel.background = element_rect(fill = "white"),
        plot.margin = unit(c(0.1,0.1,0,0.1), "cm"),
        strip.text.x = element_text(margin = margin(t = 0.1, r = 0, 
                                                    b = 0.1, l = 0,
                                                    unit = "cm")),
        panel.grid.major = element_line(colour = "grey60", size = 0.1, 
                                        linetype = 1))

ggsave('../figures/relat1/weight_empty1.png',
       width = 14.0, height = 8.0, units = "cm", family = "serif")
