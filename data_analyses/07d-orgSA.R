# Org SA outputs

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("tidyverse")
library("lubridate")

# Load data ---------------------------------------------------------------

# Load all parameters values
# Files in the results_SA folder refer to the splits outputs as well
# as parameters values and SI.

# Files paths
all_files_path <- list.files("./tables/results_SA", full.names = T, 
                             pattern = ".csv", recursive = T)

configs <- read.csv("./tables/runs_SA.csv") %>% 
  transmute(across(everything(), as.character)) %>%
  mutate(id = str_pad(id, 3, "left", "0"))

all_files <- data.frame(all_files_path) %>%
  rename(path = all_files_path) %>%
  mutate(id = str_extract(str_extract(path, "id[:digit:]{3}"), "[:digit:]{3}"),
         var_out = if_else(grepl("si_", path), 
                           "si", 
                           if_else(grepl("factors|paramsMask", path), 
                                   "factors", "outputs")),
         case = if_else(grepl("case0|case_0", path), "0", "1")) %>%
  left_join(configs)

factors <- filter(all_files, var_out == "factors")
outputs <- filter(all_files, var_out == "outputs")
si <- filter(all_files, var_out == "si")

# Process params files info -----------------------------------------------
if (file.exists("./tables/results_SA/outputs_SA.csv")){
  
  unlink("./tables/results_SA/outputs_SA.csv")
  
}

outputs_mod <- outputs %>%
  rename(path_out = path) %>%
  select(-var_out) %>%
  left_join(factors) %>%
  select(-var_out) %>%
  rename(path_fac = path)

write.csv(outputs_mod, "./tables/results_SA/outputs_SA.csv", 
          row.names=FALSE)

# Process all SI data -----------------------------------------------------
if (file.exists("./tables/info_Si_org.csv")){
  
  unlink("./tables/info_Si_org.csv")
  
}

si_mod <- si %>%
  separate(path, into=c("intro", "intro2", "type", "weather", "dat", "id_ext"), 
           sep = "_", remove = FALSE) %>%
  mutate(dat = as.numeric(str_extract(dat, "[:digit:]{3}"))) %>%
  select(-intro, -intro2, -type, -id_ext)

# mutate(var = str_extract(path, "si_[:alpha:]+_"),
#        var = str_extract(str_replace(var, "si_", ""), "[:alpha:]+"),
#        var = if_else(grepl("tt_sum", var), "tt_sum", var),
#        var = if_else(grepl("plant_yield", var), "yield", var),
#        var = if_else(grepl("f_solar", var), "fsolar", var))
  
write.csv(si_mod, "./tables/info_Si_org.csv", 
          row.names=FALSE)


# Case 0 - Indices --------------------------------------------------------



# all_Si_Filt <- all_files_si %>%
#   filter(case == 0)
# 
# all_si_l <- list()
# 
# it <- 121
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
# all_si <- Reduce(bind_rows, all_si_l)
# save(all_si, file = "./tables/results_SA/all_si_weather.RData")


# Case 0 - Curves ---------------------------------------------------------

# factors_filt <- outputs_mod %>%
#   filter(grepl("paramsMask", path_fac), case == 0) %>%
#   select(-path_out) %>%
#   distinct()
# 
# all_results_plot_l <- list()
# for (it in 1:nrow(factors_filt)){
# 
#   # Carrego o arquivo e insiro o numero de linha referente a cada teste de
#   # parametro. Mais do que isso, faco o mod
#   # pra achar a pasta e monto o nome da pasta em que estao os resultados
#   # no df de param filtrado e arrumado. Dai eu carrego numa lista e depois dou
#   # um rbind? para todos os resultados que eu quero plotar, desde que uma coluna
#   # tenha o id dos parametros.
# 
#   id_run <- factors_filt[it, "id"]
#   factors_values <- read.csv(factors_filt$path_fac[it]) %>%
#     mutate(id = id_run,
#            it_fac = row_number()-1) %>%
#     gather(-id, -it_fac, key = "param", value = "value")
# 
#   outputs_filt <- outputs_mod %>%
#     filter(outputs_mod$id == id_run, grepl("paramsMask", path_fac)) %>%
#     mutate(it_fac = str_extract(path_out, "it[[:digit:]]+"),
#            it_fac = as.numeric(gsub("it", "", it_fac))) %>%
#     arrange(it_fac) %>%
#     select(-path_fac, -run, -n_samples, -n_splits, -sensor_type,
#            -cont, -config, -comment)
# 
#   for (it2 in 1:nrow(outputs_filt)){
# 
#     temp <- read.csv(outputs_filt$path_out[it2])
# 
#     temp_ <- temp %>%
#       mutate(das = row_number(),
#              it_fac = outputs_filt$it_[it2],
#              id = factors_filt$id[it]) %>%
#       left_join(outputs_filt[it2, ]) %>%
#       left_join(factors_values) %>%
#       select(-path_out)
# 
#     if (factors_filt$model[it] == "simple"){
# 
#       temp_ <- temp_ %>%
#         mutate(k = 0.58,
#                lai = log(1 - f_solar)/-k) %>%
#         rename(w = biomass,
#                wm = plant_yield) %>%
#         select(-f_solar, -k, -tt_sum)
# 
#     }
# 
#     id_full <- paste(it, it2, sep = "_")
#     all_results_plot_l[[id_full]] <- temp_
# 
#   }
# 
# }
# 
# all_results_plot_ext <- Reduce(bind_rows, all_results_plot_l)
# save(all_results_plot_ext, file = "./tables/results_SA/all_curves_weather.RData")


# Case 1 - Indices --------------------------------------------------------
# 
# all_Si_Filt <- all_files_si %>%
#   filter(case == 1)
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
#       rename(w = tt_sum,
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
# all_si <- rbindlist(all_si_l, fill = TRUE)
# save(all_si, file = "./tables/results_SA/all_si.RData")


# # Case 1 - Outputs from extreme parameters --------------------------------
# 
# factors_filt <- outputs_mod %>%
#   filter(grepl("paramsMask", path_fac), case == 1) %>%
#   select(-path_out) %>%
#   distinct() 
# 
# all_results_plot_l <- list()
# it <- 1
# for (it in 1:nrow(factors_filt)){
# 
#   # Cada iteracao it_fac corresponde a uma combinacao de parametros, que tem o
#   # mesmo codigo de iteracao do output da simulacao. Com isso, eu transformo
#   # do numero na identificacao de qual o valor do parametro e se ele eh
#   # maximo ou minimo. Um mesmo maximo ou minimo pode aparecer varias vezes
#   # se ele foi combinado com outro maximo ou minimo.
#   # Depois eu junto a simulacao com essa identificacao.
# 
#   id_run <- factors_filt[it, "id"]
#   
#   factors_values <- read.csv(factors_filt$path_fac[it]) %>%
#     mutate(id = id_run,
#            it_fac = row_number()-1) %>%
#     gather(-id, -it_fac, key = "param", value = "value") %>%
#     group_by(param) %>%
#     mutate(flag = if_else(value == max(value), "max",
#                           if_else(value == min(value), "min", "none"))) %>%
#     ungroup() %>%
#     filter(flag != "none")
# 
#   outputs_filt <- outputs_mod %>%
#     filter(outputs_mod$id == id_run, grepl("paramsMask", path_fac)) %>%
#     mutate(it_fac = str_extract(path_out, "it[[:digit:]]+"),
#            it_fac = as.numeric(gsub("it", "", it_fac))) %>%
#     arrange(it_fac) %>%
#     select(-path_fac, -run, -n_samples, -n_splits, -sensor_type,
#            -cont, -config, -comment)
# 
#   for (it2 in 1:nrow(outputs_filt)){
# 
#     temp <- read.csv(outputs_filt$path_out[it2])
# 
#     temp_ <- temp %>%
#       mutate(das = row_number(),
#              it_fac = outputs_filt$it_[it2],
#              id = factors_filt$id[it]) %>%
#       left_join(outputs_filt[it2, ]) %>%
#       left_join(factors_values) %>%
#       select(-path_out)
# 
#     if (factors_filt$model[it] == "simple"){
# 
#       temp_ <- temp_ %>%
#         mutate(k = 0.58,
#                lai = log(1 - f_solar)/-k) %>%
#         rename(w = biomass,
#                wm = plant_yield) %>%
#         select(-f_solar, -k, -tt_sum)
# 
#     }
# 
#     id_full <- paste(it, it2, sep = "_")
#     all_results_plot_l[[id_full]] <- temp_
# 
#   }
# 
# }
# 
# all_curves_params <- rbindlist(all_results_plot_l, fill = TRUE)
# save(all_curves_params, file = "./tables/results_SA/all_curves_params.RData")

