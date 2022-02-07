# Org assimilation outputs

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("tidyverse")
library("data.table")

# Load data ---------------------------------------------------------------

# Carregar todos os arquivos e incluir numero do experimento e todos os 
# codigos relevantes para identificacao em colunas adicionais
# Salvar essa info, pq ela que vamos usar para filtrar situacoes para o plot
# Incluir o id do numero da linha pq ele vai voltar na identificacao da pasta
# em que o arquivo foi salvo

# Load assimilation outputs
all_files_path <- list.files("./tables/results_DA/", full.names = T, 
                             pattern = ".csv", recursive = T)

configs <- read.csv("./tables/runs_Filter.csv") %>% 
  rbind(read.csv("./tables/runs_Filter2.csv")) %>%
  transmute(across(everything(), as.character)) %>%
  mutate(id = str_pad(id, 4, "left", "0"),
         exp = paste0("n", str_pad(exp, 2, "left", "0")))

# Process assim outputs ---------------------------------------------------

outputs_mod <- data.frame(all_files_path) %>%
  rename(path = all_files_path) %>%
  filter(!grepl("sigmas", path), !grepl("aux_files", path)) %>%
  mutate(id = str_extract(path, "[:digit:]{4}"),
         var_out = if_else(grepl("allStates", path), "allStates",
                           if_else(grepl("updState", path), 
                                   "updState", "errors"))) %>%
  left_join(configs)

write.csv(outputs_mod, "./tables/results_DA/aux_files/assim_exp.csv", 
          row.names=FALSE)

ensembles <- data.frame(all_files_path) %>%
  rename(path = all_files_path) %>%
  filter(grepl("sigmas", path)) %>%
  mutate(id = str_extract(path, "[:digit:]{4}"),
         var_out = if_else(grepl("enkf", path), "ensembles", "sigmas")) %>%
  left_join(configs)
  

# Assimilated vs non assimilated - all states -----------------------------

allStates <- outputs_mod %>%
  filter(var_out == "allStates")

all_results <- list()

for (it in 1:nrow(allStates)) {
  
  upd <- read.csv(allStates$path[it]) %>%
    rename_all(tolower) %>%
    gather(starts_with("w"), "n", starts_with("lai"),
           key = "variable", value = "measurement") %>%
    mutate(id = allStates$id[it],
           rep=str_sub(allStates$path[it], -8, -5))
  
  all_results[[it]] <- upd  
  
}

all_assim <- rbindlist(all_results)

save(all_assim, file="./tables/results_DA/aux_files/all_states.RData")

# Outputs: states updated -------------------------------------------------

updState <- outputs_mod %>%
  filter(var_out == "updState") 

all_results <- list()

for (it in 1:nrow(updState)) {

  upd <- read.csv(updState$path[it])  %>%
    mutate(across(everything(),
                  str_replace_all,
                  pattern="\\[|\\]", replacement=""),
           rep=str_sub(updState$path[it], -8, -5))
  
  upd_mod <- upd %>%
    #select(-starts_with("P"), -starts_with("R")) %>%
    gather(contains("est"), contains("upd"), "P_s", "P_m", "R",
           "observations", "Gain", "Resid", "Cov_pred",
           key = "variable", value = "measurement") %>%
    mutate(unit = if_else((variable == "est_s" | variable == "upd_s"), 
                          "state", "meas")) %>%
    mutate(id = updState$id[it])
  
  
  all_results[[it]] <- upd_mod  
  
}

upd_assim <- rbindlist(all_results)

save(upd_assim, file="./tables/results_DA/aux_files/upd_states.RData") 


# Errors ------------------------------------------------------------------

errors <- outputs_mod %>%
  filter(var_out == "errors")

all_results <- list()

for (it in 1:nrow(errors)) {
  
  temp <- read.csv(errors$path[it]) %>%
    rename_all(tolower) %>%
    mutate(id = as.integer(errors$id[it]),
           config = errors$config[it],
           rep=str_sub(errors$path[it], -8, -5))
  
  all_results[[it]] <- temp  
  
}

all_err <- do.call(bind_rows, all_results) %>%
	arrange(id, dat, variable) %>%
      	mutate(error = obs - pred,
	       abs_error = abs(error),
	       sc_rel_error = error/max_obs)

save(all_err, file="./tables/results_DA/aux_files/all_errors.RData")

# Ensembles ---------------------------------------------------------------

ensembles_mod <- ensembles %>%
  mutate(config_ = as.numeric(config)) %>%
  filter(config_ == 4 | config_ >= 8, config_ <= 10) %>%
  select(-config_)

all_results <- list()
#for (it in 1:nrow(ensembles_mod)) {
#
#  upd <- read.csv(ensembles_mod$path[it]) %>%
#    rename_all(tolower) %>%
#    gather(starts_with("x"),
#           key = "variable", value = "measurement") %>%
#    mutate(id = ensembles_mod$id[it],
#           rep=str_sub(ensembles_mod$path[it], -8, -4))
#
#
#  all_results[[it]] <- upd
#
#}
#
#all_assim <- Reduce(bind_rows, all_results)  %>%
#  arrange(id, das)
#
#write.csv(all_assim, "./tables/results_DA/aux_files/all_ensembles.csv",
#          row.names=FALSE)

rm(list = ls())
