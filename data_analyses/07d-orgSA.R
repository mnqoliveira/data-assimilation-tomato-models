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
