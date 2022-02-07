# Organizes outputs from python scripts and include 
# information regarding plants

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library("tidyverse")

lai_ids_all <- list.files('./data/observations/monitoring/lai/', 
                              pattern = '^raw_lai_scans_cycle.',
                              full.names = TRUE)



codes_exp <- read.csv("./tables/codes_exp.csv") %>%
  select(-id, -node, -city_exp) %>%
  distinct()
cycle_dates <- read.csv("./data/cycle_dates.csv")

# Proc --------------------------------------------------------------------
temp <- list()

for (filename in lai_ids_all){
  
  temp[[filename]] <- read.csv(filename)
  
}

lai_ids <- Reduce(rbind, temp)

lai_ids_mod <-  lai_ids %>%
  separate(filename, c("date", "id_plant", "count"), "_| ") %>%
  mutate(id_plant = gsub("id", "", id_plant)) %>%
  group_by(date, id_plant) %>%
  summarise(leaf_area = sum(area)) %>%
  ungroup() %>%
  # Initial values could not be determined in the same day, so the value
  # identified in the following morning was ascribed as initial
  mutate(date = if_else(date == "2021-03-10", "2021-03-09", date)) %>%
  left_join(cycle_dates) %>%
  left_join(codes_exp) %>%
  mutate(lai = leaf_area*ro) %>%
  rename(id = id_plant) %>%
  select(-ro)

tail(lai_ids_mod)

write.csv(lai_ids_mod, 
          file = './data/observations/monitoring/lai/lai_ids.csv', 
          row.names = FALSE)