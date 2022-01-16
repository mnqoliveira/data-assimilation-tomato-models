# Organizes and joins data from destructive analyses including mass, nodes 
# and LAI.

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------
library(tidyverse)
library(zoo)

# Load data ---------------------------------------------------------------
obsData <- read.csv("../data/observations/monitoring/analysis.csv")
nodes_ids_1 <- read.csv("../data/observations/monitoring/nodes/n_calib_ids_ciclo02.csv")
nodes_ids_2 <- read.csv("../data/observations/monitoring/nodes/n_calib_ids.csv")
lai_ids <- read.csv("../data/observations/monitoring/lai/lai_ids.csv")

cycle_dates <- read.csv("../data/cycle_dates.csv")

# Initial processing ------------------------------------------------------
# DM
codes_exp <- read.csv("../tables/codes_exp.csv") %>%
  distinct()

codes_exp_mod <- read.csv("../tables/codes_exp.csv") %>%
  select(-id, -node, -city_exp) %>%
  distinct()

# LAI ---------------------------------------------------------------------
lai <- lai_ids %>%
  left_join(cycle_dates)

lai_sum <- lai %>%
  group_by(date, dat, cycle) %>%
  summarise(lai_avg = mean(lai),
            lai_sd = sd(lai)) %>%
  rename(lai = lai_avg)

write.csv(lai_sum, 
          file = '../data/observations/monitoring/lai/lai_summ.csv', 
          row.names = FALSE)

# Nodes -------------------------------------------------------------------
n_ids <- nodes_ids_1 %>%
  bind_rows(nodes_ids_2) 

nodes <- n_ids %>%
  left_join(cycle_dates) %>%
  # Assuming growth would have been stopped after 80 days for all cycles.
  # Includes removing monitored plants as they also had growth interrupted.
  filter(dat <= 80)

nodes_sum <- nodes %>%
  group_by(cycle, date, dat) %>%
  summarise(n_sd = round(sd(count),0),
            n = round(mean(count),0)) 

# Biomass datasets --------------------------------------------------------
# Biomass should account for all parts all the time, because zero fruits is
# also an observation, even if it is not recorded
parts <- c("fruit_green", "fruit_mature")

# Some plants may have been harvested without being samples for 
# destructive analysis. I want to exclude those that do not show 
# leaves (or all) as it means they were not fully used
samples <- obsData %>%
  left_join(cycle_dates) %>%
  mutate(flag_leave = if_else(part == "all" | part == "leaves", 1, 0)) %>%
  group_by(cycle, id) %>%
  mutate(flag_remove = sum(flag_leave)) %>%
  ungroup() %>%
  filter(flag_remove > 0) %>%
  # select(-flag_leave, -flag_remove) %>%
  # mutate(flag_max = if_else(date == max(dat), 1, 0)) %>%
  # filter(flag_max == 1)
  select(cycle, id) %>%
  distinct(.) %>%
  mutate(id = as.integer(id))

# Dry mass data -----------------------------------------------------------
# Biomass should account for all parts all the time, because zero fruits is
# also an observation, even if it is not recorded
parts <- c("fruit_green", "fruit_mature")

obsPlant <- obsData %>%
  left_join(cycle_dates) %>% 
  mutate(id = as.integer(id)) %>%
  right_join(samples)

dates_ids <- paste(obsPlant$date, obsPlant$id, sep = "_")

part_fruit0 <- expand.grid(part = parts, date_id = dates_ids, 
                           stringsAsFactors = FALSE) %>%
  filter(!paste(part, date_id, sep = "_") %in% 
           paste(obsData$part, obsData$date, obsData$id, sep = "_")) %>%
  separate(date_id, into = c("date", "id"), sep = "_") %>%
  distinct() %>%
  mutate(id = as.integer(id))

obsData_mod <- obsData %>%
  mutate(id = as.integer(id)) %>%
  full_join(part_fruit0) %>%
  left_join(cycle_dates) %>%
  select(-wm, -dm, -ratio) %>%
  mutate(dm_zero = if_else(is.na(dm_zero), 0, dm_zero),
         wm_zero = if_else(is.na(wm_zero), 0, wm_zero),
         zero = if_else(is.na(zero), 0, zero)) %>%
  # Drying period was not enough, weighted samples are bad
  mutate(dm_zero = if_else(date == "2019-09-02" | date == "2019-09-09",
                      -99., dm_zero),
         dm_zero = na_if(dm_zero, -99.)) %>%
  # # Lost data referring to the full wet mass, comment if imputed
  # mutate(wm_zero = if_else(date == "2021-01-23" & as.integer(id) == 15,
  #                          -99., wm_zero),
  #        wm_zero = na_if(wm_zero, -99.)) %>%
  # mutate(wm_zero = if_else(date == "2021-01-29" & as.integer(id) == 17,
  #                          -99., wm_zero),
  #        wm_zero = na_if(wm_zero, -99.)) %>%
  arrange(date, id, part) %>%
  # Some water is lost in the paper bag used during drying. The average
  # of the sampled bags is used to fix the values measured in dry mass.
  # On cycle 2, this was not measured, so the average from other cycles 
  # was included as referring to cycle two.
  # Since there are different paper bags, different zero values are ascribed. 
  mutate(zero_type = if_else(zero < 5, 1, 
                             if_else(zero > 5 & zero < 7, 2, 
                                     if_else(zero > 10, 3, 4))),
         zero_mod = if_else(part == "_zero", wm_zero - dm_zero, -99),
         zero_mod = na_if(zero_mod, -99)) %>%
  group_by(zero_type) %>%
  mutate(zero_mod = mean(zero_mod, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(zero_mod = if_else(is.nan(zero_mod),
                            min(zero_mod, na.rm = TRUE),
                            zero_mod),
         dm_zero = if_else(dm_zero != 0, dm_zero + zero_mod, 0)) %>%
  select(-zero_mod, -zero_type) %>%
  filter(!is.na(as.integer(id))) %>%
  mutate(id = as.integer(id)) %>%
  arrange(cycle, dat, id) %>%
  right_join(samples)

ratio_dm_parts  <- obsData_mod %>%
  mutate(dm = dm_zero - zero,
         wm = wm_zero - zero) %>%
  group_by(date, dat, id, part) %>%
  summarise(dm_sum = sum(dm),
            wm_sum = sum(wm),
            ratio_dm = dm_sum/wm_sum) %>%
  ungroup() %>%
  filter(!is.nan(ratio_dm))

write.csv(ratio_dm_parts, 
          file = "../data/observations/monitoring/dry_mass_aboveground/dm_parts_ids.csv", 
          row.names = FALSE)

# Aboveground biomass: sum all the parts from the same plant
# Because some plants were harvested before the end of the cycle,
# their ids need to be considered only when they were actually destroyed,
# instead of throughout the growth.
obs_above <- obsData_mod %>%
  mutate(dm = dm_zero - zero,
         fm = wm_zero - zero) %>%
  group_by(cycle, id) %>%
  summarise(date = as.character(max(as.Date(date))),
            dat = max(dat),
            w = sum(dm),
            w_fm = sum(fm)) %>%
  ungroup() %>%
  arrange(date, id) %>%
  left_join(codes_exp_mod, by = "cycle") %>%
  mutate(w_plant = w,
         w_plant_fm = w_fm,
         w = w*ro,
         w_fm = w_fm*ro) 

# Ratio parts
mass_parts <- obsData_mod %>%
  mutate(dm_part = dm_zero - zero) %>%
  select(-zero, -wm_zero, -dm_zero) %>%
  group_by(cycle, id, part) %>%
  summarise(dm_part = sum(dm_part)) %>%
  ungroup()

ratio_parts <- obs_above %>%
  full_join(mass_parts) %>%
  mutate(ratio = dm_part*100/w_plant) %>%
  select(-w_fm, -ro) %>%
  filter(ratio != 0)

write.csv(ratio_parts, 
          file = "../data/observations/monitoring/dry_mass_aboveground/parts_ids.csv", 
          row.names = FALSE)

# Fruit mass: should include mature and non-mature fruits
obs_fruit <- obsData_mod %>%
  mutate(dm = dm_zero - zero,
         fm = wm_zero - zero,
         fruit = if_else(part == "fruit_green" | part == "fruit_mature", 
                         "yes", "no")) %>%
  group_by(cycle, id, fruit) %>%
  summarise(date = as.character(max(as.Date(date))),
            dat = max(dat),
            wf = sum(dm),
            wf_fm = sum(fm)) %>%
  filter(fruit == "yes") %>%
  select(-fruit) %>%
  mutate(ratio_f = wf/wf_fm) %>%
  ungroup() %>%
  arrange(date, id) %>%
  left_join(codes_exp_mod, by = "cycle") %>%
  mutate(wf_plant = wf,
         wf_plant_fm = wf_fm,
         wf = wf*ro,
         wf_fm = wf_fm*ro)

# Mature fruit biomass should account for fruits that were harvested before
# the exact date of plant analysis. Even though this mass could be cummulative 
# added as observations, since as zero is an observation, so should be the 
# partial harvests, they do not account for all mature fruits, as some are not
# immediately removed because the whole truss has not yet completely matured.

obs_m_fruit <- obsData_mod %>%
  filter(part == "fruit_mature") %>%
  mutate(dm = dm_zero - zero,
         fm = wm_zero - zero) %>%
  group_by(cycle, id) %>%
  summarise(date = as.character(max(as.Date(date))),
            dat = max(dat),
            wm = sum(dm),
            wm_fm = sum(fm)) %>%
  mutate(ratio_m_f = wm/wm_fm) %>%
  ungroup() %>%
  arrange(date, id) %>%
  left_join(codes_exp_mod, by = "cycle") %>%
  mutate(wm_plant = wm,
         wm_plant_fm = wm_fm,
         wm = wm*ro,
         wm_fm = wm_fm*ro)

write.csv(obs_above, 
          file = "../data/observations/monitoring/dry_mass_aboveground/w_ids.csv", 
          row.names = FALSE)
write.csv(obs_fruit, 
          file = "../data/observations/monitoring/dry_mass_fruit/wf_ids.csv", 
          row.names = FALSE)
write.csv(obs_m_fruit, 
          file = "../data/observations/monitoring/dry_mass_mature_fruit/wm_ids.csv", 
          row.names = FALSE)

# All ids
obs_ids <- obs_above %>%
  full_join(obs_fruit) %>%
  full_join(obs_m_fruit) %>%
  select(-starts_with("ratio")) %>%
  full_join(lai) %>%
  full_join(nodes) %>%
  rename(n = count) %>%
  mutate(city = "cps")

write.csv(obs_ids, 
          file = "../data/observations/monitoring/obs_exp_all_ids.csv", 
          row.names = FALSE)  


maps <- data.frame(id = as.character(c(1:72)),
                   line = rep(c("a", "b", "c", "d"), each = 18),
                   pos = rep(c(1:18), 4))

obs_map <- obs_ids %>%
  mutate(id = as.character(as.integer(id))) %>%
  left_join(maps)

write.csv(obs_map, 
          file = "../data/observations/monitoring/obs_exp_all_map.csv", 
          row.names = FALSE)  

# Harvests per plant
# As I need info for monitored and non-monitored plants, I will not filter
# some of them. As some of them were marked in the harvest tab even if they
# were also sampled in the same day, this info will be modified in the
# type variable.
harvests <- obsData_mod %>%
  group_by(cycle, id) %>%
  mutate(type = if_else(dat == max(dat), "dest", type)) %>%
  filter(type == "harv", part == "fruit_mature") %>%
  mutate(dm = dm_zero - zero,
         fm = wm_zero - zero) %>%
  arrange(date, id) %>%
  select(-contains("zero"), -part)

write.csv(harvests, 
          file = "../data/observations/monitoring/harvests.csv", 
          row.names = FALSE)

# Summary: averages and standard deviations
summ_above <- obs_above %>%
  group_by(cycle, dat, date) %>%
  summarise(w_sd = sd(w, na.rm = TRUE),
            w = mean(w, na.rm = TRUE),
            w_fm_sd = sd(w_fm, na.rm = TRUE),
            w_fm = mean(w_fm, na.rm = TRUE))

summ_fruit <- obs_fruit %>%
  group_by(cycle, dat, date) %>%
  summarise(wf_sd = sd(wf, na.rm = TRUE),
            wf = mean(wf, na.rm = TRUE),
            wf_fm_sd = sd(wf_fm, na.rm = TRUE),
            wf_fm = mean(wf_fm, na.rm = TRUE))

summ_m_fruit <- obs_m_fruit %>%
  group_by(cycle, dat, date) %>%
  summarise(wm_sd = sd(wm, na.rm = TRUE),
            wm = mean(wm, na.rm = TRUE),
            wm_fm_sd = sd(wm_fm, na.rm = TRUE),
            wm_fm = mean(wm_fm, na.rm = TRUE))

summ_exp <- summ_above %>%
  left_join(summ_fruit) %>%
  left_join(summ_m_fruit) %>%
  arrange(cycle, date)

# Join all ----------------------------------------------------------------
summ_exp_all <- summ_exp %>%
  full_join(lai_sum) %>%
  full_join(nodes_sum) %>%
  full_join(codes_exp) %>%
  arrange(cycle, date)

write.csv(summ_exp_all, 
          file = "../data/observations/monitoring/experiments.csv", 
          row.names = FALSE)
