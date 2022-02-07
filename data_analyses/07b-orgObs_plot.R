# Org observations for plots

# Set configurations ------------------------------------------------------
rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setenv(LANG = "En")

# Libraries ---------------------------------------------------------------

library(tidyverse)

# Load files --------------------------------------------------------------

obs_all <- read.csv("./data/observations/full_set_obs.csv")
obs_ids <- read.csv("./data/observations/monitoring/obs_exp_all_ids.csv") %>%
  mutate(hi = wm/w)

#plot(obs_ids$dat, obs_ids$hi)

# Load dictionary of measurement and observations
cycle_dates <- read.csv("./data/cycle_dates.csv")
codes_exp <- read.csv("./tables/codes_exp.csv")

# Process observations ----------------------------------------------------
obs_sd <- obs_all %>%
  mutate(das = dat,
         model = "obs") %>%
  select(cycle, das, city_exp, exp, contains("_sd")) %>%
  rename(w = w_sd,
         w_fm = w_fm_sd,
         wf = wf_sd,
         wf_fm = wf_fm_sd,
         wm = wm_sd,
         wm_fm = wm_fm_sd, 
         lai = lai_sd,
         n = n_sd,
         n_lat = n_lat_sd,
         lai_lat = lai_lat_sd,
         lai_abv = lai_abv_sd,
         height = height_sd) %>%
  gather(starts_with("w"), "n", starts_with("lai"),
         starts_with("n_"), "height",
         key = "variable", value = "measurement_sd") %>%
  filter(!is.na(measurement_sd))

obs <- obs_all %>%
  select(-contains("sd")) %>%
  mutate(das = dat,
         model = "obs") %>%
  gather(starts_with("w"), "n", starts_with("lai"),
         starts_with("n_"), "height",
         key = "variable", value = "measurement") %>%
  filter(!is.na(measurement)) %>%
  left_join(obs_sd)

obs_last <- obs_ids %>%
  group_by(cycle) %>%
  filter(dat == max(dat)) %>%
  ungroup() %>%
  select(cycle, id, dat, w, wf, wm, lai, n) %>%
  gather(starts_with("w"), "n", starts_with("lai"),
         starts_with("n_"),
         key = "variable", value = "measurement") %>%
  left_join(codes_exp) %>%
  filter(!is.na(node))

write.csv(obs, "./data/observations/monitoring/observations_proc.csv", 
          row.names=FALSE)
write.csv(obs_last, "./data/observations/monitoring/observations_last.csv", 
          row.names=FALSE)
