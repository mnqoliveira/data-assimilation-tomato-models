# -*- coding: utf-8 -*-
"""
Created on Mon Jan 21 13:43:33 2019

@author: monique
"""
import numpy as np
import pandas as pd

import models.model_Simple as model
import auxiliary_functions.f_aux as aux


# %% Functions
def load_dataset(city, sensor, calib, treat, config_obs):

    all_data = aux.load_data(model="simple", weather=city, sensor=sensor,
                             experiment=treat, calib=calib,
                             config_obs=config_obs)

    # Eventual modifications in weather
    # For these implementation, weather refers only to the days in the cycle.
    # This pre-processing happens before running them and there is no
    # modification in the dataset except for removing eventual initial
    # state days (dat = 0).
    dataset_daily = all_data['weather']

    if "dat" not in dataset_daily.columns:
        dataset_daily["dat"] = dataset_daily["das"]
    dataset_daily = aux.convert_rad(dataset_daily, "MJg")

    all_data['weather'] = dataset_daily

    # Treat initial states
    states_names = [('lai', 'f_solar'), ('w', 'biomass'),
                    ('wm', 'plant_yield')]
    default_s0 = {'biomass': 1., 'dBiomass': 0., 'f_s_water': 0,
                  'I50B': all_data['params']['I50B'],
                  'f_solar': all_data['info']['in_solar'],
                  'tt_sum': 0., 'plant_yield': 0.5}

    i = all_data['info']
    s = all_data['states']

    if (s == {}):
        s = default_s0.copy()

        if (config_obs is not None):
            all_data['obs'].reset_index(drop=True, inplace=True)
            observations = all_data['obs']

            if (observations.shape[0] > 0):
                for st, ST in states_names:
                    if ((not (np.isnan(observations.loc[0, st]))) &
                        (observations.loc[0, "dat"] == 0)):
                        try:
                            temp = observations
                            s[ST] = temp.loc[0, st]
                        except:
                            pass

                # Treat maximum leaf area
                p = all_data['params']
                if (observations.shape[0] > 0):
                    if (not np.isnan(np.nanmax(observations.lai))):
                        # Convert lai into FSolar
                        observations.lai = 1-np.exp(-p['K']*observations.lai)
                        if np.isnan(i["f_solarMax"]):
                            temp = np.nanmax(observations.lai)
                            p["f_solarMax"] = temp

                #all_data['obs'] = observations.set_index('dat').copy()
                all_data['obs'] = observations.copy()
                all_data['params'] = p

    else:
        s_mod = default_s0.copy()
        for st, ST in states_names:
            s_mod[ST] = s[st]
        s = s_mod

    all_data['states'] = s

    all_data['impacts'] = {'temp': 0, 'heat': 0,
                           'CO2': 0, 'drought': 0}

    # Treat info harvest
    info = all_data['info']
    #info_mod = aux.info_dates(info)

    return all_data


def run_simul(info, params, states, impacts, weather):

    state_all = np.array(list(states.values()))[np.newaxis, :]
    #weather = weather.set_index('dat')
    all_dat = [x for x in weather.dat.unique().tolist() if x > 0]

    for dat in all_dat:
        model.simple(dt=1, dat=dat, info=info,
                     weather=weather, params=params,
                     states=states, impacts=impacts)
        simple_it = list(states.values())
        state_all = np.vstack((state_all,
                               np.array(simple_it)[np.newaxis, :]))

    return state_all


def main():
    weather_loc = ['cps']
    sensor_type = ['A']
    calibrations = ['cpsopt']
    experiments_int = [7, 8]

    weather_loc = ['gnv']
    sensor_type = ["A"]
    experiments_int = [4, 5, 6]
    calibrations = ['gnvopt']

    experiments = [str(s) for s in experiments_int]

    city = weather_loc[0]
    calib = calibrations[0]
    treat = experiments[0]
    config_obs = "summ"

    comb = aux.expand_grid({'city': weather_loc,
                            'sensor': sensor_type,
                            'calib': calibrations,
                            'treat': experiments})

    it_exp = 0

    for it_exp in range(comb.shape[0]):
        city = comb.city[it_exp]
        calib = comb.calib[it_exp]
        treat = comb.treat[it_exp]
        sensor = comb.sensor[it_exp]

        dataset = load_dataset(city=city, sensor=sensor,
                            calib=calib, treat=treat, config_obs=config_obs)

        weather = dataset['weather'].copy()
        params = dataset['params'].copy()
        states = dataset['states'].copy()
        info = dataset['info'].copy()
        impacts = dataset['impacts'].copy()

        state = run_simul(info, params, states, impacts, weather)
        state = pd.DataFrame(state.reshape(-1, 7),
                             columns=['biomass', 'dBiomass', 'f_s_water',
                                      'I50B', 'f_solar', 'tt_sum',
                                      'plant_yield'])
        print(state)

        aux.save_output("simple", "simul", state, city, calib, treat, 1)

        if config_obs is not None:
            var_names = ['lai', 'w', 'wm']
            st_names = [('lai', 'f_solar'), ('w', 'biomass'),
                            ('wm', 'plant_yield')]
            state_ = (state
                      .rename(columns={v: k for k, v in
                                       dict(st_names).items()}))
            outputs_ = (state_.loc[:, var_names]
                        .reset_index()
                        .melt(id_vars=['index'])
                        .rename(columns={"index": "dat"}))

            observations = aux.get_obs(dataset, var_names, st_names)

            error = aux.calcError(observations, outputs_, var_names)
            aux.save_output("simple", "error_simul",
                            error, city, calib, treat, 1)

#main()
