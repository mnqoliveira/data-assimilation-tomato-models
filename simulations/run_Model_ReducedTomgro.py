# -*- coding: utf-8 -*-
"""
Created on Mon Jan 21 13:43:33 2019

@author: monique
"""
import numpy as np
import pandas as pd

import models.model_ReducedTomgro as model
import auxiliary_functions.f_aux as aux

#np.warnings.filterwarnings('error', category=np.VisibleDeprecationWarning)

# %% Load dataset, parameters and inputs


def load_dataset(city, sensor, calib, treat, config_obs):

    all_data = aux.load_data(model="tomgro", weather=city, sensor=sensor,
                             experiment=treat, calib=calib,
                             config_obs=config_obs)

    # Eventual modifications in weather
    # For these implementation, weather refers only to the days in the cycle.
    # This pre-processing happens before running them and there is no
    # modification in the dataset except for removing eventual initial
    # state days (dat = 0)
    dataset_hourly = all_data['weather']

    if "dat" not in dataset_hourly.columns:
        dataset_hourly["dat"] = dataset_hourly["dap"]
    dataset_hourly = aux.convert_rad(dataset_hourly,
                                     "mmolPAR")
    all_data['weather'] = dataset_hourly

    # Treat initial states
    states_names = ['n', 'lai', 'w', 'wf', 'wm']
    default_s0 = {'n': np.array(4.0, ndmin=1),
                  'lai': np.array(0.014, ndmin=1),
                  'w': np.array(0.09, ndmin=1),
                  'wf': np.array(0.0, ndmin=1),
                  'wm': np.array(0.0, ndmin=1)}

    s = all_data['states']
    if (s == {}):
        s = default_s0.copy()
    else:
        for st in states_names:
            s[st] = np.array(s[st], ndmin=1)

    if (config_obs is not None):
        if ("artif" not in config_obs):
            all_data['obs'].reset_index(drop=True, inplace=True)
            observations = all_data['obs'].copy()

            if (s == {}):
                s = default_s0.copy()
                for st in states_names:
                    if ((not (np.isnan(observations.loc[0, st]))) &
                        (observations.loc[0, "dat"] == 0)):
                        try:
                            temp = observations
                            s[st] = np.array(temp.loc[0, st], ndmin=1)
                        except:
                            pass


            #all_data['obs'] = observations.set_index('dat')
            all_data['obs'] = observations

            # Treat maximum leaf area
            i = all_data['info']
            if (not np.isnan(np.nanmax(observations.lai))
                and np.isnan(i["LAI_max"])):
                i["LAI_max"] = np.nanmax(observations.lai)
            #print('lai', i["LAI_max"], np.nanmax(observations.lai))
            all_data['info'] = i

    all_data['states'] = s

    return all_data


def run_simul(info, params, rates, states, weather):

    state_it = np.array(list(states.values()))[np.newaxis, :]
    all_dat = [x for x in weather.dat.unique().tolist() if x > 0]
    pg = list()
    rm = list()
    dw = list()

    weather_l = list(weather.groupby("dat"))

    for it, dat in enumerate(all_dat):
        weather_d = weather_l[it][1]
        model.tomgro(dat, info, params, rates, states, weather_d)

        if len(list(states.values())) == 5:
            current_tomgro = list(states.values())
            # aux.print_state(current_tomgro)
            state_it = np.vstack((state_it,
                                  np.array(current_tomgro)[np.newaxis, :]))
            dw.append(rates['dw_dt'].item())
            pg.append(params['photo'])
            rm.append(params['resp'])

    return (state_it, pg, rm, dw)

    # if len(list(states.values())) == 5:

    # else:
    #     return None


# %%  Run model
def main():
    weather_loc = ['cps']
    sensor_type = ['A']
    experiments_int = [8]
    calibrations = ['cpsman']
    experiments = [str(s) for s in experiments_int]
    config_obs = "summ"

    city = weather_loc[0]
    calib = calibrations[0]
    treat = experiments[0]

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

        dataset = load_dataset(city, sensor, calib, treat, config_obs)

        weather = dataset['weather'].copy()
        params = dataset['params'].copy()
        rates = {'dn_dt': np.array(0.0, ndmin=1),
                 'dlai_dt': np.array(0.0, ndmin=1),
                 'dw_dt': np.array(0.0, ndmin=1),
                 'dwf_dt': np.array(0.0, ndmin=1),
                 'dwm_dt': np.array(0.0, ndmin=1)}
        states = dataset['states'].copy()
        info = dataset['info'].copy()

        state_tup = run_simul(info, params, rates, states, weather)
        state = state_tup[0]

        state_all = pd.DataFrame(state.reshape(-1, 5),
                                 columns=['n', 'lai', 'w', 'wf', 'wm'])
        state_all['Pg'] = [0] + state_tup[1]
        state_all['Rm'] = [0] + state_tup[2]
        state_all['dw'] = [0] + state_tup[3]
        state_all['das'] = list(range(state_all.shape[0]))

        aux.save_output("tomgro", "simul", state_all, city, calib, treat,
                        sensor)

        if config_obs is not None:
            var_names = ['n', 'lai', 'w', 'wf', 'wm']
            outputs_ = (state_all.loc[:, var_names]
                        .reset_index()
                        .melt(id_vars=['index'])
                        .rename(columns={"index": "dat"}))

            observations = aux.get_obs(dataset, var_names, [])

            error = aux.calcError(observations, outputs_, var_names)
            aux.save_output("tomgro", "error_simul",
                            error, city, calib, treat, sensor)

    return state_all

#main()
