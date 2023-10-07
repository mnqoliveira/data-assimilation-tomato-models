# -*- coding: utf-8 -*-
"""
Created on Mon Jan 21 13:43:33 2019

@author: monique
"""
import numpy as np
import pandas as pd

import models.model_Vanthoor as model
import auxiliary_functions.f_aux as aux


# %% Initial values
def load_dataset(city, sensor, calib, treat, config_obs):

    all_data = aux.load_data(model="vanthoor", weather=city,
                             sensor=sensor, experiment=treat,
                             calib=calib, config_obs=config_obs)

    # Eventual modifications in weather
    # For these implementation, weather refers only to the days in the cycle.
    # This pre-processing happens before running them and there is no
    # modification in the dataset except for removing eventual initial
    # state days (dat = 0)
    dataset_hourly = all_data['weather']
    if "dat" not in dataset_hourly.columns:
        dataset_hourly["dat"] = dataset_hourly["dap"]
    dataset_hourly = aux.convert_rad(dataset_hourly, "mmolPAR")
    all_data["weather"] = dataset_hourly

    params = all_data['params']
    params["nDev"] = np.round(params["nDev"], 0).astype('int')

    rates = {
        'CLeaf': 0.,
        'CStem': 0.,
        'CFruit': np.zeros(params["nDev"]),
        'DMHar': 0.,
        'CBuf': 0.,
        'NFruit': np.zeros(params["nDev"]),
        'TSCan': 0.,
        'TCan24': 0.,

        "MCBufFruit": 0.,
        "MCBufFruit_i": np.zeros(params["nDev"]),
        "MCFruitFruit": np.zeros(params["nDev"]),
        "MNFruitFruit": np.zeros(params["nDev"]),

        'rgr': []
    }

    default_s0 = {
        # GreenLight: start with 3.12 plants/m2,
        # assume they are each 0.1 g DM.
        'CLeaf': 0.7 * 0.1 * 1000 * all_data["info"]["nPlants"],
        'CStem': 0.25 * 0.1 * 1000 * all_data["info"]["nPlants"],
        'CFruit': np.zeros(params["nDev"]),
        'DMHar': 0.,
        'CBuf': 0.,  # GreenLight
        'NFruit': np.zeros(params["nDev"]),
        # Considerado igual a TAir qdo nao eh medido
        'TCan': 25,
        'TSCan': params['ttsum'],
        'TCan24': 25
    }

    default_s0['Total'] = [default_s0['CLeaf'] + default_s0['CStem'] +
                           default_s0['CFruit'].sum()]

    states = all_data['states']

    if (states == {}):
        states = default_s0.copy()

        if (config_obs is not None):

            all_data['obs'].reset_index(drop=True, inplace=True)
            observations = all_data['obs'].copy()

            if ((not (np.isnan(observations.loc[0, "w"]))) &
                    (observations.loc[0, "dat"] == 0)):
                    try:
                        temp = observations.loc[0, "w"]
                        states['CLeaf'] = 0.7 * temp * 1000
                        states['CStem'] = 0.25 * temp * 1000
                        states['Total'] = [states['CLeaf'] + states['CStem'] +
                                           states['CFruit'].sum()]
                    except:
                        pass

    all_data['states'] = states
    all_data['rates'] = rates
    all_data['params'] = params

    return all_data


def run_simul(info, params, rates, states, weather):

    all_it = weather.index.tolist()
    day_end = weather.loc[weather.hour == 23, ].index.tolist()

    states['TSCan'] = params['ttsum']
    states_aux = states.copy()

    rem_states = ['CLeaf', 'CStem', 'CFruit', 'DMHar']
    for x in list(states_aux.keys()):
        if x not in rem_states:
            states_aux.pop(x)

    states_aux['CFruit'] = states_aux['CFruit'].sum()

    state_it = np.array(list(states_aux.values()))[np.newaxis, :]
    state_it = state_it[:, :4]

    pg = list()
    pg_24 = list()
    rm = list()
    rm_24 = list()
    dw = list()
    dw_24 = list()

    for it in all_it:
        # 398 ultimo antes de fruto em gnv
        # 387 ultimo antes de fruto em cps
        model.vanthoor(it=it, weather=weather, info=info, params=params,
                       rates=rates, states=states)

        pg.append(params['photo'])
        rm.append(rates['resp'])
        dw.append(rates['dw'])

        for x in list(states_aux.keys()):
            if x == "CFruit":
                states_aux[x] = states[x].sum()
            else:
                states_aux[x] = states[x]

        if it in day_end:
            current_vanthoor = list(states_aux.values())
            temp = np.array(current_vanthoor)[np.newaxis, :]
            state_it = np.vstack((state_it, temp[:, :4]))

            # print("DAT", round(it/24))
            pg_24.append(sum(pg[-24:]))
            rm_24.append(sum(rm[-24:]))
            dw_24.append(sum(dw[-24:]))

    # Format output
    state = state_it/1000
    # Replace stem and roots with 10% of all biomass, to correspond to
    # aboveground biomass
    state[:, 1] = (state[:, 0] + state[:, 1] +
                       state[:, 2] + state[:, 3])*0.9
    # Replace CLeaf with LAI
    state[:, 0] = params['sla'] * 1000 * state[:, 0]
    # Replace Wf to account for all fruits, even those harvested
    state[:, 2] = state[:, 2] + state[:, 3]

    return (state, pg_24, rm_24, dw_24)


def main():
    weather_loc = ['cps']
    sensor_type = ["A"]
    experiments_int = [1, 2, 3, 4, 5, 6, 7, 8]
    calibrations = ['cps4']
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
        states = dataset['states'].copy()
        rates = dataset['rates'].copy()
        info = dataset['info'].copy()
        # info['dt'] = 1

        # Run
        state_tup = run_simul(info, params, rates, states, weather)
        state_all = state_tup[0]

        state_all = pd.DataFrame(state_all,
                                 columns=['lai', 'w', 'wf', 'wm'])
        state_all['Pg'] = [0] + state_tup[1]
        state_all['Rm'] = [0] + state_tup[2]
        state_all['dw'] = [0] + state_tup[3]
        state_all['das'] = list(range(state_all.shape[0]))

        aux.save_output("vanthoor", "simul", state_all,
                        city, calib, treat, sensor)

        if config_obs is not None:
            var_names = ['lai', 'w', 'wf', 'wm']
            outputs_ = (state_all.loc[:, var_names]
                        .reset_index()
                        .melt(id_vars=['index'])
                        .rename(columns={"index": "dat"}))

            observations = aux.get_obs(dataset, var_names, [])

            error = aux.calcError(observations, outputs_, var_names)
            aux.save_output("vanthoor", "error_simul",
                            error, city, calib, treat, sensor)

        # obs = dataset['obs']
        # print(obs.loc[~np.isnan(obs.wm), ["lai", "w", "wf", "wm"]])

    return state_all

pd.set_option("display.max_columns", 8)
#main()
