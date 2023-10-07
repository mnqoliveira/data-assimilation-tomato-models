# -*- coding: utf-8 -*-
"""
Created on Sun Feb 28 08:44:36 2021

@author: Monique
"""

import numpy as np
import pandas as pd

import simulations.run_ModelSimple as simple
import simulations.run_ModelReducedTomgro as tomgro
import simulations.run_ModelVanthoor as vanthoor
#import simulations.run_ModelHortsyst as hortsyst

import auxiliary_functions.f_aux as aux
import scipy.optimize

import json
from datetime import datetime
from joblib import Parallel, delayed
#import winsound


# %%
def chooseModel(model):
    """
    Returns arguments given the chosen model.

    Parameters
    ----------
    model : string
        Which model is being calibrated. Current options:
            'tomgro' and 'simple'

    Returns
    -------
    load_dataset : Function
        Function that properly loads data for running model
    f_run : Function
        Function that runs the model
    states : list
        Which states are calculated
    states_comp : list
        Correspondence between calculated states and observation names

    """

    if model == "tomgro":
        load_dataset = tomgro.load_dataset
        f_run = tomgro.run_simul
        states = ['n', 'lai', 'w', 'wf', 'wm']
        states_comp = []

    elif model == "simple":
        load_dataset = simple.load_dataset
        f_run = simple.run_simul
        states = ['biomass', 'dBiomass', 'f_s_water',
                  'I50B', 'f_solar', 'tt_sum',
                  'plant_yield']
        states_comp = [('lai', 'f_solar'), ('w', 'biomass'),
                       ('wm', 'plant_yield')]

    elif model == "vanthoor":
        load_dataset = vanthoor.load_dataset
        f_run = vanthoor.run_simul
        states = ['lai', 'w', 'wf', 'wm']
        states_comp = []

    elif model == "hortsyst":
        #load_dataset = hortsyst.load_dataset
        #f_run = hortsyst.run_simul
        states = ['w', 'ttsum', 'n_up', 'lai', 'et_c']
        states_comp = []

    return load_dataset, f_run, states, states_comp


def runModel(dataset, model, var_names):

    _, f_run, _, _ = chooseModel(model)

    info = dataset['info']
    params = dataset['params']
    states_0 = dataset['states']
    weather = dataset['weather']

    if model == "simple":
        impacts = dataset['impacts'].copy()
        states = states_0.copy()
        weather = aux.convert_rad(weather, "MJg")

        # Simple simulations
        state_mod = f_run(info, params, states, impacts, weather)

    elif model == "tomgro":

        rates = {'dn_dt': 0.0, 'dlai_dt': 0.0, 'dw_dt': 0.0,
                 'dwf_dt': 0.0, 'dwm_dt': 0.0}
        states = states_0.copy()
        weather = aux.convert_rad(weather, "mmolPAR")

        # Tomgro simulations
        state_tup = f_run(info, params, rates, states, weather)
        state = state_tup[0]
        state_mod = state.reshape((-1, 5))

    # elif model == "hortsyst":

    #     rates = {}
    #     states = states_0.copy()
    #     weather = aux.convert_rad(weather, "mmolPAR")

    #     # Hortsyst simulations # XXX
    #     state_tup = f_run(info, params, rates, states, weather)
    #     state = state_tup[0]
    #     state_mod = state.reshape((-1, 5))

    elif model == "vanthoor":

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

        states = states_0.copy()
        states['CFruit'] = np.zeros(params["nDev"])
        states['NFruit'] = np.zeros(params["nDev"])

        weather = aux.convert_rad(weather, "mmolPAR")

        # Vanthoor simulations
        state_tup = f_run(info, params, rates, states, weather)
        state = state_tup[0]
        state_mod = state.reshape((-1, 4))

    return state_mod


def loop_exps(params_list, params_names, var_names, model,
              weather_loc, sensor_type, calibration, experiments, config_obs):

    load_dataset, _, states_names, states_comp = chooseModel(model)

    out = pd.DataFrame(None)
    comb = aux.expand_grid({'city': weather_loc,
                            'sensor': sensor_type,
                            'calib': calibration,
                            'treat': experiments})

    it_exp = 0

    for it_exp in range(comb.shape[0]):
        city = comb.city[it_exp]
        calib = comb.calib[it_exp]
        treat = comb.treat[it_exp]
        sensor = comb.sensor[it_exp]

        dataset = load_dataset(city=city,
                               sensor=sensor,
                               calib=calib,
                               treat=treat,
                               config_obs=config_obs)
        for it, p_it in enumerate(params_names):
            dataset['params'][p_it] = params_list[it]

        temp = runModel(dataset, model, var_names)
        outputs = pd.DataFrame(temp, columns=states_names)
        outputs_ = (outputs.loc[:, var_names]
                    .reset_index()
                    .melt(id_vars=['index'])
                    .rename(columns={"index": "dat"}))
        outputs_['exp'] = treat

        observations = aux.get_obs(dataset, var_names, states_comp)

        error = aux.calcError(observations, outputs_, var_names)
        out = out.append(error)

    return out


def error_all(params_list, params_names, var_names, model,
              weather_loc, sensor_type, calibration, experiments, config_obs):
    """
    The parametrization may take into account
    errors from lai, aboveground biomass and yield.

    Types of errors:
    "dif_sq": error = (state - obs)**2
    "dif_rel_sq": error = ((state - obs)/obs)**2
    "dif_log_sq_simple": error = (np.log(state) - np.log(obs))**2
    "dif_log_sq_double": error = ((np.log(state) - np.log(obs))**2 +
                             (np.log(obs) - np.log(state))**2)
    """

    error = loop_exps(params_list, params_names, var_names, model,
                      weather_loc, sensor_type, calibration, experiments,
                      config_obs)
    #error['metric'] = aux.tot_error_calc(error, 'dif_log_sq_simple')
    #error_mod = error.sort_index().drop(['max_obs', 'mean_obs'], axis=1)
    error_ = aux.tot_error_calc(error, 'dif_log_sq_simple').sum()
    #error_ = aux.tot_error_calc(error, 'dif_sq').sum()
    # if error_ < 10:
    #     print(error_mod)
    #print(params_list, error_)

    return error_


def main(method, weather_loc, sensor_type, calibration, experiments, config_obs,
         model, var_names, params_names, it):
    params_file = "./tables/parameters_inputs/params_limits_case1.csv"

    limits = aux.retrieve_limits(filename=params_file, model_name=model)
    bounds = limits[0]

    problem = {'num_vars': len(bounds.keys()),
               'names': list(bounds.keys()),
               'bounds': list(bounds.values())}

    params_list = []
    params_bounds = []

    for it_param in range(len(params_names)):
        if params_names[it_param] in problem['names']:
            par_it = params_names[it_param]
            bds = problem['bounds'][problem['names'].index(par_it)]
            coef = np.random.rand(1)
            avg = np.max(bds)*coef
            #avg = np.mean(bds)*coef*it
            params_list.append(avg)
            params_bounds.append(tuple(bds))

    np.random.seed(42)
    res = scipy.optimize.dual_annealing(error_all,
                                  #params_list,
                                  args=(params_names, var_names, model,
                                        weather_loc, sensor_type,
                                        calibration, experiments, config_obs),
                                    #method=method,
                                    bounds=params_bounds,
                                    #maxfun=1000,
                                    # options={
                                        # 'maxtime': 2*5,
                                        #      'maxev': 10,
                                        #      'maxfev': 10
                                        #'eps': 1e-09,
                                            #'maxiter': len(params_names)*300
                                    #        }
                                  )

    # res = scipy.optimize.minimize(error_all,
    #                               params_list,
    #                               args=(params_names, var_names, model,
    #                                     weather_loc, sensor_type,
    #                                     calibration, experiments, config_obs),
    #                                 method=method,
    #                                 bounds=params_bounds,
    #                                 #maxfun=1000,
    #                                 # options={
    #                                     # 'maxtime': 2*5,
    #                                     #      'maxev': 10,
    #                                     #      'maxfev': 10
    #                                     #'eps': 1e-09,
    #                                         #'maxiter': len(params_names)*300
    #                                 #        }
    #                               )

    return res


# %% Run Calib
weather_loc = ['cps']
sensor_type = "A"
calibration = 'gnvMod'

# config_obs = "ids", "summ", "monit"
# ids only cps
config_obs = "ids"
# Se em sequência, inserir numero de experimentos + 1
# experiments_int = np.array(range(3, 7))
# # cps1 - all
# experiments_int = [3, 4, 5, 6, 7, 8]
# cps2 - ciclo2
# experiments_int = [3, 4]
# # cps3 - ciclo3
#experiments_int = [5, 6]
# # cps4 - ciclo4
experiments_int = [7, 8]

#weather_loc = ['gnv']
#sensor_type = "A"
#calibration = 'gnv2'
#config_obs = "summ"
#experiments_int = [4, 5, 6]

experiments = [str(s) for s in experiments_int]

# model = "simple"
# var_names = ['f_solar', 'biomass', 'plant_yield']
# params_names = ['HI', 'I50A', 'RUE', 'T_base',
#                    'T_ext', 'T_heat', 'T_sum']

model = "tomgro"
var_names = ['n', 'lai', 'w', 'wf', 'wm']
params_names = ['alpha_F', 'beta', 'delta', 'DFmax',
                'K_F', 'N_b', 'N_FF', 'N_max',
                'Qe', 'T_crit']

#model = "vanthoor"
#var_names = ['lai', 'w', 'wf', 'wm']
#params_names = ['cDev2',
#                'ksMaxTCan',
#                'ksMaxTCan24',
#                'ksMinTCan',
#                'ksMinTCan24',
#                'nDev', 'TSumEnd', 'sla',
#                'ttsum']

# 'differential_evolution' 'maxiter: 100'
# 'dual_annealing' 'max_fun: 1000'

method = 'Nelder-Mead'
method = 'Dual Annealing'

params_file = "./tables/parameters_inputs/params_limits_case1.csv"
limits = aux.retrieve_limits(filename=params_file, model_name=model)

it = 0

def save_calibs(method, weather_loc, sensor_type, calibration, experiments,
                config_obs,
                model, var_names, params_names, it):

    try:
        np.random.seed(42+int(it*1000))
        res = main(method, weather_loc, sensor_type, calibration, experiments,
                    config_obs,
                    model, var_names, params_names, it)
    except:
        pass

    #print(it, 42+int(it*100))
    #print(res)

    result = {"model": model,
              "city": weather_loc,
              "exps": experiments,
              "var_names": var_names,
              "params_names": params_names,
              "limits": limits,
              "method": method,
              "error": res.fun.tolist(),
              "values": res.x.tolist(),
              "status": res.success
              }

    now = datetime.today().strftime("%Y%m%d_%H%M%S")

    cond_vant = ((result["values"][1] > result["values"][2]) &
                  (result["values"][3] < result["values"][4]))

    if (((model == "vanthoor") & cond_vant) | (model != "vanthoor")):
        path_json = "./tables/parameters_inputs/calibration/an_" + \
          str(res.success) + "_" + str(res.fun) + "_" + \
          model + "_" + weather_loc[0] + "_" + now + ".json"
    else:
        path_json = "./tables/parameters_inputs/calibration/nope_an_" + \
          str(res.success) + "_" + str(res.fun) + "_" + \
          model + "_" + weather_loc[0] + "_" + now + ".json"

    with open(path_json, 'w') as json_file:
        json.dump(result, json_file)

list_it = np.arange(1, 4, 1)
njobs = 6

save_calibs(method, weather_loc, sensor_type,
            calibration, experiments, config_obs,
            model, var_names, params_names, 1)

Parallel(n_jobs=njobs,
          verbose=3)(delayed(
              lambda x: save_calibs(method, weather_loc, sensor_type,
                                    calibration, experiments,
                                    config_obs,
                                    model, var_names, params_names,
                                    list_it[x]))(cont)
              for cont in range(len(list_it)))

winsound.Beep(440, 1000)
