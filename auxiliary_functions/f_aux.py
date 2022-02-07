# -*- coding: utf-8 -*-
"""
Created on Mon Oct 28 14:28:52 2019

@author: Monique
"""

import cv2
import csv
import pandas as pd
import numpy as np
import datetime as dt
import itertools
import os
import random

#import yaml
#import joblib

#from sklearn.linear_model import LogisticRegression
#from sklearn.pipeline import Pipeline
#from sklearn.preprocessing import StandardScaler

# %% General auxiliary tools

def expand_grid(data_dict):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    rows = itertools.product(*data_dict.values())

    return pd.DataFrame.from_records(rows, columns=data_dict.keys())


def print_state(state):
    """
    Pretty print assuming a list that have N, LAI, W, Wf, Wm in that order

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    base_string = 'N: {:.4f} | LAI: {:.4f} | ' + \
        'W: {:.4f} | Wf: {:.4f} | Wm {:.4f}'
    print(base_string.format(*state))
    return None


def dictFromCSV(filename, exp):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    new_dict = {}
    with open(filename, mode='r') as infile:
        reader = csv.reader(infile)
        for rows in reader:
            if (rows[2] == exp) or (rows[2] == "all"):
                k, v = rows[0:2]
                try:
                    new_dict[k] = float(v)
                except ValueError:
                    new_dict[k] = v

    return new_dict


def save_output(conf, type_output, x, city, calib, treat, rep):

    if (int(treat) < 10):
        treat = "n0" + str(treat)
    else:
        treat = "n" + str(treat)

    exp_n = city + "-" + treat + "-calib_" + calib

    if type_output == "simul":
        x['das'] = list(range(x.shape[0]))

        path_file = './tables/results_simul/exp/'
        output_file_name = path_file + conf + '-' + exp_n + '-sensor_' + \
            rep + '.csv'

    elif type_output == "error_simul":
        path_file = './tables/results_simul/errors/'
        output_file_name = path_file + conf + '-' + exp_n + '-sensor_' + \
            rep + '.csv'

    elif type_output == "error_assim":

        prefix = conf['filt'] + '-' + conf['model'] + '-'
        suffix = str(int(conf['id'])).zfill(4) + "-" + exp_n + '_' + \
            str(int(rep)).zfill(4) + '.csv'
        path_save = './tables/results_DA/errors/'
        output_file_name = path_save + prefix + suffix

    elif type_output == "updState":
        var = conf['state_var']
        x = pd.DataFrame(x, columns=[var+'_est_s', 'P_s',
                                     var+'_upd_s', 'P_upd_s',
                                     var+'_est_m', 'P_m',
                                     var+'_upd_m', 'P_upd_m',
                                     'observations', 'R', 'Gain',
                                     'Resid', 'Cov_pred',
                                     'das'])
        x['exp'] = treat

        prefix = conf['filt'] + '-' + conf['model'] + '-updState-'
        suffix = str(int(conf['id'])).zfill(4) + "-" + exp_n + '_' + \
            str(int(rep)).zfill(4) + '.csv'
        path_save = './tables/results_DA/'
        output_file_name = path_save + prefix + suffix

    elif type_output == "allState":
        prefix = conf['filt'] + '-' + conf['model'] + '-allStates-'
        path_save = './tables/results_DA/'
        suffix = str(int(conf['id'])).zfill(4) + "-" + exp_n + '_' + \
            str(int(rep)).zfill(4) + '.csv'

        output_file_name = path_save + prefix + suffix
        x = pd.DataFrame(x, columns=conf['states_names'] + ['das'])
        x['exp'] = treat

    elif type_output == "sigmas":
        prefix = conf['filt'] + '-' + conf['model'] + '-sigmas-'
        path_save = './tables/results_DA/ensembles/'
        suffix = str(int(conf['id'])).zfill(4) + "-" + exp_n + '_' + \
            str(int(rep)).zfill(4) + '.csv'

        output_file_name = path_save + prefix + suffix
        x = pd.DataFrame(x)
        x.rename(columns=lambda x:f"x{x}", inplace=True)
        x.rename(columns={x.columns[-1]: 'das'}, inplace=True)

    x.to_csv(output_file_name, index=False)

    return None


def retrieve_limits(filename: str, model_name: str) -> dict:
    """
    Retrieves parameters for uncertainty and sensitivity analyses.

    From a csv file, retrieve upper and lower limits of parameters and inputs
    to be sampled for uncertainty and sensitivity analyses and return them
    as a dictionary comprised by the name of the parameter and a tuple of
    limits.

    Parameters
    ----------
    filename: string
        CSV file path
    model: string
        Model name as per instruction (red_tomgro, tomsim, vanthoor, simple)

    Returns
    -------
    Dictionary with the name of the parameter and a list corresponding to
    upper and lower limits.

    """
    new_dict = {}
    dist = []
    sd2 = []

    # filename = "./tables/parameters_inputs/params_limits_case1.csv"
    # model_name = "simple"

    with open(filename, mode='r') as infile:
        reader = csv.reader(infile)
        for rows in reader:
            if rows[4] == model_name:
                k, v1, v2, d = rows[0:4]
                sd = rows[5]
                try:
                    new_dict[k] = [float(v1), float(v2)]
                except ValueError:
                    new_dict[k] = [v1, v2]
                try:
                    dist.append(str(d))
                except ValueError:
                    dist.append(d)
                try:
                    sd2.append(float(sd))
                except ValueError:
                    sd2.append(sd)

    return [new_dict, dist, sd2]


# %% Tools for running filters
def sample_frequency_obs(obs_, config_it):

    freq = config_it['frequency']

    # Identify which days have an observation
    mask = ~np.isnan(obs_.loc[:, config_it['meas_var']]).any(axis=1)

    # Define the interval by calculating the number of observations
    interval = int(1 / freq)

    # Sample observations according to the interval
    mask_mod = (np.cumsum(mask) % interval).values
    mask_bool = mask_mod != 0
    mask_new = (mask & mask_bool).to_list()

    if config_it["rep"] > 1:
        np.random.seed(42+config_it["rep"])
        random.shuffle(mask_new)

    obs_mod = obs_.copy()
    # Replace those to be discarded with nan
    obs_mod.loc[mask_new, config_it['meas_var']] = np.nan
    # obs_.wf.values
    # obs_mod.wf.values

    return obs_mod


def relevant_obs(obs, config_it):
    """
    Retrieve all available observations for a given state variable as well as
    their respective standard deviations.

    Parameters
    ----------
    obs : TYPE
        DESCRIPTION.
    config_it : TYPE
        DESCRIPTION.

    Returns
    -------
    list
        DESCRIPTION.

    """
    columns = config_it['meas_var']
    columns_sd = [s + '_sd' for s in columns]

    cols_total = ['stage'] + ['cycle'] + ['dat'] + columns + columns_sd

    if config_it["id"] > 500:
        columns_sd_md = [s + '_sd_md' for s in columns]
        cols_total = cols_total + columns_sd_md

    obs_ = obs[cols_total]
    obs_mod = sample_frequency_obs(obs_, config_it)

    return obs_mod


def modify_obs(obs, config_it):
    """
    Filter valid observations.

    The dataset may include, for the selected variables, values for the
    individual plant as well as for the calibration plants. So calibration
    values must be removed when returning the dataset.

    Parameters
    ----------
    obs : TYPE
        DESCRIPTION.
    config_it : TYPE
        DESCRIPTION.

    Returns
    -------
    obs_mod : TYPE
        DESCRIPTION.

    """
    exp = 'n0' + str(int(config_it['exp']))

    var = config_it['state_var']
    meas_var = "".join(config_it['meas_var'])

    # Select appropriate observations
    if meas_var == var:
        obs_ = obs.loc[(obs.node == "calib"), ]

        if int(config_it["id"])  >= 500:
            if int(config_it["config"]) < 100:
                # Case controlled error
                if config_it["config"] <= 6:
                    obs_ = obs_.loc[((obs["calib"] == "cps4") &
                                     (obs["config"] == config_it["config"])  &
                                     (obs["exp"] == exp)), ]
                elif (config_it["config"] >= 8) & (config_it["config"] <= 10):
                    obs_ = obs_.loc[((obs["calib"] == "cps4") &
                                     (obs["config"] == (config_it["config"] - 4))  &
                                     (obs["exp"] == exp)), ]
                elif (config_it["config"] >= 11) & (config_it["config"] <= 13):
                    obs_ = obs_.loc[((obs["calib"] == "cps4") &
                                     (obs["config"] == (config_it["config"] - 7))  &
                                     (obs["exp"] == exp)), ]
                else:
                    obs_ = obs_.loc[((obs["calib"] == "cps4") &
                                     (obs["config"] == 5)  &
                                     (obs["exp"] == exp)), ]
            else:
                # Case non-controled error
                obs_ = obs_.loc[((obs["calib"] == "cps4") &
                                 (obs["config"] > 100) &
                                 (obs["exp"] == exp)), ]
        else:
            obs_ = obs_.loc[(obs["node"] == "calib"), ]


    else:
        obs_ = obs.loc[((obs.exp == exp) & (obs.node != "calib")), ]

    # Remove last observation as it should not be assimilated
    last = obs_.shape[0] - 1
    obs_.index = obs_.index.astype('int')
    # obs = obs.drop(obs.index[last], axis=0)
    obs_.loc[last, config_it['meas_var']] = np.nan

    obs_mod = relevant_obs(obs_, config_it)
    obs_mod.reset_index(inplace=True, drop=True)

    return obs_mod


def expand_dict_with_list(d, key):

    local_dict = d.copy()
    local_dict.pop(key)
    l = []

    for dki in d[key]:
        li = local_dict.copy()
        try:
            li[key] = dki.item()
        except:
            li[key] = dki
        l.append(li)

    return l


def gen_perturb(config_it, x):

    case = config_it["case"]
    N = int(config_it["N"])
    z = np.zeros(1)
    x_mod = np.zeros((N, 1))

    # Case 0: Initial states
    # Sigmas representing state particles are passed through
    # the predict function. They have been perturbed by initial states
    # uncertainty and this will be the only disturbance
    if case == "case0":
        None

    # Case 1: States
    # Sigmas representing state particles are passed through
    # the predict function and then perturbed with the
    # uncertainty Q
    elif case == "case1":
        None

    # Case 2: Parameters
    # A pre-selected parameter is perturbed and this value is then
    # applied in the prediction of state particles
    elif case == "case2":
        e = np.random.multivariate_normal(z, np.eye(1)*x*0.1, N)
        x_mod = x + e
        x_mod[x_mod < 0] = 0.

    # Case 3: inputs
    # A pre-selected input is perturbed and this value is then
    # applied in the prediction of state particles
    elif case == "case3":
        for h in x:
            e = np.random.multivariate_normal(z, np.eye(1)*h*0.3, N)
            # Particles in lines, hours in columns
            x_mod = np.hstack((x_mod, (h + e)))
        x_mod = x_mod[:, 1:]
        x_mod[x_mod < 0] = 0.

    return x_mod


# %% Tools for running models
def load_data(model, weather, sensor, experiment, calib, config_obs):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    data = {}

    # Two cases:
    # 1. Simple run of the model: weather is particular, but parameters,
    # inputs and initial states (isp) are from the experiment, that is the same
    # regardless of weather.
    # 2. And SA case in which weather refer to the overall
    # city. So weather must refer to the input and isp, have to be adapted.

    # Parameters - To each simulation, a parameter defined previously
    # Parameters and inputs are not experiment dependent,
    # Weather, intial state and observations are
    # Input may be, especially in my case.

    # Special cases: Campinas and SA Case 1
    if (((sensor == 'A') | (sensor == 'B')) & (weather == "cps")):
        city_mod = 'cps' + sensor
    else:
        city_mod = weather

    # Calib Fixed = SA Case 1
    if calib == "fixed":
        city_mod = 'fixed'
        experiment = 'fixed'
        exp_n = 'fixed'
        weather_suffix = 'fixed.csv'
    else:
        if (int(experiment) < 10):
            n = "n0" + experiment
            exp_n = weather + "_" + n
            experiment = "0" + experiment
        else:
            n = "n" + experiment
            exp_n = weather + "_" + n
        # Match cycle
        cycle = np.ceil(int(experiment)/2)
        weather_suffix = city_mod + "_" + n + ".csv"

    # Parameters
    filename = model + "_p"
    filename = "./tables/parameters_inputs/" + filename + ".csv"
    params_all = dictFromCSV(filename, calib)

    # Inputs
    filename = model + "_i"
    filename = "./tables/parameters_inputs/" + filename + ".csv"
    info_all = dictFromCSV(filename, exp_n)

    # Initial states
    filename = model + "_s0"
    filename = "./tables/parameters_inputs/" + filename + ".csv"
    states0_all = dictFromCSV(filename, exp_n)

    # Weather dataset
    if model == "simple":
        temp = "daily_"
    else:
        temp = "hourly_"

    weather_file_name = "./data/weather/" + temp + weather_suffix
    weather_all = pd.read_csv(weather_file_name)

    # Observations
    filename = "./data/observations/full_set_obs.csv"
    if config_obs == "summ":
        filename = "./data/observations/full_set_obs.csv"
    elif config_obs == "monit":
        filename = "./data/observations/full_set_obs.csv"
    elif config_obs == "ids":
        filename = "./data/observations/monitoring/obs_exp_all_ids.csv"
    elif config_obs == "artif_obs":
        filename = "./data/synthetic/obs_artif_all.csv"
    elif config_obs == "artif_truth":
        filename = "./data/synthetic/truth_tomgro.csv"

    obs_temp = pd.read_csv(filename)

    if config_obs == "summ":
        mask = ((obs_temp.loc[:, "city"] == weather) &
                (obs_temp.loc[:, "exp"] == n) &
                (obs_temp.loc[:, "node"] == "calib"))
        obs_temp_mod = obs_temp.loc[mask, ]
    elif config_obs == "monit":
        mask = ((obs_temp.loc[:, "city"] == weather) &
                (obs_temp.loc[:, "exp"] == ("n" + experiment)) &
                (obs_temp.loc[:, "node"] != "calib"))
        obs_temp_mod = obs_temp.loc[mask, ]
    elif config_obs is None:
        obs_temp_mod  = None
    elif config_obs == "ids":
        mask = ((obs_temp.loc[:, "city"] == weather) &
                (obs_temp.loc[:, "cycle"] == cycle))
        obs_temp_mod = obs_temp.loc[mask, ]
    elif config_obs == "artif_obs":
        obs_temp_mod = obs_temp
    else:
        mask = (obs_temp.loc[:, "exp"] == n)
        obs_temp_mod = obs_temp.loc[mask, ]

    data['obs'] = obs_temp_mod
    data['weather'] = weather_all
    data['params'] = params_all
    data['info'] = info_all
    data['states'] = states0_all

    return data


def convert_rad(weather, target):
    """
    Auxiliary function to convert the radiation in the dataset into PAR.

    Conferir a conversão da radiação. Estou medindo PAR dentro da estufa.
    So 4.57 is the conversion factor and 0.45 is to convert from global to PAR
    https://www.researchgate.net/post/Can_I_convert_PAR_photo_active_radiation_value_of_micro_mole_M2_S_to_Solar_radiation_in_Watt_m22

    Parameters
    ----------
    weather : TYPE
        DESCRIPTION.
    target : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """
# Conversion factors:
# 1 MJ m-2 = 23.90 Langley (cal cm-2)
# 10^6 E / 3600 s = 277.78 E / s
# 1 cal = 4.184 J
# 23.923 / parfac * 277.78 = 550.6

# From Keulen and Dayan (1993):
# PARFAC = 12.07. This is a factor to convert global short wave radiation data
# from cal cm-2 to PAR in [microE m-2 s-1] E/m-2 h-1

# CONVERT MJ/M2 TO LY TO E/M2-H TO micro-E/M2-S
# weather.rad = (weather.rad * 23.923 / parfac * 277.78)

    rad_var = "rad"

# MJg = MJ m-2 d-1 Global Radiation
# mmolPAR = mmol m-2 s-1 PAR
# JPAR = J m-2 s-1 PAR

# From Jones et al 1999, spreadsheet:
    if weather.radiation_unit.iloc[0] != "mmolPAR" and target == "mmolPAR":
        if weather.radiation_unit.iloc[0] == "MJg":
            # "MJ" infers MJ m-2 day-1
            # 23.923/12.07*277.78 = 556.6
            weather.loc[:, rad_var] = (weather.loc[:, rad_var] * 555.6)
        else:
            weather.loc[:, rad_var] = (weather.loc[:, rad_var] * 2.148)
        weather.loc[:, "radiation_unit"] = "mmolPAR"

    elif weather.radiation_unit.iloc[0] != "MJg" and target == "MJg":
        if weather.radiation_unit.iloc[0] == "mmolPAR":
            weather.loc[:, rad_var] = (weather.loc[:, rad_var] / 555.6)
        weather.loc[:, "radiation_unit"] = "MJg"

    elif weather.radiation_unit.iloc[0] != "JPAR" and target == "JPAR":
        if weather.radiation_unit.iloc[0] == "mmolPAR":
            weather.loc[:, rad_var] = (weather.loc[:, rad_var] / 4.57)
        elif weather.radiation_unit.iloc[0] == "MJg":
            weather.loc[:, rad_var] = (weather.loc[:, rad_var] * 555.6)
            weather.loc[:, rad_var] = (weather.loc[:, rad_var] / 4.57)
        weather.loc[:, "radiation_unit"] = "JPAR"

    return weather


def calc_obs_ttsum(dataset):

    weather = dataset['weather'].copy()
    params = dataset['params'].copy()
    params["T_base"] = 10
    obs = dataset['obs'].copy()

    wfruits = obs[['wf', 'wm']]

    # Fruits
    wf_dats = wfruits.loc[(wfruits.wf == 0), :]
    wf_start = np.max(list(wf_dats.index))

    weather_mat = weather[weather.dat < wf_start]
    if "hour" not in weather.columns:
        t_avg = 0.5 * (weather_mat.tmax + weather_mat.tmin)

        tt_sum_fruits = np.maximum(list(t_avg - params["T_base"]),
                                   list(np.repeat(0., len(t_avg)))).sum()

    else:
        t_avg = weather_mat.tmean

        tt_sum_fruits = np.maximum(list(t_avg - params["T_base"]),
                                   list(np.repeat(0., len(t_avg)))).sum()/24

    # Mature fruits
    wm_dats = wfruits.loc[(wfruits.wm == 0), :]
    wm_start = np.max(list(wm_dats.index))

    weather_mat = weather[weather.dat < wm_start]
    if "hour" not in weather.columns:
        t_avg = 0.5 * (weather_mat.tmax + weather_mat.tmin)

        tt_sum_m_fruits = np.maximum(list(t_avg - params["T_base"]),
                                     list(np.repeat(0., len(t_avg)))).sum()

    else:
        t_avg = weather_mat.tmean

        tt_sum_m_fruits = np.maximum(list(t_avg - params["T_base"]),
                                     list(np.repeat(0., len(t_avg)))).sum()/24

    return tt_sum_fruits, tt_sum_m_fruits


# %%  Auxiliary Reduced TOMGRO
def calc_T_daytime(weather_d):
    """
    Auxiliary function to calculate the mean temperature during daylight hours.

    Parameters
    ----------
    dat : TYPE
        DESCRIPTION.
    weather_d : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """

    if sum(weather_d.rad.values > 0):
        Tdaytime = weather_d.tmean.loc[weather_d.rad.values > 0]
    else:
        minH = 6
        maxH = 19

        Tdaytime = weather_d.tmean.loc[
            (weather_d.hour >= minH) & (weather_d.hour <= maxH)]

    Tdaytime = Tdaytime.mean()

    return(Tdaytime)


# %% Auxiliary SIMPLE
def info_dates(info):

    if "plant_doy" not in list(info.keys()):
        info["plant_doy"] = dt.date(int(info["plant_y"]),
                                    int(info["plant_m"]),
                                    int(info["plant_d"])).timetuple().tm_yday
        info["harv_doy"] = dt.date(int(info["harv_y"]),
                                   int(info["harv_m"]),
                                   int(info["harv_d"])).timetuple().tm_yday
        info["plant_ymd"] = dt.date(int(info["plant_y"]),
                                    int(info["plant_m"]),
                                    int(info["plant_d"]))
        info["harv_ymd"] = dt.date(int(info["harv_y"]), int(info["harv_m"]),
                                   int(info["harv_d"]))
    else:
        zero = dt.date(int(info["plant_y"]), 1, 1)
        delta = dt.timedelta(days=int(info["plant_doy"]))
        info["plant_ymd"] = zero + delta

        zero = dt.date(int(info["harv_y"]), 1, 1)
        delta = dt.timedelta(days=int(info["harv_doy"]))
        info["harv_ymd"] = zero + delta

    return info


# %% Error metrics
def get_obs(dataset: dict,
            var_names: list,
            states_comp: list) -> pd.DataFrame:
    """
    From the

    Parameters
    ----------
    dataset : dict
        DESCRIPTION.
    var_names : list
        DESCRIPTION.
    states_comp : list
        DESCRIPTION.

    Returns
    -------
    observations_ : TYPE
        DESCRIPTION.

    """

    observations = dataset['obs']

    if len(states_comp) > 0:
        for old, new in states_comp:
            observations[new] = observations[old]

    var_names_ = var_names + ["dat"]
    var_names_ = [x for x in var_names_ if x in observations.columns.tolist()]

    observations_ = (observations.loc[:, var_names_]
                     .melt(id_vars=['dat'])
                     .dropna()
                     .query('dat > 0'))
                     #.reset_index())

    if (('wm', 'plant_yield') in states_comp):
        mask = ((observations_. variable != "plant_yield") |
                ((observations_.variable == "plant_yield") &
                 (observations_.dat == max(observations_.dat))))
        observations_ = observations_[mask]

    return observations_


def calcError(observations_: pd.DataFrame,
              outputs_: pd.DataFrame,
              var_names: list) -> pd.DataFrame:

    out = (pd.merge(observations_, outputs_,
                    left_on=['dat', 'variable'],
                    right_on=['dat', 'variable'],
                    how='left')
           .rename(columns={'value_x': 'obs', 'value_y': 'pred'}))
    out.sort_values(by='variable', inplace=True)
    out['max_obs'] = out.groupby('variable').obs.transform('max')
    out['mean_obs'] = out.groupby(['variable', 'dat']).obs.transform('mean')
    out['dat'] = out['dat'].astype(int)

    return out


def tot_error_calc(error, type_error):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    # Because I (likely) don't have constant variance in my residuals,
    # instead of calculating just the diference, I calculate the difference
    # between the log of the observation and the log of the estimated value

    state = error['pred'].replace({0: 0.1})
    obs = error['obs'].replace({0: 0.1})
    #max_obs = error['max_obs'].replace({0: 1})
    mean_obs = error['mean_obs'].replace({0: 1})

    if type_error == "dif_sq":
        error_calc = (state - obs)**2

    elif type_error == "dif_rel_sq":
        error_calc = ((state - obs)/mean_obs)**2

    elif type_error == "dif_log_sq_simple":
        error_calc = np.sqrt((np.log10(state) - np.log10(obs))**2)

    return error_calc



# %% Processing images
def save_ref(img_ref):

    filename = 'E:/drive_unicamp/SISDA/projetoModelagemTomateiro/pidata/scans/ciclo02/guardar/regua_carta_h.jpeg'
    ref0 = cv2.imread(filename)

    gray = cv2.cvtColor(src = ref0.copy(), code = cv2.COLOR_BGR2GRAY)

    # Mask card
    blur = cv2.blur(gray, (11, 11))
    ret, mask_card = cv2.threshold(blur, 25, 255, cv2.THRESH_BINARY_INV)

    k = np.ones((15, 15), np.uint8)
    mask_close = cv2.morphologyEx(mask_card, cv2.MORPH_CLOSE, k)

    k = np.ones((15, 15), np.uint8)
    mask_dilate = cv2.dilate(mask_close, k,iterations = 1)

    # Contour
    (_, contours_ref, _) = cv2.findContours(mask_dilate,
                                       cv2.RETR_EXTERNAL,
                                       cv2.CHAIN_APPROX_NONE)

    img_mod = cv2.bitwise_and(ref0.copy(), ref0.copy(),
                              mask=mask_dilate)

    # Extract out the object and place into output image
    out = np.zeros_like(img_mod)
    out[mask_dilate == 255] = img_mod[mask_dilate == 255]

    # Now crop
    (y, x) = np.where(mask_dilate == 255)
    (topy, topx) = (np.min(y), np.min(x))
    (bottomy, bottomx) = (np.max(y), np.max(x))
    out = out[topy:bottomy+1, topx:bottomx+1]

    filename = 'E:/drive_unicamp/SISDA/projetoModelagemTomateiro/pidata/scans/ciclo02/guardar/ref_ciclo02.jpeg'
    cv2.imwrite(filename=filename, img=out)


def crop_image_borders(input_mask, tol, mod, f):
    '''Remove dark border lines by converting them
    into black or white lines
    From: https://codereview.stackexchange.com/questions/132914/crop-black-border-of-image-using-numpy
    '''
    # img is 2D image data
    # tol  is tolerance
    if f == ">":
        mask = input_mask > tol
    else:
        mask = input_mask <= tol

    m, n = input_mask.shape
    mask0, mask1 = mask.any(0), mask.any(1)

    # Any place in the image
    # img[np.ix_(mask.any(1),mask.any(0))]

    # Only borders
    # col_start, col_end = mask0.argmax(), n-mask0[::-1].argmax()
    # row_start, row_end = mask1.argmax(), m-mask1[::-1].argmax()

    col_start, col_end = mask0.argmax(), n-mask0[::-1].argmax()
    row_start, row_end = mask1.argmax(), m-mask1[::-1].argmax()

    img_bg = input_mask.copy()
    img_bg[input_mask >= 0] = mod
    img_bg[row_start:row_end,
           col_start:col_end] = input_mask[row_start:row_end,
                                           col_start:col_end]

    return img_bg


def rect_from_extreme(contour_card, img):

    cnt = contour_card
    # Create rectangle following coordinates from the card
    leftmost = tuple(cnt[cnt[:,:,0].argmin()][0])
    rightmost = tuple(cnt[cnt[:,:,0].argmax()][0])
    topmost = tuple(cnt[cnt[:,:,1].argmin()][0])
    bottommost = tuple(cnt[cnt[:,:,1].argmax()][0])

    min_x = 0
    min_y = 0
    max_x = img.shape[1]
    max_y = img.shape[0]

    pt1 = leftmost[0]
    pt2 = topmost[1]
    x = max_x
    y = max_y
    if abs(leftmost[0] - min_x) < abs(rightmost[0] - max_x):
        # If the leftmost point is closer to the border on x,
        # the internal point is the rightmost
        pt1 = rightmost[0]
        # But I also want the border value for x
        x = min_x

    if abs(topmost[1] - min_y) < abs(bottommost[1] - max_y):
        # If the topmost point is closer to the border on y,
        # the internal point is the bottommost
        pt2 = bottommost[1]
        # But I also want the border value for y
        y = min_y

    rect_est = cv2.rectangle(img.copy(), (pt1, y), (x, pt2), 255)
    # contour_est = np.int0(rect_est)
    # contours_draw = cv2.drawContours(image=img.copy(),
    #                      contours=[contour_est],
    #                      contourIdx=-1,
    #                      color=(255, 0, 0),
    #                      thickness=3)
    # img_name = path_save + "/c3_" + img_it
    # cv2.imwrite(filename=img_name, img=rect_est)

    return rect_est


def find_contour(img, lower_thresh, upper_thresh):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    cover_thresh = cv2.inRange(img.copy(), lower_thresh, upper_thresh)
    # ret, cover_thresh = cv2.threshold(cover.copy(), 0, 255,
    #                                   cv2.THRESH_BINARY|cv2.THRESH_OTSU)

    # img_name = path_save + "thresh_" + img_it
    # cv2.imwrite(filename = img_name, img = cover_thresh)

    contours_all = []
    _, contours_all, _ = cv2.findContours(cover_thresh,
                                            cv2.RETR_LIST,
                                            cv2.CHAIN_APPROX_NONE)

    contours_ext = []
    _, contours_ext, _ = cv2.findContours(cover_thresh,
                                            cv2.RETR_EXTERNAL,
                                            cv2.CHAIN_APPROX_NONE)

    # contours_draw = cv2.drawContours(image = img.copy(),
    #       contours = contours_all,
    #       contourIdx = -1,
    #       color = (255, 0, 0),
    #       thickness = 3)
    # img_name = path_save + "contours_" + img_it
    # cv2.imwrite(filename = img_name, img = contours_draw)

    return contours_all, contours_ext


def easyleafarea(cycle, leaves):

    # Approach 1: Code from Easy Leaf Area:
    b = np.zeros((leaves.shape[0], leaves.shape[1]), dtype=leaves.dtype)
    g = np.zeros((leaves.shape[0], leaves.shape[1]), dtype=leaves.dtype)
    r = np.zeros((leaves.shape[0], leaves.shape[1]), dtype=leaves.dtype)
    b[:, :] = leaves[:, :, 0]
    g[:, :] = leaves[:, :, 1]
    r[:, :] = leaves[:, :, 2]

    ratGB = 1.04
    minG = 30
    if cycle == "cycle01caseA":
        ratGR = 0.98
        ratGB = 1.2
        minG = 10
    elif cycle == "cycle01caseB":
        ratGR = 1.02
        ratGB = 1.04
        minG = 10
    elif cycle == "cycle01caseC":
        ratGR = 1.10
        ratGB = 1.04
        minG = 10
    elif cycle == "cycle01caseD":
        ratGR = 0.9
        ratGB = 1.04
        minG = 10
    elif cycle == "cycle02":
        ratGR = 1.02
    elif cycle == "cycle03":
        ratGR = 1.0

    test = (r*ratGR < g) & (b*ratGB < g) & (g > minG)
    leaves_mod = leaves.copy()
    leaves_mod[leaves_mod > 0] = 0
    leaves_mod[test] = 255
    leaves_mask = cv2.cvtColor(leaves_mod, cv2.COLOR_BGR2GRAY)

    return leaves_mask


def filter_contours(input_obj, area_min, cont_type):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    mode = cv2.RETR_TREE
    if cont_type == 'ext':
        mode = cv2.RETR_EXTERNAL

    if cont_type != "large":
        img, contours_all, hier = cv2.findContours(input_obj, mode,
                                                   cv2.CHAIN_APPROX_NONE)
    # img_name = 'E:/doc_local/imgs_proc//scans/ciclo02/teste.png'
    # cv2.imwrite(filename=img_name, img=img)

    # h: id_next_same_h, id_prev, id_child, id_parent
    # Eu nao quero pegar so os externos. Eu quero tirar se for externo com area
    # pequena
    # Entao eu quero incluir se for interno ou se for externo com area grande

    contours_filt = []
    if cont_type != "large":
        for it, c in enumerate(contours_all):
            M = cv2.moments(c)
            area = M['m00']
            if not ((area < area_min) and (hier[:, it][0, 3] == -1)):
                #print(area)
                # print((area < area_min))
                # print((hier[:, it][0, 3] == -1))
                contours_filt.append(c)

    else:
        # In this case, I want to filter out small contours
        contours_filt = []
        for it, c in enumerate(input_obj):
            M = cv2.moments(c)
            area = M['m00']
            if (area > area_min):
                #print('large',  area)
                # print((area > area_min))
                contours_filt.append(c)

    return contours_filt


def calc_area(img_it, contours, conv_sq, list_save):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    # Calculate areas
    sum_file = 0
    for c in contours:
        M = cv2.moments(c)
        area = M['m00']
        area_conv = area * conv_sq

        sum_file = sum_file + area_conv
    # print(sum_file)

    list_save.append([img_it, sum_file])

    return(list_save)


def calc_height(img_it, contour_height, conv, list_save):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """

    topmost = []
    bottommost = []

    # Find the most extreme points of all bounding boxes
    for it, cnt in enumerate(contour_height):
        topmost.append(cnt[cnt[:, :, 1].argmin()][0])
        bottommost.append(cnt[cnt[:, :, 1].argmax()][0])

    topmost_ = np.array(topmost)
    bottommost_ = np.array(bottommost)

    # Find the most extreme points accross all bounding boxes
    topmost_i = topmost_[topmost_[:, 1].argmin()]
    bottommost_i = bottommost_[bottommost_[:, 1].argmax()]

    # Reference length
    height = abs((topmost_i[1] - bottommost_i[1])*conv)

    list_save.append([img_it, height])

    return list_save

# # %% Segmenter Felipe

# def load_id(path, image_id):
#     base_path = os.path.join(path, image_id)
#     img = cv2.imread(base_path + '.png').reshape(-1, 3)
#     lab = cv2.imread(base_path + '_lb.png').reshape(-1, 3)

#     return img, lab


# def parse_label(label_rgb, encoding_rgb):
#     labels = -np.ones(label_rgb.shape[0])

#     label_encoding = {}

#     for i, k in enumerate(encoding_rgb):
#         label_encoding[i] = k
#         labels[(label_rgb == np.array(encoding_rgb[k])).all(1)] = i

#     return labels.ravel(), label_encoding


# def predict_mask(img, model):
#     X = img.reshape(-1, 3)

#     pred = model.predict_proba(X)[:, 1]
#     pred = pred.reshape(img.shape[0], img.shape[1])
#     return pred


# def build_training_data(path, encoding_rgb, compress=True):
#     ''' For a folder, look for all files that have a corresponding
#     file with _lb suffix, and load the data corresponding a those
#     files and labels.

#     After concatenating the dataset, if compress is True, it will
#     group duplicated rows and create a weight vector corresponding
#     to the occurences of that row.
#     '''
#     contents = os.listdir(path)
#     imgs = [x.split('.')[0] for x in contents if not x.endswith('_lb.png')]

#     imgs = [x for x in imgs if (x + '_lb.png') in contents]

#     label_encoding = {}

#     X = []
#     y = []

#     for img in imgs:
#         img, lab = load_id(path, img)
#         lab, img_label_encoding = parse_label(lab, encoding_rgb)
#         X.append(img)
#         y.append(lab)
#         label_encoding.update(img_label_encoding)

#     X = np.vstack(X)
#     y = np.hstack(y)
#     labeled = y != -1

#     X = X[labeled]
#     y = y[labeled]

#     if compress:
#         X, ii, w = np.unique(X, return_index=True, return_counts=True, axis=0)
#         y = y[ii]
#     else:
#         w = np.ones_like(y)

#     return X, y, w, label_encoding


# def create_pipeline():
#     pipe = Pipeline(
#             [
#                 ('scaler', StandardScaler()),
#                 ('classif', LogisticRegression())
#                 ]
#             )
#     return pipe


# def learner():
#     with open('./data/constants.yml', 'r') as f:
#         constants = yaml.full_load(f)

#     pipe = create_pipeline()

#     X, y, weights, le = build_training_data('./data/train', constants['encoding'])

#     pipe.fit(X, y, classif__sample_weight=np.log10(weights + 1))

#     with open('model/pipe.pkl', 'wb') as f:
#         joblib.dump(pipe, f)



# def pred_fun(f):

#     with open('./model/pipe.pkl', 'rb') as f:
#         pipe = joblib.load(f)

#     predict_path = './data/val'
#     predict_imgs = os.listdir(predict_path)

#     for img_path in predict_imgs:
#         img = cv2.imread(os.path.join(predict_path, img_path))
#         mask = predict_mask(img, pipe)

#         # fig, ax = plt.subplots(1, 2)
#         # ax[0].imshow(img)
#         # ax[1].imshow(mask, cmap='gray')
#         # plt.show()

#         cv2.imwrite(os.path.join('./data/pmasks', img_path), mask)



# %% Old

def fixyears(yearstr):
    """
    Summary

    Parameters
    ----------
    yearstr : TYPE
        DESCRIPTION.

    Returns
    -------
    TYPE
        DESCRIPTION.

    """
    if len(yearstr) == 4:
        return yearstr
    else:
        if int(yearstr) < 70:
            return '19' + yearstr
        else:
            return '20' + yearstr

# def truncate_cycle(plant_d, harv_d, weather):

#     dataset_daily_all["date2"] = pd.to_datetime(dataset_daily_all.date)

#     plant = pd.Timestamp(info["plant_ymd"]) - pd.Timedelta(days=1)
#     harv = pd.Timestamp(info["harv_ymd"])

#     # Filter only lines in the weather dataset that refer
#     #  to the growth period
#     select_days = ((dataset_daily_all.date2 >= plant) &
#                    (dataset_daily_all.date2 < harv))
#     dataset_weather = dataset_daily_all.loc[select_days, :].copy()

#     select_days = ((weather.loc[:, 'date2'] >= plant_d) &
#                    (weather.loc[:, 'date2'] < harv_d))
#     weather_mod = weather.loc[select_days, :].copy()

#     return weather_mod
