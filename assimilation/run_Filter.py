# -*- coding: utf-8 -*-
"""
Created on Mon Aug 17 14:50:50 2020

@author: Monique
"""

import pandas as pd
import numpy as np
from joblib import Parallel, delayed
import itertools
import gc
import sys
import tracemalloc
from tracemalloc import Filter

import assimilation.filters as filters
import simulations.run_Model_Simple as simple
#import simulations.run_Model_ReducedTomgro as tomgro
import simulations.run_Model_ReducedTomgro_oo as tomgro

import assimilation.measurementFunctions as hx
import auxiliary_functions.f_aux as aux

import winsound
# np.warnings.filterwarnings('error', category=np.VisibleDeprecationWarning)

# %% Agnostic auxiliary functions
def choose_model(config_it):

    model_name = config_it['model']

    if model_name == "tomgro":
        load_dataset = tomgro.load_dataset
        st_names = ['n', 'lai', 'w', 'wf', 'wm']
        rates_0 = {'dn_dt': np.array(0.0, ndmin=1),
                   'dlai_dt': np.array(0.0, ndmin=1),
                   'dw_dt': np.array(0.0, ndmin=1),
                   'dwf_dt': np.array(0.0, ndmin=1),
                   'dwm_dt': np.array(0.0, ndmin=1)}
        model_init = tomgro.rt.tomgro

    elif model_name == "simple":
        load_dataset = simple.load_dataset
        st_names = ['tt_sum', 'f_solar', 'biomass', 'plant_yield']
        rates_0 = st_names

    return load_dataset, st_names, rates_0, model_init


def select_config_filter(config_it):

    model_name = config_it['model']
    state_variable = config_it['state_var']
    meas_variable = config_it['meas_var']

    if model_name == "tomgro":

        fz = tomgro.rt.tomgro.integrate_mod_assim
        proc_func = tomgro.rt.tomgro.integrate

        if state_variable == 'w':
            if meas_variable == [state_variable]:
                meas_func = hx.W_dir

            elif meas_variable == ["w_fm"]:
                meas_func = hx.W_ind_dest

            elif meas_variable == ["w_fm_full"]:
                meas_func = hx.W_ind_non_dest

            elif meas_variable == ["lai_lat"]:
                meas_func = hx.W_ind_non_dest1

            elif meas_variable == ["height"]:
                meas_func = hx.W_ind_non_dest2

        elif state_variable == 'lai':
            if meas_variable == [state_variable]:
                meas_func = hx.LAI_dir

            elif meas_variable == ["lai_lat"]:
                meas_func = hx.LAI_ind1

            elif meas_variable == ["lai_abv"]:
                meas_func = hx.LAI_ind2

            elif (meas_variable == ["lai_lat", "lai_abv"] or
                  meas_variable == ["lai_abv", "lai_lat"]):
                meas_func = hx.LAI_ind3

        elif state_variable == 'wf':
            if meas_variable == [state_variable]:
                meas_func = hx.Wf_dir

            elif meas_variable == ["wf_lat"]:
                meas_func = hx.Wf_ind1

        elif state_variable == 'wm':
            if meas_variable == [state_variable]:
                meas_func = hx.Wm_dir

            elif meas_variable == ["wm_lat"]:
                meas_func = hx.Wm_ind1

        elif state_variable == 'n':
            meas_func = hx.N_dir

    return proc_func, fz, meas_func


def filter_state(model_obj, proc_func, meas_func,
                 dataset, config_it,
                 dt,
                 dim_x, dim_z):

    if config_it['filt'] == "ukf":
        filter_f = filters.filter_ukf(model_obj, proc_func, meas_func,
                                       dataset, config_it, dt,
                                       dim_x, dim_z)
    elif config_it['filt'] == "pf":
        filter_f = filters.filter_pf(model_obj, proc_func, meas_func,
                                      dataset, config_it, dt,
                                      dim_x, dim_z)
    elif config_it['filt'] == "enkf":
        filter_f = filters.filter_enkf(model_obj, proc_func, meas_func,
                                        dataset, config_it, dt,
                                        dim_x, dim_z)

    elif config_it['filt'] == "ekf":
        filter_f = filters.filter_ekf(model_obj, proc_func, meas_func,
                                       dataset, config_it, dt,
                                       dim_x, dim_z)

    return filter_f


def save_outputs(x, obj, type_out,
                 config_it=None, dat=None, meas_func=None,
                 measurement=None, obs=None):

    filter_f = obj

    if type_out == "sigmas":
        if config_it["filt"] == "pf":
            mask = [c == config_it["state_var"]
                    for c in config_it["states_names"]]
            sigmas = np.ravel(filter_f.sigmas[:, mask])
        else:
            sigmas = np.ravel(filter_f.sigmas)

        x.append(np.append(sigmas, dat))

    else:

        if config_it["filt"] == "pf":
            mask = [c == config_it["state_var"]
                    for c in config_it["states_names"]]
            x_prior = filter_f.x_prior[mask].item()
            P_prior = filter_f.P_prior[mask].item()
            x_post = filter_f.x_post[mask].item()
            #P_post = None
            P_post = filter_f.P_post[mask].item()
            filter_f.K = 0
            filter_f.y = 0
            filter_f.S = 0
        else:
            x_prior = filter_f.x_prior.item()
            P_prior = filter_f.P_prior.item()
            x_post = filter_f.x_post.item()
            P_post = filter_f.P_post.item()

        x.append([
            # State - Unit state
            x_prior,
            # Error - Unit state
            P_prior,

            # Updated state - Unit state
            x_post,
            # Updated error - Unit state
            P_post,

            # State - Unit measurement
            meas_func(x_prior, obs.loc[obs.dat == dat, :]),
            # State - Unit measurement
            meas_func(P_prior, obs.loc[obs.dat == dat, :]),

            # Updated State - Unit measurement
            meas_func(x_post, obs.loc[obs.dat == dat, :]),
            # Updated State - Unit measurement
            meas_func(P_post, obs.loc[obs.dat == dat, :]),
            # Measurement
            measurement,
            # Measurement error
            float(filter_f.R),
            # Gain
            filter_f.K,
            # Residual
            filter_f.y,
            # Covariance of prediction
            filter_f.S,
            # DAT
            dat
            ])

    return x


def run_filter(dataset, config_it):

    # Dataset
    weather = dataset['weather'].copy()
    params = dataset['params'].copy()
    _, _, rates, model_init = choose_model(config_it)
    st = dataset['states'].copy()
    info = dataset['info'].copy()

    obs0 = dataset['obs'].copy()
    obs = aux.modify_obs(obs0, config_it)
    err = config_it['model_err']

    all_dat = weather.dat.unique().tolist()
    weather_l = list(weather.groupby("dat"))

    dim_x = len([config_it['state_var']])
    dim_z = len(config_it['meas_var'])
    dt = 1

    # Define filter and accessory functions
    proc_func, fz, meas_func = select_config_filter(config_it)

    # Initialize model as object
    model_obj = model_init(info, params, rates, st)

    # Pass model object to filter initialization
    filter_f = filter_state(model_obj, proc_func, meas_func,
                            dataset, config_it,
                            dt=dt,
                            dim_x=dim_x, dim_z=dim_z)

    # Initialize outputs to be saved
    assim = list()
    sigmas = list()
    state = np.ravel(list(st.values()))

    # all_dat = all_dat[0:10]
    # it = 0
    # dat = all_dat[it]
    # all_dat = all_dat[0:-1]

    for it, dat in enumerate(all_dat):

        weather_d = weather_l[it][1]
        filter_f.model.weather_d = weather_d

        # Ascribe uncertainty to the proper scenario
        if config_it["case"] == "case3":
            weather_pert = config_it["weather_pert"]
            x = weather_d.rad
            weather_d_temp = weather_d.to_dict()
            weather_d_temp[weather_pert] = aux.gen_perturb(config_it, x)
            weather_exp = aux.expand_dict_with_list(weather_d_temp,
                                                    weather_pert)
            for it_w, temp in enumerate(weather_exp):
                var_temp = pd.Series(weather_exp[it_w][weather_pert],
                                     index=weather_d.index)
                weather_exp[it_w][weather_pert] = var_temp
                weather_exp[it_w] = pd.DataFrame(weather_exp[it_w])

            filter_f.model.list_weather_d = weather_exp

        # Identify observation
        # Only one observation at a time
        if dat in obs.dat.tolist():
            measurement = float(obs.loc[it, config_it['meas_var']].values)
            # Add perturbation caused by repetition - case of controlled errors
            #if ((config_it["config"] < 100) &
            #(config_it['state_var'] == config_it['meas_var'][0])):
            #    measurement = measurement * config_it["noise"][it]
            if np.isnan(np.sum(measurement)):
                measurement = None
        else:
            # measurement = np.nan
            measurement = None

        # Ascribe observation uncertainty
        if (np.isnan(config_it["R"])) and (measurement is not None):
            sd = [s + '_sd' for s in config_it['meas_var']]
            filter_f.R = obs.loc[it, sd].item()**2 + 0.0001

        # Predict step
        # For Kalman Filters, this only modifies the state of interest
        #print(dat)
        filter_f.predict(fx=proc_func,
                         dt=dt, dat=dat,
                         info=info, params=params, rates=rates, states=st,
                         weather_d=weather_d)

        # Ascribe model uncertainty
        if np.isnan(config_it["Q"]):
            if config_it['state_var'] == config_it['meas_var'][0]:
                # Model error equals to the non-calibrated error in the
                # full watered and fertilized experiment
                sd = [s + '_sd_md' for s in [config_it['state_var']]]
                Q = (obs.loc[it, sd].item() * filter_f.x.item())**2
            else:
                # Model error depends on experiment
                err_ = err.loc[dat <= err.dat, "r_abs_error"].iloc[0]
                Q = (err_ * filter_f.x.item())**2
        else:
            if int(config_it["id"]) >= 500:
                Q = (config_it["Q"] * filter_f.x.item())**2
            else:
                Q = (config_it["Q"])**2

        if Q == 0:
            Q = 0.0001

        filter_f.Q = Q
        # print(filter_f)

        # sigmas = save_outputs(x=sigmas, obj=filter_f,
        #                       type_out="sigmas",
        #                       dat=dat,
        #                       config_it=config_it)

        # Update step
        # For Kalman Filters, this only modifies the state of interest
        filter_f.update(z=measurement, hx=meas_func,
                        dataset=obs.loc[obs.dat == dat, :])
        filter_f.x = np.max([filter_f.x, [0]], keepdims=True)
        filter_f.x_post = filter_f.x

        # Save results
        assim = save_outputs(x=assim, obj=filter_f,
                             type_out="other",
                             config_it=config_it,
                             dat=dat, meas_func=meas_func,
                             measurement=measurement, obs=obs)

        if config_it["filt"] != "pf":
            fz(filter_f, config=config_it)
            model_upd = []
            for st in config_it["states_names"]:
                model_upd.append(np.array(getattr(filter_f.model, st), 
                                          ndmin=2))
            state = np.vstack((state, np.ravel(model_upd)))
            # print(state)
        else:
            model_upd = filter_f.x
            state = np.vstack((state, np.ravel(model_upd)))

    all_states_upd = np.hstack((state,
                                np.array(range(state.shape[0])).reshape(-1, 1)))

    return assim, all_states_upd, sigmas


# %% Main
rep = 1
def main_real(rep):

    config = pd.read_csv("./tables/runs_Filter.csv")
    config_run = config.loc[config["run"] == 1].reset_index()

    it = 0
    fail_list = list()
    rep = 1

    for it in range(config_run.shape[0]):

        config_it = config_run.loc[it, :].to_dict()

        city = config_it['city']
        calib = config_it['calib']
        exp = int(config_it['exp'])
        sensor_type = config_it['sensor_type']
        config_it['meas_var_j'] = config_it['meas_var']
        config_it['meas_var'] = config_it['meas_var'].split(",")
        config_it["rep"] = rep

        if config_it["state_var"] == "lai":
            config_it["param_pert"] = "delta"
        elif config_it["state_var"] == "w":
            config_it["param_pert"] = "Qe"
        elif config_it["state_var"] == "wf":
            config_it["param_pert"] = "alpha_F"
        elif config_it["state_var"] == "wm":
            config_it["param_pert"] = "DFmax"

        config_it["weather_pert"] = "rad"

        # Retrieve from destructive analyses errors and model performances.
        # As model performs differently in each growth cycle, experiment
        # is also subset. Includes sensors because this changes the outcome.
        errors_notCalib = pd.read_csv("./tables/results_simul/all_errors.csv")
        mask = ((errors_notCalib.loc[:, "city"] == city) &
                (errors_notCalib.loc[:, "calib"] == calib) &
                (errors_notCalib.loc[:, "exp"] == ("n0"+str(exp))) &
                (errors_notCalib.loc[:, "variable"] == config_it["state_var"]) &
                (errors_notCalib.loc[:, "model"] == "tomgro") &
                (errors_notCalib.loc[:, "sensor"] == sensor_type)
                )
        config_it["model_err"] = errors_notCalib.loc[mask,
                                                     ["dat", "variable",
                                                      "r_abs_error"]].reset_index()

        load_dataset, states_names, _, _ = choose_model(config_it)
        config_it["states_names"] = states_names
        dataset = load_dataset(city, sensor_type, calib, str(exp), "monit")
        
        config_it["seed"] = 42+rep

        try:
            assim, all_states_upd, sigmas = run_filter(dataset, config_it)

            aux.save_output(conf=config_it, type_output="updState", x=assim,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            aux.save_output(conf=config_it, type_output="allState",
                            x=all_states_upd,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            aux.save_output(conf=config_it, type_output="sigmas",
                            x=sigmas,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            outputs = pd.DataFrame(all_states_upd,
                                   columns=states_names + ['dat'])
            outputs_ = (outputs.loc[:, states_names]
                        .reset_index()
                        .melt(id_vars=['index'])
                        .rename(columns={"index": "dat"}))

            dataset = load_dataset(city, sensor_type, calib, str(exp), "summ")
            observations = aux.get_obs(dataset, states_names, [])
            error = aux.calcError(observations, outputs_, states_names)
            aux.save_output(config_it, "error_assim", error, city, calib, exp,
                            rep=rep)

            print(it)

        except:
            print(it, "fail")
            fail_list.append(it)

    print(fail_list)

def main_artif(rep):

    config = pd.read_csv("./tables/runs_Filter2.csv")
    config_run = config.loc[config["run"] == 1].reset_index()
    #config_run = config_run[0:4]

    it = 0
    # it = 5
    rep = 1
    fail_list = list()

    for it in range(config_run.shape[0]):

        config_it = config_run.loc[it, :].to_dict()

        city = config_it['city']
        calib = config_it['calib']
        exp = int(config_it['exp'])
        sensor_type = config_it['sensor_type']
        config_it['meas_var_j'] = config_it['meas_var']
        config_it['meas_var'] = config_it['meas_var'].split(",")
        config_it["rep"] = rep

        if config_it["state_var"] == "lai":
            config_it["param_pert"] = "delta"
        elif config_it["state_var"] == "w":
            config_it["param_pert"] = "Qe"
        elif config_it["state_var"] == "wf":
            config_it["param_pert"] = "alpha_F"
        elif config_it["state_var"] == "wm":
            config_it["param_pert"] = "DFmax"

        config_it["weather_pert"] = "rad"

        # Retrieve from destructive analyses errors and model performances.
        # As truth is simulated using the calibration from the last cycle,
        # exp is fixed.
        errors_notCalib = pd.read_csv("./tables/results_simul/all_errors.csv")
        mask = ((errors_notCalib.loc[:, "city"] == city) &
                (errors_notCalib.loc[:, "calib"] == calib) &
                (errors_notCalib.loc[:, "exp"] == "n07") &
                (errors_notCalib.loc[:, "variable"] == config_it["state_var"]) &
                (errors_notCalib.loc[:, "model"] == "tomgro") &
                (errors_notCalib.loc[:, "sensor"] == sensor_type)
                )
        config_it["model_err"] = errors_notCalib.loc[mask,
                                                     ["dat", "variable",
                                                      "r_abs_error"]].reset_index()

        load_dataset, states_names, _, _ = choose_model(config_it)
        config_it["states_names"] = states_names
        dataset = load_dataset(city, sensor_type, calib, str(exp), "artif_obs")

        np.random.seed(42+rep)
        config_it["seed"] = 42+rep
        config_it["noise"] = np.random.normal(1, 0.09, size=200)

        try:
            np.random.seed(config_it["seed"])
            assim, all_states_upd, sigmas = run_filter(dataset, config_it)

            aux.save_output(conf=config_it, type_output="updState", x=assim,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            aux.save_output(conf=config_it, type_output="allState",
                            x=all_states_upd,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            # aux.save_output(conf=config_it, type_output="sigmas",
            #                 x=sigmas,
            #                 city=city, calib=calib, treat=exp,
            #                 rep=rep)

            outputs = pd.DataFrame(all_states_upd,
                                   columns=states_names + ['dat'])
            outputs_ = (outputs.loc[:, states_names]
                        .reset_index()
                        .melt(id_vars=['index'])
                        .rename(columns={"index": "dat"}))

            dataset = load_dataset(city, sensor_type, calib, str(exp),
                                   "artif_truth")
            observations = aux.get_obs(dataset, states_names, [])
            error = aux.calcError(observations, outputs_, states_names)
            aux.save_output(config_it, "error_assim", error, city, calib, exp,
                            rep)

            print(it)

        except:
            print(it, "fail")
            fail_list.append(it)

    print(fail_list)


def parallel_real(config_run, config_rep, it_out):

        it = config_rep['it'][it_out]
        rep = config_rep['rep'][it_out]
        config_it = config_run.loc[it, :].to_dict()

        city = config_it['city']
        calib = config_it['calib']
        exp = int(config_it['exp'])
        sensor_type = config_it['sensor_type']
        config_it['meas_var_j'] = config_it['meas_var']
        config_it['meas_var'] = config_it['meas_var'].split(",")
        config_it["rep"] = rep

        if config_it["state_var"] == "lai":
            config_it["param_pert"] = "delta"
        elif config_it["state_var"] == "w":
            config_it["param_pert"] = "Qe"
        elif config_it["state_var"] == "wf":
            config_it["param_pert"] = "alpha_F"
        elif config_it["state_var"] == "wm":
            config_it["param_pert"] = "DFmax"

        config_it["weather_pert"] = "rad"

        # Retrieve from destructive analyses errors and model performances.
        # As model performs differently in each growth cycle, experiment
        # is also subset
        errors_notCalib = pd.read_csv("./tables/results_simul/all_errors.csv")
        mask = ((errors_notCalib.loc[:, "city"] == city) &
                (errors_notCalib.loc[:, "calib"] == calib) &
                (errors_notCalib.loc[:, "exp"] == ("n0"+str(exp))) &
                (errors_notCalib.loc[:, "variable"] == config_it["state_var"]) &
                (errors_notCalib.loc[:, "model"] == "tomgro") &
                (errors_notCalib.loc[:, "sensor"] == sensor_type)
                )
        config_it["model_err"] = errors_notCalib.loc[mask,
                                                     ["dat", "variable",
                                                      "r_abs_error"]].reset_index()

        load_dataset, states_names, _, _ = choose_model(config_it)
        config_it["states_names"] = states_names
        dataset = load_dataset(city, sensor_type, calib, str(exp), "monit")
        
        config_it["seed"] = 42+rep

        try:
            assim, all_states_upd, sigmas = run_filter(dataset, config_it)

            aux.save_output(conf=config_it, type_output="updState", x=assim,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            aux.save_output(conf=config_it, type_output="allState",
                            x=all_states_upd,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            # aux.save_output(conf=config_it, type_output="sigmas",
            #                 x=sigmas,
            #                 city=city, calib=calib, treat=exp,
            #                 rep=rep)

            outputs = pd.DataFrame(all_states_upd,
                                   columns=states_names + ['dat'])
            outputs_ = (outputs.loc[:, states_names]
                        .reset_index()
                        .melt(id_vars=['index'])
                        .rename(columns={"index": "dat"}))

            dataset = load_dataset(city, sensor_type, calib, str(exp), "summ")
            observations = aux.get_obs(dataset, states_names, [])
            error = aux.calcError(observations, outputs_, states_names)
            aux.save_output(config_it, "error_assim", error, city, calib, exp,
                            rep=rep)

            print(it)

        except:
            print(it, "fail")


def parallel_artif(config_run, config_rep, it_out):

        it = config_rep['it'][it_out]
        rep = config_rep['rep'][it_out]
        config_it = config_run.loc[it, :].to_dict()
        config_it["rep"] = rep

        city = config_it['city']
        calib = config_it['calib']
        exp = int(config_it['exp'])
        sensor_type = config_it['sensor_type']
        config_it['meas_var_j'] = config_it['meas_var']
        config_it['meas_var'] = config_it['meas_var'].split(",")

        if config_it["state_var"] == "lai":
            config_it["param_pert"] = "delta"
        elif config_it["state_var"] == "w":
            config_it["param_pert"] = "Qe"
        elif config_it["state_var"] == "wf":
            config_it["param_pert"] = "alpha_F"
        elif config_it["state_var"] == "wm":
            config_it["param_pert"] = "DFmax"

        config_it["weather_pert"] = "rad"

        # Retrieve from destructive analyses errors and model performances.
        # As truth is simulated using the calibration from the last cycle,
        # exp is fixed.
        errors_notCalib = pd.read_csv("./tables/results_simul/all_errors.csv")
        mask = ((errors_notCalib.loc[:, "city"] == city) &
                (errors_notCalib.loc[:, "calib"] == calib) &
                (errors_notCalib.loc[:, "exp"] == "n08") &
                (errors_notCalib.loc[:, "variable"] == config_it["state_var"]) &
                (errors_notCalib.loc[:, "model"] == "tomgro") &
                (errors_notCalib.loc[:, "sensor"] == sensor_type)
                )
        config_it["model_err"] = errors_notCalib.loc[mask,
                                                     ["dat", "variable",
                                                      "r_abs_error"]].reset_index()

        load_dataset, states_names, _, _ = choose_model(config_it)
        config_it["states_names"] = states_names
        dataset = load_dataset(city, sensor_type, calib, str(exp), "artif_obs")

        np.random.seed(42+rep)
        config_it["seed"] = 42+rep
        config_it["noise"] = np.random.normal(1, 0.09, size=200)

        try:
            np.random.seed(config_it["seed"])
            assim, all_states_upd, sigmas = run_filter(dataset, config_it)

            aux.save_output(conf=config_it, type_output="updState", x=assim,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            aux.save_output(conf=config_it, type_output="allState",
                            x=all_states_upd,
                            city=city, calib=calib, treat=exp,
                            rep=rep)

            # aux.save_output(conf=config_it, type_output="sigmas",
            #                 x=sigmas,
            #                 city=city, calib=calib, treat=exp,
            #                 rep=rep)

            outputs = pd.DataFrame(all_states_upd,
                                   columns=states_names + ['dat'])
            outputs_ = (outputs.loc[:, states_names]
                        .reset_index()
                        .melt(id_vars=['index'])
                        .rename(columns={"index": "dat"}))

            dataset = load_dataset(city, sensor_type, calib, str(exp),
                                   "artif_truth")
            observations = aux.get_obs(dataset, states_names, [])
            error = aux.calcError(observations, outputs_, states_names)
            aux.save_output(config_it, "error_assim", error, city, calib, exp,
                            rep)

            print(it)

        except:
            print(it, "fail")


#main_artif(1)
#main_real(1)

# # PAPER 3
# config = pd.read_csv("./tables/runs_Filter2.csv")
# config_run = config.loc[config["run"] == 1].reset_index()

# n_configs = range(config_run.shape[0])
# rep_min = 1
# rep_max = 20
# rep = range(rep_min, rep_max + 1)
# njobs = 4

# config_rep = pd.DataFrame.from_records(itertools.product(n_configs, rep),
#                                         columns = ['it', 'rep'])

# it_out = 2

# Parallel(n_jobs=njobs,
#           verbose=5)(delayed(
#               lambda x: parallel_artif(config_run, config_rep, x))(cont)
#               for cont in range(config_rep.shape[0]))

# PAPER 2
config = pd.read_csv("./tables/runs_Filter.csv")
config_run = config.loc[config["run"] == 1].reset_index()

n_configs = range(config_run.shape[0])
rep_min = 1
rep_max = 20
rep = range(rep_min, rep_max + 1)
njobs = 4

config_rep = pd.DataFrame.from_records(itertools.product(n_configs, rep),
                                        columns = ['it', 'rep'])

Parallel(n_jobs=njobs,
          verbose=5)(delayed(
              lambda x: parallel_real(config_run, config_rep, x))(cont)
              for cont in range(config_rep.shape[0]))

winsound.Beep(440, 1000)
