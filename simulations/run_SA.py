# -*- coding: utf-8 -*-
"""
Created on Tue Jan  5 10:23:31 2021

@author: Monique
"""

import os

import numpy as np
import pandas as pd

import f_aux as aux
import run_Model_Simple as simple
import run_Model_ReducedTomgro as tomgro
import run_Model_Vanthoor as vanthoor

from SALib.sample import saltelli
from SALib.analyze import sobol
from joblib import Parallel, delayed

# %% Model blind

def generateSplits(n_splits, len_weather):

    len_split = int(len_weather/n_splits)

    splits = [list(range(((x-1)*len_split + 1), (x*len_split + 1)))
              for x in range(1, n_splits+2)]

    splits_mod = [[element for element in sublist if element <= len_weather]
                  for sublist in splits]

    splits_mod = [sublist for sublist in splits_mod if len(sublist) > 0]

    return splits_mod


def loadOutSA(config, states_names):

    path = ('../tables/results_SA/case_' + str(config.case) + '/')

    pq_files = os.listdir(path)
    pq_files = [s for s in pq_files if "outpart" in s]
    pq_files = [s for s in pq_files if config.model in s]
    pq_files = [s for s in pq_files if str(config.id).zfill(3) in s]
    pq_files = [path + s for s in pq_files]
    selected_paths = [s for s in pq_files if int(s[-11:-8]) < 60]

    outSA = np.zeros([1, len(states_names) + 2])
    fileSA = pq_files[0]
    for fileSA in selected_paths:

        out_pd = pd.read_parquet(fileSA).to_numpy()
        outSA = np.vstack((outSA, out_pd))

    mask = np.sum(outSA, axis = 1) != 0
    outSA = outSA[mask]

    return outSA


def saveSI(x, problem, states, config, name, path, dat):

        x_mod = np.hstack([
            np.hstack(problem['names'] * 4).reshape(-1, 1),
            x])
        out_pd = pd.DataFrame(x_mod, columns=["factors"] + states + ['index'])
        path_out = ('../tables/results_SA/si/case' +
                    str(config.case) + '/' +
                    config.model + '_' + name +
                    '_path' + str(int(path)).zfill(3) +
                    '_dat' + str(int(dat)).zfill(3) +
                    "_id" + str(config.id).zfill(3) +
                    ".csv")
        out_pd.to_csv(path_out, index=False)

        return None


def chooseModel(model):

    if model == "tomgro":
        load_dataset = tomgro.load_dataset
        f_run = tomgro.run_simul
        states = ['n', 'lai', 'w', 'wf', 'wm']

    elif model == "simple":
        load_dataset = simple.load_dataset
        f_run = simple.run_simul
        states = ['tt_sum', 'f_solar', 'biomass', 'plant_yield']
        #states = ['biomass', 'f_solar', 'tt_sum', 'plant_yield']

    elif model == "vanthoor":
        load_dataset = vanthoor.load_dataset
        f_run = vanthoor.run_simul
        states = ['lai', 'w', 'wf', 'wm']

    return load_dataset, f_run, states


def prepareProblem(model_p, params_file, n_samples):
    """
    Create the sampled parameter values.

    Parameters
    ----------
    params_file : TYPE
        DESCRIPTION.
    n_samples : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """

    dict_limits = aux.retrieve_limits(filename=params_file, model_name=model_p)
    bounds = dict_limits[0]
    problem = {'num_vars': len(bounds.keys()),
               'names': list(bounds.keys()),
               'bounds': list(bounds.values())}
    problem['dists'] = dict_limits[1]
    problem['sd2'] = dict_limits[2]
    np.random.seed(42)
    param_values = saltelli.sample(problem=problem, N=n_samples,
                                   calc_second_order=False)
    param_values.shape

    return[problem, param_values]


def runModel(model, dataset):

    _, f_run, states_names = chooseModel(model)

    info = dataset['info']
    params = dataset['params']
    states_0 = dataset['states']
    weather = dataset['weather']

    if model == "simple":
        impacts = dataset['impacts'].copy()
        states = states_0.copy()
        weather = aux.convert_rad(weather, "MJg")

        # Simple simulations
        state = f_run(info, params, states, impacts, weather)
        mask = [x in states_names for x in list(states_0.keys())]
        state_mod = state[:, mask]

    elif model == "tomgro":

        rates = {'dn_dt': 0.0, 'dlai_dt': 0.0, 'dw_dt': 0.0,
                 'dwf_dt': 0.0, 'dwm_dt': 0.0}
        states = states_0.copy()
        weather = aux.convert_rad(weather, "mmolPAR")

        # Reduced TOMGRO simulations
        state_tup = f_run(info, params, rates, states, weather)
        state = state_tup[0]
        state_mod = state.reshape((-1, 5))

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


# %% Case 0
def prepFactCase0(param_values, it_sample, problem, weather0):

    weather = weather0.copy()

    for it_par in range(len(problem["names"])):
        fac_it = problem['names'][it_par]
        if fac_it in list(weather0.columns):
            # pert = problem['sd2'][it_par]

            if (problem['dists'][it_par] == "norm"):
                new_mean = param_values.iloc[it_sample, it_par]
                np.random.seed(it_sample)
            #     pert_samp = np.random.normal(new_mean, pert,
            #                                   weather0.shape[0])

            # elif (problem['dists'][it_par] == "unif"):
            #     pert_samp = np.random.uniform(pert[0], pert[1],
            #                                   weather0.shape[0])

            pert_samp = new_mean

            weather.loc[:, fac_it] = weather0.loc[:, fac_it].copy() + \
                weather0.loc[:, fac_it].copy() * pert_samp

            # Bound negative values for CO2 and radiation
            if (fac_it == "co2") or (fac_it == "rad"):
                mask_neg = weather.loc[:, fac_it] < 0.
                weather.loc[mask_neg, fac_it] = 0.

    return weather


def runFull0(it_sample, model, problem, param_values, dataset):
    """
    Case 0: perturbation in actual weather
    In this case, the weather is slightly perturbed to simulate
    variability within the greenhouse as well as measurement
    errors.
    So it is comprised of a normal simulation, but with the weather
    perturbed by errors sampled from a distribution which has their
    parameters previously specified.

    Parameters
    ----------
    def runCase0 : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """

    # Load other parameters
    weather0 = dataset['weather'].copy()

    # Modify factors according to perturbations and run model
    weather = prepFactCase0(param_values, it_sample, problem, weather0)
    dataset["weather"] = weather
    outSim = runModel(model, dataset)

    dataset["weather"] = weather0

    return it_sample, outSim


def runCase0(it_sample, model, problem, param_values,
             dataset, row_id=[-1]):

    it_sample, outSim = runFull0(it_sample, model, problem,
                                 param_values, dataset)

    return it_sample, outSim[row_id]


# %% Case 1
def prepFactCase1(param_values, it_sample, params, info, problem):

    sa_update = dict(zip(problem['names'],
                     param_values.loc[it_sample, :]))

    params.update(sa_update)
    info.update(sa_update)

    return params, info


def runFull1(it_sample, model, problem, param_values, dataset):
    """
    In this case, all factors are sampled simultaneously.
    Since there are several weather cases, the output sensibility has
    to be averaged for each case.

    If weather is sampled as a codified discrete variable, it interferes
    in the importance calculation and therefore should be included by averaging
    importances

    Parameters
    ----------
    def runCase1 : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """

    params = dataset['params'].copy()
    info = dataset['info'].copy()

    dataset['params'], dataset['info'] = prepFactCase1(param_values,
                                                       it_sample, params,
                                                       info, problem)

    outSim = runModel(model, dataset)

    return it_sample, outSim


def runCase1(it_sample, model, problem, param_values, dataset, row_id=[-1]):

    it_sample, outSim = runFull1(it_sample, model, problem,
                                 param_values, dataset)
    outSim[:, 0]

    return it_sample, outSim[row_id]


# %% Masks
def prepMask(config, njobs, dataset, factors, problem):

    _, _, states_names = chooseModel(config.model)

    # Determine the upper and lower values of parameters evaluated to save
    # their sampling results

    # Case 0 = 0.7 porque distribuicao eh normal, Case 1= min_max
    if config.case == 0:
        param_min = factors.apply(lambda x: x[(np.quantile(x, 0.0001) - x)
                                              .abs().argmin()]).values
        param_max = factors.apply(lambda x: x[(np.quantile(x, 0.9999) - x)
                                              .abs().argmin()]).values
        mask = ((factors.values <= param_min) | (factors.values >= param_max))
        mask = mask.any(axis=1)

    else:
        param_min = factors.min().values
        param_max = factors.max().values

        mask = ((factors.values == param_min) | (factors.values == param_max))
        mask = mask.any(axis=1)

    params_mask = factors[mask].reset_index(drop=True)

    params_file_name = ('../tables/results_SA/case_' + str(config.case) + '/' +
                        config.model + '_paramsMask_' + config.city +
                        "_id" + str(config.id).zfill(3) +
                        ".csv")
    params_mask.to_csv(params_file_name, index=False)

    return params_mask


def runMask(config, njobs, dataset, params_mask, problem):

    _, _, states_names = chooseModel(config.model)

    if config.case == 0:

        outMask = Parallel(n_jobs=njobs,
                           verbose=1)(delayed(
            lambda x: runFull0(x, config.model, problem,
                               params_mask, dataset))(it_sample)
            for it_sample in range(params_mask.shape[0]))

    elif config.case == 1:

        paths = dataset['paths']
        if config.city == "cps":
            dataset['weather'] = pd.read_csv(paths[2])
        else:
            dataset['weather'] = pd.read_csv(paths[0])

        outMask = Parallel(n_jobs=njobs,
                           verbose=1)(delayed(
            lambda x: runFull1(x, config.model, problem,
                               params_mask, dataset))(it_sample)
            for it_sample in range(params_mask.shape[0]))

        outMask = sorted(outMask, key=lambda x: x[0])

    for it in range(len(outMask)):
        outSim = outMask[it][1]

        output_file_name = ('../tables/results_SA/mask_case_' +
                            str(config.case) + '/' +
                            config.model + '_' + config.city +
                            "_it" + str(it) +
                            "_id" + str(config.id).zfill(3) +
                            '.csv')
        outputs = pd.DataFrame(outSim,
                               columns=states_names)
        outputs.to_csv(output_file_name, index=False)

    return None


# %% SA functions
def prepSA(config, params_file, weather_filt):

    load_dataset, _, _ = chooseModel(config.model)

    problem_full = prepareProblem(config.model, params_file, config.n_samples)
    problem = problem_full[0]
    param_values = problem_full[1]

    if config.case == 0:

        dataset = load_dataset(city=config.city, sensor=config.sensor_type,
                               calib=config.city, treat=str(config.exp),
                               config_obs=None)

    elif config.case == 1:

        dataset = load_dataset(city=config.city, sensor=None,
                               calib="fixed", treat=None,
                               config_obs=None)
        dataset['paths'] = weather_filt

    params_file_name = ('../tables/results_SA/case_' + str(config.case) + '/' +
                        config.model + '_factors_' + config.city +
                        "_id" + str(config.id).zfill(3) +
                        '.csv')
    params = pd.DataFrame(param_values, columns=[problem["names"]])
    params.to_csv(params_file_name, index=False)

    return problem, params, dataset


def calcSI(config, outSA, njobs, problem):

    _, _, states = chooseModel(config.model)

    factors = problem["names"]

    dats = np.unique(outSA[:, len(states)])
    paths = np.unique(outSA[:, len(states) + 1])

    comb = aux.expand_grid({'dats': dats, 'paths': paths})

    def SI_it(it, comb, config, outSA, factors, problem):

        S1 = np.zeros((len(factors), len(states)))
        S1_conf = np.zeros((len(factors), len(states)))
        ST = np.zeros((len(factors), len(states)))
        ST_conf = np.zeros((len(factors), len(states)))

        dat = comb.dats[it]
        path = comb.paths[it]

        for it_var, var_out in enumerate(states):

            mask1 = outSA[:, len(states)] == dat
            mask2 = outSA[:, len(states) + 1] == path

            mask = mask1 & mask2

            Y = outSA[mask, it_var]
            Si = sobol.analyze(problem, Y,
                               calc_second_order=False,
                               conf_level=0.95, print_to_console=False)
            S1[:, it_var] = list(Si['S1'])
            S1_conf[:, it_var] = list(Si['S1_conf'])
            ST[:, it_var] = list(Si['ST'])
            ST_conf[:, it_var] = list(Si['ST_conf'])

        S_all = np.vstack([
            np.hstack([S1, np.repeat("S1",
                                     S1.shape[0]).reshape(-1, 1)]),
            np.hstack([S1_conf, np.repeat("S1_conf",
                                          S1.shape[0]).reshape(-1, 1)]),
            np.hstack([ST, np.repeat("ST",
                                     S1.shape[0]).reshape(-1, 1)]),
            np.hstack([ST_conf, np.repeat("ST_conf",
                                          S1.shape[0]).reshape(-1, 1)])
            ])

        saveSI(S_all, problem, states, config, "si", path, dat)

    Parallel(n_jobs=njobs,
             verbose=3)(delayed(
                 lambda x: SI_it(x, comb, config, outSA, factors, problem))(it)
                 for it in range(comb.shape[0]))


def runSA(config, njobs, problem, factors, dataset, row_ids):

    _, _, states_names = chooseModel(config.model)

    if config.case == 0:

        output = Parallel(n_jobs=njobs,
                          verbose=1)(delayed(
            lambda x: runCase0(x, config.model, problem,
                               factors, dataset, row_ids))(it_sample)
            for it_sample in range(factors.shape[0]))

        output = sorted(output, key=lambda x: x[0])

        outRuns = np.vstack([x[1] for x in output])
        outSA = np.hstack([outRuns,
                             np.hstack([row_ids] *
                                       factors.shape[0]).reshape(-1, 1),
                             np.repeat(0, outRuns.shape[0]).reshape(-1, 1)
                             ])

        path_out = ('../tables/results_SA/case_' + str(config.case) + '/' +
                    config.model + '_outpart_' + config.city +
                    "_id" + str(config.id).zfill(3) +
                    "_path" + str(0).zfill(3)+
                    ".parquet")
        cols_outSA = states_names + ['dat'] + ['weather_id']
        out_pd = pd.DataFrame(outSA, columns=cols_outSA)
        out_pd.to_parquet(path_out)


    elif config.case == 1:

        paths = dataset['paths']
        # States, row_id, weather_id
        outOrgAll = np.zeros([1, len(states_names) + 2])

        path_id = 0
        path = paths[path_id]

        for path_id, path in enumerate(paths[0:], 0):
            print("path_id: ", path_id)

            dataset['weather'] = pd.read_csv(path)

            # Run simulations
            output = Parallel(n_jobs=njobs,
                              verbose=1)(delayed(
                lambda x: runCase1(x, config.model, problem,
                                   factors, dataset, row_ids))(it_sample)
                for it_sample in range(factors.shape[0]))

            output = sorted(output, key=lambda x: x[0])

            outRuns = np.vstack([x[1] for x in output])
            outRuns = np.hstack([outRuns,
                                 np.hstack([row_ids] *
                                           factors.shape[0]).reshape(-1, 1),
                                 np.repeat(path_id,
                                           outRuns.shape[0]).reshape(-1, 1)
                                 ])

            path_out = ('../tables/results_SA/case_' + str(config.case) + '/' +
                        config.model + '_outpart_' + config.city +
                        "_id" + str(config.id).zfill(3) +
                        "_path" + str(path_id).zfill(3)+
                        ".parquet")
            cols_outSA = states_names + ['dat'] + ['weather_id']
            out_pd = pd.DataFrame(outRuns, columns=cols_outSA)
            out_pd.to_parquet(path_out)

            outOrgAll = np.vstack((outOrgAll, outRuns))

        if len(np.unique(outOrgAll[:, len(states_names) + 1])) == len(paths):
            outSA = outOrgAll[1:, :]
        else:
            outSA = loadOutSA(config, states_names)

    return outSA, states_names


def main():

    path_unc = '../data/weather/unc/all/'
    weather_files = os.listdir(path_unc)

    njobs = 6

    config = pd.read_csv("../tables/runs_SA.csv")
    config_run = config.loc[config["run"] == 1].reset_index()
    #config_run = config_run.loc[0, :]
    it = 0

    for it in range(config_run.shape[0]):

        # model = config_run.model[it]
        # case = config_run.case[it]
        # n_samples = config_run.n_samples[it]
        # config = config_run.config[it]
        # weather_loc = config_run.city[it]
        # experiment = str(config_run.exp[it])
        # sensor_type = config_run.sensor_type[it]

        config = config_run.loc[it, :]
        #config = config_run
        #config.n_samples = 10

        params_file = "../tables/parameters_inputs/params_limits_case" + \
            str(config.case) + ".csv"

        if (config.model == "tomgro") or (config.model == "vanthoor"):
            frequency = "hourly"
        elif config.model == "simple":
            frequency = "daily"

        weather_filt = [s for s in weather_files if config.city in s]
        weather_filt = [s for s in weather_filt if frequency in s]
        weather_filt = [path_unc + s for s in weather_filt]
        #weather_filt = weather_filt[0:2]

        problem, factors, dataset = prepSA(config=config,
                                           params_file=params_file,
                                           weather_filt=weather_filt)

        # Note that this splitting relies on knowing the length of the weather
        # and for case 1, this set will be replaced. This is dealt with by
        # using the fixed temporary weather set with the same length as
        # the others, but there may be a problem if more than one length
        # is tried.
        n_splits = config.n_splits
        len_weather = dataset['weather'].loc[:, 'dat'].unique()
        len_weather = len([x for x in len_weather if x > 0])
        splits = generateSplits(n_splits, len_weather)
        row_ids = np.array([x for y in splits for x in y if x == max(y)])

        outSA, states_names = runSA(config, njobs, problem, factors,
                                    dataset, row_ids)

        # cols_outSA = states_names + ['dat'] + ['weather_id']
        # out_pd = pd.DataFrame(outSA, columns=cols_outSA)
        # path_out = ('../tables/results_SA/case' +
        #             str(config.case) + '/' +
        #             config.model + '_outputs_' + config.city +
        #             "_id" + str(config.id).zfill(3) +
        #             ".parquet")
        # out_pd.to_csv(path_out, index=False)
        # out_pd.to_parquet(path_out)

        _, _, states_names = chooseModel(config.model)
        outSA = loadOutSA(config, states_names)
        calcSI(config, outSA, np.min((njobs, 10)), problem)

        # params_mask = prepMask(config, njobs, dataset, factors,
        #                        problem)
        # runMask(config, njobs, dataset, params_mask, problem)

        print("it", it)

if __name__ == "__main__":
    main()
