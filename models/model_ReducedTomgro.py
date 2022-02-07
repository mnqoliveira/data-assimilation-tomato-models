# -*- coding: utf-8 -*-
"""
Created on Sun Mar 31 11:20:25 2019.

@author: monique

All functions and parameters are extracted from Jones, Kening and
Vallejos (1999) except when indicated. Auxiliary information came from Jones
et al. 1991 and the spreadsheet provided bt Dr Jim Jones, referring to the
development of the Reduced Tomgro.

"""

import numpy as np
import pandas as pd
import auxiliary_functions.f_aux as aux

# import sys

# if not sys.warnoptions:
#     import warnings
#     warnings.simplefilter("error")

# =============================================================================
#  Notation
# =============================================================================
# time= t
# Hourly temperature = T
# Average daily temperature = Tavg
# Average temperature during daytime hours = Tdaytime


# %% Photosynthesis
def photosynthesis_hourly(co2_hour: np.ndarray, rad_hour: np.ndarray,
                          Tmed_hour: np.ndarray, info: dict,
                          params: dict, states: dict) -> np.ndarray:
    """
    Photosynthesis model.

    Parameters
    ----------
    co2_hour : np.ndarray
        Hourly CO2 concentration in the air [ppm]
    rad_hour : np.ndarray
        Photosynthetic photon flux density for all hours of the day dat.
        [micromol per m2 per second]
    Tmed_hour : np.ndarray
        Average hourly temperature for all hours of the day dat. [oC]
    info : dict
        Inputs defined by user.
    params : dict
        Model parameters.
    states : dict
        State variables.

    Returns
    -------
    np.ndarray
        Gross photosynthesis,
        hourly values to be integrated over a day. [g CH2O m-2 hour-1]

    """
    co2 = co2_hour[0]

    # maximum leaf photosynthetic rate [micromol m-2 s-1]
    if co2 <= 350.:
        LFmax = params['tau1'] * co2
    else:
        LFmax = params['tau1'] * 350 + params['tau2'] * (co2 - 350.)

    # Jones 1999, spreadsheet
    LFmax = np.round(LFmax)

    adjustment = ((info['D'] / 24) * LFmax *
                  PGRED_T(Tmed_hour=Tmed_hour, params=params)/params['K'])

# Source: Jones 1991
#    photo_process = (
#            ((1-params['m'])*LFmax + params['Qe']*params['K']*PPFD) /
#            ((1-params['m'])*LFmax + params['Qe']*params['K']*PPFD *
#             np.exp(-params['K']*states['lai']))
#            )

# Source: Jones et al 1999, spreadsheet
    #Qe = 0.084 * (1 - 0.143 * np.exp(0.0295 * (Tmed_hour - params['TQe'])))
    Qe = params['Qe']
    # print(Qe)
    photo = (((1-params['m'])*LFmax +
              Qe*params['K']*rad_hour*info['TRGH']) /
             ((1-params['m'])*LFmax + Qe*params['K']*rad_hour*info['TRGH'] *
              np.exp(-params['K']*states['lai']))
             )

    Pg = adjustment * np.log(photo)
    params['photo'] = Pg.sum()

    return Pg


# %% Respiration
def respiration_hourly(Tmed_hour: np.ndarray,
                       params: dict, states: dict) -> np.ndarray:
    """
    Respiration model.

    Parameters
    ----------
    Tmed_hour : np.ndarray
        Average hourly temperature for all hours of the day dat. [oC]
    params : dict
        Model parameters.
    states : dict
        State variables.

    Returns
    -------
    np.ndarray
        Maintenance respiration, computed hourly [g CH2O m-2 hour-1].

    """
    Rm = (
            params['Q10'] ** ((Tmed_hour - 20.) * 0.1) * (params['rm'] / 24) *
            (states['w'] - states['wm'])
            )
    params['resp'] = Rm.sum()

    return Rm


# %% Daily assimilation
def GR_net(weather_d: pd.DataFrame,
           info: dict, params: dict, states: dict) -> float:
    """
    Daily net aboveground biomass.

    Calculates daily net aboveground biomass growth rate
    based on respiration and photosynthesis, and accounting
    for efficiency and partitioning to roots.

    Parameters
    ----------
    weather_d : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day dat.
    info : dict
        Inputs defined by user.
    params : dict
        Model parameters.
    states : dict
        State variables.

    Returns
    -------
    float
        Daily net biomass. [g m-2 day-1]

    """
    rad_hour = weather_d.rad.values
    Tmed_hour = weather_d.tmean.values
    co2_hour = weather_d.co2.values

    Pg_d = photosynthesis_hourly(co2_hour=co2_hour, rad_hour=rad_hour,
                                 Tmed_hour=Tmed_hour, info=info,
                                 params=params, states=states)
    Rm_d = respiration_hourly(Tmed_hour=Tmed_hour, params=params,
                              states=states)

    GR_net_part_hourly = (Pg_d - Rm_d).reshape(-1, 24)

    GR_net_out = (params['E'] * GR_net_part_hourly.sum(axis=1) *
                  (1 - fR_N(states['n'], params)))
    GR_net_out_1d = np.ravel(GR_net_out)

    return GR_net_out_1d


# %% Auxiliary functions
def gF_Tdaytime(weather_d: pd.DataFrame, params: dict) -> float:
    """
    Accounts for the effect of critical daytime temperature in fruit growth.

    Parameters
    ----------
    weather_d : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day dat.
    params : dict
        Model parameters.

    Returns
    -------
    float
        Coefficient to modify partitioning to fruits caused by hot weather [-]

    """
    Tdaytime = aux.calc_T_daytime(weather_d=weather_d)

# Source: Jones et al., 1999, paper
#    if Tdaytime > params['T_crit']:
#        g = 1.0 - 0.154 * (Tdaytime - params['T_crit'])
#    else:
#        g = 1.0

# Source: Jones et al. 1999, spreadsheet
    g = np.max([0.09, np.min([1, 1 - 0.154 * (Tdaytime - params['T_crit'])])])
    return g


def fN_T(Tmed_hour: np.ndarray, params: dict) -> np.ndarray:
    """
    Modifies node development rate as a function of hourly temperature.

    Parameters
    ----------
    Tmed_hour : np.ndarray
        Average hourly temperature for all hours of the day dat. [oC]

    Returns
    -------
    np.ndarray
        Array of hourly coefficients to modify node development. [-]

    """
# Jones 1991
#    fN_T_x = [0.0, 9., 12., 28., 50.]
#    fN_T_y = [0., 0, 0.55, 1., 0.]
#    modifier_fN_T = np.interp(Tmed_hour, fN_T_x, fN_T_y)

# Jones 1999
    modifier_fN_T = np.minimum(0.25 + params['sl_N1']*Tmed_hour,
                               2.5 - params['sl_N2']*Tmed_hour)
    np.minimum(modifier_fN_T, 1., out=modifier_fN_T)
    # modifier_fN_T = np.minimum(modifier_fN_T, 1.)

    return modifier_fN_T


def PGRED_T(Tmed_hour: np.ndarray, params: dict) -> np.ndarray:
    """
    Modifies hourly values of photosynthesis based on daytime temperatures.

    Parameters
    ----------
    Tmed_hour : np.ndarray
        Average hourly temperature for all hours of the day dat. [oC]

    Returns
    -------
    np.ndarray
        Array of hourly coefficients to modify hourly photosynthesis. [-].

    """
# Source: Jones 1991
#    PGRED_T_x = [0.0, 9., 12., 15., 21., 28., 35., 50.]
#    PGRED_T_y = [0., 0.67, 1., 1., 1., 1., 0., 0.]
#    modifier_Pg = np.interp(Tmed_hour, PGRED_T_x, PGRED_T_y)

# Source: Jones et al 1999, spreadsheet
    modifier_Pg = np.minimum(
                            1. / (1. + 9. * np.exp(-0.5*(Tmed_hour - 10.))),
                            1. - 1./(1. + 9. *
                                     np.exp(-0.5*(Tmed_hour -
                                                  params['tmaxPg'])))
                            )

    return modifier_Pg


def fR_N(n: float, params: dict) -> float:
    """
    Fraction partitioning of biomass to roots as a function of development.

    Parameters
    ----------
    n : float
        Number of nodes. State variable n.

    Returns
    -------
    float
        Coefficient to modify partitioning to roots given
        stage of development [-]

    """
# Source: Jones et al. (1991) Corresponds to PRoot on Jones et al (1991)
#    fR_N_x = [1., 2., 9., 12., 15., 21., 30., 50., 90.]
#    fR_N_y = [0.2, 0.2, 0.2, 0.15, 0.15, 0.10, 0.07, 0.07, 0.07]
#    modifier_fR_N = np.interp(n, fR_N_x, fR_N_y)

# Source: Jones et al. (1999), spreadsheet
    modifier_fR_N = np.max([0.18 - params['sl_R'] * n, [0.02]])

    return modifier_fR_N


def lambda_Tavg(weather_d: pd.DataFrame, params: dict) -> float:
    """
    Function to reduce rate of leaf area expansion given daytime temperature.

    Parameters
    ----------
    dat : int
        Day dat to which corresponds the iteration
    weather_d : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day dat.
    params : dict
        Model parameters.

    Returns
    -------
    float
        Coefficient to modify dLAI given daytime temperature [-]

    """
# Source: Jones et al 1999, spreadsheet
    Tdaytime = aux.calc_T_daytime(weather_d)
    modifier_lambda = (45 - Tdaytime)/params["TSlop"]
    modifier_lambda = np.min([modifier_lambda, 1.])

    return modifier_lambda


def fF_Tavg(weather_d: pd.DataFrame, params: dict) -> float:
    """
    Function to modify partitioning to fruit vs. avg daily temp, T.

    Parameters
    ----------
    weather_d : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day dat.
    params : dict
        Model parameters.

    Returns
    -------
    float
        Coefficient to modify fruit
        partitioning given low daily temperatures [-]
    """
# Source: Jones et al 1999, spreadsheet
    Tavg = weather_d.tmean.mean()
    modifier_fF = np.max([0., np.min([1.,
                                      0.0625*(Tavg-params['tmin_fr_gr'])])])

    return modifier_fF


# %% States
# =============================================================================
#  State: Number of nodes
# =============================================================================
def rate_nNodes(weather_d: pd.DataFrame, params: dict) -> float:
    """
    Daily rate of node development. Integrated from hourly to daily.

    Parameters
    ----------
    weather_d : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day dat.
    params : dict
        Model parameters.

    Returns
    -------
    float
        Daily rate of node development. [number of nodes m-2 day-1]

    """
    # Hourly rate of node development.
    # Integration to daily rate is needed

    Tmed_hour = weather_d.tmean.values
    dNdt_sum = params['N_max'] * 1/24 * fN_T(Tmed_hour, params).sum()

    return dNdt_sum


# =============================================================================
#  State: Leaf area index
# =============================================================================
def rate_LAI(info: dict, params: dict, rates: dict,
             weather_d: pd.DataFrame,
             N_prev: float, LAI_prev: float) -> float:
    """
    Calculates daily rate of lai.

    Leaf area index is updated daily and depends on the enviroment, on
    inputs defined by the user and on processes as described by the model,
    as well as the current n and lai values.

    Parameters
    ----------
    dat : int
        Day dat to which corresponds the iteration
    info : dict
        Inputs defined by user.
    params : dict
        Model parameters.
    rates : dict
        Current rates of the state variables of the model,
        before iteration update.
    weather_d : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day dat.
    N_prev : float
        Number of nodes referring to day dat-1
    LAI_prev : float
        Leaf area index referring to day dat-1

    Returns
    -------
    float
        Daily rate of leaf area index. [m2 {leaves} m-2 {soil} day-1]

    """
    mask = LAI_prev <= info['LAI_max']
    dLAIdt = np.array(np.repeat(0., np.array(mask).size), ndmin=1)

    try:
        temp = (
            info['ro'] * params['delta'] *
            lambda_Tavg(weather_d, params) *
            np.exp(params['beta'] * (N_prev[mask] - params['N_b'])) *
            rates['dn_dt'] /
            (1 + np.exp(params['beta'] * (N_prev[mask] - params['N_b'])))
            )
        dLAIdt[mask] = temp
    except ValueError:
        dLAIdt

    return dLAIdt


# =============================================================================
#  State: Aboveground dry weight
# =============================================================================
def rate_aboveground(net_biomass: float,
                     info: dict, params: dict, rates: dict,
                     states: dict) -> float:
    """
    Calculates daily rate of aboveground biomass.

    Calculates rate of aboveground biomass increase based either on
    node development rate and lai or on a parameter for maximum growth.

    Parameters
    ----------
    net_biomass : float
        Daily net aboveground biomass growth
    info : dict
        Inputs defined by user.
    params : dict
        Model parameters.
    rates : dict
        Current rates of the state variables of the model,
        before iteration update.
    states : dict
        State variables.

    Returns
    -------
    float
        Daily rate of aboveground biomass. [g {DM} m-2 {soil} day-1]

    """

    mask = states['lai'] < info['LAI_max']
    p_1 = np.repeat(params['p_1'], np.array(mask).size)
    p_1[mask] = 0.

    dWdt = net_biomass - p_1 * rates['dn_dt']
    dWdt = net_biomass - p_1 * info['ro'] * rates['dn_dt']
    dWdt_max = rates['dwf_dt'] + (params['V_max'] - p_1) * info['ro'] * \
        rates['dn_dt']

    # dWdt_max = dWdt
    dW = np.min([dWdt, dWdt_max], axis=0)

    return dW


# =============================================================================
#  State: Total fruit dry weight
# =============================================================================
def rate_fruits(weather_d: pd.DataFrame,
                net_biomass: float,
                info: dict, params: dict, rates: dict, states: dict) -> float:
    """
    Calculates rate of fruit growth based on available biomass.

    Parameters
    ----------
    weather_d : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day dat.
    net_biomass : float
        Daily net aboveground biomass growth
    info : dict
        Inputs defined by user.
    params : dict
        Model parameters.
    rates : dict
        Current rates of the state variables of the model,
        before iteration update.
    states : dict
        State variables.

    Returns
    -------
    float
        Daily rate of fruit weight. [g {DM} m-2 {soil} day-1]

    """

    # Mascara tem a dimensao de N, mas W tem mais dimensoes e quebra
    # qdo faco o subset do netbiomass. Talvez aqui eu tenha que ter duas mascaras
    # uma pro tamanho de net_biomass e uma pro N. Pensar sobre
    # se existe chance de propagar mais dimensao pro N. Me resolver com
    # essa possibilidade de propagacao para decidir os proximos passos.
    mask = states['n'] > params['N_FF']
    dWfdt = np.array(np.repeat(0., np.array(mask).size), ndmin=1)

    if np.sum(mask) > 0:
        try:
            dWfdt[mask] = (
                net_biomass[mask] * params['alpha_F'] *
                fF_Tavg(weather_d, params) *
                (1 - np.exp(-params['V']*(states['n'][mask] -
                                          params['N_FF']))) *
                gF_Tdaytime(weather_d, params)
                )
        except ValueError:
            dWfdt

    return dWfdt


# =============================================================================
#  State: Mature fruit dry weight
# =============================================================================
def rate_mature(weather_d: pd.DataFrame,
                params: dict, states: dict) -> float:
    """
    Calculates rate of mature fruit growth based on available biomass.

    Parameters
    ----------
    weather_d : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day dat.
    params : dict
        Model parameters.
    states : dict
        State variables.

    Returns
    -------
    float
        Daily rate of mature fruit weight. [g {DM} m-2 {soil} day-1]

    """
    # Source: Jones et al (1999), paper
    #    DVS = params['DFmax']
    #    if states['n'] > (params['N_FF'] + params['K_F']):
    #
    #        dWmdt = DVS*(states['wf'] - states['wm'])
    #
    #    else:
    #        dWmdt = 0

    # Source: Jones et al (1999), spreadsheet
    Tavg = weather_d.tmean.mean()

    # Source: Jones et al (1999), spreadsheet
    # According to them, the following interpolation corresponds to
    # Marcelis and Koning (1995)
#   fDVS_x = [0., 9., 17., 19., 21., 23., 28., 35., 50.]
#   fDVS_y = [0., 0., 0.54, 0.7, 0.8, 1., 1., 1., 0]
#   DVS = np.interp(n, fDVS_x, fDVS_y)
    # However, this approximation is good enough
    DVS = np.min([1, 0.0714 * (Tavg - 9.)])

    mask = states['n'] > (params['N_FF'] + params['K_F'])
    dWmdt = np.array(np.repeat(0., np.array(mask).size), ndmin=1)

    mult = np.max([0., DVS])
    try:
        dWmdt[mask] = params['DFmax']*(states['wf'][mask] -
                                       states['wm'][mask]) * mult
    except ValueError:
        dWmdt

    return dWmdt


# %% Model
# =============================================================================
# Original model
# =============================================================================
def tomgro(dat: int,
           info: dict, params: dict, rates: dict, states: dict,
           weather_d: pd.DataFrame) -> dict:
    """
    Process model for biomass accumulation in tomato plants

    Obs: The order of the equations matter

    Parameters
    ----------
    dat : int
        Day dat to which corresponds the iteration
    info : dict
        Inputs defined by user.
    params : dict
        Model parameters.
    rates : dict
        Current rates of the state variables of the model,
        before iteration update.
    states : dict
        State variables.
    weather : pd.DataFrame
        Weather dataset (radiation and temperatures) for the day
        of the iteration.

    Returns
    -------
    states : dict
        Updated values of all model state variables

    """
    # print(dat)
    net_biomass = GR_net(weather_d, info, params, states)
    # print(dat, net_biomass)

    N_prev = np.array(states['n'], ndmin=1)
    rates['dn_dt'] = np.array(rate_nNodes(weather_d, params), ndmin=1)
    states['n'] = np.array(states['n'] + rates['dn_dt'], ndmin=1)

    LAI_prev = states['lai']
    rates['dlai_dt'] = rate_LAI(info, params, rates, weather_d,
                                N_prev, LAI_prev)
    states['lai'] = np.min([states['lai'] + rates['dlai_dt'],
                            np.repeat(info['LAI_max'],
                                      np.array(states['lai']).size)],
                           axis=0)

    rates['dwm_dt'] = rate_mature(weather_d, params, states)
    states['wm'] = states['wm'] + rates['dwm_dt']

    rates['dwf_dt'] = rate_fruits(weather_d, net_biomass, info,
                                  params, rates, states)
    states['wf'] = states['wf'] + rates['dwf_dt']

    rates['dw_dt'] = rate_aboveground(net_biomass, info, params, rates,
                                      states)
    states['w'] = states['w'] + rates['dw_dt']

    return states


# %% Modified versions for assimilation
# As the model is based on dictionaries for states and parameters,
# instead of numpy arrays, the variable to be assimilated
# should be easily detected as a numpy array and therefore the choice
# was to isolate it as the first argument and only output of the functions.
# This was a "limitation" of working with filterpy.

# =============================================================================
# State variable assimilated: lai
# =============================================================================
def tomgro_LAI(lai, dt, dat, info, params, rates, states,
               weather_d):
    """Process model for biomass accumulation in tomato plants"""
    result = states.copy()
    rates_temp = rates.copy()

    result['lai'] = np.max([lai, np.repeat(0, np.array(lai).size)])

    tomgro(dat, info, params, rates_temp, result, weather_d)

    return result['lai']


def tomgro_LAIz(LAI_z, dat, info, params, rates, states, weather_d):
    """Process model for biomass accumulation in tomato plants"""

    net_biomass = GR_net(weather_d, info, params, states)

    rates['dn_dt'] = rate_nNodes(weather_d, params)
    states['n'] = states['n'] + rates['dn_dt']

    states['lai'] = np.array(float(LAI_z), ndmin=1)

    rates['dwm_dt'] = rate_mature(weather_d, params, states)
    states['wm'] = states['wm'] + rates['dwm_dt']

    rates['dwf_dt'] = rate_fruits(weather_d, net_biomass, info,
                                  params, rates, states)
    states['wf'] = states['wf'] + rates['dwf_dt']

    rates['dw_dt'] = rate_aboveground(net_biomass, info, params, rates,
                                      states)
    states['w'] = states['w'] + rates['dw_dt']

    return None


# =============================================================================
# Modified for UKF. State variable: w
# =============================================================================
def tomgro_W(w, dt, dat, info, params, rates, states, weather_d):
    """
    Process model for biomass accumulation in tomato plants

    Since the other states will not be updated in this run, this run should be
    paired with the Wz one.
    Preciso do return do result w pra salvar e rodar os demais. No modelo
    full, como eu estou atualizando direto nos estados de entrada da funcao,
    o retorno nao eh necessario. Nesse caso aqui eu preciso do retorno porque
    a atualizacao nao esta acontecendo no dicionario
    """
    result = states.copy()
    rates_temp = rates.copy()

    result['w'] = np.max([w, np.repeat(0, np.array(w).size)])

    tomgro(dat, info, params, rates_temp, result, weather_d)

    #outputs = proc_func(st_pt[var], 1, dat, i, p, r, st, weather)

    return result['w']


def tomgro_Wz(W_z, dat, info, params, rates, states, weather_d):
    """Process model for biomass accumulation in tomato plants"""
    # Instead of calculating aboveground biomass state, it uses the updated
    # value and calculates other states

    net_biomass = GR_net(weather_d, info, params, states)

    N_prev = states['n']
    rates['dn_dt'] = rate_nNodes(weather_d, params)
    states['n'] = states['n'] + rates['dn_dt']

    LAI_prev = states['lai']
    rates['dlai_dt'] = rate_LAI(info, params, rates, weather_d,
                                N_prev, LAI_prev)
    states['lai'] = min(states['lai'] + rates['dlai_dt'],
                        info['LAI_max'])

    rates['dwm_dt'] = rate_mature(weather_d, params, states)
    states['wm'] = states['wm'] + rates['dwm_dt']

    rates['dwf_dt'] = rate_fruits(weather_d, net_biomass, info,
                                  params, rates, states)
    states['wf'] = states['wf'] + rates['dwf_dt']

    # rates['dw_dt'] = rate_aboveground(net_biomass, info, params, rates,
    #                                   states)
    states['w'] = float(W_z)

    return None


# =============================================================================
# Modified for UKF. State variable: wf
# =============================================================================
def tomgro_Wf(wf, dt, dat, info, params, rates, states, weather_d):
    """
    Process model for biomass accumulation in tomato plants

    Since the other states will not be updated in this run, this run should be
    paired with the Wfz one.
    Preciso do return do result wf pra salvar e rodar os demais. No modelo
    full, como eu estou atualizando direto nos estados de entrada da funcao,
    o retorno nao eh necessario. Nesse caso aqui eu preciso do retorno porque
    a atualizacao nao esta acontecendo no dicionario
    """
    result = states.copy()
    rates_temp = rates.copy()

    result['wf'] = np.max([wf, np.repeat(0, np.array(wf).size)])

    tomgro(dat, info, params, rates_temp, result, weather_d)

    #outputs = proc_func(st_pt[var], 1, dat, i, p, r, st, weather)

    return result['wf']


def tomgro_Wfz(Wf_z, dat, info, params, rates, states, weather_d):
    """Process model for biomass accumulation in tomato plants"""
    # Instead of calculating aboveground biomass state, it uses the updated
    # value and calculates other states

    net_biomass = GR_net(weather_d, info, params, states)

    N_prev = states['n']
    rates['dn_dt'] = rate_nNodes(weather_d, params)
    states['n'] = states['n'] + rates['dn_dt']

    LAI_prev = states['lai']
    rates['dlai_dt'] = rate_LAI(info, params, rates, weather_d,
                                N_prev, LAI_prev)
    states['lai'] = min(states['lai'] + rates['dlai_dt'],
                        info['LAI_max'])

    rates['dwm_dt'] = rate_mature(weather_d, params, states)
    states['wm'] = states['wm'] + rates['dwm_dt']

    # rates['dwf_dt'] = rate_fruits(weather_d, net_biomass, info,
    #                               params, rates, states)
    states['wf'] = np.array(float(Wf_z), ndmin=1)

    rates['dw_dt'] = rate_aboveground(net_biomass, info, params, rates,
                                      states)
    states['w'] = states['w'] + rates['dw_dt']

    return None


# =============================================================================
# Modified for UKF. State variable: wm
# =============================================================================
def tomgro_Wm(wm, dt, dat, info, params, rates, states, weather_d):
    """
    Process model for biomass accumulation in tomato plants

    Since the other states will not be updated in this run, this run should be
    paired with the Wfz one.
    Preciso do return do result wf pra salvar e rodar os demais. No modelo
    full, como eu estou atualizando direto nos estados de entrada da funcao,
    o retorno nao eh necessario. Nesse caso aqui eu preciso do retorno porque
    a atualizacao nao esta acontecendo no dicionario
    """
    result = states.copy()
    rates_temp = rates.copy()

    result['wm'] = wm

    tomgro(dat, info, params, rates_temp, result, weather_d)

    #outputs = proc_func(st_pt[var], 1, dat, i, p, r, st, weather)

    return result['wm']


def tomgro_Wmz(Wm_z, dat, info, params, rates, states, weather_d):
    """Process model for biomass accumulation in tomato plants"""
    # Instead of calculating aboveground biomass state, it uses the updated
    # value and calculates other states

    net_biomass = GR_net(weather_d, info, params, states)

    N_prev = states['n']
    rates['dn_dt'] = rate_nNodes(weather_d, params)
    states['n'] = states['n'] + rates['dn_dt']

    LAI_prev = states['lai']
    rates['dlai_dt'] = rate_LAI(info, params, rates, weather_d,
                                N_prev, LAI_prev)
    states['lai'] = min(states['lai'] + rates['dlai_dt'],
                        info['LAI_max'])

    rates['dwm_dt'] = rate_mature(weather_d, params, states)
    states['wm'] = np.array(float(Wm_z), ndmin=1)

    rates['dwf_dt'] = rate_fruits(weather_d, net_biomass, info,
                                  params, rates, states)
    states['wf'] = states['wf'] + rates['dwf_dt']

    rates['dw_dt'] = rate_aboveground(net_biomass, info, params, rates,
                                      states)
    states['w'] = states['w'] + rates['dw_dt']

    return None


# =============================================================================
# Modified fo UKF. State variable: n
# =============================================================================
def tomgro_N(n, dt, dat, info, params, rates, states, weather_d):
    """
    Process model for biomass accumulation in tomato plants

    Since the other states will not be updated in this run, this run should be
    paired with the Wz one.
    Preciso do return do result w pra salvar e rodar os demais. No modelo
    full, como eu estou atualizando direto nos estados de entrada da funcao,
    o retorno nao eh necessario. Nesse caso aqui eu preciso do retorno porque
    a atualizacao nao esta acontecendo no dicionario
    """
    result = states.copy()
    rates_temp = rates.copy()

    result['n'] = n

    tomgro(dat, info, params, rates_temp, result, weather_d)

    return result['n']


def tomgro_Nz(N_z, dat, info, params, rates, states, weather_d):
    """Process model for biomass accumulation in tomato plants"""
    # Instead of calculating aboveground biomass state, it uses the updated
    # value and calculates other states

    net_biomass = GR_net(weather_d, info, params, states)

    N_prev = states['n']
    # rates['dn_dt'] = rate_nNodes(weather, params)
    states['n'] = N_z

    LAI_prev = states['lai']
    rates['dlai_dt'] = rate_LAI(info, params, rates, weather_d,
                                N_prev, LAI_prev)
    states['lai'] = np.min([states['lai'] + rates['dlai_dt'],
                            info['LAI_max']])

    rates['dwm_dt'] = rate_mature(weather_d, params, states)
    states['wm'] = states['wm'] + rates['dwm_dt']

    rates['dwf_dt'] = rate_fruits(weather_d, net_biomass, info,
                                  params, rates, states)
    states['wf'] = states['wf'] + rates['dwf_dt']

    rates['dw_dt'] = rate_aboveground(net_biomass, info, params, rates,
                                      states)
    states['w'] = states['w'] + rates['dw_dt']

    return None


# =============================================================================
# Modified for UKF. State variable: lai, w
# =============================================================================
def tomgro_LAI_W(lai, w, dt, dat, info, params, rates, states,
                 weather_d):
    """Process model for biomass accumulation in tomato plants"""
    result = states.copy()
    rates_temp = rates.copy()

    result['lai'] = lai
    result['w'] = w

    result = tomgro(dat, info, params, rates_temp, result,
                    weather_d)

    return ([result['lai'], result['w']])


def tomgro_LAIz_Wz(LAI_z, W_z, dat, info, params, rates, states,
                   weather_d):
    """Process model for biomass accumulation in tomato plants"""

    net_biomass = GR_net(weather_d, info, params, states)

    N_prev = states['n']
    rates['dn_dt'] = rate_nNodes(weather_d, params)
    states['n'] = states['n'] + rates['dn_dt']

    LAI_prev = states['lai']
    rates['dlai_dt'] = rate_LAI(info, params, rates, weather_d,
                                N_prev, LAI_prev)
    states['lai'] = LAI_z

    rates['dwm_dt'] = rate_mature(weather_d, params, states)
    states['wm'] = states['wm'] + rates['dwm_dt']

    rates['dwf_dt'] = rate_fruits(weather_d, net_biomass, info,
                                  params, rates, states)
    states['wf'] = states['wf'] + rates['dwf_dt']

    rates['dw_dt'] = rate_aboveground(net_biomass, info, params, rates,
                                      states)
    states['w'] = W_z

    return None
