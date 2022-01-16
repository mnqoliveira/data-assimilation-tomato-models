"""

Created on Oct 1 2019

@author: monique

Model: SIMPLE (Zhao et al 2019)

"""

import numpy as np

# %%  Auxiliary functions
def f_impact_co2(info, params):

    if (info["co2"] >= 700):
        stress = 1 + params["S_CO2"] * 350/100

    else:
        stress = np.max((1, (params["S_CO2"]*info["co2"]*0.01 + 1 -
                             0.01*350*params["S_CO2"])))

    return stress


def f_impact_temp(t_avg, params):

    if t_avg < params["T_base"]:
        impact = 0.

    elif (t_avg >= params["T_base"] and t_avg < params["T_opt"]):
        impact = ((t_avg - params["T_base"]) /
                  (params["T_opt"] - params["T_base"]))

    elif (t_avg >= params["T_opt"]):
        impact = 1.

    return impact


def f_stress_heat(tmax, params):

    if tmax <= params["T_heat"]:
        stress = 1.

    elif (tmax > params["T_heat"] and tmax <= params["T_ext"]):
        stress = np.max((0, 1. - ((tmax - params["T_heat"]) /
                                  (params["T_ext"] - params["T_heat"]))))

    elif (tmax > params["T_ext"]):
        stress = 0.

    return stress


def f_arid(info, weather_d):
    # Since the crop is irrigated, and there is no stress,
    # I'm postponing this step of the code
    arid = 0.

    return arid


def f_stress_water(params, info, weather_d):

    stress = np.max((0, 1 - params['S_water'] * f_arid(info, weather_d)))

    return stress


def f_solar(dat, info, params, states):
    """
    fSolar affects growth by influencing solar radiation interception
    fSolar for leaf growth and senescence period
    """

    stress_water = states["f_s_water"]
    fSolarA = (
            info["f_solarMax"] / (
                    1 + np.exp(-0.01 *
                               (states['tt_sum'] - params['I50A'])
                               ))
              )
    # Slight difference in implementation as in the original model,
    # statesI50B refers to the day before the previous, differently from
    # this case.
    fSolarB = (
            info["f_solarMax"] / (
                    1 + np.exp(0.01 *
                               (states['tt_sum'] - (params['T_sum'] -
                                states["I50B"])
                                ))
               ))

    fSolar = np.min((1, fSolarA, fSolarB)) * np.min((1, stress_water))

    return fSolar


def f_solar_water(stress_water):

    if stress_water < 0.1:
        f_s_water = 0.9 + stress_water
    else:
        f_s_water = 1

    return f_s_water


def f_i50b(dat, states, params, impacts):
    """
    Cumulative temperature required to reach 50% of radiation
    interception during canopy senescence
    """

    i_50b_h = states["I50B"] + (params["I50_maxH"] *
                                (1. - impacts["heat"]))

    i_50b_w = states["I50B"] + (params["I50_maxW"] *
                                (1. - impacts["drought"]))

    i50b = np.max((0, i_50b_h, i_50b_w))

    return i50b


def f_thermal_time_sum(weather_d, params):

    # The function doesn't use tavg as an argument because it may calculate
    # from the previous day
    # t_avg is the daily mean temperature
    # t_base is the base temperature for phenological development and
    # also for crop growth.

    t_avg = 0.5 * (float(weather_d.tmax) +
                   float(weather_d.tmin))

    tt_sum = np.max(((t_avg - params["T_base"]), 0.))

    return tt_sum


# %% Model
def impacts_calc(info, weather_d, params, impacts):

    tmax = float(weather_d.tmax)
    tmin = float(weather_d.tmin)
    co2 = float(weather_d.co2)
    tavg = 0.5 * (tmax + tmin)

    info["co2"] = co2
    impacts['CO2'] = f_impact_co2(info, params)
    impacts['temp'] = f_impact_temp(t_avg=tavg, params=params)

    impacts['drought'] = f_stress_water(params, info, weather_d)
    impacts['heat'] = f_stress_heat(tmax=tmax, params=params)

    impact = (impacts['CO2'] * impacts['temp'])
    stress = np.min((impacts['drought'], impacts['heat']))

    return impact, stress


def simple(dt, dat, info, weather, params, states, impacts):

    weather_d = weather.loc[weather.dat == dat, :]

    impact, stress = impacts_calc(info, weather_d, params, impacts)

    radiation = float(weather_d.rad)

    if dat > 1:
        weather_prev = weather.loc[weather.dat == (dat - 1), :]
        states['biomass'] = states['biomass'] + states['dBiomass']*dt
        states['tt_sum'] = (f_thermal_time_sum(weather_prev, params) +
                            states["tt_sum"])
        states['f_solar'] = f_solar(dat, info, params, states)

    states['f_s_water'] = f_solar_water(impacts['drought'])
    states['I50B'] = f_i50b(dat, states, params, impacts)

    states['dBiomass'] = 1*(radiation * states['f_solar'] *
                            params["RUE"] * impact * stress)

    states['plant_yield'] = states['biomass']*params['HI']

    return

