"""
 -*- coding: utf-8 -*-.

Created on Mon Apr 13 10:11:00 2020

@author: Monique

Implemented from Vanthoor 2011 and the suplementary material
"""

import numpy as np
import pandas as pd


# %% State variables of the model
def rateTSCan(params: dict, states: dict) -> float:
    """
    Temperature Sum [oC d]

    TSCan is set from negative in the vegetative phase
    and to 0oC d at the start of the generative phase
    (i.e. the ﬁrst fruit set).

    When TSCan exceeds 0 oC d, the carbohydrate distribution to
    the fruits increases linearly from zero till its full potential is
    reached at the temperature sum TSSumEnd . At values higher
    than TSSumEnd, the carbohydrate distribution to the fruits remains at its
    potential value.

    Parameters
    ----------
    params: dictionary
        tau: float
            Time constant
    states: dictionary
        TCan: float
            TCan is the simulated or measured canopy temperature. [oC]

    Returns
    -------
    tsCan_: float
        Development rate of the plant, expressed as the time
        derivative of the temperature sum [oC].
    """
    tsCan_ = (1/params['tau'])*states["TCan"]

    return tsCan_


def rateCBuf(co2: float, par: float, params: dict,
             rates: dict, states: dict) -> float:
    """
    Rate of increase of carbohydrates in the buffer

    During the light period, carbohydrates produced by
    photosynthesis are stored in the buffer and, whenever
    carbohydrates are available in the buffer, carbohydrates ﬂow
    to the plant organs. This carbohydrate ﬂow stops when the
    buffer approaches its lower limit. When the buffer approaches
    its upper limit, further carbohydrates cannot be stored and
    photosynthesis will be inhibited.

    Parameters
    ----------
    co2 : float
        Carbon dioxide concentration on air [μmol {CO2} mol-1 {air}]
    par : float
        Photosynthetic active radiation [μmol {photons} m-2 s-1]
    params : dict
        Parameters saved as model constants
    rates : dict
        Auxiliary variable including rates and
            flows for the different fruit development stages
    states : dict
        State variables of the model

    Returns
    -------
    float
        Rate of accumulation of carbohydrates in the buffer [mg m-2 s-1]

    """

    # These rates relate to the carbs available in the buffer by the maximum
    # value available for the buffer. So in theory even if all of them
    # are maximum, they would be compatible. However, the buffer is not always
    # in the maximum. So they could reach their potential and demand more
    # carbs than are available in the buffer.
    # If there are not enough, there is the inhibition phenomena, but right
    # now they don't seem compatible, as there is growth without
    # enough carbs because of the different treatment of the first fruit
    # stage.
    rates["MCBufLeaf"] = mcBufOrg(organ="leaf", params=params, states=states)
    rates["MCBufFruit"] = mcBufOrg(organ="fruit", params=params, states=states)
    rates["MCBufStem"] = mcBufOrg(organ="stem", params=params, states=states)

    co2_st = co2Stom(co2=co2, params=params)

    # Photosynthesis Rate
    mcAirBuf_ = mcAirBuf(co2=co2_st, par=par, params=params, states=states)
    # Growth respiration
    mcBufAir_ = mcBufAir(params=params, states=states)

    cBuf_ = (mcAirBuf_ - rates["MCBufLeaf"] - rates["MCBufFruit"] - rates["MCBufStem"] -
             mcBufAir_)

    return cBuf_


def rateCLeaf(info: dict, params: dict, rates: dict, states: dict) -> float:
    """
    Calculates carbohydrates stored on leaves.

    Balance of the carbohydrate ﬂow from the buffer to leaves, the
    maintenance respiration of the leaves and leaf pruning.

    Parameters
    ----------
    info : dict
        Additional inputs for the model
    params : dict
        Parameters saved as model constants
    rates : dict
        Auxiliary variable including rates and
            flows for the different fruit development stages
    states : dict
        State variables of the model

    Returns
    -------
    float
        Carbohydrates stored in the leaves. [mg m-2 s-1]

    """

    # Maintenance respiration
    mcLeafMaint = mcOrgAir_m(organ="leaf", params=params, states=states)
    rates['resp_leaf'] = mcLeafMaint

    # Prune
    mcLeafHar = mcLeafHar_s(info=info, params=params, states=states)

    cLeaf_ = (rates["MCBufLeaf"] - mcLeafMaint - mcLeafHar)

    return cLeaf_


def rateCStem(params: dict, rates: dict, states: dict) -> float:
    """
    Rate of carbohydrates accumulation in the stem and roots.

    Parameters
    ----------
    params : dict
        Parameters saved as model constants
    rates : dict
        Auxiliary variable including rates and
            flows for the different fruit development stages
    states : dict
        State variables of the model

    Returns
    -------
    float
        Rate of carbohydrates accumulation in the stem and roots.
        [mg m-2 s-1]

    """

    # Maintenance respiration
    mcStemMaint = mcOrgAir_m(organ="stem", params=params, states=states)
    rates['resp_stem'] = mcStemMaint

    cStem_ = (rates["MCBufStem"] - mcStemMaint)

    return cStem_


def rateCFruit(info: dict, params: dict, rates: dict,
               states: dict) -> np.ndarray:
    """
    Rate of carbohydrates accumulation in the fruits.

    This approach takes into account the different stages of fruit development.

    Parameters
    ----------
    info : dict
        Additional inputs for the model
    params : dict
        Parameters saved as model constants
        nDev: int
            Number of development stages ascribed to fruits
    rates : dict
        Auxiliary variable including rates and
            flows for the different fruit development stages
        MCFruitFruit: np.ndarray
            Mass flow from one fruit stage to the next
        MCBufFruit: np.ndarray
            Carbohydrate flow from the buffer to fruits in each developement
            stage

    states : dict
        State variables of the model
        CFruit: np.ndarray
            Carbohydrates accumulated in the fruits
            in each development stage [mg m-2]

    Returns
    -------
    np.ndarray
        Rate of carbohydrates storage in fruits in each of all ndev
        development stages [mg m-2 s-1]

    """

    cFruit_ = states["CFruit"].copy()

    rates["MCBufFruit_i"] = mcBufFruit_i(info=info, params=params,
                                         rates=rates, states=states)
    rates["MCFruitFruit"] = mcFruitFruit(params=params, states=states)
    maint_resp = mcOrgAir_m(organ="fruit", params=params, states=states)
    rates['resp_fruit'] = maint_resp

    # For the first development stage, the inflow from the previous stage
    # is zero
    inflow = np.concatenate((np.array([0]),
                            rates["MCFruitFruit"][0:params["nDev"]-1]))

    # For the last stage, the outflow for the next stage
    # is described by MCFruitHar
    # XXX Conferir
    outflow = rates["MCFruitFruit"]
    outflow[params["nDev"] - 1] = rateDMHar(params, rates)

    cFruit_ = rates["MCBufFruit_i"] + inflow - outflow - maint_resp

    return cFruit_


def rateDMHar(params: dict, rates: dict) -> float:
    """
    For simplicity, a continuous harvest rate was assumed.

    Parameters
    ----------
    params : dict
        nC_DM: float
            conversion factor from carbohydrates to dry matter.
        nDev: float
            Number of development stages

    rates : dict
        MCFruitFruit: Mass flow from one fruit stage to the next

    Returns
    -------
    float
        Accumulated harvested tomato dry matter. [mg {DM} m-2 s-1]
    """

    # Outflow of dry matter from the last development stage
    mcFruitHar = rates["MCFruitFruit"][params["nDev"] - 1]

    dmHar_ = params['nC_DM'] * mcFruitHar

    # Katzin 2019
    # % Fruit harvest [mg{CH2O} m^{-2] s^{-1}]
    # % Equation A45 [5]
    # addAux(gl, 'mcFruitHar',
    # ifElse('x.cFruit<p.cFruitMax',0,x.cFruit-p.cFruitMax));

    return dmHar_


def rateNFruit(info: dict, params: dict,
               rates: dict, states: dict) -> np.ndarray:
    """
    Summary
    Fruit development is modelled by describing the number of fruits and
    the amount of carbohydrates for each fruit development stage (Fig. 2).
    Fruits and carbohydrates flow through different fruit development stages
    are represented by a series of pools. Fruit numbers are modelled to
    determine for each fruit development stage the carbohydrate demand.
    The fruit number and carbohydrate flows to different fruit development
    stages are described in this section. (Vanthoor, 2011)

    Parameters
    ----------
    info : dict
        Additional inputs for the model
    params : dict
        Parameters saved as model constants
        nDev: int
            Number of development stages
    rates : dict
        MNFruitFruit: np.ndarray
            Fruit number flow from one development stage to the next
    states : dict
        NFruit : np.ndarray
            Number of fruits in each development stage [fruits m-2]


    Returns
    -------
    np.ndarray
        Rate of number of fruits in each development stage [fruits m-2 s-1]

    """
    # MNFruit_j-1_j and MNFruit_j_j1 represent the fruit number flow from
    # stage j-1 to j and j to j+1, respectively.

    # For the first fruit stage, MNFruit_j-1_j is replaced by MNBufFruit(1).
    # This depends on carbohydrates available for fruit growth and on truss
    # appearance rate

    nFruit_ = states["NFruit"].copy()
    rates["MNFruitFruit"] = mnFruitFruit(params=params, states=states)

    nFruit_[0] = (mnBufFruit_1(info=info, params=params, rates=rates,
                               states=states) - rates["MNFruitFruit"][0])
    nFruit_[1:] = (rates["MNFruitFruit"][0:(params["nDev"]-1)] -
                   rates["MNFruitFruit"][1:params["nDev"]])

    return nFruit_


def rateTCan24(params: dict, states: dict) -> float:
    """
    Summary

    Parameters
    ----------
    params : dict
        tau: float
            Time constant of the process.
        k: float
            Gain of the process.

    states : dict
        TCan: float
            TCan is the simulated or measured canopy temperature. [oC]
        TCan24: float
            The 24 hour mean canopy temperature [oC s-1]

    Returns
    -------
    float
        24 h mean canopy temperature, approximated by
        a ﬁrst order differential equation. [oC s-1]
    """

    tCan24_ = (1/params["tau"])*(params["k"]*states["TCan"] - states["TCan24"])

    return tCan24_


# %% Carbohydrate flow to the individual plant organs

def mcAirBuf(co2: float, par: float, params: dict, states: dict) -> float:
    """
    Photosynthesis rate.

    Photosynthesis rate is inhibited by saturation of
    the leaves with carbohydrates.

    Parameters
    ----------
    par : float
        Photosynthetic active radiation [μmol {photons} m-2 s-1]
    co2 : float
        Carbon dioxide concentration on air [μmol {CO2} mol-1 {air}]
    params : dict
        Parameters saved as model constants
        p_MCH2O: float
            Molar mass of CH2O. [mg μmol-1]
        sMaxBuf: float
            Slope ak kswitch
        ksMaxBuf: float
            Maximum buffer capacity [mg m-2]
    states : dict
        State variables of the model
        CBuf: float
            Rate of accumulation of carbohydrates in the buffer [mg m-2 s-1]

    Returns
    -------
    float
        Photosynthesis rate [mg m-2 s-1]

    """
    # Gross photosynthesis
    P = rateP(co2=co2, par=par, params=params, states=states)
    # Photorespiration
    R = rateR(co2=co2, par=par, params=params, states=states)


    # Smoothed version as defined is ambiguous for switch value,
    # ascribing to it the maximum value, which would mean 0 if higher than
    # it as well as the 50% switch value, which would ascribe 0.5.
    # Value was changed to separate CMaxBuf from ksMaxBuf.
    # hAirBuf = h_smooth(s=params["sMaxBuf"],
    #                     k=states["CBuf"],
    #                     k_sw=params["ksMaxBuf"])
    if states["CBuf"] > params["CMaxBuf"]:
        hAirBuf = 0
    else:
        hAirBuf = 1

    mcAirBuf_ = (params['p_MCH2O'] * hAirBuf * (P - R))

    params["photo"] = (P - R) * params['p_MCH2O'] * 3600 / 1000

    return mcAirBuf_


def mcBufOrg(organ: str, params: dict, states: dict) -> float:
    """
    Potential organ growth multiplied by diverse inhibition factors.

    Parameters
    ----------
    organ : str
        Which organ is being evaluated: fruits, leaves or stem
    params : dict
        Parameters saved as model constants
        sMinBuf: float
            Slope ak kswitch
        sMinTCan24: float
            Slope ak kswitch
        ksMinTCan24: float
            Value of TCan24 at which S is 0.5
        ksMaxTCan24: float
            Value of TCan24 at which S is 0.5

    states : dict
        State variables of the model
        CBuf: float
            Rate of accumulation of carbohydrates in the buffer [mg m-2 s-1]
        TCan24: float
            The 24 hour mean canopy temperature [oC s-1]

    Returns
    -------
    float
        Amount of carbohydrates flow from buffer to organ. [mg m-2 s-1]

    """
    # Potential organ growth rate coefficient at 20oC. [mg CH2O m-2 s-1]
    p_rgOrgan = "rg_" + organ

    # Inhibition factor: insuficient carbohydrates in the buffer

    # Smoothed version as defined is ambiguous for switch value,
    # ascribing to it the minimum value, which would mean 0 if lower than
    # it as well as the 50% switch value, which would ascribe 0.5.
    # Value was changed to separate CMinBuf from ksMinBuf.
    hBufOrg = (h_smooth(k=states['CBuf'],
                        k_sw=params["ksMinBuf"],
                        s=params["sMinBuf"]))
    # This format is harsher when penalizing lower values, but it is
    # in accordance to the material. The smoothed version is preferred.
    # if states['CBuf'] <= params["CMinBuf"]:
    #     hBufOrg = 0
    # else:
    #     hBufOrg = 1

    # Inhibition factor: non-optimal 24-hour canopy temperatures
    hTCan24 = (h_smooth(k=states['TCan24'],
                        k_sw=params["ksMinTCan24"],
                        s=params["sMinTCan24"]) *
               h_smooth(k=states["TCan24"],
                        k_sw=params["ksMaxTCan24"],
                        s=params["sMaxTCan24"]))

    # Effect of temperature on the carbohydrate flow to the organ
    gTCan24_ = gTCan24(states)

    if organ == "fruit":

        # Inhibition factor: non-optimal instantaneous temperature
        hTCan = (h_smooth(k=states['TCan'],
                          k_sw=params["ksMinTCan"],
                          s=params["sMinTCan"]) *
                 h_smooth(k=states["TCan"],
                          k_sw=params["ksMaxTCan"],
                          s=params["sMaxTCan"]))

        # Crop development stage
        hTSum = h_TCanSum(params=params, states=states)

        mcBufOrg_ = hBufOrg*hTCan*hTCan24*hTSum*gTCan24_*params[p_rgOrgan]
    else:
        mcBufOrg_ = hBufOrg*hTCan24*gTCan24_*params[p_rgOrgan]

    return mcBufOrg_


def gTCan24(states: dict) -> float:
    """
    Growth rate coefficients dependency on temperature. [-]

    Parameters
    ----------
    states : dictionary
        TCan24: float
            The 24 hour mean canopy temperature [oC s-1]

    Returns
    -------
    gTCan24_ : float
        Coefficient to adjust growth rate coefficients based on temperature

    """
    gTCan24_ = 0.047*states["TCan24"] + 0.060

    return gTCan24_

# %% Fruits

# Number of fruits


def mnFruitFruit(params: dict, states: dict) -> np.ndarray:
    """
    Fruit number flow from one development stage to the next

    Parameters
    ----------
    params : dict
        cDev1: float
            Fruit development rate coefficient 1 [s-1]
        cDev2: float
            Fruit development rate coefficient 2 [s-1 °C-1]
        sTSumCan: float
            Slope ak kswitch
        ksTSumCan: float
            Value of TSCan at which S is 0.5
        nDev: float
            Number of fruit development stages
    states : dict
        TCan24: float
            The 24 hour mean canopy temperature [oC s-1]
        TSCan: float
            Temperature sum [oC d]
        NFruit: float
            Number of fruits in each development stage [fruits m-2]

    Returns
    -------
    np.ndarray
        Fruit number flow from one development stage to the next
        [fruits m-2 s-1]

    """
    r_dev = params["cDev1"] + params["cDev2"] * states["TCan24"]
    hMN = h_smooth(k=states["TSCan"],
                   k_sw=params["ksTSumCan"],
                   s=params["sTSumCan"])

    # # Non-differentiable version
    # if states["TSCan"] <= params["ttsum"]:
    #     hMN = 0
    # else:
    #     hMN = 1

    # Fruit number flow from fruit stage j-1 to stage j
    flow_NFruit = (r_dev * params["nDev"] * hMN * states["NFruit"])

    return flow_NFruit


def mnBufFruit_1(info: dict, params: dict, rates: dict, states: dict) -> float:
    """
    Fruit set in the first development stage

    Parameters
    ----------
    info : dict
        Additional inputs for the model
        nPlants: float
            Plant density
    params : dict
        Parameters saved as model constants
        cMaxBufFruit1: float
            Regression coefficient of truss appearance
        cMaxBufFruit2: float
            Regression coefficient of truss appearance
        sBufFruit: float
            Slope ak kswitch
        ksBufFruit: float
            Value of MCBufFruit at which S is 0.5
    rates : dict
        Auxiliary variable including rates and
            flows for the different fruit development stages
    states : dict
        State variables of the model
        TCan24: float
            The 24 hour mean canopy temperature [oC s-1]

    Returns
    -------
    np.ndarray
        Rate of number of fruits appearing in the first development stage
        [fruits m-2 s-1]

    """
    mnMaxBufFruit1_ = (info["nPlants"] *
                       (params["cMaxBufFruit1"] +
                        params["cMaxBufFruit2"] * states["TCan24"]))

    mnBufFruit1_ = h_smooth(k=rates["MCBufFruit"],
                            k_sw=params["ksBufFruit"],
                            s=params["sBufFruit"]) * mnMaxBufFruit1_

    return mnBufFruit1_


def mcBufFruit_i(info: dict, params: dict,
                 rates: dict, states: dict) -> np.ndarray:
    """
    Carbohydrate flow from buffer to fruits in all development stages

    Parameters
    ----------
    info : dict
        Additional inputs for the model
    params : dict
        Parameters saved as model constants
    rates : dict
        Auxiliary variable including rates and
            flows for the different fruit development stages
        MCBufFruit: np.ndarray
            Carbohydrate flow from buffer to fruits in all development stages
    states : dict
        State variables of the model
        NFruit: np.ndarray
            Number of fruits in each development stage [fruits m-2]

    Returns
    -------
    np.ndarray
        Carbohydrate flow from buffer to fruits in each development stage
        [mg m-2 s-1]

    """
    # Initialization
    mask = states["NFruit"] > 0
    flow_BufFruit = rates["MCBufFruit_i"].copy()
    flow_temp = flow_BufFruit.copy()

    # First stage
    # Depends on the potential dry matter per fruit in fruit development
    # stage one and the number of fruits flowing to stage 1.
    # [mg m-2 s-1].
    # MNBufFruit1 fruits m-2 s-1

    gr, gr_der1 = gompertz(params=params, states=states)

    flow_temp[0] = (gr[0] *
                    mnBufFruit_1(info=info, params=params,
                                 rates=rates, states=states))

    # Other stages
    # etaBufFruit is a conversion factor to ensure that MCBufFruit
    # equals the sum of the carbohydrates that flow to the different
    # development stages
    # [d m2 mg-1]
    with np.errstate(divide='raise'):
        try:
            etaBufFruit = ((states["NFruit"][1:] * gr_der1[1:]).sum())**-1
        except:
            etaBufFruit = 0.

    # As the first stage is independent from what is available in the buffer,
    # and not subject to the inhibition factor, it may be the case that
    # this value is larger than what is in the buffer,
    # what would be incompatible and generate negative numbers for the
    # following stages. So this was limited to the maximum value available.
    mcBufFruit_ = rates["MCBufFruit"]
    mcBufFruits1_ = min(flow_temp[0], mcBufFruit_)

    aux = (etaBufFruit * states["NFruit"] * gr_der1 *
           (mcBufFruit_ - mcBufFruits1_))

    flow_temp[1:] = aux[1:]
    flow_BufFruit[mask] = flow_temp[mask]

    return flow_BufFruit


def mcFruitFruit(params: dict, states: dict) -> np.ndarray:
    """
    Carbohydrate flow from one fruit development stage to the other

    Parameters
    ----------
    params : dict
        cDev1: float
            Fruit development rate coefficient 1 [s-1]
        cDev2: float
            Fruit development rate coefficient 2 [s-1 °C-1]
        nDev: int
            Number of fruit development stages
    states : dict
        TCan24: float
            The 24 hour mean canopy temperature [oC s-1]
        CFruit: np.ndarray
            Carbohydrates stored in fruits in each development stage [mg m-2]

    Returns
    -------
    np.ndarray
        Carbohydrate flow from one fruit development stage to the other
        [mg m-2 s-1]

    """
    r_dev = params["cDev1"] + params["cDev2"] * states["TCan24"]
    flow_FF = r_dev * params["nDev"] * states["CFruit"]

    return flow_FF


def gompertz(params: dict, states: dict) -> np.ndarray:
    """
    Potential growth rate per fruit

    Parameters
    ----------
    params : dict
        cDev1: float
            Fruit development rate coefficient 1 [s-1]
        cDev2: float
            Fruit development rate coefficient 2 [s-1 °C-1]
        nDev: float
            Number of fruit development stages
        tau: float
            time constant [s]
        GMax: float
            Potential fruit dry weight at harvest [mg {CH2O} fruit-1]
    states : dict
        TCan24: float
            The 24 hour mean canopy temperature [oC s-1]

    Returns
    -------
    gr : np.ndarray
        Fruit potential growth in each of the development stages
        [mg CH2O fruit-1]
    gr_der1 : np.ndarray
        Fruit potential growth in each of the development stages
        [mg CH2O fruit-1]

    """
    # Fruit development rate [s-1]
    r_dev = params["cDev1"] + params["cDev2"] * states["TCan24"]

    # Fruit growth period [d]
    FGP = 1/(r_dev * params["tau"])
    # Fruit development time in days where gr_der1 is maximal [d]
    M = -4.93 + 0.548*FGP
    # Steepness of the Gompertz curve [d-1]
    B = 1/(2.44 + 0.403*M)

    # Development stages
    j = np.arange(params["nDev"]) + 1
    # Days after fruit set for each development stage
    t = ((j - 1) + 0.5)*FGP / params["nDev"]

    expo = -B*(t - M)
    # Integral of Gompertz curve
    gr = params['GMax'] * np.exp(-np.exp(expo))
    # PFGRMt = C * exp((-exp(-B * (FDS - M)))) * B * exp(-B *( FDS - M))
    # First derivative of Gompertz equation to time [mg CH2O fruit-1 d-1]
    # Daily potential growth rate per fruit in each fruit development stage
    gr_der1 = params['GMax'] * np.exp(-np.exp(expo))*B*np.exp(expo)

    return gr, gr_der1


# %% Photosynthesis subprocesses


def rateP(co2: float, par: float, params: dict, states: dict) -> float:
    """
    Canopy photosynthesis rate

    Parameters
    ----------
    par : float
        Photosynthetic active radiation [μmol {photons} m-2 s-1]
    co2 : float
        Carbon dioxide concentration on air [μmol {CO2} mol-1 {air}]
    params : dict
        Parameters saved as model constants.
        ne: float
            maximum rate of electron transport for the leaf at 25°C
            [μmol {e-} m-2 {leaf} s-1]
    states : dict
        State variables of the model.

    Returns
    -------
    float
        Gross canopy photosynthesis rate[(μmol-1 {CO2} m-2 s-1]

    """
    c_gama = co2Comp(params=params, states=states)
    J = rateEl(par=par, params=params, states=states)

    P = (J*(co2 - c_gama)) / (params['ne'] * (co2 + 2*c_gama))

    return P


def rateR(co2: float, par: float, params: dict, states: dict) -> float:
    """
    Photorespiration

    Parameters
    ----------
    par : float
        Photosynthetic active radiation [μmol {photons} m-2 s-1]
    co2 : float
        Carbon dioxide concentration on air [μmol {CO2} mol-1 {air}]
    params : dict
        Parameters saved as model constants.
    states : dict
        State variables of the model.

    Returns
    -------
    float
        Photorespiration during the photosynthesis process
        [μmol-1 {CO2} m-2 s-1]

    """
    c_gama = co2Comp(params=params, states=states)

    P = rateP(co2=co2, par=par, params=params, states=states)
    R = P * c_gama / co2

    return R


def rateEl(par: float, params: dict, states: dict) -> float:
    """
    Max rate of electron transport

    Parameters
    ----------
    par : float
        Photosynthetic active radiation [μmol {photons} m-2 s-1]
    params : dict
        Parameters saved as model constants
        Jmax25Leaf: float
            Maximal rate of electron transport at 25°C for the leaf
        convC_K: float
            conversion from oC to K
        Ej: float
            activation energy for Jpot [J mol-1]
        T25K: float
            Reference temperature at 25°C [K]
        R: float
            Molar gas constant [J mol-1 K-1]
        S: float
            Entropy term [J mol-1 K-1]
        H: float
            Deactivation energy [J mol-1]
        alpha: float
            Degree of curvature of the electron transport rate [-]
        theta_curv:float
            Conversion factor from carbohydrate to dry matter
            [mg {DM} mg-1 {CH2O}]
    states : dict
        State variables of the model
        TCan: float
            TCan is the simulated or measured canopy temperature. [oC]

    Returns
    -------
    float
        Max rate of electron transport at 25°C
        for the canopy [μmol {e-} m-2 s-1]

    """
    Jmax25Can = lai(params=params, states=states) * params['Jmax25Leaf']

    # canopy temperature [K]
    TCanK = states['TCan'] + params['convC_K']

    expo = (
        (params['Ej'] * (TCanK - params['T25K'])) /
            (params['R'] * TCanK * params['T25K'])
            )
    num = (1 + np.exp(
        (params['S']*params['T25K'] - params['H']) /
                      (params['R']*params['T25K'])
                      )
        )
    den = (1 + np.exp(
        (params['S']*TCanK-params['H']) / (params['R']*TCanK)
        )
        )

    # Potential rate of electron transport [μmol {e-} m-2 s-1]
    Jpot = Jmax25Can * np.exp(expo) * num/den

    # Absorbed PAR [μmol {photons} m-2 s-1]
    parCan = parAbs(par=par, params=params, states=states)

    prod = params['alpha'] * parCan
    sq = (Jpot + prod)**2 - 4 * params['theta_curv'] * Jpot * prod

    J = (Jpot + prod - sq**0.5) / (2 * params['theta_curv'])

    return J


def parAbs(par: float, params: dict, states: dict) -> float:
    """
    Total PAR absorbed by the canopy

    Parameters
    ----------
    par : float
        Photosynthetic active radiation [μmol {photons} m-2 s-1]
    params : dict
        Parameters saved as model constants
        roFlr: float
            Reflection coefficient of the greenhouse floor.
            Assumed for white mulching.
        roCan: float
            Reflection coefficient of the canopy for PAR
        K2: float
            Extinction coefficient of the canopy for PAR.
        K1: float
            Extinction coefficient of the canopy when PAR is
            reflected from the floor. Assumed equal to K1
    states : dict
        State variables of the model

    Returns
    -------
    float
        Sum of PAR directly absorbed through the cover and indirectly
        absorbed after floor reflection [μmol {photons} m-2 s-1]

    """
    # # Outside global radiation
    # iGlob = 1

    # # Conversion factor from global radiation to PAR [μmol {photons} J-1]
    # nGlob_par = 2.3

    # # Light transmission coefficient of the greenhouse cover
    # tauGh = 0.78

    # # PAR above the canopy [μmol {photons} m-2 s-1]
    # parGh = tauGh * nGlob_par * iGlob

    parGh = par

    lai_ = lai(params=params, states=states)

    # PAR directly absorbed by the canopy [μmol {photons} m-2 s-1]
    parGhCan = parGh * (1 - params['roCan']) * (1-np.exp(-params['K1']*lai_))

    # Absorption of PAR reflected by the greenhouse floor
    # [μmol {photons} m-2 s-1]
    parFlrCan = (params['roFlr'] * parGh *
                 (1 - params['roCan']) * np.exp(-params['K1']*lai_) *
                 (1-np.exp(-params['K2']*lai_)))

    parCan = parFlrCan + parGhCan

    return parCan


def co2Stom(co2: float, params: dict) -> float:
    """
    CO2 concentration in the stomata

    Parameters
    ----------
    co2 : float
        Carbon dioxide concentration on air [μmol {CO2} mol-1 {air}]
    params : dict
        n_convCO2: float
            Conversion factor from the CO2-concentration of the greenhouse air

    Returns
    -------
    float
        CO2 concentration in the stomata [μmol {CO2} mol-1 {air}]

    """
    co2Stom = params['n_convCO2']*co2

    return co2Stom


def co2Comp(params: dict, states: dict) -> float:
    """
    CO2 compensation point

    Parameters
    ----------
    params : dict
        Parameters saved as model constants
        Jmax25Leaf: float
            Maximal rate of electron transport at 25°C for the leaf
        c_gama: float
            Determines the effect of canopy temperature on the CO2
            compensation point [μmol {CO2} mol-1 {air} K-1]
    states : dict
        State variables of the model
        TCan: float
            TCan is the simulated or measured canopy temperature. [oC]

    Returns
    -------
    co2Comp_ : float
        CO2 compensation point [μmol {CO2} mol 1 {air}]

    """
    # Maximum rate of electron transport at 25°C for the canopy
    # [μmol {e-} m-2 s-1]
    lai_ = lai(params=params, states=states)
    Jmax25Can = lai_ * params['Jmax25Leaf']

    # ratio = 1/lai_
    ratio = params['Jmax25Leaf']/Jmax25Can

    # Weird behavior for low LAI values - leads to negative photosynthesis
    # Didn't find the original paper
    if lai_ > 0.4:
        co2Comp_ = (params['c_gama']*ratio*states['TCan'] +
                    20*params['c_gama']*(1 - ratio))
    else:
        co2Comp_ = (params['c_gama']*states['TCan'])

    return co2Comp_

# %% Growth and maintanance respiration


def mcBufAir(params: dict, states: dict) -> float:
    """
    Growth respiration

    Parameters
    ----------
    params : dict
        Parameters saved as model constants
    states : dict
        State variables of the model

    Returns
    -------
    float
        Growth respiration of the plant [mg m-2 s-1]

    """
    mcBufAir_ = (mcOrgAir_g(organ="fruit", params=params, states=states) +
                 mcOrgAir_g(organ="leaf", params=params, states=states) +
                 mcOrgAir_g(organ="stem", params=params, states=states))

    return mcBufAir_


def mcOrgAir_g(organ: str, params: dict, states: dict) -> float:
    """
    Organ growth respiration

    Parameters
    ----------
    organ : str
        Which organ is being evaluated: fruits, leaves or stem
    params : dict
        Parameters saved as model constants
        cg_*: float
            Organ growth respiration coefficient
    states : dict
        State variables of the model

    Returns
    -------
    float
        Organ growth respiration [mg m-2 s-1]

    """
    c_organ = "cg_" + organ
    mcOrgAir_g_ = params[c_organ]*mcBufOrg(organ=organ, params=params,
                                           states=states)

    return mcOrgAir_g_


def mcOrgAir_m(organ: str, params: dict, states: dict) -> float:
    """
    Organ maintenance respiration

    Parameters
    ----------
    organ : str
        Which organ is being evaluated: fruits, leaves or stem
    params : dict
        Parameters saved as model constants
        cm_*: float
            Organ maintenance respiration coefficient
            [mg {CH2O} mg-1 {CH2O} s-1]
        Q10: float
            Temperature effect on maintenance respiration [°C]
        c_rgr: float
            Regression coefficient in maintenance respiration function [s]
        rgr_: float
            Relative growth rate [s-1]
    states : dict
        TCan24: float
            The 24 hour mean canopy temperature [oC s-1]
        CLeaf: float
            Carbohydrates stored in the leaves [mg m-2]
        CStem: float
            Carbohydrates stored in the stem and roots [mg m-2]
        CFruit: np.ndarray
            Carbohydrates stored in fruits in each development stage [mg m-2]

    Returns
    -------
    float
        Organ maintenance respiration [mg m-2 s-1]

    """
    c_organ = "cm_" + organ
    C_organ = "C" + organ.title()
    f1 = params[c_organ] * params["Q10"]**(0.1 * (states["TCan24"] - 25))
    f2 = states[C_organ] * (1 - np.exp(-params["c_rgr"]*params['rgr_']))

    mcOrgAir_m_ = f1 * f2

    return mcOrgAir_m_


# %% Secondary and smooth functions

def h_smooth(k: float, k_sw: float, s: float) -> float:
    """
    Smoothed conditional if/else statements

    Parameters
    ----------
    k : float
        state or flow that determines S
    k_sw : float
        value of k where S is 0.5
    s : float
        slope at k_sw

    Returns
    -------
    float
        Values close to 0 or 1, equivalent of if/else statement

    """
    S = 1 / (1 + np.exp(s*(k - k_sw)))

    return S


def h_TCanSum(params: dict, states: dict) -> float:
    """
    Gradual increase in fruit growth

    Parameters
    ----------
    params : dict
        Parameters saved as model constants
    states : dict
        State variables of the model

    Returns
    -------
    float
        Increase in fruit growth depending on development stage

    """
    TSumCan = states["TSCan"]
    TSumEnd = params["TSumEnd"]

    ratio = TSumCan/TSumEnd
    dif = TSumCan - TSumEnd
    dif_ratio = dif/TSumEnd

    pt1 = ratio + (np.abs(ratio**2) + 10**-4)**0.5
    pt2 = dif_ratio + (np.abs(dif_ratio**2) + 10**-4)**0.5

    hTCanSum_ = 0.5*pt1 - 0.5*pt2

    return hTCanSum_


def lai(params: dict, states: dict) -> float:
    """
    Calculates leaf area index

    Parameters
    ----------
    params: dict
        sla: float
            Specific leaf area. [m2 {leaf} mg-1 {CH2O}]
     states: dict
        CLeaf: float
            Carbohydrates stored in the leaves [mg m-2]

    Returns
    -------
    float
        LAI is a semi-state of the model. [m2 {leaf} m-2]

    """
    lai_ = params['sla'] * states['CLeaf']

    return lai_


def calc_rgr(conv: float, params: dict, states: dict) -> float:
    """
    Calculates relative growth rate

    Parameters
    ----------
    params: dict
        rgr: float
            Relative growth rate [s-1] Constant value from Katzin 2019
    states : dict
        Total: float
            Sum of all plant biomass [mg m-2]
    conv : float
        RGR is used in calculations before converting states into mg per hour

    Returns
    -------
    float
        Updated value of relative growth rate [mg mg -1 dt-1]

    """
    twt = states["Total"].copy()

    if len(twt) == 1:
        rgr_calc = params['rgr']
    else:
        # Hall 1993, Higashide and Heuvelink 2009
        twt = twt[-2:]
        rgr_calc = np.log(twt[-1]) - np.log(twt[-2])
        rgr_calc = rgr_calc/conv

    # Harvesting or pruning could lead to negative rates. In this case,
    # switch back to default.
    if rgr_calc < 0:
        rgr_calc = params['rgr']

    return rgr_calc


def mcLeafHar_s(info: dict, params: dict, states: dict) -> float:
    """
    Leaf pruning
    Pruning that occurs when maximum leaf area index is reached.
    The desired effect is to replace the if clause:
        if Cleaf < CMaxLeaf:
            0
        else:
            CLeaf - CMaxLeaf

    Parameters
    ----------
    info: dict
        laiMax: float
            Leaves are pruned back to this LAI [m2 m-2]
        dt: int
            Timestep for simulation [s]
    params : dict
        sla: float
            Specific leaf area. [m2 {leaf} mg-1 {CH2O}]
    states : dict
        CLeaf: float
            Carbohydrates stored in the leaves [mg m-2]

    Returns
    -------
    float
        Leaf pruning rate [mg m-2 s-1]

    """
    # Maximum allowed carbohydrates on the leaves [mg m-2]
    CMaxLeaf = info["laiMax"]/params["sla"]

    # Differential form of the pruning function. [mg m-2 s-1]
    # On the beginning of the growth, this is ascribing mass to leaves
    # as the difference is negative. Perhaps requires change in k_sw, but
    # the if clause was chosen.
    # mcLeafHar_ = (h_smooth(k=states["CLeaf"],
    #                       k_sw=CMaxLeaf,
    #                       s=params["sLeafHar"]) *
    #               (states["CLeaf"] - CMaxLeaf))

    # # Form of the pruning function adopted by Katzin 2019. [mg m-2 s-1]
    if states['CLeaf'] < CMaxLeaf:
        mcLeafHar_ = 0.
    else:
        mcLeafHar_ = states["CLeaf"] - CMaxLeaf

    # As this difference comes from the increment that happened in the last
    # dt, not necessarily from the second, it should be fractioned
    # into through the following dt.
    mcLeafHar_ = mcLeafHar_/info["dt"]

    return mcLeafHar_


# %% Model
def vanthoor(it: int, weather: pd.DataFrame,
             info: dict, params: dict, rates: dict,
             states: dict) -> dict:
    """
    Runs the model for calculating rates

    Parameters
    ----------
    weather : TYPE
        Environmental variables
    info: dict
        dt: int
            Timestep for simulation [s]
    params : dict
        Parameters saved as model constants
        rgr: float
            Constant value for relative growth rate
    rates : dict
        Auxiliary variable including rates and
            flows for the different fruit development stages
    states : dict
        State variables of the model

    Returns
    -------
    dict
        State variables updated.

    """
    # it = 15
    # it = 1127

    co2 = weather.co2.values[it]
    par = weather.rad.values[it]

    params['rgr_'] = calc_rgr(info["dt"], params, states)

    for i in range(int(3600/info["dt"])):

        rates['rgr'].append(params['rgr_'])
        if len(rates['rgr']) > 24:
            rates['rgr'] = rates['rgr'][-24:]

        # As TCan is not measured, it is assumed equal to TAir
        states['TCan'] = weather.tmean.values[it]

        rates['TCan24'] = rateTCan24(params=params, states=states)
        states['TCan24'] = (rates['TCan24']*info["dt"] + states['TCan'])

        rates['CBuf'] = rateCBuf(co2=co2, par=par, params=params,
                                 rates=rates, states=states)
        # The model assumes the buffer is never totally empty
        states['CBuf'] = max(rates['CBuf']*info['dt'] + states['CBuf'], 0)

        rates['CLeaf'] = rateCLeaf(info=info, params=params,
                                   rates=rates, states=states)
        #print(rates['CLeaf'])
        states['CLeaf'] = rates['CLeaf']*info["dt"] + states['CLeaf']

        rates['CStem'] = rateCStem(params=params,
                                   rates=rates, states=states)
        states['CStem'] = rates['CStem']*info["dt"] + states['CStem']

        rates['TSCan'] = rateTSCan(params=params, states=states)
        states['TSCan'] = rates['TSCan']*info["dt"] + states['TSCan']

        rates['resp_fruit'] = [0., 0.]

        if states['TSCan'] > 0:

            rates['NFruit'] = rateNFruit(info=info, params=params, rates=rates,
                                         states=states)
            states['NFruit'] = (rates['NFruit']*info["dt"] + states['NFruit'])

            rates['CFruit'] = rateCFruit(info=info, params=params,
                                         rates=rates,
                                         states=states)
            states['CFruit'] = (rates['CFruit']*info["dt"] + states['CFruit'])

            rates['DMHar'] = rateDMHar(params=params, rates=rates)
            states['DMHar'] = (rates['DMHar']*info["dt"] + states['DMHar'])

        states['Total'].append((states['CLeaf'] + states['CStem'] +
                                states['CFruit'].sum()))

        if len(states['Total']) > 26:
            states['Total'] = states['Total'][-25:]

        rates['resp'] = (sum(rates['resp_fruit']) + rates['resp_stem'] +
                         rates['resp_leaf'])*3600/1000
        rates['dw'] = (sum(rates['CFruit']) + rates['CStem'] +
                       rates['CLeaf'])*3600/1000

    return states
