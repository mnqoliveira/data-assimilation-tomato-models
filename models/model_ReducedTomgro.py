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
import f_aux as aux

from filterpy.common import pretty_str

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

# %% Class definition

class tomgro(object):

    def __init__(self, info, params, rates, states):

        self.n = states["n"]
        self.lai = states["lai"]
        self.w = states["w"]
        self.wf = states["wf"]
        self.wm = states["wm"]

        self.dn = rates["dn_dt"]
        self.dlai = rates["dlai_dt"]
        self.dw = rates["dw_dt"]
        self.dwf = rates["dwf_dt"]
        self.dwm = rates["dwm_dt"]

        self.params = params
        self.info = info

# class a(object):

#     def __init__(self, i):

#         self.i = i

# x = a(2)
# getattr(x, "i")
# setattr(x, "i", 3)

    def __repr__(self):

        print('Reduced Tomgro object summary\n')
        print('n', self.n, '\n')
        print('lai', self.lai, '\n')
        print('w', self.w, '\n')
        print('wf', self.wf, '\n')
        print('wm', self.wm, '\n')
        print('params', self.params, '\n')
        print('weather_d', self.weather_d, '\n')

        return ''

    # %% Photosynthesis
    def photosynthesis_hourly(self, co2_hour: np.ndarray, rad_hour: np.ndarray,
                              Tmed_hour: np.ndarray) -> np.ndarray:
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
            LFmax = self.params['tau1'] * co2
        else:
            LFmax = (self.params['tau1'] * 350 +
                     self.params['tau2'] * (co2 - 350.))

        # Jones 1999, spreadsheet
        LFmax = np.round(LFmax)

        adjustment = ((self.info['D'] / 24) * LFmax *
                      self.PGRED_T(Tmed_hour=Tmed_hour)/self.params['K'])

    # Source: Jones 1991
    #    photo_process = (
    #            ((1-params['m'])*LFmax + params['Qe']*params['K']*PPFD) /
    #            ((1-params['m'])*LFmax + params['Qe']*params['K']*PPFD *
    #             np.exp(-params['K']*states['lai']))
    #            )

    # Source: Jones et al 1999, spreadsheet
        #Qe = 0.084 * (1 - 0.143 * np.exp(0.0295 * (Tmed_hour - params['TQe'])))
        Qe = self.params['Qe']
        # print(Qe)
        photo = (
            (
                (1-self.params['m'])*LFmax +
                Qe*self.params['K']*rad_hour*self.info['TRGH']
                ) /
                 (
                     (1-self.params['m'])*LFmax +
                     (
                         Qe*self.params['K']*rad_hour*self.info['TRGH'] *
                      np.exp(-self.params['K']*self.lai)
                      )
                  )
                 )

        Pg = adjustment * np.log(photo)
        self.params['photo'] = Pg.mean(axis=0).sum(axis=0)

        return Pg


    # %% Respiration
    def respiration_hourly(self, Tmed_hour: np.ndarray) -> np.ndarray:
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
                ((self.params['rm'] / 24) *
                 (self.w - self.wm)) *
                (self.params['Q10'] ** ((Tmed_hour - 20.) * 0.1)).reshape(1, 24)
                )
        self.params['resp'] = Rm.mean(axis=0).sum(axis=0)

        return Rm


    # %% Daily assimilation
    def GR_net(self) -> float:
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
        rad_hour = self.weather_d.rad.values
        Tmed_hour = self.weather_d.tmean.values
        co2_hour = self.weather_d.co2.values

        Pg_d = self.photosynthesis_hourly(co2_hour=co2_hour,
                                          rad_hour=rad_hour,
                                          Tmed_hour=Tmed_hour)
        Rm_d = self.respiration_hourly(Tmed_hour=Tmed_hour)

        GR_net_part_hourly = (Pg_d - Rm_d).reshape(-1, 24)

        GR_net_out = (self.params['E'] *
                      GR_net_part_hourly.sum(axis=1).reshape(-1, 1) *
                      (1 - self.fR_N())
                      )
        #GR_net_out_1d = np.ravel(GR_net_out)

        return GR_net_out


    # %% Auxiliary functions
    def gF_Tdaytime(self) -> float:
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
        Tdaytime = aux.calc_T_daytime(weather_d=self.weather_d)

    # Source: Jones et al., 1999, paper
    #    if Tdaytime > params['T_crit']:
    #        g = 1.0 - 0.154 * (Tdaytime - params['T_crit'])
    #    else:
    #        g = 1.0

    # Source: Jones et al. 1999, spreadsheet
        g = np.max([0.09,
                    np.min([1, 1 - 0.154 * (Tdaytime - self.params['T_crit'])])])
        return g


    def fN_T(self, Tmed_hour: np.ndarray) -> np.ndarray:
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
        modifier_fN_T = np.minimum(0.25 + self.params['sl_N1']*Tmed_hour,
                                   2.5 - self.params['sl_N2']*Tmed_hour)
        np.minimum(modifier_fN_T, 1., out=modifier_fN_T)
        # modifier_fN_T = np.minimum(modifier_fN_T, 1.)

        return modifier_fN_T


    def PGRED_T(self, Tmed_hour: np.ndarray) -> np.ndarray:
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
                                                      self.params['tmaxPg'])))
                                )

        return modifier_Pg


    def fR_N(self) -> float:
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
        modifier_fR_N = np.maximum(0.18 - self.params['sl_R'] * self.n,
                                   [0.02])

        return modifier_fR_N


    def lambda_Tavg(self) -> float:
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
        Tdaytime = aux.calc_T_daytime(self.weather_d)
        modifier_lambda = (45 - Tdaytime)/self.params["TSlop"]
        modifier_lambda = np.min([modifier_lambda, 1.])

        return modifier_lambda


    def fF_Tavg(self) -> float:
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
        Tavg = self.weather_d.tmean.mean()
        modifier_fF = np.max([0.,
                              np.min([1.,
                                      0.0625*(Tavg-self.params['tmin_fr_gr'])])])

        return modifier_fF


    # %% States
    # =============================================================================
    #  State: Number of nodes
    # =============================================================================
    def rate_nNodes(self) -> float:
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

        Tmed_hour = self.weather_d.tmean.values
        dNdt_sum = (self.params['N_max'] * 1/24 *
                    self.fN_T(Tmed_hour).sum())

        return dNdt_sum


    # =============================================================================
    #  State: Leaf area index
    # =============================================================================
    def rate_LAI(self, N_prev: float, LAI_prev: float) -> float:
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
        mask = (LAI_prev <= self.info['LAI_max']).reshape(-1, )
        dLAIdt = np.array(np.repeat(0., np.array(mask).size)).reshape(-1, 1)

        try:
            temp = (
                self.info['ro'] * self.params['delta'] *
                self.lambda_Tavg() *
                np.exp(self.params['beta'] * (N_prev[mask] - self.params['N_b'])) *
                self.dn /
                (1 + np.exp(self.params['beta'] *
                            (N_prev[mask] - self.params['N_b'])))
                )
            dLAIdt[mask] = temp
        except ValueError:
            dLAIdt

        return dLAIdt


    # =============================================================================
    #  State: Aboveground dry weight
    # =============================================================================
    def rate_aboveground(self) -> float:
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

        mask = (self.lai < self.info['LAI_max']).reshape(-1, )
        p_1 = np.repeat(self.params['p_1'], self.N)
        p_1[mask] = 0.

        dWdt = self.net_biomass - p_1.reshape(-1,1) * self.info['ro'] * self.dn
        dWdt_max = (self.dwf +
                    (self.params['V_max'] - p_1.reshape(-1,1)) *
                    self.info['ro'] * self.dn)

        # dWdt_max = dWdt
        dW = np.minimum(dWdt, dWdt_max)

        return dW


    # =============================================================================
    #  State: Total fruit dry weight
    # =============================================================================
    def rate_fruits(self) -> float:
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
        mask = (self.n > self.params['N_FF']).reshape(-1, )
        dWfdt = np.zeros((self.N, 1))

        if sum(mask) > 0:
            try:
                dWfdt[mask] = (
                    self.net_biomass[mask] * self.params['alpha_F'] *
                    self.fF_Tavg() *
                    (1 - np.exp(-self.params['V']*(self.n[mask] -
                                              self.params['N_FF']))) *
                    self.gF_Tdaytime()
                    )
            except ValueError:
                dWfdt

        return dWfdt


    # =============================================================================
    #  State: Mature fruit dry weight
    # =============================================================================
    def rate_mature(self) -> float:
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
        Tavg = self.weather_d.tmean.mean()

        # Source: Jones et al (1999), spreadsheet
        # According to them, the following interpolation corresponds to
        # Marcelis and Koning (1995)
    #   fDVS_x = [0., 9., 17., 19., 21., 23., 28., 35., 50.]
    #   fDVS_y = [0., 0., 0.54, 0.7, 0.8, 1., 1., 1., 0]
    #   DVS = np.interp(n, fDVS_x, fDVS_y)
        # However, this approximation is good enough
        DVS = np.min([1, 0.0714 * (Tavg - 9.)])

        mask = (self.n >
                (self.params['N_FF'] + self.params['K_F'])).reshape(-1, )
        dWmdt = np.zeros((self.N, 1))

        mult = np.maximum(0., DVS)
        try:
            dWmdt[mask] = self.params['DFmax']*(self.wf[mask] -
                                                self.wm[mask]) * mult
        except ValueError:
            dWmdt

        return dWmdt


    # %% Model
    # =============================================================================
    # Original model
    # =============================================================================
    def integrate(self, dt) -> dict:
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
        try:
            self.n = self.n.reshape(self.N, 1)
            self.lai = self.lai.reshape(self.N, 1)
            self.w = self.w.reshape(self.N, 1)
            self.wf = self.wf.reshape(self.N, 1)
            self.wm = self.wm.reshape(self.N, 1)
        except:
            self.N = 1

        self.net_biomass = self.GR_net()
        # print(dat, net_biomass)

        N_prev = self.n
        self.dn = np.array(self.rate_nNodes())
        self.n = np.array(self.n + self.dn)

        LAI_prev = self.lai
        self.dlai = self.rate_LAI(N_prev, LAI_prev)
        self.lai = np.minimum((self.lai + self.dlai),
                              self.info['LAI_max'])

        self.dwm = self.rate_mature()
        self.wm = np.array(self.wm + self.dwm)

        self.dwf = self.rate_fruits()
        self.wf = np.array(self.wf + self.dwf)

        self.dw = self.rate_aboveground()
        self.w = np.array(self.w + self.dw)


    #%% Modified versions for assimilation

    def integrate_mod_assim(self, config) -> dict:
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

        setattr(self.model, config["state_var"], self.x_post)

        self.model.net_biomass = self.model.GR_net()
        # print(dat, net_biomass)

        if config["state_var"] != "n":
            N_prev = np.array(self.model.n, ndmin=1)
            self.model.dn = np.array(self.model.rate_nNodes(), ndmin=1)
            self.model.n = np.array(self.model.n + self.model.dn, ndmin=1)

        if config["state_var"] != "lai":
            LAI_prev = self.model.lai
            self.model.dlai = self.model.rate_LAI(N_prev, LAI_prev)
            self.model.lai = np.minimum((self.model.lai + self.model.dlai),
                                        self.model.info['LAI_max'])

        if config["state_var"] != "wm":
            self.model.dwm = self.model.rate_mature()
            self.model.wm = np.array(self.model.wm + self.model.dwm, ndmin=1)

        if config["state_var"] != "wf":
            self.model.dwf = self.model.rate_fruits()
            self.model.wf = np.array(self.model.wf + self.model.dwf, ndmin=1)

        if config["state_var"] != "wm":
            self.model.dw = self.model.rate_aboveground()
            self.model.w = np.array(self.model.w + self.model.dw, ndmin=1)

        # After predict and update, all states keep their original values
        # as modifications are present only on x values.
        # If I want to determine the modifications on other states caused by
        # the step, I have to replace the modified variable
        # by x_post and not calculate it manually while calculating the others.

    def integrate_mod_jac(self, config):

        setattr(self.model, config["state_var"], self.x_post)

        Tmed_hour = self.model.weather_d.tmean.values

        dlaidn = (
            (self.model.info['ro'] * self.model.params['delta'] *
             self.model.lambda_Tavg() * self.model.dn) *
            (self.model.params['beta'] * np.exp(self.model.params['beta'] *
                                                (self.model.n +
                                                 self.model.params['N_b'])) /
             (np.exp(self.model.params['beta']*self.model.params['N_b']) +
              np.exp(self.model.params['beta']*self.model.n))
             )
            )

        dwdw = (self.params['Q10'] ** ((Tmed_hour - 20.) * 0.1)).reshape(1, 24)
        dwfdw = dwdw
        dwdwm = -dwdw
        dwfdwm = -dwdw

        Tavg = self.model.weather_d.tmean.mean()
        DVS = np.min([1, 0.0714 * (Tavg - 9.)])
        if self.n <= (self.params['N_FF'] + self.params['K_F']):
            dwmdwf = 0.
        else:
            mult = np.maximum(0., DVS)
            dwmdwf = self.params['DFmax']*(self.wf - self.wm) * mult

        dwmdwm = -dwfdwm

        self.model.jacob = np.array([[0, 0, 0, 0, 0],
                                     [dlaidn, 0, 0, 0, 0],
                                     [0, 0, dwdw, 0, dwdwm],
                                     [0, 0, dwfdw, 0, dwfdwm],
                                     [0, 0, 0, dwmdwf, dwmdwm]])

