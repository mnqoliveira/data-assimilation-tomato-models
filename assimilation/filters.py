# -*- coding: utf-8 -*-
"""
Created on Fri Mar 26 09:24:51 2021

@author: Monique
"""

import numpy as np
# import math as math
import copy
import scipy.stats

import auxiliary_functions.f_aux as aux

#from filterpy.kalman import ExtendedKalmanFilter as filterpy_EKF
from filterpy.kalman import UnscentedKalmanFilter as filterpy_UKF
from filterpy.kalman import EnsembleKalmanFilter as filterpy_EnKF
from filterpy.kalman import MerweScaledSigmaPoints
from filterpy.monte_carlo import systematic_resample
from filterpy.common import pretty_str, outer_product_sum

# %% UKF functions

class UKF(filterpy_UKF):
    """
    Overwriting prediction as implemented in filterpy,
    to allow for different ensemble generations.
    """

    def compute_process_sigmas(self, dt, fx=None, **fx_args):
        """
        computes the values of sigmas_f. Normally a user would not call
        this, but it is useful if you need to call update more than once
        between calls to predict (to update for multiple simultaneous
        measurements), so the sigmas correctly reflect the updated state
        x, P.
        """

        if fx is None:
            fx = self.fx

        st_default = list()

        for st in self.config["states_names"]:
            st_default.append(getattr(self.model, st))

        # calculate sigma points for given mean and covariance
        sigmas = self.points_fn.sigma_points(self.x, self.P)

        for i, s in enumerate(sigmas):
            # As there are different state variables also affected by the
            # model, resetting all variables after calculating each
            # sigma is necessary
            setattr(self.model, self.config["state_var"], s)
            self.fx(self.model, self._dt)
            self.sigmas[i] = getattr(self.model, self.config["state_var"])

            for st_it, st in enumerate(self.config["states_names"]):
                setattr(self.model, st, st_default[st_it])


def filter_ukf(model_obj, proc_func, meas_func,
               dataset, config_it,
               dt,
               dim_x, dim_z):
    sigmas = MerweScaledSigmaPoints(dim_x, alpha=0.001, beta=2., kappa=1)

    ukf = UKF(dim_x, dim_z, fx=proc_func, hx=meas_func,
              dt=dt, points=sigmas)
    ukf.x = dataset['states'].copy()[config_it['state_var']]
    # ukf.Q = config_it['Q'] # squares included afterwards, depending on case
    ukf.Q = 0.01 # placeholder values required for initalization and reasonably low
    ukf.P = config_it['P']**2
    ukf.sigmas = ukf.sigmas_f

    ukf.config = config_it
    ukf.model = model_obj

    try:
        ukf.R = float(config_it['R'])
    except ValueError:
        ukf.R = [float(r) for r in config_it['R'].split(",")]

    ukf.R = ukf.R**2

    return ukf


# %% EnKF functions
class EnKF(filterpy_EnKF):
    """
    Overwriting predict and update as implemented in filterpy,
    as they do not account for
    other arguments in the process/measurement functions.
    Also, to allow for different ensemble generations.
    """

    def predict(self, dt=None, fx=None, **fx_args):

        if np.isscalar(self.Q):
            self.Q = np.eye(self.dim_x) * self.Q

        st_default = list()

        for st in self.config["states_names"]:
            st_default.append(getattr(self.model, st))

        for i, s in enumerate(self.sigmas):
            if (self.config["case"] == "case2"):
            # Case 2: Parameters
            # A pre-selected parameter is perturbed and this value is then
            # applied in the prediction of state particles
                self.model.params = self.list_params[i]

            elif (self.config["case"] == "case3"):
            # Case 3: inputs
            # A pre-selected input is perturbed and this value is then
            # applied in the prediction of state particles
                self.model.weather_d = self.model.list_weather_d[i]

            setattr(self.model, self.config["state_var"], s)
            self.fx(self.model, self.dt)
            self.sigmas[i] = getattr(self.model, self.config["state_var"])
            #print(self.sigmas[i])

            for st_it, st in enumerate(self.config["states_names"]):
                setattr(self.model, st, st_default[st_it])

        if (self.config["case"] == "case1"):
        # Case 1: States
        # Sigmas representing state particles are passed through
        # the predict function and then perturbed with the
        # uncertainty Q
            e = np.random.multivariate_normal(self._mean, self.Q, self.N)
            self.sigmas += e

        self.x = np.mean(self.sigmas, axis=0)
        self.P = outer_product_sum(self.sigmas - self.x) / (self.N - 1)

        # save prior
        self.x_prior = np.copy(self.x)
        self.P_prior = np.copy(self.P)


    def update(self, z, hx=None, **hx_args):

        if hx is None:
            hx = self.hx

        if z is None:
            self.z = np.array([[None]*self.dim_z]).T
            self.x_post = self.x.copy()
            self.P_post = self.P.copy()
            return

        try:
            R = float(self.R)

        except ValueError:
            R = [float(r) for r in self.R.split(",")]

        if (np.isscalar(R) or len(R) > 1):
            R = np.eye(self.dim_z) * R

        N = self.N
        sigmas_h = np.zeros((N, self.dim_z))

        # transform sigma points into measurement space
        for i in range(N):
            sigmas_h[i] = self.hx(self.sigmas[i], **hx_args)

        z_mean = np.mean(sigmas_h, axis=0)

        P_zz = (outer_product_sum(sigmas_h - z_mean) / (N-1)) + R
        P_xz = outer_product_sum(
                self.sigmas - self.x, sigmas_h - z_mean) / (N - 1)

        self.S = P_zz
        self.SI = self.inv(self.S)
        self.K = np.dot(P_xz, self.SI)
        self.y = np.subtract(z, z_mean)   # residual
        #self.y = 1

    	# Perturbs observations with normal distribution of mean R.
        e_r = np.random.multivariate_normal(self._mean_z, R, N)
        for i in range(N):
            self.sigmas[i] = (self.sigmas[i] + 
                              np.dot(self.K, z + e_r[i] - sigmas_h[i]))

        self.x = np.mean(self.sigmas, axis=0)
        self.P = self.P - np.dot(np.dot(self.K, self.S), self.K.T)

        # save measurement and posterior state
        self.z = copy.deepcopy(z)
        self.x_post = self.x.copy()
        self.P_post = self.P.copy()


    def __repr__(self):
        return '\n'.join([
            'EnKF object',
            pretty_str('x', self.x),
            pretty_str('P', self.P),
            pretty_str('x_prior', self.x_prior),
            pretty_str('P_prior', self.P_prior),
            pretty_str('x_post', self.x_post),
            pretty_str('P_post', self.P_post),
            pretty_str('R', self.R),
            pretty_str('Q', self.Q),
            pretty_str('K', self.K),
            pretty_str('S', self.S),
            pretty_str('y', self.y),
            # pretty_str('weights', self.weights),
            # pretty_str('sigmas', self.sigmas)
            ])


def filter_enkf(model_obj, proc_func, meas_func,
                dataset, config_it,
                dt, dim_x, dim_z):

    upd_var = config_it['state_var']
    param_pert = config_it['param_pert']
    # This will generate sigmas applying initial states perturbation
    enkf = EnKF(x=dataset['states'].copy()[upd_var],
                P=np.eye(dim_x)*config_it['P']**2,
                dim_z=dim_z, dt=dt,
                N=int(config_it['N']),
                fx=proc_func, hx=meas_func)

    #enkf.Q = config_it['Q'] # squares included afterwards, depending on case
    enkf.Q = 0.01 # initial values required and reasonably low
    enkf.R = config_it['R']**2
    enkf.y = 1
    enkf.config = config_it
    enkf.model = model_obj
    enkf.model.N = enkf.N

    if config_it["case"] == "case2":
        x = enkf.model.params[param_pert]
        enkf.model.params[param_pert] = aux.gen_perturb(config_it, x)
        enkf.list_params = aux.expand_dict_with_list(enkf.model.params,
                                                     param_pert)

    return enkf


# %% PF functions
class PF(object):

    """
    It should be possible to update more than one state simultaneously as
    all states compose the particles and all of them could be resampled.
    But it is also the case that only one variable is of interest and is
    resampled. This implementation used this last case.

    Differently from the implementation of Kalman Filters in this work,
    PF treats as particles all the states of the model. Only one state
    is updated, but they are all carried along together.
    
    OBS: Although it is possible to obtain results with this code, it has
    not been widely tested.

    """
    def __init__(self, dim_x, dim_z, dt, hx, fx):
        """
        Creates a particle filter.
        """
        self.x = np.zeros(dim_x)
        # XXX Checar
        self.P = np.zeros(dim_x)
        #self.P = np.eye(dim_x)
        self.R = np.eye(dim_z)
        self._dim_x = dim_x
        self._dim_z = dim_z
        self._dt = dt
        self.hx = hx
        self.fx = fx

        # these will always be a copy of x_mean,P after predict() is called
        self.x_prior = self.x.copy()
        self.P_prior = self.P.copy()

        # these will always be a copy of x_mean,P after update() is called
        self.x_post = self.x.copy()
        self.P_post = self.P.copy()

        self.N = np.zeros(1)
        self.weights = np.ones(1)

    def predict(self, dt=None, fx=None, **fx_args):

        # XXX Testar de qq forma pq é como eu vou ver se funciona qdo eu rodo um
        # por vez e os vetores não são compridos
        # # V1, loop atribui cada linha de sigmas aos meus estados em
        # # self.model

        # for l in range(self.sigmas.shape[0]):
        #     for c in range(self.sigmas.shape[1]):
        #         setattr(self.model,
        #                 self.config["states_names"][c],
        #                 self.sigmas[l, c])
        #     self.fx(self.model, self._dt)

        #     for c in range(self.sigmas.shape[1]):
        #         self.sigmas[l, c] = getattr(self.model,
        #                                     self.config["states_names"][c])

        # V2, atribui pra cada estado o vetor com todos os valores assumidos
        # pelo estado. Allegedly, vai fazer os cálculos de todos
        # simultaneamente.
        for c in range(self.sigmas.shape[1]):
            setattr(self.model,
                    self.config["states_names"][c],
                    self.sigmas[:, c])

        self.fx(self.model, self._dt)

        for c in range(self.sigmas.shape[1]):
            self.sigmas[:, c] = np.ravel(getattr(self.model,
                                                 self.config["states_names"][c]))


        for it, st in enumerate(self.config["states_names"]):
            s = getattr(self.model, st)

            if st == self.config["state_var"]:
                self.sigmas[:, it] = (np.ravel(s) +
                                      np.ravel(np.random.rand(self.N) * self.Q))

            else:
                self.sigmas[:, it] = np.ravel(s) + np.ravel(np.zeros(self.N))

        self.x, self.P = self.estimate()
        self.x_prior = np.copy(self.x)
        self.P_prior = np.copy(self.P)

    def update(self, z, R=None, hx=None, **hx_args):

        if z is None:
            return

        if hx is None:
            hx = self.hx

        if R is None:
            R = self.R
        # XXX Tratar para o caso de 2d
        # elif np.isscalar(R):
        #     R = np.eye(self._dim_z) * R

        # Select state
        mask = [c == self.config["state_var"]
                for c in self.config["states_names"]]
        sigmas_sel = self.sigmas[:, mask]

        # Differently from sigmas, sigmas_z only includes the updated
        # state variable
        #z = measurement
        #self.sigmas_z = hx(sigmas_sel, dataset=obs.loc[obs.dat == dat, :])
        self.sigmas_z = hx(sigmas_sel, **hx_args)

        # XXX Fazer loop nas dimensoes do z
        self.weights *= scipy.stats.norm(np.ravel(self.sigmas_z), R).pdf(z)
        self.weights += 1.e-300      # avoid round-off to zero
        self.weights /= np.sum(self.weights)  # normalize

        # Including resampling if too few effective particles
        # in update not to perturb the overall filter
        # sequence of predict/update
        if self.neff() < self.N/2:
        #if self.neff() < 100:
            #print(self.neff(), 'effective particles')
            indexes = systematic_resample(self.weights)
            self.resample_from_index(indexes)
            assert np.allclose(self.weights, 1/self.N)

        self.x, self.P = self.estimate()
        self.x_post = self.x.copy()
        self.P_post = self.P.copy()

    def create_particles(self):
        # Initialize particles ascribing uncertainty in initial states to
        # the target variable and just propagating initial states to other
        # variables
        self.sigmas = np.zeros([self.N, self._dim_x])

        for it, st in enumerate(self.config["states_names"]):
            s = getattr(self.model, st)

            if st == self.config["state_var"]:
                self.sigmas[:, it] = (s + (np.random.rand(self.N) * self.P))

            else:
                self.sigmas[:, it] = (s + (np.zeros(self.N)))

    def estimate(self):
        """returns mean and variance of the weighted particles"""
        mean = np.average(self.sigmas, weights=self.weights, axis=0)
        P = np.average((self.sigmas-mean)**2, weights=self.weights, axis=0)

        return mean, P

    def neff(self):
        return 1. / np.sum(np.square(self.weights))

    def resample_from_index(self, indexes):

        self.sigmas[:] = self.sigmas[indexes]
        self.weights[:] = self.weights[indexes]
        self.weights.fill(1.0 / len(self.weights))

    def __repr__(self):
        return '\n'.join([
            'ParticleFilter object',
            pretty_str('x', self.x),
            pretty_str('P', self.P),
            pretty_str('x_prior', self.x_prior),
            pretty_str('P_prior', self.P_prior),
            pretty_str('x_post', self.x_post),
            pretty_str('P_post', self.P_post),
            pretty_str('R', self.R),
            pretty_str('hx', self.hx),
            pretty_str('fx', self.fx),
            pretty_str('N', self.N),
            # pretty_str('weights', self.weights),
            # pretty_str('sigmas', self.sigmas)
            ])


def filter_pf(model_obj, proc_func, meas_func,
              dataset, config_it,
              dt=1,
              dim_x=1, dim_z=1):

    # predict > so chamar a proc_func. Nada de mto diferente.
    # update > faz update em duas variaveis. Se eu ja chamo a estimate dentro
    # de update, a saida eh x e P como no outro caso. Eu posso criar a
    # classe update e deixar com as mesmas saidas do ukf

    dt=1
    dim_x=5
    dim_z=1

    pf = PF(dim_x, dim_z,
            fx=proc_func, hx=meas_func, dt=dt)
    pf.x = dataset['states'].copy()
    pf.P = config_it['P']
    pf.Q = config_it['Q']
    pf.R = config_it['R']  # Default value for measurement error
    pf.N = int(config_it['N'])

    pf.model = model_obj
    pf.config = config_it
    pf.model.N = pf.N

    # This will generate particles applying initial state perturbation
    pf.create_particles()
    pf.weights = np.ones(pf.N) / pf.N

    return pf


# # %% EKF functions
# class EKF(filterpy_EKF):
#     """
#     Overwriting predict and update as implemented, as they do not account for
#     other arguments in the process/measurement functions.
#     """

#     def predict(self, dt=None, fx=None, **fx_args):

#         # save prior
#         self.x_prior = np.copy(self.x)
#         self.P_prior = np.copy(self.P)

#         # if not isinstance(fx_args, tuple):
#         #     args = (fx_args,)

#         # H = HJacobian(x, *args)

#         # # predict step
#         # x = dot(F, x) + dot(B, u)
#         # P = dot(F, P).dot(F.T) + Q

#         # save prior
#         self.x_prior = np.copy(self.x)
#         self.P_prior = np.copy(self.P)


#     def update(self, z, hx=None, **hx_args):

#         if not isinstance(hx_args, tuple):
#             hx_args = (hx_args,)

#         if np.isscalar(z) and self.dim_z == 1:
#             z = np.asarray([z], float)

#         self.x_post = self.x.copy()
#         self.P_post = self.P.copy()


# def filter_ekf(proc_func, meas_func,
#                 dataset, config_it,
#                 dt,
#                 dim_x, dim_z):

#     ekf = EKF(x=dataset['states'].copy()[config_it['state_var']],
#                 P=np.eye(dim_x)*config_it['P'],
#                 dim_z=dim_z, dt=dt,
#                 N=int(config_it['N']),
#                 fx=proc_func, hx=meas_func)
#     ekf.Q = config_it['Q']
#     ekf.R = config_it['R']  # Default value for measurement error

#     return ekf

