# -*- coding: utf-8 -*-
"""
Created on Sun Mar 31 12:19:34 2019

@author: monique

Measurement functions take a state variable and return the measurement that
would correspond to that state value.

"""

import numpy as np


#%% Indirect measurements
def W_ind_dest(W, dataset):
    # Wfm ~ W

    if dataset.loc[:, "cycle"].values.item() == 2:
        W_z = 8.3232752230702 * W + 0.47353059300635
    elif dataset.loc[:, "cycle"].values.item() == 3:
        W_z = 7.80892263312983 * W + -0.526373601494307
    elif dataset.loc[:, "cycle"].values.item() == 4:
        W_z = 6.8960816206108 * W + 0.678711729639502

    return W_z


def W_ind_non_dest(W, dataset):
    # W_full_fm ~ W

    # Convert to fresh mass
    W_t = W_ind_dest(W, dataset)

    # Include roots
    if ((dataset.loc[:, "stage"].values.item() == "fruits") or
        (dataset.loc[:, "stage"].values.item() == "maturity")):
        # If fruits present, root biomass corresponds to 10% of
        # total mass. Otherwise, 20%.
        W_z = W_t/0.9
    else:
        W_z = W_t/0.8

    return W_z


# def W_ind_non_dest1(W, dataset):
#     # Lai_lat ~ W
#     if dataset.loc[:, "cycle"].values.item() == 2:
#         W_z = 0.000721489301534449 * W + 0.0342958768986944
#     elif dataset.loc[:, "cycle"].values.item() == 3:
#         W_z = 0.000889037627608401 * W + 0.0113519425470375
#     elif dataset.loc[:, "cycle"].values.item() == 4:
#         W_z = 0.00132247905034675 * W + 0.00798881571198673

#     return W_z


# def W_ind_non_dest2(W, dataset):
#     # Height ~ W

#     if dataset.loc[:, "cycle"].values.item() == 2:
#         W_z = 0.0041288518609504 * W + 0.291874831697662
#     elif dataset.loc[:, "cycle"].values.item() == 3:
#         W_z = 0.00354675263026496 * W + 0.485804524050112
#     elif dataset.loc[:, "cycle"].values.item() == 4:
#         W_z = 0.00423744432241557 * W + 0.414647824361298

#     return W_z

def LAI_ind1(LAI, dataset):
    # Lai_lat ~ LAI

    if dataset.loc[:, "cycle"].values.item() == 2:
        LAI_z = 0.142357144856945 * LAI + 0.00343966879530273
    elif dataset.loc[:, "cycle"].values.item() == 3:
        LAI_z = 0.253600748522186 * LAI + 0.00296665867154313
    elif dataset.loc[:, "cycle"].values.item() == 4:
        LAI_z = 0.225886608778104 * LAI + 0.00474618482126183

    return LAI_z


def LAI_ind2(LAI, dataset):
    if dataset.loc[:, "cycle"].values.item() == 2:
        LAI_z = 0.987191393475075 * LAI + 0.000574155398015553
    elif dataset.loc[:, "cycle"].values.item() == 3:
        LAI_z = 1.30099271623708 * LAI + -0.00128416987154649
    elif dataset.loc[:, "cycle"].values.item() == 4:
        LAI_z = 1.16264780591812 * LAI + -0.000480558841441764

    return LAI_z


# def LAI_ind3(LAI, dataset):

#     # Lai_lat ~ LAI
#     #LAI_z = 0.2099*LAI**0.8716
#     LAI_z1 = float(LAI_ind1(LAI, dataset))

#     # Lai_abv ~ LAI
#     LAI_z2 = float(LAI_ind2(LAI, dataset))

#     return [LAI_z1, LAI_z2]


def Wf_ind1(Wf, dataset):

    # wf_lat ~ wf

    if dataset.loc[:, "cycle"].values.item() == 2:
        Wf_z = 0.00011861002622963 * Wf + 0.00196519701356733
    elif dataset.loc[:, "cycle"].values.item() == 3:
        Wf_z = 0.000131245632212613 * Wf + 0.0017700498103639
    elif dataset.loc[:, "cycle"].values.item() == 4:
        Wf_z = 0.00033418618841045 * Wf + 0.000135801580373565

    return Wf_z


# def LAI_W_ind(LAI, W):
# Se eu for fazer dois estados simultaneamente
#     """takes a state variable and returns the measurement that
#     would correspond to that state"""

#     # |￣￣￣￣￣￣￣￣￣￣ |
#     # |     ATENCAO         |
#     # |＿＿＿＿＿＿＿＿＿＿ |
#     #   ∧＿∧  ||
#     #  (  ´ω`) ||
#     #   / 　　づ
#     # Funcao inventada
#     LAI_z = LAI**0.6
#     W_z = W/0.3

#     return [LAI_z, W_z]


#%% Direct measurements do not require conversion
def N_dir(N, dataset):

    N_z = N

    return N_z

def W_dir(W, dataset):

    W_z = W

    return W_z

def LAI_dir(LAI):

    LAI_z = LAI

    return LAI_z


def Wf_dir(Wf, dataset):

    Wf_z = Wf

    return Wf_z


def Wm_dir(Wm, dataset):

    Wm_z = Wm

    return Wm_z


def LAI_W_dir(LAI, W):

    LAI_z = LAI
    W_z = W

    return [LAI_z, W_z]

#%% Simple
def fsolar_LAI(fsolar, coef):

    LAI_z = -np.log(1 - fsolar)/coef

    return LAI_z


def fsolar_cover(fsolar, coef):

    cover_z = 1-np.exp(-coef*fsolar)

    return cover_z
