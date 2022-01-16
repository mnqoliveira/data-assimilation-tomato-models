# -*- coding: utf-8 -*-
"""
Created on Fri Jan  3 10:11:43 2020

@author: Monique
Extracts annotation from monitored plants
"""

# -*- coding: utf-8 -*-

import cv2
import numpy as np
import os
import pandas as pd
import f_aux as aux


# %%
# =============================================================================
# Auxiliary functions
# =============================================================================
def calc_conv_fact(cycle, camera, img_leaves):
    """
    Reference: Determine conversion pixel -> m

    Parameters
    ----------
    def calc_conv_fact : TYPE
        DESCRIPTION.

    Returns
    -------
    None.

    """

    conv = []
    area = []
    if ((camera == "camerapi01") | (camera == "camerapi03")):
        pos = "lat"
    else:
        pos = "abv"

    if cycle == "ciclo02":
        # Filter ref based on RGB. Bucket height was marked in pink.
        lower_thresh = np.array([255, 0, 255])
        upper_thresh = np.array([255, 0, 255])

        ref_bw, _ = aux.find_contour(img_leaves.copy(), lower_thresh,
                                     upper_thresh)

        if len(ref_bw) > 0:
            # Bounding box
            # ( center (x,y), (width, height), angle of rotation )
            rect = cv2.minAreaRect(ref_bw[0])

            # Reference length
            ref_dim1 = rect[1][0]
            ref_dim2 = rect[1][1]

            ref_length = np.max((ref_dim1, ref_dim2))

            if pos == "lat":
                # Bucket height
                conv = 27*10**(-2) / ref_length
                # M = cv2.moments(ref_bw[0])
                # area_calc = M['m00'] * conv * conv
                # area_nom = 8.9*5.7*10**(-4)
                # area_err = np.round(np.abs((area_calc - area_nom)*100/area_nom))
                # area = [area_calc, area_nom, area_err]
            else:
                # Bucket diameter
                conv = 31*10**(-2) / ref_length


    else:
        # Sheet
        lower_thresh = np.array([255, 255, 0])
        upper_thresh = np.array([255, 255, 0])

        ref_bw, _ = aux.find_contour(img_leaves.copy(),
                                     lower_thresh, upper_thresh)

        # Bounding box
        # ( center (x,y), (width, height), angle of rotation )
        if len(ref_bw) > 0:
            rect = cv2.minAreaRect(ref_bw[0])

            # Reference length
            ref_dim1 = rect[1][0]
            ref_dim2 = rect[1][1]

            ref_length = np.max((ref_dim1, ref_dim2))

            ref_length = np.max((ref_dim1, ref_dim2))
            conv = 29.7*10**(-2) / ref_length

            M = cv2.moments(ref_bw[0])
            area_calc = M['m00'] * conv * conv
            area_nom = 29.7*21*10**(-4)
            area_err = np.round(np.abs((area_calc - area_nom)*100/area_nom))
            area = [area_calc, area_nom, area_err]
        else:
            # Pink square
            lower_thresh = np.array([255, 0, 255])
            upper_thresh = np.array([255, 0, 255])

            ref_bw, _ = aux.find_contour(img_leaves.copy(),
                                         lower_thresh, upper_thresh)

            if len(ref_bw) > 0:
                # Bounding box
                # ( center (x,y), (width, height), angle of rotation )
                rect = cv2.minAreaRect(ref_bw[0])

                # Reference length
                ref_dim1 = rect[1][0]
                ref_dim2 = rect[1][1]

                ref_length = np.max((ref_dim1, ref_dim2))
                conv = 3.0*10**(-2) / ref_length

                M = cv2.moments(ref_bw[0])
                area_calc = M['m00'] * conv * conv
                area_nom = 9*10**(-4)
                area_err = np.round(np.abs((area_calc - area_nom)*100/area_nom))
                area = [area_calc, area_nom, area_err]

    if type(conv) != list:
        conv = [conv]

    return conv, area


# %%
# =============================================================================
# Load and process files
# =============================================================================

cycle_l = ["ciclo02", "ciclo03", "ciclo04"]
camera_l = ["camerapi01", "camerapi03", "camerapi02", "camerapi04"]

path_base_in = 'E:/drive_unicamp/SISDA/projetoModelagemTomateiro/'
path_base_out = '../data/observations/monitoring/'
it_exp = 0

for cycle in cycle_l:

    heights_file = []
    leaves_file = []
    fruits_file = []
    mfruits_file = []
    nodes_file = []
    ref_file = []

    for camera in camera_l:
        print(camera)

        path = path_base_in + 'fotos_rotulos/' + cycle + '/' + camera + "/"
        orig_imgs = os.listdir(path)
        nodes = [s for s in orig_imgs if "nodes" in s]
        leaves = [s for s in orig_imgs if "leaves" in s]

        path_save = path_base_in + 'imgs_proc/cover/' + cycle + '/' + camera + '/'

        if (len(leaves) > 0):

            for img_it in leaves:
                path_read = path + img_it
                #print(img_it)

                img_leaves = cv2.imread(path_read)
                #print(img_leaves.shape)

                # Given cycle02 does not have a reference in all pictures, the
                # reference in the first of the cycle is read and then the value
                # is kept as fixed
                temp, area_ref = calc_conv_fact(cycle, camera, img_leaves)
                if len(temp) > 0:
                    conv = temp[0]
                conv_sq = conv * conv

            # =============================================================================
            # Leaves
            # =============================================================================
                # Filter leaves based on RGB
                lower_green = np.array([0, 255, 0])
                upper_green = np.array([0, 255, 0])

                contours, contour_height = aux.find_contour(img_leaves.copy(),
                                                            lower_green,
                                                            upper_green)

                if ((camera == "camerapi01") | (camera == "camerapi03")):
                    height_file = aux.calc_height(img_it,
                                                  contour_height, conv,
                                                  heights_file)

                contours_draw = cv2.drawContours(image=img_leaves.copy(),
                                                 contours=contours,
                                                 contourIdx=-1,
                                                 color=(255, 0, 0),
                                                 thickness=3)
                leaves_file = aux.calc_area(img_it, contours, conv_sq, leaves_file)
                # img_name = path_save + "c_" + img_it
                # cv2.imwrite(filename=img_name, img=contours_draw)

                if len(area_ref) == 0:
                    area_ref = [None, None, None]
                ref_file.append([img_it] + area_ref + [camera])

                if ((camera == "camerapi01") | (camera == "camerapi03")):

                    # =============================================================================
                    # Fruits
                    # =============================================================================
                        # Filter fruits based on RGB
                        lower_yellow = np.array([0, 255, 255])
                        upper_yellow = np.array([0, 255, 255])

                        contours, _ = aux.find_contour(img_leaves.copy(),
                                                       lower_yellow, upper_yellow)
                        fruits_file = aux.calc_area(img_it, contours, conv_sq, fruits_file)

                    # =============================================================================
                    # Mature fruits
                    # =============================================================================
                        # Filter mature fruits based on RGB
                        lower_red = np.array([0, 0, 255])
                        upper_red = np.array([0, 0, 255])

                        contours, _ = aux.find_contour(img_leaves.copy(),
                                                       lower_red, upper_red)
                        mfruits_file = aux.calc_area(img_it, contours, conv_sq,
                                                     mfruits_file)

            # print(leaves_file)

            if ((camera == "camerapi01") | (camera == "camerapi03")):
                [s.append(camera) for s in heights_file if len(s) < 3]
                [s.append(camera) for s in fruits_file if len(s) < 3]
                [s.append(camera) for s in mfruits_file if len(s) < 3]

            [s.append(camera) for s in leaves_file if len(s) < 3]

            # # =============================================================================
            # # Nodes
            # # =============================================================================

        if ((len(nodes) > 0) & ((camera == "camerapi01") |
                                (camera == "camerapi03"))):
            for img_it in nodes:
                path_read = path + img_it
                print(img_it)

                img_leaves = cv2.imread(path_read)
                img_leaves.shape

                # Filter nodes based on RGB
                lower_thresh = np.array([255, 255, 0])
                upper_thresh = np.array([255, 255, 0])

                contours = aux.find_contour(img_leaves, lower_thresh, upper_thresh)
                nodes_file.append([img_it, len(contours)])

            [s.append(camera) for s in nodes_file if len(s) < 3]

    end = '_' + cycle + '.csv'
    outputs = pd.DataFrame(leaves_file, columns=['filename',
                                                 'lai_', 'camera'])
    output_file_name = path_base_out + 'lai/raw_cover' + end
    outputs.to_csv(output_file_name, index=False)

    outputs = pd.DataFrame(heights_file, columns=['filename',
                                                  'height', 'camera'])
    output_file_name = path_base_out + 'dry_mass_aboveground/raw_height' + end
    outputs.to_csv(output_file_name, index=False)

    if len(fruits_file) > 0:
        outputs = pd.DataFrame(fruits_file, columns=['filename',
                                                      'lai_', 'camera'])
        output_file_name = path_base_out + 'dry_mass_fruit/raw_wfcover' + end
        outputs.to_csv(output_file_name, index=False)

    if len(mfruits_file) > 0:
        outputs = pd.DataFrame(mfruits_file, columns=['filename',
                                                      'lai_', 'camera'])
        output_file_name = path_base_out + \
            'dry_mass_mature_fruit/raw_wmcover' + end
        outputs.to_csv(output_file_name, index=False)

    outputs = pd.DataFrame(nodes_file, columns=['filename', 'count', 'camera'])
    output_file_name = '../data/observations/monitoring/nodes/raw_n' + end
    outputs.to_csv(output_file_name, index=False)

    end = 'ref_monit_' + cycle + '.csv'
    outputs = pd.DataFrame(ref_file, columns=['filename', 'area_calc',
                                              'area_nom', 'area_err', 'camera'])
    output_file_name = path_base_out +'ref_areas/' + end
    outputs.to_csv(output_file_name, index=False)