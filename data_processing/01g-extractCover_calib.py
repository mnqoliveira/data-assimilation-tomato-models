# -*- coding: utf-8 -*-
"""
Created on Fri Jan  3 10:11:43 2020

@author: Monique
Extracts annotation from calibration plants
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


def calc_conv_fact(cycle, img):
    # Filter ref based on RGB.

    pos = img_it[0:3]

    if cycle == "ciclo02":

        # Small card used for lateral view was marked in pink as
        # base of the stairs above view
        lower_thresh = np.array([255, 0, 255])
        upper_thresh = np.array([255, 0, 255])

        ref_bw, _ = aux.find_contour(img.copy(), lower_thresh, upper_thresh)

        if len(ref_bw) > 0:
            # Bounding box
            # ( center (x,y), (width, height), angle of rotation )
            rect = cv2.minAreaRect(ref_bw[0])

            # Reference length
            ref_dim1 = rect[1][0]
            ref_dim2 = rect[1][1]

            ref_length = np.max((ref_dim1, ref_dim2))
            if pos == "lat":
                conv = 8.9*10**(-2) / ref_length
                M = cv2.moments(ref_bw[0])
                area_calc = np.round(M['m00'] * conv * conv, 4)
                area_nom = 8.9*5.7*10**(-4)
                area_err = np.round(np.abs((area_calc - area_nom)*100/area_nom))
                area = [area_calc, area_nom, area_err]


            else:
                conv = 4.0*10**(-2) / ref_length
                area = [ref_length * conv, 4.0*10**(-2), 0]

    else:
        # Assuming cycles 3 and 4 follow the same patterns
        # Square in chessboard was marked in pink.
        lower_thresh = np.array([255, 0, 255])
        upper_thresh = np.array([255, 0, 255])

        ref_bw, _ = aux.find_contour(img.copy(), lower_thresh, upper_thresh)

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
            area_calc = np.round(M['m00'] * conv * conv, 4)
            area_nom = 9*10**(-4)
            area_err = np.round(np.abs((area_calc - area_nom)*100/area_nom))
            area = [area_calc, area_nom, area_err]

    if len(ref_bw) == 0:
        # A4 chessboard was marked in cian.
        lower_thresh = np.array([255, 255, 0])
        upper_thresh = np.array([255, 255, 0])

        ref_bw, _ = aux.find_contour(img.copy(), lower_thresh, upper_thresh)

        # Bounding box
        # ( center (x,y), (width, height), angle of rotation )
        rect = cv2.minAreaRect(ref_bw[0])

        # Reference length
        ref_dim1 = rect[1][0]
        ref_dim2 = rect[1][1]

        ref_length = np.max((ref_dim1, ref_dim2))
        conv = 29.7*10**(-2) / ref_length

        M = cv2.moments(ref_bw[0])
        area_calc = np.round(M['m00'] * conv * conv, 4)
        area_nom = 29.7*21*10**(-4)
        area_err = np.round(np.abs((area_calc - area_nom)*100/area_nom))
        area = [area_calc, area_nom, area_err]

    # box = cv2.boxPoints(rect)
    # box_ref = np.int0(box)

    # contours_draw = cv2.drawContours(image=img.copy(),
    #                                  contours=[box_ref],
    #                                  contourIdx=-1,
    #                                  color=(0, 255, 0),
    #                                  thickness=3)
    # cv2.imwrite(filename=img_name, img=contours_draw)

    contours_draw = cv2.drawContours(image = img.copy(),
          contours = ref_bw,
          contourIdx = -1,
          color = (255, 0, 0),
          thickness = 3)
    img_name = path_save + "ref_" + img_it
    cv2.imwrite(filename = img_name, img = contours_draw)

    return conv, area


# %%
# =============================================================================
# Load and process files
# =============================================================================

cycle_l = ["ciclo02", "ciclo03", "ciclo04"]

path_base_in = 'E:/drive_unicamp/SISDA/projetoModelagemTomateiro/'
path_base_out = '../data/observations/monitoring/'
alt_path_base_out = 'E:/doc_local/'

it_exp = 0

for it_exp in range(len(cycle_l)):
    cycle = cycle_l[it_exp]

    path = path_base_in + 'fotos_rotulos/' + cycle + "/calibracao/"
    orig_imgs = os.listdir(path)
    nodes = [s for s in orig_imgs if "nodes" in s]
    leaves = [s for s in orig_imgs if "leaves" in s]

    # first pictures have no reference
    if cycle == "ciclo02":
        leaves = leaves[3:]

    path_save = alt_path_base_out + 'imgs_proc/cover/' + cycle + '/calibracao/'

    leaves_file = []
    ref_file = []
    height_file = []
    fruits_file = []
    mfruits_file = []

    #img_it = leaves[0]

    if len(leaves) > 0:
        for img_it in leaves:
            print(img_it)
            path_read = path + img_it
            img = cv2.imread(path_read)
            # print(img.shape)

            pos = img_it[0:3]
            end = 'calib_' + cycle + '.csv'

            conv, area_ref = calc_conv_fact(cycle, img)
            conv_sq = conv * conv

        # =============================================================================
        # Leaves
        # =============================================================================
            # Filter leaves based on RGB
            lower_thresh = np.array([0, 255, 0])
            upper_thresh = np.array([0, 255, 0])

            contour_leaves, contour_height = aux.find_contour(img.copy(),
                                                              lower_thresh,
                                                              upper_thresh)

            contours_draw = cv2.drawContours(image = img.copy(),
                                              contours = contour_leaves,
                                              contourIdx = -1,
                                              color = (0, 255, 0),
                                              thickness = 3)
            img_name = path_save + "leaves_" + img_it
            cv2.imwrite(filename = img_name, img = contours_draw)


            if pos == "lat":
                height_file = aux.calc_height(img_it,
                                              contour_height, conv,
                                              height_file)

            leaves_file = aux.calc_area(img_it,
                                        contour_leaves, conv_sq, leaves_file)
            ref_file.append([img_it] + area_ref)


            if pos == "lat":

                # =============================================================================
                # Fruits
                # =============================================================================
                    # Filter fruits based on RGB
                    lower_thresh = np.array([0, 255, 255])
                    upper_thresh = np.array([0, 255, 255])

                    contour_fruits, _ = aux.find_contour(img.copy(),
                                                         lower_thresh, upper_thresh)
                    fruits_file = aux.calc_area(img_it, contour_fruits,
                                                conv_sq, fruits_file)

                    contours_draw = cv2.drawContours(image = img.copy(),
                                              contours = contour_fruits,
                                              contourIdx = -1,
                                              color = (0, 255, 0),
                                              thickness = 3)
                    img_name = path_save + "fruits_" + img_it
                    cv2.imwrite(filename = img_name, img = contours_draw)

                # =============================================================================
                # Mature fruits
                # =============================================================================
                    # Filter mature fruits based on RGB
                    lower_thresh = np.array([0, 0, 255])
                    upper_thresh = np.array([0, 0, 255])

                    contour_mfruits, _ = aux.find_contour(img.copy(), lower_thresh,
                                                          upper_thresh)
                    mfruits_file = aux.calc_area(img_it, contour_mfruits,
                                                 conv_sq, mfruits_file)

                    contours_draw = cv2.drawContours(image = img.copy(),
                                              contours = contour_mfruits,
                                              contourIdx = -1,
                                              color = (0, 255, 0),
                                              thickness = 3)
                    img_name = path_save + "mfruits_" + img_it
                    cv2.imwrite(filename = img_name, img = contours_draw)


        print(leaves_file)

        outputs = pd.DataFrame(height_file, columns=['filename', 'height'])
        output_file_name = path_base_out + \
            'dry_mass_aboveground/raw_height_' + end
        outputs.to_csv(output_file_name, index=False)

        outputs = pd.DataFrame(leaves_file, columns=['filename', 'cover'])
        output_file_name = path_base_out + 'lai/raw_cover_' + end
        outputs.to_csv(output_file_name, index=False)

        outputs = pd.DataFrame(fruits_file, columns=['filename', 'cover'])
        output_file_name = path_base_out + 'dry_mass_fruit/raw_wfcover_' + end
        outputs.to_csv(output_file_name, index=False)

        outputs = pd.DataFrame(mfruits_file, columns=['filename', 'cover'])
        output_file_name = path_base_out + \
            'dry_mass_mature_fruit/raw_wmcover_' + end
        outputs.to_csv(output_file_name, index=False)

    # =============================================================================
    # Nodes
    # =============================================================================

    # nodes_file = []
    # if ((len(nodes) > 0)):
    #     for img_it in nodes:
    #         path_read = path + img_it
    #         print(img_it)

    #         img_leaves = cv2.imread(path_read)
    #         img_leaves.shape

    #         # Filter nodes based on RGB
    #         lower_thresh = np.array([255, 255, 0])
    #         upper_thresh = np.array([255, 255, 0])

    #         contours = aux.find_contour(img_leaves, lower_thresh, upper_thresh)
    #         nodes_file.append([img_it, len(contours)])

    #     end = 'calib_' + cycle + '.csv'
    #     outputs = pd.DataFrame(nodes_file, columns=['filename', 'count'])
    #     output_file_name = '../data/observations/monitoring/nodes/raw_ncover_' + end
    #     outputs.to_csv(output_file_name, index=False)

    end = 'ref_calib_' + cycle + '.csv'
    outputs = pd.DataFrame(ref_file, columns=['filename', 'area_calc',
                                              'area_nom', 'area_err'])
    output_file_name = path_base_out +'ref_areas/' + end
    outputs.to_csv(output_file_name, index=False)
