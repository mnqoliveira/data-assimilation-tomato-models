# -*- coding: utf-8 -*-
"""
Created on Fri Nov  8 09:40:22 2019

@author: Monique

Extracts leaf area from scans.

"""

import cv2
import numpy as np
import os
import pandas as pd
import auxiliary_functions.f_aux as aux


# %%
# =============================================================================
# Auxiliary functions
# =============================================================================
def find_ref(cycle, img):

    #if cycle == "ciclo02":
    if cycle == "cycle01":
        # Reference is a black card.
        # Template matching did not work.
        # Using color thresholds to find the reference.

        gray = cv2.cvtColor(src = img.copy(), code = cv2.COLOR_BGR2GRAY)
        # Find threshold that predominantly separates the card.
        blur = cv2.blur(gray, (11, 11))
        ret, mask_card = cv2.threshold(blur, 50, 255,
                                       cv2.THRESH_BINARY_INV)

        k = np.ones((5, 5), np.uint8)
        mask_dilate = cv2.dilate(mask_card, k,iterations = 1)

        thresh = 230 # value to represent white
        mod = 0
        mask_cropped = aux.crop_image_borders(mask_dilate, thresh,
                                              mod, f="<")

        # Remove first batch of external small contours
        contours_filt1 = aux.filter_contours(mask_cropped,
                                             area_min=50000,
                                             cont_type="ext")

        area_max = 0
        for c in contours_filt1:
            M = cv2.moments(c)
            area = M['m00']

            if area > area_max:
                contour_card = c
                area_max = area

        mask = np.zeros(img.shape, np.uint8)
        last_mask = cv2.drawContours(mask, [contour_card], -1,
                                     (255,255,255), -1)

    else:
        # Filter ref based on RGB. Using post-it as reference.
        # Filter post-it using post-it colors
        blur = cv2.blur(img.copy(), (3, 3))
        # Colors for post-it identification
        lower_thresh = np.array([190, 0, 190])
        upper_thresh = np.array([240, 190, 240])
        mask = cv2.inRange(blur.copy(), lower_thresh, upper_thresh)

        # Remove noise
        k = np.ones((5, 5), np.uint8)
        mask_open = cv2.morphologyEx(mask, cv2.MORPH_OPEN, k)
        #cv2.imwrite(filename=img_name, img=mask_close)
        _, contour_card, _ = cv2.findContours(mask_open,
                                              cv2.RETR_EXTERNAL,
                                              cv2.CHAIN_APPROX_NONE)

        area_max = 0
        for c in contour_card:
            M = cv2.moments(c)
            area = M['m00']

            if area > area_max:
                contour_sel = c
                area_max = area

        rect = cv2.minAreaRect(contour_sel)
        box = cv2.boxPoints(rect)
        box_ref = np.int0(box)

        mask = np.zeros(img.shape, np.uint8)
        last_mask = cv2.drawContours(mask, [box_ref], -1,
                                     (255,255,255), -1)

    return last_mask


def calc_conv_fact(conv, cycle, mask):

    if cycle == "cycle01":
    #if cycle == "ciclo02":

        #known_dim = 8.9*10**(-2)
        known_dim = 5.7*10**(-2)
        area_nom = 8.9*10**(-2)*5.7*10**(-2)

    else:
        known_dim = 5*10**(-2)
        area_nom = 3.8*5*10**(-4)

    mask_ref = cv2.cvtColor(src=mask.copy(), code=cv2.COLOR_BGR2GRAY)

    # Contour
    (_, contours_ref, _) = cv2.findContours(mask_ref,
                                            cv2.RETR_TREE,
                                            cv2.CHAIN_APPROX_NONE)

    area_max = 0
    for c in contours_ref:
        M = cv2.moments(c)
        area = M['m00']
        #print(area)

        if area > area_max:
            contour_card = c
            area_max = area

    # Bounding box
    # ( center (x,y), (width, height), angle of rotation )
    rect = cv2.minAreaRect(contour_card)

    # Reference length
    ref_dim1 = rect[1][0]
    ref_dim2 = rect[1][1]

    ref_length = np.max((ref_dim1, ref_dim2))
    if cycle == "cycle01":
    #if cycle == "ciclo02":
        ref_length = np.min((ref_dim1, ref_dim2))
    if area > 1000:
        conv = known_dim / ref_length
    else:
        conv = conv

    area_calc = np.round(area_max * conv * conv, 4)
    area_err = np.round(np.abs((area_calc - area_nom)*100/area_nom))

    area = [area_calc, area_nom, area_err]

    return conv, area

def proc_imgs(orig_imgs, path_mod, cycle, areas_ref, areas_file):

    img_it = orig_imgs[0]
    conv = 0
    for img_it in orig_imgs:
        print(img_it)
        path = path_mod + img_it

        img_leaves = cv2.imread(path)
        img = img_leaves.copy()
        # Reference is a black card.
        card_mask = find_ref(cycle, img)
        # Inserir um if pra so fazer isso se a imagem anterior tiver tamanho
        # diferente
        conv, area_ref = calc_conv_fact(conv, cycle, card_mask)

        # if area_ref[2] > 10:
        #     img_name = path_save + "/r1_" + img_it
        #     cv2.imwrite(filename=img_name, img=card_mask)

        areas_ref.append(area_ref + [img_it])

        # Remove card
        ret, mask_inv = cv2.threshold(card_mask, 10, 255,
                                      cv2.THRESH_BINARY_INV)
        bitwiseAnd = cv2.bitwise_and(img, mask_inv)

        # Find leaves
        leaves_mask = aux.easyleafarea(cycle+case, bitwiseAnd)

        # Noise removal
        kernel = np.ones((9, 9), np.uint8)
        opening = cv2.morphologyEx(leaves_mask, cv2.MORPH_OPEN,
                                    kernel, iterations=1)

        # Contours

        #if cycle == "ciclo02":
        if cycle == "cycle01":
            min_c = 500
        else:
            min_c = 1500
        contours_filt = aux.filter_contours(opening, area_min=min_c,
                                            cont_type="all")

        contours_filt2 = aux.filter_contours(input_obj=contours_filt,
                                              area_min=100,
                                              cont_type="large")

        contours_draw = cv2.drawContours(image=img_leaves.copy(),
                                          contours=contours_filt2,
                                          contourIdx=-1,
                                          color=(255, 0, 0),
                                          thickness=3)
        #img_name = path_save + "/c_" + img_it
        #cv2.imwrite(filename=img_name, img=contours_draw)

        # # Calculate areas of leaves
        areas_file = aux.calc_area(img_it, contours_filt2,
                                    conv*conv, areas_file)

    return contours_filt2, areas_ref, areas_file


# %%
# =============================================================================
# Separate card from leaves
# =============================================================================

cycle_l = ["cycle01"]
cycle = cycle_l[0]

#path_base_in = 'E:/drive_unicamp/SISDA/projetoModelagemTomateiro/pidata/scans/'
path_base_in = "./data/scans/"
path_base_out = './data/observations/monitoring/'
#alt_path_base_out = 'E:/doc_local/'

for cycle in cycle_l:

    areas_file = []
    areas_ref = []

    path_source = path_base_in + cycle + "/"
    #path_source = alt_path_base_out + 'imgs_proc/scans/' + cycle +"/imgs/"
    cases = [s for s in os.listdir(path_source) if "case" in s]

    if len(cases) == 0:
        cases = [""]
    #case = cases[1]

    for case in cases:

        orig_imgs = os.listdir(path_source + case + "/")
        orig_imgs = [s for s in orig_imgs if "desktop" not in s]

        # path_save = alt_path_base_out + 'imgs_proc/scans/' + cycle + "/"
        # orig_imgs = os.listdir(path_save + "/" + "imgs" + "/")
        # orig_imgs = [s for s in orig_imgs if "desktop" not in s]
        # orig_imgs = [s for s in orig_imgs if "jpeg" not in s]
        #img_it = orig_imgs[50]

        #path_save = alt_path_base_out + 'imgs_proc/scans/' + cycle
        path_mod = path_source

        #if cycle == "ciclo02":
        if cycle == "cycle01":
            #path_save = alt_path_base_out + 'imgs_proc/scans/' + cycle + "/" + case + "/"
            path_mod = path_source + case + "/"

        contours_filt, areas_ref, areas_file = proc_imgs(orig_imgs,
                                                         path_mod, cycle,
                                                         areas_ref,
                                                         areas_file)


    end = cycle + '.csv'
    outputs = pd.DataFrame(areas_file, columns=['filename', 'area'])
    output_file_name = './data/observations/monitoring/lai/raw_lai_scans_' + end
    outputs.to_csv(output_file_name, index=False)

    areas_ref_pd = pd.DataFrame(areas_ref,
                                columns=['area_calc', 'area_nom', 'area_err',
                                         'filename'])
    output_file_name = './data/observations/monitoring/ref_areas/ref_scan_' + end
    areas_ref_pd.to_csv(output_file_name, index=False)
