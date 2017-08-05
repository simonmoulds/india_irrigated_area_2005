library(magrittr)
library(dplyr)
library(tidyr)
library(rgdal)
library(XLConnect)

options(stringsAsFactors = FALSE)

x = readRDS("data/apy_indiastat_combined_data.rds")

source("code/helper_functions.R")

## ======================================
## Adilabad - checked 21/06/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Adilabad", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0
mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer",
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                                         "mapspam_irri_rice",
                                         "mapspam_total_rice"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))
                           ## `apy_rice-kharif`=c(),
                           ## `apy_rice-rabi`=c()))
                           ## `apy_rice-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi",
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                                         "mapspam_irri_wheat",
                                         "mapspam_total_wheat"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year",
                                         "mapspam_irri_barley",
                                         "mapspam_total_barley"),
                na.ix=list(`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi",
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                                         "mapspam_irri_sorghum",
                                         "mapspam_total_sorghum"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi",
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                                         "mapspam_irri_maize",
                                         "mapspam_total_maize"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx[["irr_pearl_millet"]][8:9] = xx[["apy_pearl_millet-rabi"]][8:9]
xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi",
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                                         "mapspam_irri_pearl_millet",
                                         "mapspam_total_pearl_millet"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi",
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                                         "mapspam_irri_finger_millet",
                                         "mapspam_total_finger_millet"),
                na.ix=list(`irr_finger_millet`=c(3),
                           `apy_finger_millet-kharif`=c(3),
                           `apy_finger_millet-rabi`=c(3)))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi",
                                         "mapspam_irri_other_cereals",
                                         "mapspam_total_other_cereals"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(1:4),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi",
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                                         "mapspam_irri_chickpea",
                                         "mapspam_total_chickpea"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(1:11),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi",
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                                         "mapspam_irri_pigeonpea",
                                         "mapspam_total_pigeonpea"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

## TODO: MAKE SURE COWPEA IS DIVIDED BETWEEN KHARIF AND RABI ONLY
irr_frac = (xx[["mapspam_irri_cowpea"]][6] / xx[["mapspam_total_cowpea"]][6]) %>% `[<-`(is.nan(.), 0)
cowpea_total = xx[["apy_cowpea-kharif"]] + xx[["apy_cowpea-rabi"]]
irr_cowpea_total = cowpea_total * irr_frac
irr_cowpea_rabi = pmin(xx[["apy_cowpea-rabi"]], irr_cowpea_total)
irr_cowpea_kharif = pmin(xx[["apy_cowpea-kharif"]], irr_cowpea_total - irr_cowpea_rabi)
xx[["irr_cowpea-kharif"]] = irr_cowpea_kharif
xx[["irr_cowpea-rabi"]] = irr_cowpea_rabi

xx = check_data(2000:2010, xx, columns=c("irr_cowpea-kharif",
                                         "irr_cowpea-rabi",
                                         "apy_cowpea-kharif",
                                         "apy_cowpea-rabi",
                                         "mapspam_irri_cowpea",
                                         "mapspam_total_cowpea"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## TODO: MAKE SURE LENTIL IS DIVIDED BETWEEN KHARIF AND RABI ONLY
irr_frac = (xx[["mapspam_irri_lentil"]][6] / xx[["mapspam_total_lentil"]][6]) %>% `[<-`(is.nan(.), 0)
lentil_total = xx[["apy_lentil-kharif"]] + xx[["apy_lentil-rabi"]]
irr_lentil_total = lentil_total * irr_frac
irr_lentil_rabi = pmin(xx[["apy_lentil-rabi"]], irr_lentil_total)
irr_lentil_kharif = pmin(xx[["apy_lentil-kharif"]], irr_lentil_total - irr_lentil_rabi)
xx[["irr_lentil-kharif"]] = irr_lentil_kharif
xx[["irr_lentil-rabi"]] = irr_lentil_rabi

xx = check_data(2000:2010, xx, columns=c("irr_lentil-kharif",
                                         "irr_lentil-rabi",
                                         "apy_lentil-kharif",
                                         "apy_lentil-rabi",
                                         "apy_lentil-whole_year",
                                         "mapspam_irri_lentil",
                                         "mapspam_total_lentil"),
                na.ix=list(`apy_lentil-kharif`=c(),
                           `apy_lentil-rabi`=c(),
                           `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi",
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                                         "mapspam_irri_other_pulses",
                                         "mapspam_total_other_pulses"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx[["apy_sugarcane-whole_year"]][c(8,10)] = xx[["irr_sugarcane"]][c(8,10)]
xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year",
                                         "mapspam_irri_sugarcane",
                                         "mapspam_total_sugarcane"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi",
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                                         "mapspam_irri_groundnut",
                                         "mapspam_total_groundnut"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi",
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                                         "mapspam_irri_rapeseed",
                                         "mapspam_total_rapeseed"),
                na.ix=list(`irr_rape_and_mustard`=c(1:11),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c(1:11)))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi",
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                                         "mapspam_irri_wheat",
                                         "mapspam_total_wheat"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif",
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                                         "mapspam_irri_soybean",
                                         "mapspam_total_soybean"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year",
                                         "mapspam_irri_sunflower",
                                         "mapspam_total_sunflower"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi",
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                                         "mapspam_irri_other_oil_crops",
                                         "mapspam_total_other_oil_crops"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

## TODO: MAKE SURE BANANA IS ASSIGNED TO WHOLE YEAR
irr_frac = (xx[["mapspam_irri_banana"]][6] / xx[["mapspam_total_banana"]][6]) %>% `[<-`(is.nan(.), 0)
irr_banana = xx[["apy_banana-whole_year"]] * irr_frac
xx[["irr_banana"]] = irr_banana
xx = check_data(2000:2010, xx, columns=c("irr_banana",
                                         ##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year",
                                         "mapspam_irri_banana",
                                         "mapspam_total_banana"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

## TODO: MAKE SURE COCONUT IS ASSIGNED TO WHOLE YEAR
irr_frac = (xx[["mapspam_irri_coconut"]][6] / xx[["mapspam_total_coconut"]][6]) %>% `[<-`(is.nan(.), 0)
irr_coconut = xx[["apy_coconut-whole_year"]] * irr_frac
xx[["irr_coconut"]] = irr_coconut
xx = check_data(2000:2010, xx, columns=c("irr_coconut",
                                         ##"apy_coconut-kharif",
                                         "apy_coconut-whole_year",
                                         "mapspam_irri_coconut",
                                         "mapspam_total_coconut"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## TODO: MAKE SURE YAMS IS ASSIGNED TO WHOLE YEAR
irr_frac = (xx[["mapspam_irri_yams"]][6] / xx[["mapspam_total_yams"]][6]) %>% `[<-`(is.nan(.), 0)
irr_yams = xx[["apy_yams-whole_year"]] * irr_frac
xx[["irr_yams"]] = irr_yams
xx = check_data(2000:2010, xx, columns=c("irr_yams",
                                         "apy_yams-whole_year",
                                         "mapspam_irri_yams",
                                         "mapspam_total_yams"),
                na.ix=list(`apy_yams-whole_year`=c()))

## Sweet potato production can occur in either Kharif or Rabi. During
## Rabi the crop requires irrigation, whereas during Kharif it is
## typically grown as a rainfed crop.
irr_frac = (xx[["mapspam_irri_sweet_potato"]][6] / xx[["mapspam_total_sweet_potato"]][6]) %>% `[<-`(is.nan(.), 0)
sweet_potato_total = xx[["apy_sweet_potato-whole_year"]]
irr_sweet_potato_total = sweet_potato_total * irr_frac

xx[["apy_sweet_potato-kharif"]] = sweet_potato_total - irr_sweet_potato_total
xx[["apy_sweet_potato-rabi"]] = irr_sweet_potato_total

xx[["irr_sweet_potato-kharif"]] = 0
xx[["irr_sweet_potato-rabi"]] = irr_sweet_potato_total

xx = check_data(2000:2010, xx, columns=c("irr_sweet_potato-kharif",
                                         "irr_sweet_potato-rabi",
                                         "apy_sweet_potato-kharif",
                                         "apy_sweet_potato-rabi",
                                         ## "apy_sweet_potato-whole_year",
                                         "mapspam_irri_sweet_potato",
                                         "mapspam_total_sweet_potato"),
                na.ix=list(`apy_sweet_potato-kharif`=c(),
                           `apy_sweet_potato-rabi`=c()))
                           ## `apy_sweet_potato-whole_year`=c(1:11)))

## Due to South India climate, potato production in AP is most
## likely to be during Rabi (see doc/notes_on_crop_calendar.odt)
xx[["apy_potato-rabi"]] = rowSums(xx[,c("apy_potato-whole_year","apy_potato-rabi")], na.rm=TRUE)
xx[["apy_potato-whole_year"]] = 0

irr_frac = (xx[["mapspam_irri_potato"]][6] / xx[["mapspam_total_potato"]][6]) %>% `[<-`(is.nan(.), 0)
potato_total = xx[["apy_potato-kharif"]] + xx[["apy_potato-rabi"]]
irr_potato_total = potato_total * irr_frac
irr_potato_rabi = pmin(xx[["apy_potato-rabi"]], irr_potato_total)
irr_potato_kharif = pmin(xx[["apy_potato-kharif"]], irr_potato_total - irr_potato_rabi)
xx[["irr_potato-kharif"]] = irr_potato_kharif
xx[["irr_potato-rabi"]] = irr_potato_rabi

xx = check_data(2000:2010, xx, columns=c("irr_potato-kharif",
                                         "irr_potato-rabi",
                                         "apy_potato-kharif",
                                         "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         ## "apy_potato-whole_year",
                                         "mapspam_irri_potato",
                                         "mapspam_total_potato"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c(1:11)))

## TODO: MAKE SURE CASSAVA IS ASSIGNED TO WHOLE YEAR
irr_frac = (xx[["mapspam_irri_cassava"]][6] / xx[["mapspam_total_cassava"]][6]) %>% `[<-`(is.nan(.), 0)
irr_cassava = xx[["apy_cassava-whole_year"]] * irr_frac
xx[["irr_cassava"]] = irr_cassava
xx = check_data(2000:2010, xx, columns=c("irr_cassava",
                                         ##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year",
                                         "mapspam_irri_cassava",
                                         "mapspam_total_cassava"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

veg_irri_frac = xx[["mapspam_irri_vegetables"]][6] / xx[["mapspam_total_vegetables"]][6]
temf_irri_frac = xx[["mapspam_irri_temperate_fruit"]][6] / xx[["mapspam_total_temperate_fruit"]][6]
trof_irri_frac = xx[["mapspam_irri_tropical_fruits"]][6] / xx[["mapspam_total_tropical_fruits"]][6]

## For 2005,

## Using MapSPAM, get the proportion of roots and tubers (yams, sweet
## potato, potato, cassava), vegetables, temperate and tropical fruit.
## Multiply irr_fruit_and_veg by these values to get the irrigated area
## of each class that is consistent with the Indiastat data.

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "irr_vegetables-kharif",
                                         "irr_vegetables-rabi",
                                         "irr_temperate_fruit",
                                         "irr_tropical_fruit",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year",
                                         "mapspam_irri_vegetables",
                                         "mapspam_total_vegetables"),
                                         ## "mapspam_irri_temperate_fruit",
                                         ## "mapspam_total_temperate_fruit",
                                         ## "mapspam_irri_tropical_fruits",
                                         ## "mapspam_total_tropical_fruits"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_fibre_crops",
                                         "apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year",
                                         "mapspam_irri_other_fibre_crops",
                                         "mapspam_total_other_fibre_crops"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(1:11),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c(1:11)))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif",
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                                         "mapspam_irri_cotton",
                                         "mapspam_total_cotton"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("irr_coffee",
##                                          "apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_tobacco",
                                         ## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year",
                                         "mapspam_irri_tobacco",
                                         "mapspam_total_tobacco"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("irr_tea",
##                                          "apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rest_of_crops",
                                         "apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year",
                                         "mapspam_irri_rest_of_crops",
                                         "mapspam_total_rest_of_crops"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Anantapur - checked 23/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Anantapur", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0
mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         "apy_barley-kharif",
                                         "apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(`irr_barley`=c(),
                           `apy_barley-kharif`=c(),
                           `apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
                na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Chittoor - checked 23/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Chittoor", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Cuddapah - checked 23/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Cuddapah", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## East Godavari - checked 23/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "East Godavari", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Gunter - checked 23/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Guntur", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Hyderabad - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Hyderabad", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Karimnagar - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Karimnagar", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx[["apy_wheat-rabi"]][8] = xx[["irr_wheat"]][8]
xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx[["apy_pearl_millet-rabi"]][8] = xx[["irr_pearl_millet"]][8]
xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Khammam - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Khammam", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Krishna - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Krishna", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Kurnool - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Kurnool", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Mahbubnagar - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Mahbubnagar", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Medak - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Medak", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Nalgonda - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Medak", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Nellore - checked 24/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Nellore", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

mean_khar_frac = mean(xx[["apy_finger_millet-kharif"]][c(7,9)] / xx[["irr_finger_millet"]][c(7,9)])
mean_rabi_frac = mean(xx[["apy_finger_millet-rabi"]][c(7,9)] / xx[["irr_finger_millet"]][c(7,9)])

xx[["apy_finger_millet-kharif"]][c(8)] = xx[["irr_finger_millet"]][c(8)] * mean_khar_frac
xx[["apy_finger_millet-rabi"]][c(8)] = xx[["irr_finger_millet"]][c(8)] * mean_rabi_frac
xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Nizamabad - checked 26/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Nizamabad", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Prakasam - checked 26/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Prakasam", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Rangareddi - checked 26/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Rangareddi", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Srikakulam - checked 26/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Srikakulam", 2000:2010)

xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Vishakhapatnam - checked 26/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Vishakhapatnam", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Vizianagaram - checked 26/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Vizianagaram", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]] = xx[["apy_sunflower-whole_year"]]
xx[["apy_sunflower-kharif"]][8] = xx[["irr_sunflower"]][8]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## Warangal - checked 26/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "Warangal", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))


xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)

## ======================================
## West Godavari - checked 26/07/2017
## ======================================

xx = select_data(x, "Andhra Pradesh", "West Godavari", 2000:2010)
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

mean_irr_frac = mean(xx[["irr_rice-winter"]][c(2,4)] / xx[["apy_rice-kharif"]][c(2,4)])
xx[["irr_rice-winter"]][3] = xx[["apy_rice-kharif"]][3] * mean_irr_frac

xx[["apy_rice-winter"]] = xx[["apy_rice-kharif"]]
xx[["apy_rice-summer"]] = xx[["apy_rice-rabi"]]
xx[["apy_rice-kharif"]] = 0
xx[["apy_rice-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_rice-autumn",
                                         "irr_rice-winter",
                                         "irr_rice-summer",
                                         "apy_rice-autumn",
                                         "apy_rice-winter",
                                         "apy_rice-summer"),
                                         ## "apy_rice-kharif",
                                         ## "apy_rice-rabi"),
                                         ## "apy_rice-whole_year"),
                na.ix=list(`irr_rice-autumn`=c(),
                           `irr_rice-winter`=c(),
                           `irr_rice-summer`=c(),
                           `apy_rice-autumn`=c(),
                           `apy_rice-winter`=c(),
                           `apy_rice-summer`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_wheat",
                                         ## "apy_wheat-kharif"),
                                         "apy_wheat-rabi"),
                                         ## "apy_wheat-summer",
                                         ## "apy_wheat-winter",
                                         ## "apy_wheat-whole_year"),
                na.ix=list(`irr_wheat`=c(),
                           ## `apy_wheat-kharif`=c()))
                           `apy_wheat-rabi`=c()))
                           ## `apy_wheat-summer`=c(),
                           ## `apy_wheat-winter`=c(),
                           ## `apy_wheat-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_barley",
                                         ##"apy_barley-kharif",
                                         ##"apy_barley-rabi",
                                         "apy_barley-whole_year"),
                na.ix=list(##`irr_barley`=c(),
                           ##`apy_barley-kharif`=c(),
                           ##`apy_barley-rabi`=c(),
                           `apy_barley-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sorghum-kharif",
                                         "irr_sorghum-rabi",
                                         "apy_sorghum-kharif",
                                         "apy_sorghum-rabi"),
                                         ## "apy_sorghum-autumn",
                                         ## "apy_sorghum-summer",
                                         ## "apy_sorghum-whole_year"),
                na.ix=list(`irr_sorghum-kharif`=c(),
                           `irr_sorghum-rabi`=c(),
                           `apy_sorghum-kharif`=c(),
                           `apy_sorghum-rabi`=c()))
                           ## `apy_sorghum-autumn`=c(),
                           ## `apy_sorghum-summer`=c(),
                           ## `apy_sorghum-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_maize",
                                         "apy_maize-kharif",
                                         "apy_maize-rabi"),
                                         ## "apy_maize-autumn",
                                         ## "apy_maize-summer",
                                         ## "apy_maize-winter",
                                         ## "apy_maize-whole_year"),
                na.ix=list(`irr_maize`=c(),
                           `apy_maize-kharif`=c(),
                           `apy_maize-rabi`=c()))
                           ## `apy_maize-autumn`=c(),
                           ## `apy_maize-summer`=c(),
                           ## `apy_maize-winter`=c(),
                           ## `apy_maize-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_pearl_millet",
                                         "apy_pearl_millet-kharif",
                                         "apy_pearl_millet-rabi"),
                                         ## "apy_pearl_millet-summer",
                                         ## "apy_pearl_millet-whole_year"),
                na.ix=list(`irr_pearl_millet`=c(),
                           `apy_pearl_millet-kharif`=c(),
                           `apy_pearl_millet-rabi`=c()))
                           ## `apy_pearl_millet-summer`=c(),
                           ## `apy_pearl_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_finger_millet",
                                         "apy_finger_millet-kharif",
                                         "apy_finger_millet-rabi"),
                                         ## "apy_finger_millet-autumn",
                                         ## "apy_finger_millet-winter",
                                         ## "apy_finger_millet-summer",
                                         ## "apy_finger_millet-whole_year"),
                na.ix=list(`irr_finger_millet`=c(),
                           `apy_finger_millet-kharif`=c(),
                           `apy_finger_millet-rabi`=c()))
                           ## `apy_finger_millet-autumn`=c(),
                           ## `apy_finger_millet-winter`=c(),
                           ## `apy_finger_millet-summer`=c(),
                           ## `apy_finger_millet-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_cereals-kharif",
                                         "irr_other_cereals-rabi",
                                         "apy_other_cereals-kharif",
                                         "apy_other_cereals-rabi"),
                na.ix=list(`irr_other_cereals-kharif`=c(),
                           `irr_other_cereals-rabi`=c(),
                           `apy_other_cereals-kharif`=c(),
                           `apy_other_cereals-rabi`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_chickpea",
                                         "apy_chickpea-kharif",
                                         "apy_chickpea-rabi"),
                                         ## "apy_chickpea-winter",
                                         ## "apy_chickpea-whole_year"),
                na.ix=list(`irr_chickpea`=c(),
                           `apy_chickpea-kharif`=c(),
                           `apy_chickpea-rabi`=c()))
                           ## `apy_chickpea-winter`=c(),
                           ## `apy_chickpea-whole_year`=c()))
                
xx = check_data(2000:2010, xx, columns=c("irr_pigeonpea",
                                         "apy_pigeonpea-kharif",
                                         "apy_pigeonpea-rabi"),
                                         ## "apy_pigeonpea-winter",
                                         ## "apy_pigeonpea-whole_year"),
                na.ix=list(`irr_pigeonpea`=c(),
                           `apy_pigeonpea-kharif`=c(),
                           `apy_pigeonpea-rabi`=c()))
                           ## `apy_pigeonpea-winter`=c(),
                           ## `apy_pigeonpea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_cowpea-kharif",
                                         "apy_cowpea-rabi"),
                na.ix=list(`apy_cowpea-kharif`=c(),
                           `apy_cowpea-rabi`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_lentil-kharif",
##                                          "apy_lentil-rabi",
##                                          "apy_lentil-whole_year"),
##                 na.ix=list(`apy_lentil-kharif`=c(),
##                            `apy_lentil-rabi`=c(),
##                            `apy_lentil-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_pulses-kharif",
                                         "irr_other_pulses-rabi",
                                         "apy_other_pulses-kharif",
                                         "apy_other_pulses-rabi"),
                                         ## "apy_other_pulses-autumn",
                                         ## "apy_other_pulses-summer",
                                         ## "apy_other_pulses-winter",
                                         ## "apy_other_pulses-whole_year"),
                na.ix=list(`irr_other_pulses-kharif`=c(),
                           `irr_other_pulses-rabi`=c(),
                           `apy_other_pulses-kharif`=c(),
                           `apy_other_pulses-rabi`=c()))
                           ## `apy_other_pulses-autumn`=c(),
                           ## `apy_other_pulses-summer`=c(),
                           ## `apy_other_pulses-winter`=c(),
                           ## `apy_other_pulses-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sugarcane",
                                         ## "apy_sugarcane-kharif",
                                         ## "apy_sugarcane-rabi",
                                         ## "apy_sugarcane-autumn",
                                         ## "apy_sugarcane-summer",
                                         ## "apy_sugarcane-winter",
                                         "apy_sugarcane-whole_year"),
                na.ix=list(`irr_sugarcane`=c(),
                           ## `apy_sugarcane-kharif`=c(),
                           ## `apy_sugarcane-rabi`=c(),
                           ## `apy_sugarcane-autumn`=c(),
                           ## `apy_sugarcane-summer`=c(),
                           ## `apy_sugarcane-winter`=c(),
                           `apy_sugarcane-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_groundnut",
                                         "apy_groundnut-kharif",
                                         "apy_groundnut-rabi"),
                                         ## "apy_groundnut-autumn",
                                         ## "apy_groundnut-winter",
                                         ## "apy_groundnut-summer",
                                         ## "apy_groundnut-whole_year"),
                na.ix=list(`irr_groundnut`=c(),
                           `apy_groundnut-kharif`=c(),
                           `apy_groundnut-rabi`=c()))
                           ## `apy_groundnut-autumn`=c(),
                           ## `apy_groundnut-winter`=c(),
                           ## `apy_groundnut-summer`=c(),
                           ## `apy_groundnut-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_rape_and_mustard",
                                         ## "apy_rapeseed-kharif",
                                         "apy_rapeseed-rabi"),
                                         ## "apy_rapeseed-winter",
                                         ## "apy_rapeseed-whole_year"),
                na.ix=list(`irr_rape_and_mustard`=c(),
                           ## `apy_rapeseed-kharif`=c(),
                           `apy_rapeseed-rabi`=c()))
                           ## `apy_rapeseed-winter`=c(),
                           ## `apy_rapeseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_sesamum",
                                         "apy_sesameseed-kharif",
                                         "apy_sesameseed-rabi"),
                                         ## "apy_sesameseed-autumn",
                                         ## "apy_sesameseed-winter",
                                         ## "apy_sesameseed-summer",
                                         ## "apy_sesameseed-whole_year"),
                na.ix=list(`irr_sesamum`=c(),
                           `apy_sesameseed-kharif`=c(),
                           `apy_sesameseed-rabi`=c()))
                           ## `apy_sesameseed-autumn`=c(),
                           ## `apy_sesameseed-winter`=c(),
                           ## `apy_sesameseed-summer`=c(),
                           ## `apy_sesameseed-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_soyabean",
                                         "apy_soybean-kharif"),
                                         ## "apy_soybean-rabi",
                                         ## "apy_soybean-whole_year"),
                na.ix=list(`irr_soyabean`=c(),
                           `apy_soybean-kharif`=c()))
                           ## `apy_soybean-rabi`=c(),
                           ## `apy_soybean-whole_year`=c()))

xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
xx[["apy_sunflower-whole_year"]] = 0
xx[["apy_sunflower-rabi"]] = 0
xx = check_data(2000:2010, xx, columns=c("irr_sunflower",
                                         "apy_sunflower-kharif",
                                         "apy_sunflower-rabi",
                                         ## "apy_sunflower-summer",
                                         "apy_sunflower-whole_year"),
                na.ix=list(`irr_sunflower`=c(),
                           `apy_sunflower-kharif`=c(),
                           `apy_sunflower-rabi`=c(),
                           ## `apy_sunflower-summer`=c(),
                           `apy_sunflower-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_other_oil_crops",
                                         "apy_other_oil_crops-kharif",
                                         "apy_other_oil_crops-rabi"),
                                         ## "apy_other_oil_crops-winter",
                                         ## "apy_other_oil_crops-whole_year"),
                na.ix=list(`irr_other_oil_crops`=c(),
                           `apy_other_oil_crops-kharif`=c(),
                           `apy_other_oil_crops-rabi`=c()))
                           ## `apy_other_oil_crops-winter`=c(),
                           ## `apy_other_oil_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_banana-kharif",
                                         ##"apy_banana-rabi",
                                         ##"apy_banana-summer",
                                         "apy_banana-whole_year"),
                na.ix=list(##`apy_banana-kharif`=c(),
                           ##`apy_banana-rabi`=c(),
                           ##`apy_banana-summer`=c(),
                           `apy_banana-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_coconut-kharif",
                                         "apy_coconut-whole_year"),
                na.ix=list(##`apy_coconut-kharif`=c(),
                           `apy_coconut-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_yams-whole_year"),
##                 na.ix=list(`apy_yams-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_sweet_potato-kharif",
                                         ## "apy_sweet_potato-rabi",
                                         "apy_sweet_potato-whole_year"),
                na.ix=list(## `apy_sweet_potato-kharif`=c(),
                           ## `apy_sweet_potato-rabi`=c(),
                           `apy_sweet_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_potato-kharif",
                                         ## "apy_potato-rabi",
                                         ## "apy_potato-summer",
                                         ## "apy_potato-winter",
                                         "apy_potato-whole_year"),
                na.ix=list(## `apy_potato-kharif`=c(),
                           ## `apy_potato-rabi`=c(),
                           ## `apy_potato-summer`=c(),
                           ## `apy_potato-winter`=c(),
                           `apy_potato-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(##"apy_cassava-kharif",
                                         ##"apy_cassava-rabi",
                                         "apy_cassava-whole_year"),
                na.ix=list(##`apy_cassava-kharif`=c(),
                           ##`apy_cassava-rabi`=c(),
                           `apy_cassava-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_fruit_and_veg",
                                         "apy_vegetables-kharif",
                                         "apy_vegetables-rabi",
                                         ## "apy_vegetables-summer",
                                         "apy_vegetables-whole_year",
                                         "apy_temperate_fruit-whole_year",
                                         ## "apy_tropical_fruits-kharif",
                                         ## "apy_tropical_fruits-rabi",
                                         "apy_tropical_fruits-whole_year"),
                na.ix=list(`irr_fruit_and_veg`=c(),
                           `apy_vegetables-kharif`=c(),
                           `apy_vegetables-rabi`=c(),
                           ## `apy_vegetables-summer`=c(),
                           `apy_vegetables-whole_year`=c(),
                           `apy_temperate_fruit-whole_year`=c(),
                           ## `apy_tropical_fruits-kharif`=c(),
                           ## `apy_tropical_fruits-rabi`=c(),
                           `apy_tropical_fruits-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_other_fibre_crops-kharif",
                                         ## "apy_other_fibre_crops-rabi",
                                         ## "apy_other_fibre_crops-autumn",
                                         "apy_other_fibre_crops-whole_year"),
                na.ix=list(`apy_other_fibre_crops-kharif`=c(),
                           ## `apy_other_fibre_crops-rabi`=c(),
                           ## `apy_other_fibre_crops-autumn`=c(),
                           `apy_other_fibre_crops-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("irr_cotton",
                                         "apy_cotton-kharif"),
                                         ##"apy_cotton-rabi",
                                         ##"apy_cotton-summer",
                                         ##"apy_cotton-whole_year"),
                na.ix=list(`irr_cotton`=c(),
                           `apy_cotton-kharif`=c()))
                           ## `apy_cotton-rabi`=c(),
                           ## `apy_cotton-summer`=c(),
                           ## `apy_cotton-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_coffee-whole_year"),
##                 na.ix=list(`apy_coffee-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c(## "apy_tobacco-kharif",
                                         ## "apy_tobacco-rabi",
                                         ## "apy_tobacco-summer",
                                         "apy_tobacco-whole_year"),
                na.ix=list(## `apy_tobacco-kharif`=c(),
                           ## `apy_tobacco-rabi`=c(),
                           ## `apy_tobacco-summer`=c(),
                           `apy_tobacco-whole_year`=c()))

## xx = check_data(2000:2010, xx, columns=c("apy_tea-kharif",
##                                          "apy_tea-whole_year"),
##                 na.ix=list(`apy_tea-kharif`=c(),
##                            `apy_tea-whole_year`=c()))

xx = check_data(2000:2010, xx, columns=c("apy_rest_of_crops-kharif",
                                         "apy_rest_of_crops-rabi",
                                         ## "apy_rest_of_crops-autumn",
                                         ## "apy_rest_of_crops-summer",
                                         "apy_rest_of_crops-whole_year"),
                na.ix=list(`apy_rest_of_crops-kharif`=c(),
                           `apy_rest_of_crops-rabi`=c(),
                           ## `apy_rest_of_crops-autumn`=c(),
                           ## `apy_rest_of_crops-summer`=c(),
                           `apy_rest_of_crops-whole_year`=c()))

gw = district_groundwater_frac(2000:2010, xx)
