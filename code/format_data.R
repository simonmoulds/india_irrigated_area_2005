## Author : Simon Moulds
## Date   : August 2017

library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)

x = readRDS("data/apy_indiastat_combined_data.rds")

## ======================================
## consolidate apy data
## ======================================

## give consistent crop season names across all districts

## rice
## ####

## summer eq rabi
x[["apy_rice-summer"]] =
    rowSums(cbind(x[["apy_rice-summer"]],
                  x[["apy_rice-rabi"]]), na.rm=TRUE)

## autumn eq pre-kharif (zaid)
x[["apy_rice-autumn"]] = x[["apy_rice-autumn"]]

## winter eq kharif
x[["apy_rice-winter"]] =
    rowSums(cbind(x[["apy_rice-winter"]],
                  x[["apy_rice-kharif"]]), na.rm=TRUE)

## now get the proportion of summer, autumn and winter
f1 =
    x[["apy_rice-summer"]] %>% 
    `/`(rowSums(cbind(x[["apy_rice-summer"]],
                      x[["apy_rice-autumn"]],
                      x[["apy_rice-winter"]]), na.rm=TRUE))

f2 =
    x[["apy_rice-autumn"]] %>% 
    `/`(rowSums(cbind(x[["apy_rice-summer"]],
                      x[["apy_rice-autumn"]],
                      x[["apy_rice-winter"]]), na.rm=TRUE))

x[["apy_rice-summer"]] =
    rowSums(cbind(x[["apy_rice-whole_year"]] * f1,
                  x[["apy_rice-summer"]]), na.rm=TRUE)

x[["apy_rice-autumn"]] =
    rowSums(cbind(x[["apy_rice-whole_year"]] * f2,
                  x[["apy_rice-autumn"]]), na.rm=TRUE)

x[["apy_rice-winter"]] =
    rowSums(cbind(x[["apy_rice-whole_year"]] * (1 - f1 - f2),
                  x[["apy_rice-winter"]]), na.rm=TRUE)

## wheat
## #####

x[["apy_wheat-rabi"]] =
    rowSums(cbind(x[["apy_wheat-kharif"]],
                  x[["apy_wheat-winter"]],
                  x[["apy_wheat-summer"]],
                  x[["apy_wheat-whole_year"]],
                  x[["apy_wheat-rabi"]]), na.rm=TRUE)

## barley
## ######

x[["apy_barley-rabi"]] =
    rowSums(cbind(x[["apy_barley-whole_year"]],
                  x[["apy_barley-rabi"]]), na.rm=TRUE)


## sorghum
## #######

f1 =
    x[["apy_sorghum-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_sorghum-kharif"]],
                      x[["apy_sorghum-rabi"]],
                      x[["apy_sorghum-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_sorghum-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_sorghum-kharif"]],
                      x[["apy_sorghum-rabi"]],
                      x[["apy_sorghum-summer"]]), na.rm=TRUE))

x[["apy_sorghum-kharif"]] =
    rowSums(cbind(x[["apy_sorghum-autumn"]],
                  x[["apy_sorghum-whole_year"]] * f2,
                  x[["apy_sorghum-kharif"]]), na.rm=TRUE)

x[["apy_sorghum-rabi"]] =
    rowSums(cbind(x[["apy_sorghum-whole-year"]] * f1,
                  x[["apy_sorghum-rabi"]]), na.rm=TRUE)

x[["apy_sorghum-summer"]] =
    rowSums(cbind(x[["apy_sorghum-whole-year"]] * (1 - f1 - f2),
                  x[["apy_sorghum-summer"]]), na.rm=TRUE)

## maize
## #####

f1 =
    x[["apy_maize-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_maize-kharif"]],
                      x[["apy_maize-rabi"]],
                      x[["apy_maize-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_maize-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_maize-kharif"]],
                      x[["apy_maize-rabi"]],
                      x[["apy_maize-summer"]]), na.rm=TRUE))

x[["apy_maize-kharif"]] =
    rowSums(cbind(x[["apy_maize-autumn"]],
                  x[["apy_maize-whole_year"]] * f2,
                  x[["apy_maize-kharif"]]), na.rm=TRUE)

x[["apy_maize-rabi"]] =
    rowSums(cbind(x[["apy_maize-winter"]],
                  x[["apy_maize-whole_year"]] * f1,
                  x[["apy_maize-rabi"]]), na.rm=TRUE)

x[["apy_maize-summer"]] =
    rowSums(cbind(x[["apy_maize-whole_year"]] * (1 - f1 - f2),
                  x[["apy_maize-summer"]]), na.rm=TRUE)

## pearl millet (Bajra)
## ####################

f1 =
    x[["apy_pearl_millet-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_pearl_millet-kharif"]],
                      x[["apy_pearl_millet-rabi"]],
                      x[["apy_pearl_millet-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_pearl_millet-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_pearl_millet-kharif"]],
                      x[["apy_pearl_millet-rabi"]],
                      x[["apy_pearl_millet-summer"]]), na.rm=TRUE))

x[["apy_pearl_millet-kharif"]] =
    rowSums(cbind(x[["apy_pearl_millet-whole_year"]] * f2,
                  x[["apy_pearl_millet-kharif"]]), na.rm=TRUE)

x[["apy_pearl_millet-rabi"]] =
    rowSums(cbind(x[["apy_pearl_millet-whole_year"]] * f1,
                  x[["apy_pearl_millet-rabi"]]), na.rm=TRUE)

x[["apy_pearl_millet-summer"]] =
    rowSums(cbind(x[["apy_pearl_millet-whole_year"]] * (1 - f1 - f2),
                  x[["apy_pearl_millet-kharif"]]), na.rm=TRUE)

## finger millet (Ragi)
## ####################

f1 =
    x[["apy_finger_millet-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_finger_millet-kharif"]],
                      x[["apy_finger_millet-rabi"]],
                      x[["apy_finger_millet-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_finger_millet-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_finger_millet-kharif"]],
                      x[["apy_finger_millet-rabi"]],
                      x[["apy_finger_millet-summer"]]), na.rm=TRUE))

x[["apy_finger_millet-kharif"]] =
    rowSums(cbind(x[["apy_finger_millet-whole_year"]] * f2,
                  x[["apy_finger_millet-autumn"]],
                  x[["apy_finger_millet-kharif"]]))

x[["apy_finger_millet-rabi"]] =
    rowSums(cbind(x[["apy_finger_millet-whole_year"]] * f1,
                  x[["apy_finger_millet-winter"]],
                  x[["apy_finger_millet-rabi"]]), na.rm=TRUE)

x[["apy_finger_millet-summer"]] =
    rowSums(cbind(x[["apy_finger_millet-whole_year"]] * (1 - f1 - f2),
                  x[["apy_finger_millet-summer"]]), na.rm=TRUE)

## other cereals
## #############

f = x[["apy_other_cereals-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_other_cereals-kharif"]],
                      x[["apy_other_cereals-rabi"]]), na.rm=TRUE))

x[["apy_other_cereals-kharif"]] =
    rowSums(cbind(x[["apy_other_cereals-whole_year"]] * (1 - f),
                  x[["apy_other_cereals-kharif"]]), na.rm=TRUE)

x[["apy_other_cereals-rabi"]] =
    rowSums(cbind(x[["apy_other_cereals-whole_year"]] * f,
                  x[["apy_other_cereals-rabi"]]), na.rm=TRUE)

## chickpea
## ########

f =
    x[["apy_chickpea-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_chickpea-kharif"]],
                      x[["apy_chickpea-rabi"]]), na.rm=TRUE))

x[["apy_chickpea-kharif"]] =
    rowSums(cbind(x[["apy_chickpea-whole_year"]] * (1 - f),
                  x[["apy_chickpea-kharif"]]), na.rm=TRUE)

x[["apy_chickpea-rabi"]] =
    rowSums(cbind(x[["apy_chickpea-whole_year"]] * f,
                  x[["apy_chickpea-winter"]],
                  x[["apy_chickpea-rabi"]]), na.rm=TRUE)

## pigeonpea
## #########

f =
    x[["apy_pigeonpea-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_pigeonpea-kharif"]],
                      x[["apy_pigeonpea-rabi"]]), na.rm=TRUE))

x[["apy_pigeonpea-kharif"]] =
    rowSums(cbind(x[["apy_pigeonpea-whole_year"]] * (1 - f),
                  x[["apy_pigeonpea-kharif"]]), na.rm=TRUE)

x[["apy_pigeonpea-rabi"]] =
    rowSums(cbind(x[["apy_pigeonpea-winter"]],
                  x[["apy_pigeonpea-whole_year"]] * f,
                  x[["apy_pigeonpea-rabi"]]), na.rm=TRUE)

## cowpea
## ######

x[["apy_cowpea-rabi"]] = x[["apy_cowpea-rabi"]]
x[["apy_cowpea-kharif"]] = x[["apy_cowpea-kharif"]]

## lentil
## ######

f =
    x[["apy_lentil-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_lentil-kharif"]],
                      x[["apy_lentil-rabi"]]), na.rm=TRUE))

x[["apy_lentil-kharif"]] =
    rowSums(cbind(x[["apy_lentil-whole_year"]] * (1 - f),
                  x[["apy_lentil-kharif"]]), na.rm=TRUE)

x[["apy_lentil-rabi"]] =
    rowSums(cbind(x[["apy_lentil-whole_year"]] * f,
                  x[["apy_lentil-rabi"]]), na.rm=TRUE)

## other pulses
## ############

f1 =
    x[["apy_other_pulses-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_other_pulses-kharif"]],
                      x[["apy_other_pulses-rabi"]],
                      x[["apy_other_pulses-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_other_pulses-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_other_pulses-kharif"]],
                      x[["apy_other_pulses-rabi"]],
                      x[["apy_other_pulses-summer"]]), na.rm=TRUE))

x[["apy_other_pulses-kharif"]] =
    rowSums(cbind(x[["apy_other_pulses-autumn"]],
                  x[["apy_other_pulses-whole_year"]] * f2,
                  x[["apy_other_pulses-kharif"]]), na.rm=TRUE)

x[["apy_other_pulses-rabi"]] =
    rowSums(cbind(x[["apy_other_pulses-winter"]],
                  x[["apy_other_pulses-whole_year"]] * f1,
                  x[["apy_other_pulses-rabi"]]), na.rm=TRUE)

x[["apy_other_pulses-summer"]] =
    rowSums(cbind(x[["apy_other_pulses-whole_year"]] * (1 - f1 - f2),
                  x[["apy_other_pulses-summer"]]), na.rm=TRUE)

## sugarcane
## #########

x[["apy_sugarcane-whole_year"]] =
    rowSums(cbind(x[["apy_sugarcane-autumn"]],
                  x[["apy_sugarcane-kharif"]],
                  x[["apy_sugarcane-rabi"]],
                  x[["apy_sugarcane-summer"]],
                  x[["apy_sugarcane-winter"]],
                  x[["apy_sugarcane-whole_year"]]), na.rm=TRUE)

## groundnut
## #########

f1 =
    x[["apy_groundnut-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_groundnut-kharif"]],
                      x[["apy_groundnut-rabi"]],
                      x[["apy_groundnut-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_groundnut-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_groundnut-kharif"]],
                      x[["apy_groundnut-rabi"]],
                      x[["apy_groundnut-summer"]]), na.rm=TRUE))

x[["apy_groundnut-kharif"]] =
    rowSums(cbind(x[["apy_groundnut-autumn"]],
                  x[["apy_groundnut-whole_year"]] * f2,
                  x[["apy_groundnut-kharif"]]), na.rm=TRUE)

x[["apy_groundnut-rabi"]] =
    rowSums(cbind(x[["apy_groundnut-winter"]],
                  x[["apy_groundnut-whole_year"]] * f1,
                  x[["apy_groundnut-rabi"]]), na.rm=TRUE)

x[["apy_groundnut-summer"]] =
    rowSums(cbind(x[["apy_groundnut-whole_year"]] * (1 - f1 - f2),
                  x[["apy_groundnut-summer"]]), na.rm=TRUE)


## rapeseed
## ########

f =
    x[["apy_rapeseed-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_rapeseed-kharif"]],
                      x[["apy_rapeseed-rabi"]]), na.rm=TRUE))

x[["apy_rapeseed-kharif"]] =
    rowSums(cbind(x[["apy_rapeseed-whole_year"]] * (1 - f),
                  x[["apy_rapeseed-kharif"]]), na.rm=TRUE)

x[["apy_rapeseed-rabi"]] =
    rowSums(cbind(x[["apy_rapeseed-whole_year"]] * f,
                  x[["apy_rapeseed-winter"]],
                  x[["apy_rapeseed-rabi"]]), na.rm=TRUE)

## sesameseed
## ##########

f1 =
    x[["apy_sesameseed-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_sesameseed-kharif"]],
                      x[["apy_sesameseed-rabi"]],
                      x[["apy_sesameseed-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_sesameseed-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_sesameseed-kharif"]],
                      x[["apy_sesameseed-rabi"]],
                      x[["apy_sesameseed-summer"]]), na.rm=TRUE))

x[["apy_sesameseed-kharif"]] =
    rowSums(cbind(x[["apy_sesameseed-autumn"]],
                  x[["apy_sesameseed-whole_year"]] * f2,
                  x[["apy_sesameseed-kharif"]]), na.rm=TRUE)

x[["apy_sesameseed-rabi"]] =
    rowSums(cbind(x[["apy_sesameseed-winter"]],
                  x[["apy_sesameseed-whole_year"]] * f1,
                  x[["apy_sesameseed-rabi"]]), na.rm=TRUE)

x[["apy_sesameseed-summer"]] =
    rowSums(cbind(x[["apy_sesameseed-whole_year"]] * (1 - f1 - f2),
                  x[["apy_sesameseed-summer"]]), na.rm=TRUE)

## soybean
## ########

f =
    x[["apy_soybean-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_soybean-kharif"]],
                      x[["apy_soybean-rabi"]]), na.rm=TRUE))

x[["apy_soybean-kharif"]] =
    rowSums(cbind(x[["apy_soybean-whole_year"]] * (1 - f),
                  x[["apy_soybean-kharif"]]), na.rm=TRUE)

x[["apy_soybean-rabi"]] =
    rowSums(cbind(x[["apy_soybean-whole_year"]] * f,
                  x[["apy_soybean-rabi"]]), na.rm=TRUE)

## sunflower
## #########

f1 =
    x[["apy_sunflower-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_sunflower-kharif"]],
                      x[["apy_sunflower-rabi"]],
                      x[["apy_sunflower-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_sunflower-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_sunflower-kharif"]],
                      x[["apy_sunflower-rabi"]],
                      x[["apy_sunflower-summer"]]), na.rm=TRUE))

x[["apy_sunflower-kharif"]] =
    rowSums(cbind(x[["apy_sunflower-whole_year"]] * f2,
                  x[["apy_sunflower-kharif"]]), na.rm=TRUE)

x[["apy_sunflower-rabi"]] =
    rowSums(cbind(x[["apy_sunflower-whole_year"]] * f1,
                  x[["apy_sunflower-rabi"]]), na.rm=TRUE)

x[["apy_sunflower-summer"]] =
    rowSums(cbind(x[["apy_sunflower-whole_year"]] * (1 - f1 - f2),
                  x[["apy_sunflower-summer"]]), na.rm=TRUE)

## other oil crops
## ###############

f =
    x[["apy_other_oil_crops-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_other_oil_crops-kharif"]],
                      x[["apy_other_oil_crops-rabi"]]), na.rm=TRUE))

x[["apy_other_oil_crops-kharif"]] =
    rowSums(cbind(x[["apy_other_oil_crops-whole_year"]] * (1 - f),
                  x[["apy_other_oil_crops-kharif"]]), na.rm=TRUE)

x[["apy_other_oil_crops-rabi"]] =
    rowSums(cbind(x[["apy_other_oil_crops-whole_year"]] * f,
                  x[["apy_other_oil_crops-winter"]],
                  x[["apy_other_oil_crops-rabi"]]), na.rm=TRUE)

## banana
## ######

x[["apy_banana-whole_year"]] =
    rowSums(cbind(x[["apy_banana-summer"]],
                  x[["apy_banana-kharif"]],
                  x[["apy_banana-rabi"]],
                  x[["apy_banana-whole_year"]]))

## coconut
## #######

x[["apy_coconut-whole_year"]] =
    rowSums(cbind(x[["apy_coconut-kharif"]],
                  x[["apy_coconut-whole_year"]]))

## yams
## ####

x[["apy_yams-whole_year"]] = x[["apy_yams-whole_year"]] ## no change reqd

## cassava
## #######

x[["apy_cassava-whole_year"]] =
    rowSums(cbind(x[["apy_cassava-rabi"]],
                  x[["apy_cassava-kharif"]],
                  x[["apy_cassava-whole_year"]]))

## sweet potato
## ############

x[["apy_sweet_potato-kharif"]] = x[["apy_sweet_potato-kharif"]]
x[["apy_sweet_potato-rabi"]] = x[["apy_sweet_potato-rabi"]]
x[["apy_sweet_potato-whole_year"]] = x[["apy_sweet_potato-whole_year"]]

## potato
## ######

f1 =
    x[["apy_potato-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_potato-kharif"]],
                      x[["apy_potato-rabi"]],
                      x[["apy_potato-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_potato-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_potato-kharif"]],
                      x[["apy_potato-rabi"]],
                      x[["apy_potato-summer"]]), na.rm=TRUE))

x[["apy_potato-kharif"]] =
    rowSums(cbind(x[["apy_potato-whole_year"]] * f2,
                  x[["apy_potato-kharif"]]), na.rm=TRUE)

x[["apy_potato-rabi"]] =
    rowSums(cbind(x[["apy_potato-whole_year"]] * f1,
                  x[["apy_potato-winter"]],
                  x[["apy_potato-rabi"]]), na.rm=TRUE)

x[["apy_potato-summer"]] =
    rowSums(cbind(x[["apy_potato-whole_year"]] * (1 - f1 - f2),
                  x[["apy_potato-summer"]]), na.rm=TRUE)

## vegetables
## ##########

f1 =
    x[["apy_vegetables-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_vegetables-kharif"]],
                      x[["apy_vegetables-rabi"]],
                      x[["apy_vegetables-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_vegetables-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_vegetables-kharif"]],
                      x[["apy_vegetables-rabi"]],
                      x[["apy_vegetables-summer"]]), na.rm=TRUE))

x[["apy_vegetables-kharif"]] =
    rowSums(cbind(x[["apy_vegetables-whole_year"]] * f2,
                  x[["apy_vegetables-kharif"]]), na.rm=TRUE)

x[["apy_vegetables-rabi"]] =
    rowSums(cbind(x[["apy_vegetables-whole_year"]] * f1,
                  x[["apy_vegetables-rabi"]]), na.rm=TRUE)

x[["apy_vegetables-summer"]] =
    rowSums(cbind(x[["apy_vegetables-whole_year"]] * (1 - f1 - f2),
                  x[["apy_vegetables-summer"]]), na.rm=TRUE)


## tropical fruit
## ##############

x[["apy_tropical_fruit-whole_year"]] =
    rowSums(cbind(x[["apy_tropical_fruit-rabi"]],
                  x[["apy_tropical_fruit-kharif"]],
                  x[["apy_tropical_fruit-whole_year"]]))

## temperate fruit
## ###############

x[["apy_temperate_fruit-whole_year"]] = x[["apy_temperate_fruit-whole_year"]]

## cotton
## ######

f1 =
    x[["apy_cotton-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_cotton-kharif"]],
                      x[["apy_cotton-rabi"]],
                      x[["apy_cotton-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_cotton-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_cotton-kharif"]],
                      x[["apy_cotton-rabi"]],
                      x[["apy_cotton-summer"]]), na.rm=TRUE))

x[["apy_cotton-kharif"]] =
    rowSums(cbind(x[["apy_cotton-whole_year"]] * f2,
                  x[["apy_cotton-kharif"]]), na.rm=TRUE)

x[["apy_cotton-rabi"]] =
    rowSums(cbind(x[["apy_cotton-whole_year"]] * f1,
                  x[["apy_cotton-rabi"]]), na.rm=TRUE)

x[["apy_cotton-summer"]] =
    rowSums(cbind(x[["apy_cotton-whole_year"]] * (1 - f1 - f2),
                  x[["apy_cotton-summer"]]), na.rm=TRUE)

## other fibre crops
## #################

f =
    x[["apy_other_fibre_crops-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_other_fibre_crops-kharif"]],
                      x[["apy_other_fibre_crops-rabi"]]), na.rm=TRUE))

x[["apy_other_fibre_crops-kharif"]] =
    rowSums(cbind(x[["apy_other_fibre_crops-whole_year"]] * (1 - f),
                  x[["apy_other_fibre_crops-autumn"]],
                  x[["apy_other_fibre_crops-kharif"]]), na.rm=TRUE)

x[["apy_other_fibre_crops-rabi"]] =
    rowSums(cbind(x[["apy_other_fibre_crops-whole_year"]] * f,
                  x[["apy_other_fibre_crops-rabi"]]), na.rm=TRUE)

## coffee
## ######

x[["apy_coffee-whole_year"]] = x[["apy_coffee-whole_year"]]

## tobacco
## #######

f1 =
    x[["apy_tobacco-rabi"]] %>% 
    `/`(rowSums(cbind(x[["apy_tobacco-kharif"]],
                      x[["apy_tobacco-rabi"]],
                      x[["apy_tobacco-summer"]]), na.rm=TRUE))

f2 =
    x[["apy_tobacco-kharif"]] %>% 
    `/`(rowSums(cbind(x[["apy_tobacco-kharif"]],
                      x[["apy_tobacco-rabi"]],
                      x[["apy_tobacco-summer"]]), na.rm=TRUE))

x[["apy_tobacco-kharif"]] =
    rowSums(cbind(x[["apy_tobacco-whole_year"]] * f2,
                  x[["apy_tobacco-kharif"]]), na.rm=TRUE)

x[["apy_tobacco-rabi"]] =
    rowSums(cbind(x[["apy_tobacco-whole_year"]] * f1,
                  x[["apy_tobacco-rabi"]]), na.rm=TRUE)

x[["apy_tobacco-summer"]] =
    rowSums(cbind(x[["apy_tobacco-whole_year"]] * (1 - f1 - f2),
                  x[["apy_tobacco-summer"]]), na.rm=TRUE)

## tea
## ###

x[["apy_tea-whole_year"]] =
    rowSums(cbind(x[["apy_tea-kharif"]],
                  x[["apy_tea-whole_year"]]))

## rest of crops
## #############

x[["apy_rest_of_crops-kharif"]] =
    rowSums(cbind(x[["apy_rest_of_crops-autumn"]],
                  x[["apy_rest_of_crops-kharif"]]))

## remove columns
x = x[,!names(x) %in% c("apy_rice-whole_year", "apy_rice-rabi","apy_rice-kharif","apy_wheat-kharif","apy_wheat-winter","apy_wheat-summer","apy_wheat-whole_year","apy_barley-whole_year","apy_sorghum-autumn","apy_sorghum-whole_year","apy_maize-whole_year","apy_maize-autumn","apy_maize-winter","apy_pearl_millet-whole_year","apy_finger_millet-whole_year","apy_finger_millet-autumn","apy_finger_millet-winter","apy_other_cereals-whole_year","apy_chickpea-whole_year","apy_chickpea-winter","apy_pigeonpea-whole_year","apy_pigeonpea-winter","apy_lentil-whole_year","apy_other_pulses-whole_year","apy_other_pulses-autumn","apy_other_pulses-winter","apy_sugarcane-autumn","apy_sugarcane-kharif","apy_sugarcane-rabi","apy_sugarcane-summer","apy_sugarcane-winter","apy_groundnut-whole_year","apy_groundnut-autumn","apy_groundnut-winter","apy_rapeseed-whole_year","apy_rapeseed-winter","apy_sesameseed-whole_year","apy_sesameseed-autumn","apy_sesameseed-winter","apy_soybean-whole_year","apy_sunflower-whole_year","apy_other_oil_crops-whole_year","apy_other_oil_crops-winter","apy_banana-summer","apy_banana-kharif","apy_banana-rabi","apy_coconut-kharif","apy_cassava-rabi","apy_cassava-kharif","apy_potato-whole_year","apy_potato-winter","apy_vegetables-whole_year","apy_tropical_fruit-rabi","apy_tropical_fruit-kharif","apy_cotton-whole_year","apy_other_fibre_crops-autumn","apy_other_fibre_crops-whole_year","apy_tobacco-whole_year","apy_tea-kharif","apy_rest_of_crops-autumn")]

## manage sort irrigation columns
x %<>%
    add_column(`irr_wheat-rabi` = rep(NA, nrow(.)),
               `irr_barley-kharif` = rep(NA, nrow(.)),
               `irr_barley-rabi` = rep(NA, nrow(.)),
               `irr_sorghum-summer` = rep(NA, nrow(.)),
               `irr_maize-kharif` = rep(NA, nrow(.)),
               `irr_maize-rabi` = rep(NA, nrow(.)),
               `irr_maize-summer` = rep(NA, nrow(.)),
               `irr_pearl_millet-kharif` = rep(NA, nrow(.)),
               `irr_pearl_millet-rabi` = rep(NA, nrow(.)),
               `irr_pearl_millet-summer` = rep(NA, nrow(.)),
               `irr_finger_millet-kharif` = rep(NA, nrow(.)),
               `irr_finger_millet-rabi` = rep(NA, nrow(.)),
               `irr_finger_millet-summer` = rep(NA, nrow(.)),
               `irr_chickpea-kharif` = rep(NA, nrow(.)),
               `irr_chickpea-rabi` = rep(NA, nrow(.)),
               `irr_pigeonpea-kharif` = rep(NA, nrow(.)),
               `irr_pigeonpea-rabi` = rep(NA, nrow(.)),
               `irr_cowpea` = rep(NA, nrow(.)),
               `irr_cowpea-kharif` = rep(NA, nrow(.)),
               `irr_cowpea-rabi` = rep(NA, nrow(.)),
               `irr_lentil` = rep(NA, nrow=(.)),
               `irr_lentil-kharif` = rep(NA, nrow(.)),
               `irr_lentil-rabi` = rep(NA, nrow(.)),
               `irr_other_pulses-summer` = rep(NA, nrow(.)),
               `irr_sugarcane-whole_year` = rep(NA, nrow(.)),
               `irr_groundnut-kharif` = rep(NA, nrow(.)),
               `irr_groundnut-rabi` = rep(NA, nrow(.)),
               `irr_groundnut-summer` = rep(NA, nrow(.)),
               `irr_rapeseed-kharif` = rep(NA, nrow(.)),
               `irr_rapeseed-rabi` = rep(NA, nrow(.)),
               `irr_sesameseed-kharif` = rep(NA, nrow(.)),
               `irr_sesameseed-rabi` = rep(NA, nrow(.)),
               `irr_sesameseed-summer` = rep(NA, nrow(.)),
               `irr_soybean-kharif` = rep(NA, nrow(.)),
               `irr_soybean-rabi` = rep(NA, nrow(.)),
               `irr_sunflower-kharif` = rep(NA, nrow(.)),
               `irr_sunflower-rabi` = rep(NA, nrow(.)),
               `irr_sunflower-summer` = rep(NA, nrow(.)),
               `irr_other_oil_crops-kharif` = rep(NA, nrow(.)),
               `irr_other_oil_crops-rabi` = rep(NA, nrow(.)),
               `irr_banana` = rep(NA, nrow(.)),
               `irr_banana-whole_year` = rep(NA, nrow(.)),
               `irr_coconut` = rep(NA, nrow(.)),
               `irr_coconut-whole_year` = rep(NA, nrow(.)),
               `irr_yams` = rep(NA, nrow(.)),
               `irr_yams-whole_year` = rep(NA, nrow(.)),
               `irr_cassava` = rep(NA, nrow(.)),
               `irr_cassava-whole_year` = rep(NA, nrow(.)),
               `irr_sweet_potato` = rep(NA, nrow(.)),
               `irr_sweet_potato-kharif` = rep(NA, nrow(.)),
               `irr_sweet_potato-rabi` = rep(NA, nrow(.)),
               `irr_sweet_potato-whole_year` = rep(NA, nrow(.)),
               `irr_potato` = rep(NA, nrow(.)),
               `irr_potato-kharif` = rep(NA, nrow(.)),
               `irr_potato-rabi` = rep(NA, nrow(.)),
               `irr_potato-summer` = rep(NA, nrow(.)),
               `irr_vegetables` = rep(NA, nrow(.)),
               `irr_vegetables-kharif` = rep(NA, nrow(.)),
               `irr_vegetables-rabi` = rep(NA, nrow(.)),
               `irr_vegetables-summer` = rep(NA, nrow(.)),
               `irr_temperate_fruit` = rep(NA, nrow(.)),
               `irr_temperate_fruit-whole_year` = rep(NA, nrow(.)),
               `irr_tropical_fruit` = rep(NA, nrow(.)),
               `irr_tropical_fruit-whole_year`= rep(NA, nrow(.)),
               `irr_cotton-kharif` = rep(NA, nrow(.)),
               `irr_cotton-rabi` = rep(NA, nrow(.)),
               `irr_cotton-summer` = rep(NA, nrow(.)),
               `irr_other_fibre_crops-kharif` = rep(NA, nrow(.)),
               `irr_other_fibre_crops-rabi` = rep(NA, nrow(.)),
               `irr_coffee` = rep(NA, nrow(.)),
               `irr_coffee-whole_year` = rep(NA, nrow(.)),
               `irr_tobacco-kharif` = rep(NA, nrow(.)),
               `irr_tobacco-rabi` = rep(NA, nrow(.)),
               `irr_tobacco-summer` = rep(NA, nrow(.)),
               `irr_tea` = rep(NA, nrow(.)),
               `irr_tea-whole_year` = rep(NA, nrow(.)),
               `irr_rest_of_crops` = rep(NA, nrow(.)),
               `irr_rest_of_crops-kharif` = rep(NA, nrow(.)),
               `irr_rest_of_crops-rabi` = rep(NA, nrow(.)),
               `irr_rest_of_crops-summer` = rep(NA, nrow(.)),
               `irr_rest_of_crops-whole_year` = rep(NA, nrow(.)))

ix = !duplicated(x[["ADM2_CODE"]])
dists = x[["ADM2_CODE"]][ix]

allocate_irrigated_area = function(x, crop_nm, season_order) {
    ## Allocates irrigated area (which is *not* disaggregated between
    ## seasons) to seasons based on the total area of the crop grown in
    ## each season. Seasons are considered in turn according to the
    ## 'season_order' argument. Surplus irrigated area is carried over
    ## to the next season. For example, almost all wheat is grown during
    ## rabi under irrigated conditions, therefore 'rabi' is the first
    ## season in 'season_order'. 

    irr_nm = paste0("irr_", crop_nm)
    if (!irr_nm %in% names(x)) {
        stop()
    }
    
    unalloc_irr_area = x[[irr_nm]] %>% `[<-`(is.na(.), 0)

    for (i in 1:length(season_order)) {
        apy_season_nm = paste0("apy_", crop_nm, "-", season_order[i])
        irr_season_nm = paste0("irr_", crop_nm, "-", season_order[i])
        if (!all(c(apy_season_nm,irr_season_nm) %in% names(x))) {
            stop()
        }
        
        x[[irr_season_nm]] = pmax(pmin(x[[apy_season_nm]], unalloc_irr_area, na.rm=TRUE), 0, na.rm=TRUE)
        unalloc_irr_area = unalloc_irr_area - x[[irr_season_nm]]
    }
    x
}

get_mapspam_irri_frac = function(x, crop_nm, ...) {
    ## Retrieve irrigated area from MapSPAM product - this is used to
    ## fill missing inventory data

    mapspam_tot_nm = paste0("mapspam_atot_", crop_nm)
    mapspam_irr_nm = paste0("mapspam_xirr_", crop_nm)
    if (!all(c(mapspam_tot_nm, mapspam_irr_nm) %in% names(x))) {
        stop()
    }

    ix = x[["Year"]] %in% 2005
    tot = x[[mapspam_tot_nm]] %>% `[`(ix)
    irr = x[[mapspam_irr_nm]] %>% `[`(ix)
    irr_frac = (irr / tot) %>% `[<-`(is.na(.), 0)
    crop_cols = grep(paste0("apy_", crop_nm), names(x))
    if (!length(crop_cols) > 0) {
        stop()
    }
    
    crop_total = rowSums(x[,crop_cols], na.rm=TRUE)
    irr_total = crop_total * irr_frac
    irr_nm = paste0("irr_", crop_nm)
    x[[irr_nm]] = irr_total
    x
}

## ==================================
## 1 - allocate irrigated area
## ==================================

for (i in 1:length(dists)) {

    dist = dists[i]
    
    row_ix = x[["ADM2_CODE"]] %in% dist
    xx = x[row_ix,]

    xx = allocate_irrigated_area(xx, "wheat", c("rabi"))
    xx = allocate_irrigated_area(xx, "barley", c("rabi","kharif"))
    xx[["irr_sorghum-summer"]] = xx[["apy_sorghum-summer"]]
    xx = allocate_irrigated_area(xx, "maize", c("summer","rabi","kharif"))
    xx = allocate_irrigated_area(xx, "pearl_millet", c("summer","rabi","kharif"))
    xx = allocate_irrigated_area(xx, "finger_millet", c("summer","rabi","kharif"))
    xx = allocate_irrigated_area(xx, "chickpea", c("rabi","kharif"))
    xx = allocate_irrigated_area(xx, "pigeonpea", c("rabi","kharif"))

    xx = get_mapspam_irri_frac(xx, "cowpea")
    xx = allocate_irrigated_area(xx, "cowpea", c("rabi","kharif"))
    
    xx = get_mapspam_irri_frac(xx, "lentil")
    xx = allocate_irrigated_area(xx, "lentil", c("rabi","kharif"))

    xx[["irr_other_pulses-summer"]] = xx[["apy_other_pulses-summer"]]
    xx = allocate_irrigated_area(xx, "sugarcane", "whole_year")
    xx = allocate_irrigated_area(xx, "groundnut", c("summer","rabi","kharif"))
    xx = allocate_irrigated_area(xx, "rapeseed", c("rabi","kharif"))
    xx = allocate_irrigated_area(xx, "sesameseed", c("summer","rabi","kharif"))
    xx = allocate_irrigated_area(xx, "soybean", c("rabi","kharif"))
    xx = allocate_irrigated_area(xx, "sunflower", c("summer","rabi","kharif"))
    xx = allocate_irrigated_area(xx, "other_oil_crops", c("rabi","kharif"))

    xx = get_mapspam_irri_frac(xx, "banana")
    xx = allocate_irrigated_area(xx, "banana", "whole_year")

    xx = get_mapspam_irri_frac(xx, "coconut")
    xx = allocate_irrigated_area(xx, "coconut", "whole_year")

    xx = get_mapspam_irri_frac(xx, "yams")
    xx = allocate_irrigated_area(xx, "yams", "whole_year")
    
    xx = get_mapspam_irri_frac(xx, "cassava")
    xx = allocate_irrigated_area(xx, "cassava", "whole_year")

    xx = get_mapspam_irri_frac(xx, "sweet_potato")
    xx = allocate_irrigated_area(xx, "sweet_potato", c("rabi","whole_year","kharif"))

    xx = get_mapspam_irri_frac(xx, "potato")
    xx = allocate_irrigated_area(xx, "potato", c("summer","rabi","kharif"))

    xx = get_mapspam_irri_frac(xx, "vegetables")
    xx = allocate_irrigated_area(xx, "vegetables", c("summer","rabi","kharif"))
    
    xx = get_mapspam_irri_frac(xx, "temperate_fruit")
    xx = allocate_irrigated_area(xx, "temperate_fruit", "whole_year")
    
    xx = get_mapspam_irri_frac(xx, "tropical_fruit")
    xx = allocate_irrigated_area(xx, "tropical_fruit", "whole_year")
    
    xx = allocate_irrigated_area(xx, "cotton", c("summer","rabi","kharif"))
    
    xx = get_mapspam_irri_frac(xx, "coffee")
    xx = allocate_irrigated_area(xx, "coffee", "whole_year")
    
    xx = allocate_irrigated_area(xx, "tobacco", c("summer","rabi","kharif"))
    
    xx = get_mapspam_irri_frac(xx, "tea")
    xx = allocate_irrigated_area(xx, "tea", "whole_year")
    
    xx = get_mapspam_irri_frac(xx, "rest_of_crops")
    xx = allocate_irrigated_area(xx, "rest_of_crops", c("summer","rabi","whole_year","kharif"))
        
    x[row_ix,] = xx
}

## remove surplus columns (e.g. irr_wheat, irr_barley, ...)
x = x[,!names(x) %in% grep("^irr_*[^-]*$", names(x), value=TRUE)]
x = x[,!names(x) %in% grep("^irr_(.*)-total$", names(x), value=TRUE)]

## limit to 2000-2010
x$Year = as.numeric(x$Year)
x = x[x$Year %in% 2000:2010,]

saveRDS(x, "data/apy_indiastat_combined_data_formatted.rds")

