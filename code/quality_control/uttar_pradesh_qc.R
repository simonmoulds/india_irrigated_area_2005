## Author : Simon Moulds
## Date   : January 2018

## Quality control for Uttar Pradesh

## Crops not grown: banana, cassava, coconut, coffee, cowpea,
## other fibre, tea, temperate fruit, tropical fruit, yams

if (dist_nm %in% "Etawah") {
    xx[["irr_barley-rabi"]][4] = xx[["irr_barley-kharif"]][4]
    xx[["irr_barley-kharif"]] = NA
}

xx[["irr_barley-rabi"]] = rowSums(matrix(c(xx[["irr_barley-rabi"]], xx[["irr_barley-kharif"]]), ncol=2), na.rm=TRUE)
xx[["irr_barley-kharif"]] = NA

xx[["apy_cotton-kharif"]] %<>% linear_interp(4)
xx[["irr_cotton-kharif"]] %<>% linear_interp(4)
xx[["irr_cotton-kharif"]] %<>% irr_frac_interp(xx[["apy_cotton-kharif"]], 6, c(5,7))
xx[["irr_cotton-kharif"]] %<>% irr_frac_interp(xx[["apy_cotton-kharif"]], 11, 10)

if (dist_nm %in% "Baghpat") {
    xx[["apy_cotton-kharif"]] %<>% linear_interp(9)
    xx[["irr_cotton-kharif"]] %<>% linear_interp(9)
}

if (dist_nm %in% "Firozabad") {
    xx[["apy_cotton-kharif"]] %<>% linear_interp(c(6,9))
    xx[["irr_cotton-kharif"]] %<>% linear_interp(c(6,9))
}

xx[["apy_lentil-rabi"]] %<>% linear_interp(5:6)

xx[["apy_groundnut-kharif"]] %<>% linear_interp(c(6))
xx[["irr_groundnut-kharif"]] %<>% linear_interp(c(6))

xx[["irr_maize-kharif"]] %<>% irr_frac_interp(xx[["apy_maize-kharif"]], 11, 10)
xx[["apy_maize-summer"]] %<>% linear_interp(c(5:6))
xx[["irr_maize-summer"]] %<>% linear_interp(c(5:6))

if (!dist_nm %in% c("Deoria","Lakhimpur Kheri")) {
    xx[["apy_maize-summer"]][1:3] = xx[["apy_maize-rabi"]][1:3]
    xx[["apy_maize-rabi"]] = NA
    xx[["irr_maize-summer"]][1:3] = xx[["irr_maize-rabi"]][1:3]
    xx[["irr_maize-rabi"]] = NA
    xx[["irr_maize-summer"]] %<>% irr_frac_interp(xx[["apy_maize-summer"]], 11, 10)
}


xx[["apy_other_cereals-kharif"]] %<>% linear_interp(c(6))
xx[["apy_other_cereals-kharif"]] %<>% linear_interp(c(4))

xx[["irr_other_oil_crops-rabi"]] %<>% irr_frac_interp(xx[["apy_other_oil_crops-rabi"]], 6, c(5,7))

xx[["apy_pigeonpea-kharif"]] %<>% linear_interp(c(3,5,6))
xx[["irr_pigeonpea-kharif"]] %<>% linear_interp(c(3,5,6))

xx[["apy_sweet_potato-kharif"]] %<>% locf_interp(na_ix=1:4, fromLast=TRUE)
xx[["apy_potato-kharif"]] %<>% locf_interp(na_ix=1:4, fromLast=TRUE)
xx[["apy_potato-rabi"]] %<>% locf_interp(na_ix=1:4, fromLast=TRUE)

xx[["irr_barley-rabi"]] %<>% irr_frac_interp(xx[["apy_barley-rabi"]], c(6), c(5,7))
xx[["irr_barley-rabi"]] %<>% irr_frac_interp(xx[["apy_barley-rabi"]], 11, 10)

xx[["irr_chickpea-rabi"]] %<>% irr_frac_interp(xx[["apy_chickpea-rabi"]], c(6), c(5,7))
xx[["irr_chickpea-rabi"]] %<>% irr_frac_interp(xx[["apy_chickpea-rabi"]], 11, 10)

xx[["irr_cotton-rabi"]] %<>% irr_frac_interp(xx[["apy_cotton-rabi"]], c(6), c(5,7))
xx[["irr_cotton-rabi"]] %<>% irr_frac_interp(xx[["apy_cotton-rabi"]], 11, 10)

xx[["irr_groundnut-kharif"]] %<>% irr_frac_interp(xx[["apy_groundnut-kharif"]], 10, 11)


xx[["irr_rice-winter"]] = xx[["irr_rice-autumn"]]
xx[["irr_rice-autumn"]] = NA
xx[["irr_rice-winter"]] %<>% irr_frac_interp(xx[["apy_rice-winter"]], c(6), c(5,7))
xx[["irr_rice-winter"]] %<>% irr_frac_interp(xx[["apy_rice-winter"]], 11, 10)

xx[["irr_wheat-rabi"]] %<>% irr_frac_interp(xx[["apy_wheat-rabi"]], c(6), c(5,7))
xx[["irr_wheat-rabi"]] %<>% irr_frac_interp(xx[["apy_wheat-rabi"]], 11, 10)


xx[["apy_other_pulses-kharif"]] %<>% linear_interp(c(5:7))
xx[["irr_other_pulses-kharif"]] %<>% irr_frac_interp(xx[["apy_other_pulses-kharif"]], 6, c(5,7))
xx[["irr_other_pulses-kharif"]] %<>% irr_frac_interp(xx[["apy_other_pulses-kharif"]], 11, 10)

xx[["apy_other_pulses-rabi"]] %<>% linear_interp(c(5:6))
xx[["irr_other_pulses-rabi"]] %<>% irr_frac_interp(xx[["apy_other_pulses-rabi"]], 6, c(5,7))
xx[["irr_other_pulses-rabi"]] %<>% irr_frac_interp(xx[["apy_other_pulses-rabi"]], 11, 10)

xx[["apy_other_pulses-summer"]] %<>% linear_interp(c(5:7))
xx[["irr_other_pulses-summer"]] %<>% linear_interp(c(5:7))
xx[["apy_other_pulses-summer"]] %<>% locf_interp(c(1:2), fromLast=TRUE)
xx[["irr_other_pulses-summer"]] %<>% locf_interp(c(1:2), fromLast=TRUE)

xx[["irr_pigeon_pea-kharif"]] %<>% irr_frac_interp(xx[["apy_pigeon_pea-kharif"]], 11, 10)

xx[["apy_sweet_potato-kharif"]][5:6] = xx[["apy_sweet_potato-whole_year"]][5:6]
xx[["apy_sweet_potato-whole_year"]] = NA

xx[["apy_vegetables-rabi"]] %<>% locf_interp(c(1:2), fromLast=TRUE)
xx[["apy_vegetables-summer"]] %<>% locf_interp(c(1:2), fromLast=TRUE)
xx[["apy_vegetables-rabi"]] %<>% linear_interp(c(5:6))
xx[["apy_vegetables-summer"]] %<>% linear_interp(c(5:6,9))
xx[["apy_vegetables-summer"]] %<>% locf_interp(c(11))

if (dist_nm %in% c("Allahabad")) {
    xx[["apy_vegetables-rabi"]] %<>% locf_interp(c(1:3), fromLast=TRUE)
}

## Sant Ravi Das Nagar
## ###################

if (dist_nm %in% "Sant Ravi Das Nagar") {
    xx[["apy_sweet_potato-kharif"]] %<>% linear_interp(c(5:6))
}

xx[["irr_rapeseed-rabi"]] %<>% irr_frac_interp(xx[["apy_rapeseed-rabi"]], 6, c(5,7))
xx[["irr_rapeseed-rabi"]] %<>% irr_frac_interp(xx[["apy_rapeseed-rabi"]], 11, 10)

xx[["irr_sorghum-kharif"]] %<>% irr_frac_interp(xx[["apy_sorghum-kharif"]], 6, c(5,7))
xx[["irr_sorghum-kharif"]] %<>% irr_frac_interp(xx[["apy_sorghum-kharif"]], 11, 10)

xx[["irr_sugarcane-whole_year"]] %<>% irr_frac_interp(xx[["apy_sugarcane-whole_year"]], 6, c(5,7))
xx[["irr_sugarcane-whole_year"]] %<>% irr_frac_interp(xx[["apy_sugarcane-whole_year"]], 11, 10)

xx[["apy_sunflower-summer"]][1:4] = xx[["apy_sunflower-kharif"]][1:4]
xx[["irr_sunflower-summer"]][1:4] = xx[["irr_sunflower-kharif"]][1:4]
xx[["apy_sunflower-kharif"]][1:4] = NA
xx[["irr_sunflower-kharif"]][1:4] = NA

xx[["irr_sunflower-summer"]] %<>% irr_frac_interp(xx[["apy_sunflower-summer"]], 6, c(5,7))
xx[["irr_sunflower-summer"]] %<>% irr_frac_interp(xx[["apy_sunflower-summer"]], 11, 10)

if (!dist_nm %in% c("Allahabad","Gonda")) {
    xx[["irr_sesameseed-kharif"]] %<>% irr_frac_interp(xx[["apy_sesameseed-kharif"]], 6, c(5,7))
    xx[["irr_sesameseed-kharif"]] %<>% irr_frac_interp(xx[["apy_sesameseed-kharif"]], 11, 10)
}

if (dist_nm %in% c("Pilibhit","Rampur","Shahjahanpur")) {
    xx[["apy_rice-summer"]][5] = xx[["irr_rice-summer"]][5]
    xx[["apy_rice-summer"]] %<>% linear_interp(6)
}

if (dist_nm %in% c("Azamgarh","Ballia")) {
    xx[["irr_other_cereals-kharif"]] %<>% irr_frac_interp(xx[["apy_other_cereals-kharif"]], c(6), c(5,7))
}            

if (dist_nm %in% c("Azamgarh","Ballia","Basti","Etah","Faizabad","Gorakhpur","Hardoi","Maharajganj","Mau","Pratapgarh","Rae Bareli","Sultanpur","Unnao")) {
    xx[["irr_other_cereals-rabi"]] %<>% linear_interp(c(6))
}            

if (dist_nm %in% "Balrampur") {
    xx[["apy_pearl_millet-summer"]] %<>% linear_interp(c(4,6,8,9,10))
}

if (dist_nm %in% c("Bulandshahr","Etah","Etawah","Farrukhabad","Firozabad","Gautam Buddha Nagar","Ghaziabad","Hathras","Jyotiba Phule Nagar","Kannauj","Kanpur Dehat","Kanpur","Meerut")) {
    xx[["irr_pearl_millet-summer"]] %<>% irr_frac_interp(xx[["apy_pearl_millet-summer"]], 6, c(5,7))
    xx[["irr_pearl_millet-summer"]] %<>% irr_frac_interp(xx[["apy_pearl_millet-summer"]], 11, 10)
}

if (dist_nm %in% "Hamirpur") {
    xx[["apy_pearl_millet-summer"]] %<>% linear_interp(c(6))
}


if (dist_nm %in% "Kaushambi") {
    xx[["apy_maize-kharif"]] %<>% linear_interp(c(6))
}

if (dist_nm %in% c("Agra","Aligarh","Auraiya","Azamgarh","Badaun","Baghpat","Basti","Bulandshahr","Etah","Etawah","Farrukhabad","Fatehpur","Firozabad","Gautam Buddha Nagar","Ghaziabad","Gonda","Hathras","Jyotiba Phule Nagar","Kannauj","Kanpur Dehat","Kanpur","Kaushambi","Mainpuri","Mathura","Meerut","Moradabad","Muzaffarnagar","Shahjahanpur")) {
    xx[["irr_maize-kharif"]] %<>% irr_frac_interp(xx[["apy_maize-kharif"]], c(6), c(5,7))
}

if (dist_nm %in% c("Faizabad")) {
    xx[["irr_maize-kharif"]] %<>% irr_frac_interp(xx[["apy_maize-kharif"]], c(6), c(7))
}

if (dist_nm %in% c("Agra")) {
    xx[["irr_maize-kharif"]] %<>% irr_frac_interp(xx[["apy_maize-kharif"]], c(9), c(8,10))
}

## Aligarh
## #######
if (dist_nm %in% "Aligarh") {
    xx[["irr_chickpea-rabi"]] %<>% irr_frac_interp(xx[["apy_chickpea-rabi"]], 6, 5)
}

## Allahabad
## #########
if (dist_nm %in% "Allahabad") {
    xx[["apy_lentil-rabi"]] %<>% linear_interp(3)
    xx[["apy_other_cereals-kharif"]] %<>% linear_interp(c(6))
    xx[["apy_other_oil_crops-rabi"]] %<>% linear_interp(3)
}            

## Ambedkar Nagar
## ##############
if (dist_nm %in% "Ambedkar Nagar") {
    xx[["irr_chickpea-rabi"]] %<>% linear_interp(na_ix=6)
    xx[["apy_groundnut-kharif"]] %<>% linear_interp(c(6,8:9))
}

## Auraiya
## #######
if (dist_nm %in% "Auraiya") {
    xx[["irr_barley-rabi"]] %<>% irr_frac_interp(xx[["apy_barley-rabi"]], c(4), c(3,5))
    xx[["irr_chickpea-rabi"]] %<>% irr_frac_interp(xx[["apy_chickpea-rabi"]], c(4), c(3,5))
    xx[["irr_groundnut-kharif"]] %<>% irr_frac_interp(xx[["apy_groundnut-kharif"]], c(4), c(3,5))
    xx[["apy_lentil-rabi"]] %<>% linear_interp(8)

    xx[["irr_maize-kharif"]] %<>% irr_frac_interp(xx[["apy_maize-kharif"]], c(4), c(3,5))
    xx[["irr_other_pulses-rabi"]] %<>% irr_frac_interp(xx[["apy_other_pulses-rabi"]], 4, c(3,5))

    xx[["irr_rapeseed-rabi"]] %<>% irr_frac_interp(xx[["apy_rapeseed-rabi"]], c(4), c(3,5))
    xx[["irr_rice-winter"]] %<>% irr_frac_interp(xx[["apy_rice-winter"]], c(4), c(3,5))                
    xx[["irr_sugarcane-whole_year"]] %<>% irr_frac_interp(xx[["apy_sugarcane-whole_year"]], c(4), c(3,5))
    xx[["irr_sunflower-summer"]] %<>% irr_frac_interp(xx[["apy_sunflower-summer"]], c(4), c(3,5))                
    xx[["apy_vegetables-rabi"]] %<>% linear_interp(9)
    xx[["apy_vegetables-rabi"]] %<>% locf_interp(11)
    xx[["irr_wheat-rabi"]] %<>% irr_frac_interp(xx[["apy_wheat-rabi"]], c(4), c(3,5))
}

## ## Azamgarh
## ## ########
## if (dist_nm %in% "Azamgarh") {
##     xx[["apy_other_cereals-kharif"]] %<>% linear_interp(c(4))
## }

## Badaun
## ######
if (dist_nm %in% "Badaun") {
    xx[["irr_barley-rabi"]][10] = xx[["irr_barley-kharif"]][10] + xx[["irr_barley-rabi"]][10]
    xx[["irr_barley-kharif"]] = NA

    xx[["apy_barley-rabi"]] %<>% tot_frac_interp(xx[["irr_barley-rabi"]], 10, 9)
    xx[["irr_barley-kharif"]]
}

## ## Bahraich
## ## ########
## if (dist_nm %in% "Bahraich") {
##     xx[["apy_other_cereals-kharif"]] %<>% linear_interp(c(4))
## }

## Baghpat
## #######
if (dist_nm %in% "Baghpat") {
    xx[["apy_other_pulses-kharif"]][5:8] = xx[["irr_other_pulses-kharif"]][5:8]
}

## Ballia
## ######
if (dist_nm %in% "Ballia") {
    xx[["apy_other_cereals-kharif"]][4] = xx[["apy_other_cereals-kharif"]][5]
    xx[["irr_other_cereals-rabi"]] %<>% locf_interp(xx[["apy_other_cereals-rabi"]], 11)
    xx[["apy_other_pulses-rabi"]] %<>% tot_frac_interp(xx[["irr_other_pulses-rabi"]], 9, c(8,10))
}

## Bara Banki
## ##########

if (dist_nm %in% "Bara Banki") {
    xx[["irr_other_oil_crops-rabi"]] %<>% irr_frac_interp(xx[["apy_other_oil_crops-rabi"]], 2, c(1,3))
    xx[["irr_other_oil_crops-rabi"]] %<>% irr_frac_interp(xx[["apy_other_oil_crops-rabi"]], 11, 10)
}

## Bareilly
## ########
if (dist_nm %in% "Bareilly") {
    xx[["irr_other_oil_crops-rabi"]] %<>% irr_frac_interp(xx[["apy_other_oil_crops-rabi"]], 11, 10)
}

## Basti
## #####

if (dist_nm %in% "Basti") {
    xx[["apy_chickpea-rabi"]] %<>% linear_interp(8)
    xx[["apy_other_cereals-rabi"]] %<>% linear_interp(c(4,8))
    xx[["apy_other_cereals-rabi"]] %<>% locf_interp(11)
    xx[["irr_other_cereals-rabi"]] %<>% locf_interp(11)
}

## ## Bijnor
## ## ######

## if (dist_nm %in% "Bijnor") {
##     xx[["apy_other_pulses-kharif"]] %<>% tot_frac_interp(xx[["irr_other_pulses-kharif"]], 9, c(8,10))
## }

## Chandauli
## #########

if (dist_nm %in% "Chandauli") {
    xx[["apy_other_cereals-rabi"]] %<>% linear_interp(c(4,6))
    xx[["irr_other_cereals-rabi"]] %<>% locf_interp(5)
    xx[["apy_other_oil_crops-rabi"]] %<>% locf_interp(1, fromLast=TRUE)
}

## Chitrakoot
## ##########

if (dist_nm %in% "Chitrakoot") {
    xx[["apy_groundnut-kharif"]] %<>% linear_interp(9)
    xx[["apy_groundnut-kharif"]] %<>% linear_interp(9)
}

## Deoria
## ######
if (dist_nm %in% "Deoria") {
    xx[["apy_maize-rabi"]] %<>% linear_interp(c(5))
    xx[["irr_maize-rabi"]] %<>% linear_interp(c(5))
    xx[["irr_maize-rabi"]] %<>% irr_frac_interp(xx[["apy_maize-rabi"]], 11, 10)                
}

## Etah
## ####
if (dist_nm %in% "Etah") {
    xx[["irr_other_cereals-kharif"]] %<>% locf_interp(6)
    xx[["irr_other_cereals-kharif"]] %<>% linear_interp(7)
}

## Faizabad
## ########
if (dist_nm %in% "Faizabad") {
    xx[["apy_maize-kharif"]] %<>% linear_interp(5)
}

## Farrukhabad
## ###########

if (dist_nm %in% "Farrukhabad") {
    xx[["apy_chickpea-rabi"]] %<>% tot_frac_interp(xx[["irr_chickpea-rabi"]], 8, c(7,9))
}

## Ghazipur
## ########

if (dist_nm %in% "Ghazipur") {
    xx[["apy_chickpea-rabi"]] %<>% linear_interp(7:8)
}


## Kanpur
## ######

if (dist_nm %in% "Kanpur") {

    f = xx[["apy_barley-rabi"]][4] / xx[["irr_barley-rabi"]]
    xx[["apy_barley-rabi"]][1:3] = xx[["apy_barley-rabi"]][1:3] * f

    xx[["apy_chickpea-rabi"]] %<>% locf_interp(c(1:3), fromLast=TRUE)
    xx[["irr_chickpea-rabi"]] %<>% locf_interp(c(1:3), fromLast=TRUE)

    xx[["apy_maize-kharif"]] %<>% locf_interp(c(1:2), fromLast=TRUE)
    xx[["irr_maize-kharif"]] %<>% locf_interp(c(1:2), fromLast=TRUE)

    xx[["apy_rapeseed-rabi"]] %<>% locf_interp(c(1:3), fromLast=TRUE)
    xx[["irr_rapeseed-rabi"]] %<>% irr_frac_interp(xx[["apy_rapeseed-rabi"]], 1:3, 4)
    xx[["apy_rice-winter"]][1:3] = xx[["irr_rice-winter"]][1:3]
    xx[["apy_wheat-rabi"]] %<>% locf_interp(c(1:3), fromLast=TRUE)
    xx[["irr_wheat-rabi"]] %<>% irr_frac_interp(xx[["apy_wheat-rabi"]], 1:3, 4)
}

## Kushinagar
## ##########
if (dist_nm %in% "Kushinagar") {
    xx[["irr_other_oil_crops-rabi"]] %<>% irr_frac_interp(xx[["apy_other_oil_crops-rabi"]], 11, 10)
}

## Lakhimpur Kheri
## ###############

if (dist_nm %in% "Lakhimpur Kheri") {
    xx[["apy_maize-rabi"]] %<>% linear_interp(5)
    xx[["irr_maize-rabi"]] %<>% linear_interp(5)
}            

## Lalitpur
## ########

if (dist_nm %in% "Lalitpur") {
    xx[["apy_groundnut-kharif"]] %<>% linear_interp(c(6,9))
    xx[["apy_other_cereals-kharif"]] %<>% linear_interp(c(4:7))
}

## Lucknow
## #######

if (dist_nm %in% "Lucknow") {
    xx[["irr_other_cereals-kharif"]] %<>% irr_frac_interp(xx[["apy_other_cereals-kharif"]], 6, c(5,7))
}

## Mahoba
## ######

if (dist_nm %in% "Mahoba") {
    xx[["apy_groundnut-kharif"]] %<>% linear_interp(c(6,9))
}

## Maharajganj
## ###########

if (dist_nm %in% "Maharajganj") {
    xx[["apy_sesameseed-kharif"]] %<>% linear_interp(c(6,9))
}

## Mau
## ###

if (dist_nm %in% "Mau") {
    xx[["apy_chickpea-rabi"]] %<>% linear_interp(c(7:8))
    xx[["irr_chickpea-rabi"]] %<>% linear_interp(c(7:8))

    xx[["apy_sesameseed-kharif"]] %<>% linear_interp(c(8))
}

## Sant Kabir Nagar
## ################

if (dist_nm %in% "Sant Kabir Nagar") {
    xx[["apy_sesameseed-kharif"]] %<>% linear_interp(c(2))
}

## Siddharth Nagar
## ###############
if (dist_nm %in% "Siddharth Nagar") {
    xx[["apy_other_oil_crops-rabi"]] %<>% linear_interp(8)
}

## Sitapur
## ##########
if (dist_nm %in% "Sitapur") {
    xx[["apy_other_oil_crops-rabi"]] %<>% locf_interp(9:11)
    xx[["irr_other_oil_crops-rabi"]] %<>% locf_interp(9:11)           
    xx[["irr_other_oil_crops-rabi"]] %<>% irr_frac_interp(xx[["apy_other_oil_crops-rabi"]], 4, c(3,5))
}

## Sultanpur
## #########

if (dist_nm %in% "Sultanpur") {
    xx[["apy_groundnut-kharif"]] %<>% linear_interp(9)
}

## Unnao
## #####

if (dist_nm %in% "Unnao") {
    xx[["apy_other_cereals-kharif"]] %<>% linear_interp(8)
}            
