## correct confusion between irrigated rice seasons
xx[["irr_rice-summer"]][7:11] = xx[["irr_rice-winter"]][7:11]
xx[["irr_rice-winter"]] = xx[[c("irr_rice-autumn")]]
xx[["irr_rice-autumn"]] = 0

xx[["irr_rice-winter"]] %<>% irr_frac_interp(xx[["apy_rice-winter"]], 3, c(2,4))

## ## confusion between sunflower
## xx[["apy_sunflower-kharif"]][c(6:7,9:11)] = xx[["apy_sunflower-whole_year"]][c(6:7,9:11)]
## xx[["apy_sunflower-kharif"]][c(8)] = xx[["apy_sunflower-rabi"]][c(8)]
## xx[["apy_sunflower-whole_year"]] = 0
## xx[["apy_sunflower-rabi"]] = 0

## district-level corrections
if (dist_nm %in% "Adilabad") {
    xx[["irr_pearl_millet-summer"]][8:9] = xx[["apy_pearl_millet-rabi"]][8:9]
    xx[["apy_other_cereals-kharif"]][1:4] = NA
    xx[["apy_sugarcane-whole_year"]][c(8,10)] = xx[["irr_sugarcane-whole_year"]][c(8,10)]
}

if (dist_nm %in% "Karimnagar") {
    xx[["apy_wheat-rabi"]][8] = xx[["irr_wheat-rabi"]][8]
    xx[["apy_pearl_millet-rabi"]][8] = xx[["irr_pearl_millet-rabi"]][8]
}   
