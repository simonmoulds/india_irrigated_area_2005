## Author : Simon Moulds
## Date   : August 2017

## this script loads all the data required by write_gams_input.R, saving
## the various objects as a single RData file.

library(magrittr)
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)


## Subnational inventory data
x = readRDS("data/apy_indiastat_combined_data_qc.rds")

## administrative area map (use this to define cells included in analysis)
adm2 = readOGR("data-raw/india_adm2_2001/data", layer="g2008_2_India")
template = raster(nrows=360, ncols=360, xmn=68, xmx=98, ymn=6, ymx=36)
india_map = rasterize(adm2, template, "ADM0_CODE")
india_map = focal(india_map, w=matrix(data=1, nrow=7, ncol=7), fun=mean, na.rm=TRUE, pad=TRUE)

## suitability maps
get_gaez_suit_data = function(crop, path, ...) {
    input_levels = c("h_schi","i_scii","h_schr","i_scir","l_sclr")
    out = list()
    count = 0
    for (i in 1:length(input_levels)) {
        level = input_levels[i]
        dir = paste0("res03crav6190", sub("_", "", level), crop, "_package")
        fn = paste0("res03_crav6190", level, "_", crop, ".tif")
        if (file.exists(file.path(path, dir, fn))) {
            count = count + 1
            r = raster(file.path(path, dir, fn))
            out[[level]] = r
        }
    }
    if (count == 0) {
        stop("no files available for the supplied crop")
    }
    out
}

get_gaez_pot_yield_data = function(crop, path, ...) {
    irri_input_levels = c("h","i")
    rain_input_levels = c("h","i","l")
    out = list()
    count = 0
    for (i in 1:length(irri_input_levels)) {
        level = irri_input_levels[i]
        dir = paste0("res02crav6190", level, crop, "000ayld_package")
        fn = paste0("res02_crav6190", level, "_", crop, "000a_yld.tif")
        if (file.exists(file.path(path, dir, fn))) {
            count = count + 1
            r = raster(file.path(path, dir, fn))
            nm = paste0(level, "_irri")
            out[[nm]] = r
        }
    }

    for (i in 1:length(rain_input_levels)) {
        level = rain_input_levels[i]
        dir = paste0("res02crav6190", level, crop, "150byld_package")
        fn = paste0("res02_crav6190", level, "_", crop, "150b_yld.tif")
        if (file.exists(file.path(path, dir, fn))) {
            count = count + 1
            r = raster(file.path(path, dir, fn))
            nm = paste0(level, "_rain")
            out[[nm]] = r
        }
    }
    if (count == 0) {
        stop("no files available for the supplied crop")
    }
    out
}    

gaez_path = "data/GAEZ"
india_ext = extent(68,98,6,36)

barley_suit = get_gaez_suit_data("brl", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

barley_pot_yield = get_gaez_pot_yield_data("brly", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

banana_suit = get_gaez_suit_data("ban", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

banana_pot_yield = get_gaez_pot_yield_data("bana", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

## no irrigation suit maps, so use rainfed suit instead
cassava_suit = get_gaez_suit_data("csv", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,1), fun=max) %>% `[[`(c(1,1)) %>% setNames(c("irri_suit","rain_suit"))

cassava_pot_yield = get_gaez_pot_yield_data("casv", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

chickpea_suit = get_gaez_suit_data("chk", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

chickpea_pot_yield = get_gaez_pot_yield_data("chck", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

coconut_suit = get_gaez_suit_data("con", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

coconut_pot_yield = get_gaez_pot_yield_data("cocn", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

coffee_suit = get_gaez_suit_data("cof", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

coffee_pot_yield = get_gaez_pot_yield_data("coff", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

cotton_suit = get_gaez_suit_data("cot", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

cotton_pot_yield = get_gaez_pot_yield_data("cott", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

cowpea_suit = get_gaez_suit_data("cow", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

cowpea_pot_yield = get_gaez_pot_yield_data("cowp", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

finger_millet_suit = get_gaez_suit_data("fml", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

finger_millet_pot_yield = get_gaez_pot_yield_data("fmlt", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

groundnut_suit = get_gaez_suit_data("grd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

groundnut_pot_yield = get_gaez_pot_yield_data("grnd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

lentil_suit = get_gaez_suit_data("chk", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

lentil_pot_yield = get_gaez_pot_yield_data("chck", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

maize_suit = get_gaez_suit_data("mze", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

maize_pot_yield = get_gaez_pot_yield_data("maiz", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

other_cereals_suit = get_gaez_suit_data("oat", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

other_cereals_pot_yield = get_gaez_pot_yield_data("oats", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

other_fibre_crops_suit = get_gaez_suit_data("flx", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

other_fibre_crops_pot_yield = get_gaez_pot_yield_data("flax", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

other_oil_crops_suit = get_gaez_suit_data("olv", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

other_oil_crops_pot_yield = get_gaez_pot_yield_data("oliv", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

other_pulses_suit = get_gaez_suit_data("chk", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

other_pulses_pot_yield = get_gaez_pot_yield_data("chck", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

pearl_millet_suit = get_gaez_suit_data("pml", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

pearl_millet_pot_yield = get_gaez_pot_yield_data("pmlt", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

pigeonpea_suit = get_gaez_suit_data("pig", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

pigeonpea_pot_yield = get_gaez_pot_yield_data("pigp", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

potato_suit = get_gaez_suit_data("wpo", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

potato_pot_yield = get_gaez_pot_yield_data("wpot", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

rapeseed_suit = get_gaez_suit_data("rsd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

rapeseed_pot_yield = get_gaez_pot_yield_data("rape", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

rest_of_crops_suit = get_gaez_suit_data("mze", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

rest_of_crops_pot_yield = get_gaez_pot_yield_data("maiz", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

## rcw - irrigated, rcd - rainfed
rice_suit_w = get_gaez_suit_data("rcw", gaez_path) %>% `[`(1:2) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1), fun=max) %>% setNames(c("irri_suit"))

rice_suit_d = get_gaez_suit_data("rcd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,1), fun=max) %>% setNames(c("rain_suit"))

rice_suit = stack(list(rice_suit_w, rice_suit_d))

rice_pot_yield_w = get_gaez_pot_yield_data("ricw", gaez_path) %>% `[`(1:2) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1), fun=max) %>% setNames(c("irri_potyield"))

rice_pot_yield_d = get_gaez_pot_yield_data("ricd", gaez_path) %>% `[`(3:5) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,1), fun=max) %>% setNames(c("rain_potyield"))

rice_pot_yield = stack(list(rice_pot_yield_w, rice_pot_yield_d))

sesameseed_suit = get_gaez_suit_data("rsd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

sesameseed_pot_yield = get_gaez_pot_yield_data("rape", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

sorghum_suit = get_gaez_suit_data("srg", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

sorghum_pot_yield = get_gaez_pot_yield_data("sorg", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

soybean_suit = get_gaez_suit_data("soy", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

soybean_pot_yield = get_gaez_pot_yield_data("soyb", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

sugarcane_suit = get_gaez_suit_data("suc", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

sugarcane_pot_yield = get_gaez_pot_yield_data("sugc", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

sunflower_suit = get_gaez_suit_data("sfl", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

sunflower_pot_yield = get_gaez_pot_yield_data("sunf", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

sweet_potato_suit = get_gaez_suit_data("spo", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

sweet_potato_pot_yield = get_gaez_pot_yield_data("spot", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

teas_suit = get_gaez_suit_data("tea", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

teas_pot_yield = get_gaez_pot_yield_data("teas", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

temperate_fruit_suit = get_gaez_suit_data("mze", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

temperate_fruit_pot_yield = get_gaez_pot_yield_data("maiz", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

tobacco_suit = get_gaez_suit_data("tob", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

tobacco_pot_yield = get_gaez_pot_yield_data("toba", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

tropical_fruit_suit = get_gaez_suit_data("ban", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

tropical_fruit_pot_yield = get_gaez_pot_yield_data("bana", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

vegetables_suit = get_gaez_suit_data("oni", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

vegetables_pot_yield = get_gaez_pot_yield_data("onio", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

wheat_suit = get_gaez_suit_data("whe", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

wheat_pot_yield = get_gaez_pot_yield_data("whea", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

## as for cassava, there are no suitability maps for irrigated yams, so
## use rainfed instead
yams_suit = get_gaez_suit_data("yam", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,1), fun=max) %>% `[[`(c(1,1)) %>% setNames(c("irri_suit","rain_suit")) 

yams_pot_yield = get_gaez_pot_yield_data("rape", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_potyield","rain_potyield"))

gaez_suit = list(banana = banana_suit,
                 barley = barley_suit,
                 cassava = cassava_suit,
                 chickpea = chickpea_suit,
                 coconut = coconut_suit,
                 coffee = coffee_suit,
                 cotton = cotton_suit,
                 cowpea = cowpea_suit,
                 finger_millet = finger_millet_suit,
                 groundnut = groundnut_suit,
                 lentil = lentil_suit,
                 maize = maize_suit,
                 other_cereals = other_cereals_suit,
                 other_fibre_crops = other_fibre_crops_suit,
                 other_oil_crops = other_oil_crops_suit,
                 other_pulses = other_pulses_suit,
                 pearl_millet = pearl_millet_suit,
                 pigeonpea = pigeonpea_suit,
                 potato = potato_suit,
                 rapeseed = rapeseed_suit,
                 rest_of_crops = rest_of_crops_suit,
                 rice = rice_suit,
                 sesameseed = sesameseed_suit,
                 sorghum = sorghum_suit,
                 soybean = soybean_suit,
                 sugarcane = sugarcane_suit,
                 sunflower = sunflower_suit,
                 sweet_potato = sweet_potato_suit,
                 tea = teas_suit,
                 temperate_fruit = temperate_fruit_suit,
                 tobacco = tobacco_suit,
                 tropical_fruit = tropical_fruit_suit,
                 vegetables = vegetables_suit,
                 wheat = wheat_suit,
                 yams = yams_suit)

gaez_pot_yield = list(banana = banana_pot_yield,
                 barley = barley_pot_yield,
                 cassava = cassava_pot_yield,
                 chickpea = chickpea_pot_yield,
                 coconut = coconut_pot_yield,
                 coffee = coffee_pot_yield,
                 cotton = cotton_pot_yield,
                 cowpea = cowpea_pot_yield,
                 finger_millet = finger_millet_pot_yield,
                 groundnut = groundnut_pot_yield,
                 lentil = lentil_pot_yield,
                 maize = maize_pot_yield,
                 other_cereals = other_cereals_pot_yield,
                 other_fibre_crops = other_fibre_crops_pot_yield,
                 other_oil_crops = other_oil_crops_pot_yield,
                 other_pulses = other_pulses_pot_yield,
                 pearl_millet = pearl_millet_pot_yield,
                 pigeonpea = pigeonpea_pot_yield,
                 potato = potato_pot_yield,
                 rapeseed = rapeseed_pot_yield,
                 rest_of_crops = rest_of_crops_pot_yield,
                 rice = rice_pot_yield,
                 sesameseed = sesameseed_pot_yield,
                 sorghum = sorghum_pot_yield,
                 soybean = soybean_pot_yield,
                 sugarcane = sugarcane_pot_yield,
                 sunflower = sunflower_pot_yield,
                 sweet_potato = sweet_potato_pot_yield,
                 tea = teas_pot_yield,
                 temperate_fruit = temperate_fruit_pot_yield,
                 tobacco = tobacco_pot_yield,
                 tropical_fruit = tropical_fruit_pot_yield,
                 vegetables = vegetables_pot_yield,
                 wheat = wheat_pot_yield,
                 yams = yams_pot_yield)

## market access
r = raster("data/market-influence/mkt_access_5m/w001001.adf") %>% crop(india_ext)

## interpolate, because the raw map is missing data for some coastal
## regions that other input maps include

## IDW doesn't work, for some reason, so use nearest-neighbour instead

## library(gstat)
## spdf = as(r, "SpatialPointsDataFrame") %>% setNames("mkt_access")
## gs = gstat(formula=mkt_access~1, data=spdf)
## idw = interpolate(r, gs)
## idw[!is.na(r)] = r[!is.na(r)]
## idw[is.na(india_map)] = NA
## india_mkt_access = idw

nn = r
pass = 1
repeat {
    nn = focal(nn, w=matrix(data=1, nrow=5, ncol=5), fun=mean, na.rm=TRUE, pad=TRUE)
    if (!any(is.na(nn[india_map]))) {
        break
    }
    pass = pass + 1
}
    
nn[!is.na(r)] = r[!is.na(r)]
nn[is.na(india_map)] = NA
india_mkt_access = nn

## crop fraction
india_cropland_frac = raster("data/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m.tif") / 100
india_cropland_area = india_cropland_frac * area(india_cropland_frac) * 100 ## km2 -> hectare

## irrigated fraction
india_irri_frac = list.files("data/SCIENTIFIC_DATA_IRRIGATION_MAP_2000_2015", "^[0-9]{4}-[0-9]{4}_5m.tif$", full.names=TRUE) %>% sort %>% stack
india_irri_area = india_irri_frac * area(india_irri_frac) * 100 ## km2 -> hectare
india_irri_area[is.na(india_irri_area)] = 0

## GRIPC
proj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
gripc_irri_area = raster("data-raw/GRIPC/GRIPC_irrigated_area.asc") %>% `projection<-`(proj)
gripc_rain_area = raster("data-raw/GRIPC/GRIPC_rainfed_area.asc") %>% `projection<-`(proj)
gripc_pady_area = raster("data-raw/GRIPC/GRIPC_paddy_area.asc") %>% `projection<-`(proj)

print("Saving objects...")
save(x, template, gaez_suit, gaez_pot_yield, india_mkt_access, india_cropland_area, india_irri_area, gripc_irri_area, gripc_rain_area, gripc_pady_area, file="data/input_data.RData")
