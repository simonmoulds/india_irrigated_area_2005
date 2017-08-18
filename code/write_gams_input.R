## Author : Simon Moulds
## Date   : August 2017

library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(raster)

## Use this script to write GAMS input files. This will involve:
## 1 - take irrigated/rainfed areas from
##     data/apy_indiastat_combined_data_qc.rds
## 2 - obtain prior estimates using MapSPAM, India irrigated area
##     maps and rice irrigated maps
## 3 - write GAMS script
## 4 - use web service to execute GAMS script, retrieve results and
##     add these to a raster map

x = readRDS("data/apy_indiastat_combined_data_qc.rds")

nms = names(x)

nms[nms == "irr_rice-summer"] = "irr_rice-rabi"
nms[nms == "irr_rice-winter"] = "irr_rice-kharif"
nms[nms == "irr_rice-autumn"] = "irr_rice-autumn"

nms[nms == "apy_rice-summer"] = "apy_rice-rabi"
nms[nms == "apy_rice-winter"] = "apy_rice-kharif"
nms[nms == "apy_rice-autumn"] = "apy_rice-autumn"

names(x) = nms

crop_nms = grep("^irr_", names(x), value=TRUE) %>% gsub("^irr_", "", .) %>% strsplit("-") %>% sapply(FUN=function(x) x[[1]]) %>% unique %>% sort

mapspam_crop_nms = list("BANA","BARL","CASS","CHIC","CNUT",c("ACOF","RCOF"),"COTT","COWP","PMIL","GROU","LENT","MAIZ","OCER","OFIB","OOIL","OPUL","PMIL","PIGE","POTA","RAPE","REST","RICE","SESA","SORG","SOYB","SUGC","SUNF","SWPO","TEAS","TEMF","TOBA","TROF","VEGE","WHEA","YAMS")

## TODO: check these correspond

seasons = c("kharif","rabi","summer")
years = x$Year %>% unique %>% sort
dists = x$ADM2_CODE %>% unique

## get names of crops in each season
kharif_crops = grep("-kharif$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-kharif$", "", .) %>% unique %>% sort

rabi_crops = grep("-rabi$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-rabi$", "", .) %>% unique %>% sort

summer_crops = grep("-summer$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-summer$", "", .) %>% unique %>% sort

whole_year_crops = grep("-whole_year$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-whole_year$", "", .) %>% unique %>% sort

n_kharif = length(kharif_crops)
n_rabi = length(rabi_crops)
n_summer = length(summer_crops)
n_whole_year = length(whole_year_crops)

whole_year_col_nms = paste0(c("irr_","rain_"), rep(whole_year_crops, each=2), "-whole_year")

kharif_col_nms = paste0(c("irr_","rain_"), rep(kharif_crops, each=2), "-kharif")

rabi_col_nms = paste0(c("irr_","rain_"), rep(rabi_crops, each=2), "-rabi")

summer_col_nms = paste0(c("irr_","rain_"), rep(summer_crops, each=2), "-summer")

all_col_nms = c(kharif_col_nms, rabi_col_nms, summer_col_nms, whole_year_col_nms) %>% unique

get_gaez_suit_data = function(crop, path, ...) {
    input_levels = c("h_suhi","i_suii","h_suhr","i_suir","l_sulr")
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

gaez_path = "data/GAEZ"
india_ext = extent(68,98,6,36)

barley_suit = get_gaez_suit_data("brl", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

banana_suit = get_gaez_suit_data("ban", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

## no irrigation suit maps, so use rainfed suit instead
cassava_suit = get_gaez_suit_data("csv", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,1), fun=max) %>% `[[`(c(1,1)) %>% setNames(c("irri_suit","rain_suit"))

chickpea_suit = get_gaez_suit_data("chk", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

coconut_suit = get_gaez_suit_data("con", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

coffee_suit = get_gaez_suit_data("cof", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

cotton_suit = get_gaez_suit_data("cot", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

cowpea_suit = get_gaez_suit_data("cow", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

finger_millet_suit = get_gaez_suit_data("fml", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

groundnut_suit = get_gaez_suit_data("grd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

lentil_suit = get_gaez_suit_data("chk", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

maize_suit = get_gaez_suit_data("mze", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

other_cereals_suit = get_gaez_suit_data("oat", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

other_fibre_crops_suit = get_gaez_suit_data("flx", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

other_oil_crops_suit = get_gaez_suit_data("olv", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

other_pulses_suit = get_gaez_suit_data("chk", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

pearl_millet_suit = get_gaez_suit_data("pml", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

pigeonpea_suit = get_gaez_suit_data("pig", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

potato_suit = get_gaez_suit_data("wpo", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

rapeseed_suit = get_gaez_suit_data("rsd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

rest_of_crops_suit = get_gaez_suit_data("mze", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

## rcw - irrigated, rcd - rainfed
rice_suit_w = get_gaez_suit_data("rcw", gaez_path) %>% `[`(1:2) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1), fun=max) %>% setNames(c("irri_suit"))

rice_suit_d = get_gaez_suit_data("rcd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,1), fun=max) %>% setNames(c("rain_suit"))

rice_suit = stack(list(rice_suit_w, rice_suit_d))

sesameseed_suit = get_gaez_suit_data("rsd", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))
    
sorghum_suit = get_gaez_suit_data("srg", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

soybean_suit = get_gaez_suit_data("soy", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

sugarcane_suit = get_gaez_suit_data("suc", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

sunflower_suit = get_gaez_suit_data("sfl", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

sweet_potato_suit = get_gaez_suit_data("spo", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

teas_suit = get_gaez_suit_data("tea", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

temperate_fruit_suit = get_gaez_suit_data("mze", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

tobacco_suit = get_gaez_suit_data("tob", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

tropical_fruit_suit = get_gaez_suit_data("ban", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

vegetables_suit = get_gaez_suit_data("oni", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

wheat_suit = get_gaez_suit_data("whe", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,2,2,2), fun=max) %>% setNames(c("irri_suit","rain_suit"))

## as for cassava, there are no suitability maps for irrigated yams, so
## use rainfed instead
yams_suit = get_gaez_suit_data("yam", gaez_path) %>% stack %>% crop(india_ext) %>% stackApply(indices=c(1,1,1), fun=max) %>% `[[`(c(1,1)) %>% setNames(c("irri_suit","rain_suit")) 

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

stop()

india_crop_frac = raster("data/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m.tif") / 100
india_crop_area = india_crop_frac * area(india_crop_frac)

india_irri_frac = list.files("data/SCIENTIFIC_DATA_IRRIGATION_MAP_2000_2015", "^[0-9]{4}-[0-9]{4}_5m.tif$", full.names=TRUE) %>% sort %>% stack
india_irri_area = india_irri_frac * area(india_irri_frac)

proj = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
gripc_irri_area = raster("data-raw/GRIPC/GRIPC_irrigated_area.asc") %>% `projection<-`(proj)
gripc_rain_area = raster("data-raw/GRIPC/GRIPC_rainfed_area.asc") %>% `projection<-`(proj)
gripc_pady_area = raster("data-raw/GRIPC/GRIPC_paddy_area.asc") %>% `projection<-`(proj)

for (i in 1:length(years)) {
    for (j in 1:length(dists)) {

        xx = x[x$Year %in% years[i] & x$ADM2_CODE %in% dists[j],,drop=FALSE]

        ## get district frac (data/district_frac)
        dist_frac_map = raster(file.path("data", "district_frac", paste0("dist_", dists[j], "_frac1ll.tif")))
        dist_frac_pts = as(dist_frac_map, "SpatialPoints")
        dist_frac_val = dist_frac_map[dist_frac_pts]
        n_cell = length(dist_frac_pts)

        ## TODO: get crop/irrigated area
        ## TODO: get rice area (GRIPC)
        
        dist_tbl = as.data.frame(matrix(data=0, nrow=n_cell, ncol=length(all_col_nms))) %>% setNames(all_col_nms)

        for (k in 1:length(all_col_nms)) {
            nm = all_col_nms[k]
            dist_total = xx[[nm]][1]

            type = sub("^([a-z]+)_(.*)$", "\\1", nm)
            crop_nm = sub("^([a-z]+)_(.*)-(.*)$", "\\2", nm)

            if (isTRUE(grepl("^irr_", nm))) {
                is_irrigated = TRUE;
            } else if (isTRU(grepl("^rain_", nm))) {
                is_rainfed = TRUE; 
            } else {
                stop("Error getting water supply type")
            }
            
            if (dist_total > 0) {

                ## TODO: get GAEZ code for crop name (lookup table?)
                
                irri_area = india_irri_area[[i]][dist_frac_pts] ## km2
                crop_area = india_crop_area[dist_frac_pts]

                if (is_irrigated) {
                    suit = gaez_suit[[crop_nm]][["irri_suit"]][dist_frac_pts]
                    ## query: how is the GAEZ suitability value calculated

                    suit[suit < 0] = 0       ## this gets rid of -ve nos
                    suit[irri_area == 0] = 0 
                    
                } else {
                    suit = gaez_suit[[crop_nm]][["rain_suit"]][dist_frac_pts]
                }

                tsuit = suit
                

                crop_area[crop_area < irri_area] = irri_area[crop_area < irri_area]


                if (isTRUE(all.equal(crop_nm, "rice"))) {

                    if 
                    rice_irri_area = gripc_irri_area[dist_frac_pts]
                    rice_rain_area = gripc_rain_area[dist_frac_pts]
                    rice_pady_area = gripc_pady_area[dist_frac_pts]

                }

                prob = tsuit / sum(tsuit)
                prior_estimate = dist_total * prob
                
                
                ## now calculate prior estimate - see how they do this
                ## in MapSPAM

                ## 1. for rice - use GRIPC
                ## 2. for wheat and sugarcane - 'boost' probability by
                ##    adding area from GIAM
                ## 3. otherwise, scale according to suitability score
                ##    from GAEZ

                ## constraints:
                ##    irrigated area
                ##    total cropland area
                
                
            }
        }

        ## ## now divide into kharif, rabi, summer, whole_year
        ## kharif_dist_tbl  
        ## rabi_dist_tbl    
        ## summer_dist_tbl 

        ## write GAMS input file (write function)        
        
    }
}
                 
write_gams_input = function(x, row_target, col_target, fn, ...) {

    n_cell = nrow(x)
    n_crop = ncol(x)

    rownms = paste0("cell", seq_len(n_cell))
    colnms = paste0("crop", seq_len(n_crop))

    set_I  = paste0(c(rownms, "CTOTS"), collapse=", ")
    set_I2 = paste0(rownms, collapse=", ")
    set_J  = paste0(c(colnms, "COLNCT", "RTOTS"), collapse=", ")
    set_J2 = paste0(colnms, collapse=", ")
    
    header = readLines("code/GPCEMA_header.txt")
    model  = readLines("code/GPCEMA_model.txt")

    ## open file
    con = file(fn, "w")

    cat("<document>\n", file=con)
    cat("<category>milp</category>\n", file=con)
    cat("<solver>CPLEX</solver>\n", file=con)
    cat("<inputType>GAMS</inputType>\n", file=con)
    cat("<client>Mozilla/5.0 (X11; Ubuntu; Linux x86 <- 64; rv:54.0) Gecko/20100101 Firefox/54.0@144.173.225.124</client>\n", file=con)
    cat("<priority>long</priority>\n", file=con)
    cat("<email>sim.moulds@gmail.com</email>\n", file=con)
    cat("<model><![CDATA[\n", file=con
    
    ## write model header
    writeLines(header, con)

    ## write sets, data
    cat("\n\nSET I Row labels", "/", set_I, "/;\n", file=con)
    cat("SET I2 Row labels without CTOTS", "/", set_I2, "/;\n", file=con)
    cat("SET J Column labels", "/", set_J, "/;\n", file=con)
    cat("SET J2 Column labels without RTOTS", "/", set_J2, "/;\n", file=con)
    cat("\n")
    cat("ALIAS (I2,I2J);")
    cat("ALIAS (J2,J2J);")
    cat("\n")
    cat("TABLE\n")
    cat("    DATA(I,J)\n")
    cat(formatC(c("", colnms, "RTOTS"), width=10), "\n")
    for (i in 1:n_cell) {
        cat(formatC(c(rownms[i], x[i,,drop=TRUE], row_target[i]), width=10), "\n")
    }
    cat(";\n\n")

    ## add model code and close the connection
    writeLines(model, con)

    ## finish XML, then close file
    cat("]]></model>\n", file=con)
    cat("<options><![CDATA[]]></options>\n", file=con)
    cat("<gdx><![CDATA[]]></gdx>\n", file=con)
    cat("<comments><![CDATA[]]></comments>\n", file=con)
    cat("</document>\n", file=con)
    close(con)
}
