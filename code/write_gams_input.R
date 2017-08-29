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

## load input data
source("code/allocation_helper_funs.R")
## source("code/load_input_data.R")
load("data/input_data.RData")

crops = names(x)[-(1:5)]
years = x[["Year"]] %>% unique %>% sort
states = x[["State"]] %>% unique

## for testing only:
years = 2000
states = "Andhra Pradesh"

dir = file.path("data", "gams")
if (!dir.exists(dir)) {
    dir.create(dir)
}

## write input files
for (i in 1:length(years)) {
    for (j in 1:length(states)) {
        dists = x$ADM2_CODE[x$State %in% states[j]] %>% unique
        for (k in 1:length(dists)) {

            xx = x[x$Year %in% years[i] & x$ADM2_CODE %in% dists[k],]

            ## get district frac (data/district_frac)
            dist_frac_map = raster(file.path("data", "district_frac", paste0("dist_", dists[k], "_frac1_ll.tif")))
            dist_frac_pts = as(dist_frac_map, "SpatialPoints")
            dist_frac_val = dist_frac_map[dist_frac_pts]
            n_cell = length(dist_frac_pts)

            dist_tbl = as.data.frame(matrix(data=0, nrow=n_cell, ncol=length(crops))) %>% setNames(crops)

            mkt_access = india_mkt_access[dist_frac_pts]

            for (m in 1:length(crops)) {

                nm = crops[m]
                dist_total = xx[[nm]][1]

                type = sub("^([a-z]+)_(.*)$", "\\1", nm)
                crop_nm = sub("^([a-z]+)_(.*)-(.*)$", "\\2", nm)

                if (isTRUE(grepl("^irr_", nm))) {
                    is_irrigated = TRUE
                } else if (isTRUE(grepl("^rain_", nm))) {
                    is_irrigated = FALSE
                } else {
                    stop("Error getting water supply type")
                }

                if (dist_total > 0) {

                    irri_area = india_irri_area[[i]][dist_frac_pts] 
                    cropland_area = india_cropland_area[dist_frac_pts]

                    ## in cells where crop area is less than irrigated area,
                    ## set crop area to irrigated area
                    ix = cropland_area < irri_area
                    cropland_area[ix] = irri_area[ix]

                    irri_area = irri_area * dist_frac_val
                    cropland_area = cropland_area * dist_frac_val

                    ## get suitability and potential yield for the crop
                    if (is_irrigated) {
                        suit = gaez_suit[[crop_nm]][["irri_suit"]][dist_frac_pts] * (irri_area > 0) * (cropland_area > 0)
                        pot_yield = gaez_pot_yield[[crop_nm]][["irri_potyield"]][dist_frac_pts]
                    } else {
                        suit = gaez_suit[[crop_nm]][["rain_suit"]][dist_frac_pts] * (cropland_area > 0)
                        pot_yield = gaez_pot_yield[[crop_nm]][["rain_potyield"]][dist_frac_pts]
                    }

                    suit[is.na(suit)] = 0
                    pot_yield[is.na(pot_yield)] = 0
                    
                    ## use arbitrary price because I don't think it makes a
                    ## difference
                    suit = as.numeric(suit %in% 1:7)

                    n_suit = length(which(suit > 0))
                    n_pot_yield = length(which(pot_yield > 0))

                    if (n_suit == 0) {
                        if (is_irrigated) {
                            suit[irri_area > 0 & cropland_area > 0] = 1
                        } else {
                            suit[cropland_area > 0] = 1
                        }
                    }

                    if (n_pot_yield == 0) {
                        if (is_irrigated) {
                            pot_yield[irri_area > 0 & cropland_area > 0] = 1
                        } else {
                            pot_yield[cropland_area > 0] = 1
                        }
                    }

                    price = 1
                    revenue = price * mkt_access * pot_yield * suit
                    if (any(is.na(revenue))) {
                        stop("revenue cannot contain NA")
                    }

                    ## if (isTRUE(all.equal(crop_nm, "rice"))) {
                    ##     rice_irri = gripc_irri_area[dist_frac_pts]
                    ##     rice_rain = gripc_rain_area[dist_frac_pts]
                    ##     rice_pady = gripc_pady_area[dist_frac_pts]
                    ## }

                    ## now calculate prior estimate of area shares
                    area = dist_total * revenue / sum(revenue)
                    pi = area / sum(area)

                    dist_tbl[[nm]] = pi * dist_total

                }
            }

            ## now divide into kharif, rabi, summer, whole_year
            kharif_cols = grep("^.*-kharif$", crops)
            rabi_cols = grep("^.*-rabi$", crops)
            summer_cols = grep("^.*-summer$", crops)
            whole_year_cols = grep("^.*-whole_year$", crops)

            ## this variable contains the two constraints
            kharif_dist_tbl = dist_tbl[,kharif_cols]
            rabi_dist_tbl = dist_tbl[,rabi_cols]
            summer_dist_tbl = dist_tbl[,summer_cols]
            whole_year_dist_tbl = dist_tbl[,whole_year_cols]

            ## kharif and whole year crops

            ## include crops grown over the whole year in the optimisation
            kharif_dist_tbl %<>% cbind(whole_year_dist_tbl)
            
            flag = TRUE
            kharif_dist_tbl2 = try(allocate_fun(kharif_dist_tbl, dists[k], years[i], season="kharif", cropland_area=cropland_area, irri_area=irri_area, dir=dir))

            if (!isTRUE(all.equal(dim(kharif_dist_tbl), dim(kharif_dist_tbl2)))) {
                flag = FALSE
            }
            
            ## flag indicates whether allocation was successful
            if (flag) {
                whole_year_dist_tbl2 =
                    kharif_dist_tbl2 %>%
                    dplyr::select(grep("^.*-whole_year$", names(.)))

                kharif_dist_tbl2 =
                    kharif_dist_tbl2 %>%
                    dplyr::select(grep("^.*-kharif$", names(.)))

                ## for the remaining seasons we need to subtract the
                ## total of whole year crops from the row targets
                whole_year_cropland_area = rowSums(whole_year_dist_tbl2)
                whole_year_irri_cols = grepl("^irr_.*$", names(whole_year_dist_tbl))
                whole_year_irri_area = rowSums(whole_year_dist_tbl2[,whole_year_irri_cols])

                ## recalculate cropland and irrigated area, taking
                ## into account the area allocated to crops grown
                ## over the full year
                cropland_area2 = cropland_area - whole_year_cropland_area
                irri_area2 = irri_area - whole_year_irri_area
            }

            ## rabi crops
            
            if (flag) {
                rabi_dist_tbl2 = try(allocate_fun(rabi_dist_tbl,
                                                  dists[k],
                                                  years[i],
                                                  season="rabi",
                                                  cropland_area=cropland_area2,
                                                  irri_area=irri_area2,
                                                  dir=dir))
                if (!isTRUE(all.equal(dim(rabi_dist_tbl), dim(rabi_dist_tbl2)))) {
                    flag = FALSE
                }
            }

            ## summer crops
            
            if (flag) {
                summer_dist_tbl2 = try(allocate_fun(summer_dist_tbl,
                                                  dists[k],
                                                  years[i],
                                                  season="summer",
                                                  cropland_area=cropland_area2,
                                                  irri_area=irri_area2,
                                                  dir=dir))
                if (inherits(summer_dist_tbl2, "try-error")) {
                    warning()
                    flag = FALSE
                }
            }

            if (flag) {

                saveRDS(whole_year_dist_tbl2, file.path(dir, paste0("gams_output_", dists[k], "_", years[i], "_whole_year.rds")))
                saveRDS(kharif_dist_tbl2, file.path(dir, paste0("gams_output_", dists[k], "_", years[i], "_kharif.rds")))
                saveRDS(rabi_dist_tbl2, file.path(dir, paste0("gams_output_", dists[k], "_", years[i], "_rabi.rds")))
                saveRDS(summer_dist_tbl2, file.path(dir, paste0("gams_output_", dists[k], "_", years[i], "_summer.rds")))
                
            } else {
                warning()
            }
        }
    }
}

    
            
