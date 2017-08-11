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
kharif_crops = grep("kharif$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-kharif$", "", .) %>% unique %>% sort

rabi_crops = grep("rabi$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-rabi$", "", .) %>% unique %>% sort

summer_crops = grep("summer$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-summer$", "", .) %>% unique %>% sort

whole_year_crops = grep("whole_year$", names(x), value=TRUE) %>% gsub("^[a-z]{3}_", "", .) %>% gsub("-whole_year$", "", .) %>% unique %>% sort

n_kharif = length(kharif_crops)
n_rabi = length(rabi_crops)
n_summer = length(summer_crops)
n_whole_year = length(whole_year_crops)

kharif_col_nms = paste0(c("irr_","rain_"), rep(kharif_crops, each=2), "-kharif")

rabi_col_nms = paste0(c("irr_","rain_"), rep(rabi_crops, each=2), "-rabi")

summer_col_nms = paste0(c("irr_","rain_"), rep(summer_crops, each=2), "-summer")

whole_year_col_nms = paste0(c("irr_","rain_"), rep(whole_year_crops, each=2), "-whole_year")

for (i in 1:length(years)) {
    for (j in 1:length(dists)) {

        ## get district frac (data/district_frac)
        dist_frac_map = raster(file.path("data", "district_frac", paste0("dist_", dists[j], "_frac1ll.tif")))
        dist_frac_pts = as(dist_frac_map, "SpatialPoints")
        dist_frac_val = dist_frac_map[dist_frac_pts]
        n_cell = length(dist_frac_pts)

        kharif_tbl = as.data.frame(matrix(data=NA, nrow=n_cell, ncol=n_kharif * 2 + n_whole_year * 2)) %>% setNames(c(kharif_col_nms, whole_year_nms))
        rabi_tbl = as.data.frame(matrix(data=NA, nrow=n_cell, ncol=n_rabi * 2 + n_whole_year * 2)) %>% setNames(c(rabi_col_nms, whole_year_nms))
        
        summer_tbl = as.data.frame(matrix(data=NA, nrow=n_cell, ncol=n_summer * 2 + n_whole_year * 2)) %>% setNames(c(summer_col_nms, whole_year_nms))
        
        for (k in 1:length(crop_nms)) {
            irr_kharif_nm = paste0("irr_", crop_nms[k], "-kharif")
            apy_kharif_nm = paste0("irr_", crop_nms[k], "-kharif")

            ## get biophysical suitability (data/mapspam_data)
            ## get crop area
            ## get irrigated area
            ## get rice area
            
        }
    }
}


