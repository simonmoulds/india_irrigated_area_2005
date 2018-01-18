## Author : Simon Moulds
## Date   : August 2017

library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(raster)

load("data/input_data.RData")

## years = x[["Year"]] %>% unique %>% sort
## states = x[["State"]] %>% unique
## dists = x[["ADM2_CODE"]] %>% unique 

fs = list.files("data/gams", pattern="gams_output")
years = sapply(strsplit(fs, "_"), FUN=function(x) x[4]) %>% unique %>% as.numeric
dists = sapply(strsplit(fs, "_"), FUN=function(x) x[3]) %>% unique %>% as.numeric

## create directory for output maps, if it does not already exist
if (!dir.exists("data/output_maps")) {
    dir.create("data/output_maps")
}

read_crop_data = function(dist, year, path, ...) {
    list(kharif = readRDS(file.path(path, paste0("gams_output_", dist, "_", year, "_kharif.rds"))),
         rabi = readRDS(file.path(path, paste0("gams_output_", dist, "_", year, "_rabi.rds"))),
         summer = readRDS(file.path(path, paste0("gams_output_", dist, "_", year, "_summer.rds"))),
         whole_year = readRDS(file.path(path, paste0("gams_output_", dist, "_", year, "_whole_year.rds"))))
}

## get crop names
tmp =
    read_crop_data(dists[1], years[1], dir) %>%
    unname %>%
    do.call(cbind, .)

crops = grep("^(irr|rain).*$", names(tmp), value=TRUE)
    
for (i in 1:length(years)) {

    for (k in 1:length(crops)) {

        map = template
        map[] = 0
        
        for (j in 1:length(dists)) {

            ## get district frac (data/district_frac)
            dist_frac_map = raster(file.path("data", "district_frac", paste0("dist_", dists[j], "_frac1_ll.tif")))
            dist_frac_pts = as(dist_frac_map, "SpatialPoints")

            dist_tbl =
                read_crop_data(dists[j], years[i], dir) %>%
                unname %>%
                do.call(cbind, .)

            if (!all(names(dist_tbl) %in% crops)) {
                stop()
            }

            vals0 = map[dist_frac_pts]
            vals1 = dist_tbl[,crops[k],drop=TRUE]
            vals2 = rowSums(data.frame(vals0, vals1), na.rm=TRUE)
            map[dist_frac_pts] = vals2
        }

        fn = file.path("data","output_maps", paste0("INDIA_", toupper(gsub("-","_",crops[k])), "_", years[i], ".tif"))
        writeRaster(map, filename=fn, format="GTiff", overwrite=TRUE)
    }
}
