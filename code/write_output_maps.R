## Author : Simon Moulds
## Date   : August 2017

library(magrittr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(raster)

load("data/input_data.RData")

years = x[["Year"]] %>% unique %>% sort
states = x[["State"]] %>% unique
dists = x[["ADM2_CODE"]] %>% unique 

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
    read_crop_data(dists[2], years[1], dir) %>%
    unname %>%
    do.call(cbind, .)

crops = grep("^(irr|rain).*$", names(tmp), value=TRUE)
    
for (i in 1:length(years)) {

    ## create raster maps to hold output
    maps = vector(mode="list", length=length(crops))
    for (k in 1:length(crops)) {
        fn = file.path("data","output_maps", paste0("INDIA_", toupper(gsub("-","_",crops[k])), "_", years[i], ".tif"))

        if (!file.exists(fn)) {        
            writeRaster(template,
                        filename=fn,
                        format="GTiff",
                        overwrite=TRUE)
        }
        maps[[k]] = raster(fn)
    }
    
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

        for (k in 1:length(crops)) {
            crop = crops[k]
            vals0 = maps[[j]][dist_frac_pts]
            vals1 = dist_tbl[,crop,drop=TRUE]
            vals2 = rowSums(data.frame(vals0, vals1), na.rm=TRUE)
            maps[[j]][dist_frac_pts] = vals2
        }
    }
}
