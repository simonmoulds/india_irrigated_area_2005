## Author : Simon Moulds
## Date   : June 2017

library(raster)
library(magrittr)
library(tidyr)
library(dplyr)
library(rgdal)
options(stringsAsFactors = FALSE)

## ======================================
## global cropland map - using IIASA-IFPRI map
## (http://geo-wiki.org/downloads/)
## ======================================

lulc_path = "data/iiasa-ifpri-cropland-map"
if (!dir.exists(lulc_path)) {
    dir.create(lulc_path)
}

crop_area_2005 = raster("data/iiasa-ifpri-cropland-map/Hybrid_10042015v9.img")

ext = extent(crop_area_2005)
new_ext = extent(-180,180,ext@ymin,ext@ymax)

crop_area_2005 = crop(crop_area_2005, new_ext)
crop_area_2005 = raster::aggregate(crop_area_2005, fact=10, fun=mean)

writeRaster(crop_area_2005, "data/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m.tif", format="GTiff", overwrite=TRUE)
