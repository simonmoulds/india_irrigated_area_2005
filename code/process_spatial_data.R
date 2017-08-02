## Author  : Simon Moulds
## Date    : Aug 2015 - Feb 2016
## Version : 0.1
## Licence : GPL v3

library(magrittr)
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(gdalUtils)

## Here we generate a template raster image with the correct extent and
## resolution for India

aea.crs = "+proj=aea +lat_1=28 +lat_2=12 +lat_0=20 +lon_0=78 +x_0=2000000 +y_0=2000000 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

ogr2ogr(src_datasource_name = "data-raw/india_adm2_2001/data/g2008_2_India.shp",
        dst_datasource_name = "data/g2008_2_India_aea.shp",
        t_srs=aea.crs,
        overwrite=TRUE,
        verbose=TRUE)

adm2_ll = readOGR("data-raw/india_adm2_2001/data", layer="g2008_2_India")
adm2_aea = readOGR("data", layer="g2008_2_India_aea")

## neighbouring countries
system("unzip -o data-raw/FAO_GAUL_boundary_levels.zip -d data")

ogr2ogr(src_datasource_name = "data/g2008_0.shp",
        dst_datasource_name = "data/g2008_0_aea.shp",
        t_srs=aea.crs,
        overwrite=TRUE,
        verbose=TRUE)

india_nb_ll = readOGR("data", layer="g2008_0")
india_nb_aea = readOGR("data", layer="g2008_0_aea")

## India bounding box (lat-long)
east  <- 98  
west  <- 68
north <- 38
south <- 6
nr <- (north - south) / (1/12) ## 1/12 degree resolution
nc <- (east - west) / (1/12)   

template <- raster(nrow=nr,
                   ncol=nc,
                   xmn=west,
                   xmx=east,
                   ymn=south,
                   ymx=north)

template <- setValues(template, values=rep(1,ncell(template)))

if (file.exists(file.path("data", "template_ll.tif")))
    file.remove(file.path("data", "template_ll.tif"))

if (file.exists(file.path("data", "template_aea.tif")))
    file.remove(file.path("data", "template_aea.tif"))

writeRaster(template,
            file.path("data", "template_ll.tif"),
            format="GTiff",
            overwrite=TRUE)

gdalwarp(srcfile=file.path("data", "template_ll.tif"),
         dstfile=file.path("data", "template_aea.tif"),
         t_srs=aea.crs,
         r="bilinear",
         overwrite=TRUE,
         verbose=TRUE,
         output_Raster=FALSE)

template_coarse_aea = raster(file.path("data", "template_aea.tif"))
template_fine_aea = disaggregate(template_coarse_aea, fact=10)

template_coarse_ll = template
template_fine_ll = disaggregate(template_coarse_ll, fact=10)

## ======================================
## 1. rasterize district vector map (latlong)
## ======================================

dist_map = rasterize(adm2_ll, template_coarse_ll, "ADM2_CODE")
state_map = rasterize(adm2_ll, template_coarse_ll, "ADM1_CODE")
rgn_map = raster(state_map)

## regions based on Aquastat irrigation calendar
india_north_code = c(1492,1493,40781,1505,70081,70074,1489,70082)
india_east_code = c(15,1487,70073,70078,1504,1511,1500,1501,1502,1503,1507,1509)
india_south_code = c(1485,1494,1495,70080,1508)
india_west_code = c(1491,70079,1498,1506,1490,70077,70076,70075)

rgn_map[state_map %in% india_north_code] = 1
rgn_map[state_map %in% india_east_code]  = 2
rgn_map[state_map %in% india_south_code] = 3
rgn_map[state_map %in% india_west_code]  = 4

writeRaster(dist_map, "data/g2008_2_India_rast.tif", format="GTiff", overwrite=TRUE)
writeRaster(state_map, "data/g2008_1_India_rast.tif", format="GTiff", overwrite=TRUE)
writeRaster(rgn_map, "data/g2008_rgn_India_rast.tif", format="GTiff", overwrite=TRUE)

## ======================================
## 2. district fraction maps
## ======================================

## create directory to store output
if (!dir.exists("data/district_frac")) {
    dir.create("data/district_frac")
}

myfun = function(adm2, nb, template_coarse, template_fine, path, suffix, ...) {

    ## This section of code rasterizes each polygon in the modified
    ## administrative area map
    district_coarse = rasterize(adm2, template_coarse, field="ADM2_CODE")
    district_fine = rasterize(adm2, template_fine, field="ADM2_CODE")
    nb_coarse = rasterize(nb, template_coarse, field="ADM0_CODE")
    nb_fine = rasterize(nb, template_fine, field="ADM0_CODE")

    ## get unique districts in study region
    dists <- sort(unique(getValues(district_fine)))

    for (i in 1:length(dists)) {    
        dist <- dists[i]
        tmp1 <- district_fine 
        tmp1[(!is.na(tmp1) & tmp1 != dist)] <- 0

        ## zoom region to current district
        tmp2 = district_coarse
        xy = as.data.frame(xyFromCell(object=tmp1, cell=which(getValues(tmp1) %in% dist)))
        ext <- extent(min(xy$x), max(xy$x), min(xy$y), max(xy$y))
        tmp2 <- crop(tmp2, ext, snap="out")
        ext <- extent(extend(tmp2, y=2))

        tmp3 <- tmp1

        ## crop fine resolution district map to current region
        tmp1[(is.na(tmp1) & (!is.na(nb_fine)))] <- 0
        tmp1[tmp1 == dist] <- 1
        tmp1 <- crop(tmp1, ext)
        tmp1 <- aggregate(tmp1, fact=10, fun=mean, na.rm=TRUE)
        tmp1[tmp1 == 0] <- NA

        ## "*_frac1_*.tif" : coast cells set to 1, land border cells
        ## indicate fraction belonging to India
        fn1 = paste0("dist_", dist, "_frac1", suffix, ".tif")
        tmp1 <- writeRaster(tmp1, file.path(path, fn1), format="GTiff", overwrite=TRUE)

        tmp3[tmp3 == dist] <- 1
        tmp3 <- crop(tmp3, ext)
        tmp3 <- aggregate(tmp3, fact=10, fun=mean, na.rm=TRUE)
        tmp3[tmp3 == 0] <- NA

        ## "*_frac2_*.tif" : all border cells (coast, land) set to 1
        fn2 = paste0("dist_", dist, "_frac2", suffix, ".tif")
        tmp3 <- writeRaster(tmp3, file.path(path, fn2), format="GTiff", overwrite=TRUE) 
    }
}

## a. Albers Equal Area projection
myfun(adm2_aea, nb_india_aea, template_coarse_aea, template_fine_aea, path="data/district_frac", suffix="aea")

## b. Latitude-longitude
myfun(adm2_ll, nb_india_ll, template_coarse_ll, template_fine_ll, path="data/district_frac", suffix="ll")

## ======================================
## MapSPAM
## ======================================

## use mapspam to get irrigated fraction for crops where this is not
## available from agricultural inventory data

if (!dir.exists("data/mapspam")) {
    dir.create("data/mapspam")
}

system("unzip -o data-raw/MapSPAM/spam2005V3r1_global_harv_area.geotiff.zip -d data/mapspam")

fs = list.files("data/mapspam", "SPAM2005V3r1_global_H_T(A|I|R)_[A-Z]{4}_(A|I|R).tif$", full.names=TRUE)

for (i in 1:length(fs)) {
    f = fs[i]
    r = raster(f)
    r = crop(r, extent(template_coarse_ll))
    ext = alignExtent(extent(r), template)
    r = setExtent(r, ext, keepres=TRUE)
    aea_nm = paste0(sub("^([^.]*).*", "\\1", f), "_India_aea.tif")
    ll_nm = paste0(sub("^([^.]*).*", "\\1", f), "_India_ll.tif")
    writeRaster(r, ll_nm, format="GTiff", overwrite=TRUE, NAflag=-1)
    ## gdalwarp(srcfile=file.path(ll_nm),
    ##          dstfile=file.path(aea_nm),
    ##          t_srs=aea.crs,
    ##          r="bilinear",
    ##          overwrite=TRUE,
    ##          verbose=TRUE,
    ##          output_Raster=FALSE)
}

## ## ======================================
## ## Siebert et al. (2015) maps
## ## ======================================

## fs <- paste0("AEI_HYDE_FINAL_IR_",
##              c(1950,1960,1970,1980,1985,1990,1995,2000,2005),
##              ".asc")

## for (i in 1:length(fs)) {

##     f <- fs[i]
##     r <- raster(file.path("data", "original", "siebert2015", f))
##     crs(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
##     r <- crop(r, extent(template))
##     ext <- alignExtent(extent(r), template)
##     r   <- setExtent(r, ext, keepres=TRUE)
    
##     aea.nm <- file.path(spatial_path, paste0(sub("^([^.]*).*", "\\1", basename(f)), "_India_aea.tif"))
##     ll.nm <- file.path(spatial_path, paste0(sub("^([^.]*).*", "\\1", basename(f)), "_India_ll.tif"))

##     writeRaster(r, ll.nm, format="GTiff", overwrite=TRUE, NAflag=-1)

##     gdalwarp(srcfile=file.path(ll.nm),
##              dstfile=file.path(aea.nm),
##              t_srs=aea.crs,
##              r="bilinear",
##              overwrite=TRUE,
##              verbose=TRUE,
##              output_Raster=FALSE)
    
## }

## yrs <- 1950:2010
## st <- stack(lapply(seq_len(length(yrs)), FUN=function(x)
##     setValues(template_coarse, values=NA)))

## for (i in 1:length(yrs)) {

##     f <- file.path(spatial_path, paste0("AEI_HYDE_FINAL_IR_", yrs[i], "_India_aea.tif"))
    
##     if (file.exists(file.path(spatial_path, paste0("AEI_HYDE_FINAL_IR_", yrs[i], "_India_aea.tif")))) {
##         st[[i]] <- raster(f)
##     }
## }

## st <- approxNA(st, rule=2)

## for (i in 1:length(yrs)) {
##     f <- file.path(spatial_path,
##                    paste0("AEI_HYDE_FINAL_IR_",
##                    yrs[i], "_interp_India_aea.tif"))

##     writeRaster(x=st[[i]], filename=f, overwrite=TRUE)
## }

## ## ======================================
## ## FAO crop suitability
## ## ======================================

## ## GAEZ crop suitability: use high input
## fs <- list.files(file.path("data", "original", "FAO_suitability"), pattern="^.*suhi.*\\.zip", full.names=TRUE)
## for (i in 1:length(fs)) {

##     f <- fs[i]; print(f)
##     d <- file.path(spatial_path, sub("^([^.]*).*", "\\1", basename(f)))
##     unzip(f, exdir=d)

##     f <- list.files(d, pattern="^[^.]*.tif", full.names=TRUE)
##     if (length(f) != 1) {
##         stop()
##     }
    
##     r <- raster(f)
##     r <- crop(r, extent(template))
##     ext <- alignExtent(extent(r), template)
##     r   <- setExtent(r, ext, keepres=TRUE)

##     ## r[r == -1] <- 0  ## set all NA values to 0
##     r[r == -1] <- NA

##     aea.nm <- file.path(spatial_path, paste0(sub("^([^.]*).*", "\\1", basename(f)), "_India_aea.tif"))
##     ll.nm <- file.path(spatial_path, paste0(sub("^([^.]*).*", "\\1", basename(f)), "_India_ll.tif"))

##     writeRaster(r, ll.nm, format="GTiff", overwrite=TRUE, NAflag=-1)

##     gdalwarp(srcfile=file.path(ll.nm),
##              dstfile=file.path(aea.nm),
##              t_srs=aea.crs,
##              r="bilinear",
##              overwrite=TRUE,
##              verbose=TRUE,
##              output_Raster=FALSE)
    
## }

## ## ======================================
## ## GRIPC
## ## ======================================

## fs <- list.files(file.path("data", "original", "GRIPC"), pattern="^.*_area.tif", full.names=TRUE)
## for (i in 1:length(fs)) {
##     f <- fs[i]
##     r <- raster(f)
##     crs(r) <- "+proj=longlat +datum=WGS84 +ellps=WGS84"
##     r <- crop(r, extent(template))
##     ext <- alignExtent(extent(r), template)
##     r   <- setExtent(r, ext, keepres=TRUE)
    
##     aea.nm <- file.path(spatial_path, paste0(sub("^([^.]*).*", "\\1", basename(f)), "_India_aea.tif"))
##     ll.nm <- file.path(spatial_path, paste0(sub("^([^.]*).*", "\\1", basename(f)), "_India_ll.tif"))

##     writeRaster(r, ll.nm, format="GTiff", overwrite=TRUE, NAflag=-1)

##     gdalwarp(srcfile=file.path(ll.nm),
##              dstfile=file.path(aea.nm),
##              t_srs=aea.crs,
##              r="bilinear",
##              overwrite=TRUE,
##              verbose=TRUE,
##              output_Raster=FALSE)
## }
