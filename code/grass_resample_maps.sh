#!/bin/bash

# =======================================
# India irrigated area
# =======================================

if [ ! -d data/SCIENTIFIC_DATA_IRRIGATION_MAP_2000_2015 ]
then
    unrar data-raw/SCIENTIFIC_DATA_IRRIGATION_MAP_2000_2015.rar data
    # rm -r data/SCIENTIFIC_DATA_IRRIGATION_MAP_2000_2015
fi

ls data/SCIENTIFIC_DATA_IRRIGATION_MAP_2000_2015 | grep '[0-9]\{4\}-[0-9]\{4\}.tif$' > tmp

echo 'export GRASS_MESSAGE_FORMAT=plain
datadir=data/SCIENTIFIC_DATA_IRRIGATION_MAP_2000_2015
g.region e=98E w=68E n=36N s=6N res=0:05:00
g.region -p
while read line
do
    basenm=${line%%.*}
    echo $basenm
    r.in.gdal input="${datadir}"/"${line}" output="${basenm}" --overwrite
    r.null map="${basenm}" null=0
    r.resamp.stats -w input="${basenm}" output="${basenm}"_aggr method=average --overwrite
    r.out.gdal input="${basenm}"_aggr output="${datadir}"/"${basenm}"_5m.tif format=GTiff --overwrite
done < tmp' > mygrassjob_pt1.sh

chmod u+x mygrassjob_pt1.sh
export GRASS_BATCH_JOB=mygrassjob_pt1.sh
grass70 /home/simon/grassdata/latlong/PERMANENT
unset GRASS_BATCH_JOB
rm tmp mygrassjob_pt1.sh

# ## ======================================
# ## IIASA-IFPRI crop map
# ## ======================================

# NB this doesn't work for some reason - perhaps because the original
# data file does not fit in geographic coordinates? Instead we use
# process_cropland_map.R

# if [ ! -d data/iiasa-ifpri-cropland-map ]
# then
#     unzip -o data-raw/cropland_hybrid_10042015v9.zip -d data/iiasa-ifpri-cropland-map
# fi

# echo 'export GRASS_MESSAGE_FORMAT=plain
# datadir=data/iiasa-ifpri-cropland-map
# g.region e=98E w=68E n=36N s=6N res=0:05:00
# g.region -p
# r.in.gdal input="${datadir}"/Hybrid_10042015v9.img output=iiasa_ifpri_cropland_map --overwrite
# r.resamp.stats -w input=iiasa_ifpri_cropland_map output=iiasa_ifpri_cropland_map_5m method=average --overwrite
# r.out.gdal input=iiasa_ifpri_cropland_map_5m output=data/iiasa-ifpri-cropland-map/iiasa_ifpri_cropland_map_5m.tif --overwrite' > mygrassjob_pt2.sh

# chmod u+x mygrassjob_pt2.sh
# export GRASS_BATCH_JOB=mygrassjob_pt2.sh
# grass70 /home/simon/grassdata/latlong/PERMANENT
# unset GRASS_BATCH_JOB
# rm tmp mygrassjob_pt2.sh
