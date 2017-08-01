library(magrittr)
library(dplyr)
library(tidyr)
library(raster)
library(rgdal)
library(XLConnect)

options(stringsAsFactors = FALSE)

## ======================================
## 1 - Administrative boundaries (GAUL)
## ======================================

## system("unzip -o data/rawdata/IND_adm_gpkg.zip -d data/IND_adm")

adm2 = readOGR("/home/simon/projects/india_adm2_2001/data", layer="g2008_2_India")

## ## administrative polygons
## dat =
##     as(adm2, "data.frame") %>%
##     dplyr::select(ADM0_CODE, ADM0_NAME, ADM1_CODE, ADM1_NAME, ADM2_CODE, ADM2_NAME) %>%
##     dplyr::arrange(ADM1_NAME, ADM2_NAME) %>%
##     unique %>%
##     write.csv("data/intermediate/g2008_2_India_db.csv", row.names=F)

## ======================================
## 2 - Irrigated Area (Indiastat)
## ======================================

## unpack original data (overwriting if necessary)
system("unrar -o+ x data-raw/Time_series_agri_data.rar data")
system("cp data-raw/apy.csv data")

## get lookup tables for state/district names
state_lut = read.csv("data/state_name_lut.csv", header=TRUE)
dist_lut = read.csv("data/district_name_lut.csv", header=TRUE)

## Original data consists of a single directory (2000-01 to 2012-13/)
## with four Excel files containing data for sourcewise irrigated
## area, cropwise irrigated area (cereal crops), cropwise irrigated
## area (food crops) and cropwise irrigated area (food and nonfood
## crops), respectively. 

rename_states = function(x, lut) {
    ## Function to rename State names using a lookup table

    states=gsub("^\\s+|\\s+$", "", x$State)
    repl = lut$Name[match(states, lut$State)]
    x$State = repl
    x
}

rename_districts = function(x, lut) {
    ## Function to rename district names using a lookup table
    
    states <- unique(x$State)
    data <- vector(mode="list", length=length(states))

    ## handle each State seperately because some states have
    ## districts have the same name as districts in other states
    for (i in 1:length(states)) {
        state <- states[i]
        xx   <- x[x$State %in% state, ]

        ## the line 'lut2 <- ...' reduces the district lookup table
        ## to the state under consideration in order to correctly
        ## handle districts with the same name. For this purpose the
        ## following states are considered together because the
        ## second state was created from the first.
        if (state %in% c("Uttar Pradesh","Uttarakhand")) {
            state <- c("Uttar Pradesh","Uttarakhand")
        } else if (state %in% c("Madhya Pradesh","Chhattisgarh")) {
            state <- c("Madhya Pradesh","Chhattisgarh")
        } else if (state %in% c("Bihar","Jharkhand")) {
            state <- c("Bihar","Jharkhand")
        }
        
        lut2  <- lut[lut$State %in% state, ]
        dists = toupper(xx$District)
        repl = lut2$NAME[match(dists, lut2$District)]
        xx$District = repl
        xx = xx[!is.na(xx$District),] ## remove NA
        data[[i]] = xx        
    }
    x <- do.call(rbind, data)
    x
}

## yrs = rev(paste0(2000:2012, "-", formatC(1:13, width=2, flag=0)))
yrs = rev(2000:2012)

## directory containing raw data files
dir = file.path("data",
                ## "rawdata",
                ## "Time_series_agri_data",
                "District-wise Area Irrigated (1951-51 to 1992-93)",
                "2000-01 to 2012-13")

## file names
files <- c("01. Source-wise Area Irrigated.xls",
           "02. Cropwise Irrigated Area for Cereals.xls",
           "03. Cropwise Irrigated Area for Food Crops.xls",
           "04. Cropwise Irrigated Area for Food and Non-Food Crops.xls")

## row containing first line of data in each of the four files
startrow <- c(6,7,7,6)

## names of columns in the respective Excel files
sia_nms = c("State","District","Year","canal_government_nia","canal_private_nia","canal_total_nia","tanks_nia","tubewells_nia","other_wells_nia","other_sources_nia","total_nia","canal_government_gia","canal_private_gia","canal_total_gia","tanks_gia","tubewells_gia","other_wells_gia","other_sources_gia","total_gia")

cia_cereal_nms = c("State","District","Year","irr_rice-autumn","irr_rice-winter","irr_rice-summer","irr_rice-total","irr_sorghum-kharif","irr_sorghum-rabi","irr_sorghum-total","irr_pearl_millet","irr_maize","irr_finger_millet","irr_wheat","irr_barley","irr_other_cereals-kharif","irr_other_cereals-rabi","irr_other_cereals-total","irr_total_cereals")

cia_food_nms = c("State","District","Year","irr_chickpea","irr_pigeonpea","irr_other_pulses-kharif","irr_other_pulses-rabi","irr_other_pulses-total","irr_total_pulses","irr_total_food_grains","irr_sugarcane","irr_condiments_and_spices","irr_fruit_and_veg","irr_other_food_crops","irr_total_food_crops")

cia_nonfood_nms = c("State","District","Year","irr_groundnut","irr_sesamum","irr_rape_and_mustard","irr_linseed","irr_soyabean","irr_sunflower","irr_other_oil_crops","irr_total_oil_crops","irr_cotton","irr_tobacco","irr_fodder_crops","irr_other_nonfood_crops","irr_total_nonfood_crops","irr_total_all_crops")

## group the above objects in a list, so that we can loop through
## them
column_nms = list(sia=sia_nms,
                  cia_cereal=cia_cereal_nms,
                  cia_food=cia_food_nms,
                  cia_nonfood=cia_nonfood_nms)

## preallocate list to hold data frames
data <- vector(mode="list", length=length(files))
for (j in 1:length(files)) {

    wb <- loadWorkbook(file.path(dir, files[j]))
    sheets <- getSheets(wb)
    nms = column_nms[[j]]

    data2 <- vector(mode="list", length=length(sheets))
    for (i in 1:length(sheets)) {

        ## use dummy object to get number of columns
        dummy = readWorksheet(wb, sheets[i], header=FALSE)
        nc <- ncol(dummy)
        rm(dummy)

        df <- readWorksheet(wb,
                            sheets[i],
                            header=FALSE,
                            colTypes=c(rep("character", 2),
                                       rep("numeric", (nc-2))),
                            forceConversion=TRUE)
        sr <- startrow[j]
        df <- df[sr:nrow(df),]
        df <- df[which(!is.na(df[,2])),]
        df[,1] <- zoo::na.locf(df[,1])
        df <- cbind(df[,1:2], years=yrs[i], df[,3:ncol(df)])

        ## handle a special case
        if (i==3 && j==3) {
            df <- cbind(df[,1:3], aaa=NA, df[,4:ncol(df)])
        }
        
        names(df) <- nms
        df = rename_states(x=df, lut=state_lut)
        df = rename_districts(x=df, lut=dist_lut)

        states = unique(df$State)
        for (state in states) {
            ix = which(df$State %in% state)
            if (length(ix) > 1) {
                ixx = tail(ix, n=1)
                df = df[-ixx,]
            }
        }
            
        data2[[i]] = df
    }

    data[[j]] <- do.call(rbind, data2)
}

sia         = data[[1]]
cia_cereal  = data[[2]]
cia_food    = data[[3]]
cia_nonfood = data[[4]]

## Note:

## The following districts are repeated in the original data:
## Chattisgarh - Balod
## Orissa      - Nayagarh
## Karnataka   - Bangalore
## Karnataka   - Bangalore Rural
## Mizoram     - Aizawl

## To deal with the repeated values we take the first occurrence
sia         = sia[!duplicated(sia[,1:3]),]
cia_cereal  = cia_cereal[!duplicated(cia_cereal[,1:3]),]
cia_food    = cia_food[!duplicated(cia_food[,1:3]),]
cia_nonfood = cia_nonfood[!duplicated(cia_nonfood[,1:3]),]

## GAUL codes
gaul_lut = read.csv("data/indiastat_gaul_lut.csv", header=TRUE)
## all(irr$State %in% gaul_lut$STATE_NAME)       ## check
## all(irr$District %in% gaul_lut$DISTRICT_NAME) ## check

## combine objects into one large data frame

## NB the gather/spread/gather/spread routine in the middle of the
## pipe is to ensure that all districts have entries (even if NA)
## for all years. We do this after summarising to ensure that NAs are
## inserted in missing entries (these would be converted to 0 if we
## summarised afterwards because sum(NA, na.rm=TRUE) = 0
irr =
    sia %>%
    left_join(cia_cereal, by=c("State","District","Year")) %>%
    left_join(cia_food, by=c("State","District","Year")) %>%
    left_join(cia_nonfood, by=c("State", "District", "Year")) %>%
    left_join(gaul_lut, by=c("State", "District")) %>%
    filter(!is.na(GAUL_CODE)) %>%
    group_by(GAUL_CODE, Year, State) %>%
    summarise_each(funs(sum(., na.rm=TRUE)), -District, -GAUL_NAME) %>%
    gather(Crop, Area, -(GAUL_CODE:State)) %>%
    spread(Year, Area) %>%
    gather(Year, Area, -(GAUL_CODE:Crop)) %>%
    spread(Crop, Area) %>%
    left_join(unique(gaul_lut[c("GAUL_CODE","GAUL_NAME")]), by="GAUL_CODE") %>%
    dplyr::select(State, GAUL_NAME, GAUL_CODE, Year, everything()) %>%
    rename(District = GAUL_NAME) %>%
    arrange(State, District, Year)    

## nms_lut = read.csv("data/indiastat_names_lut.csv", header=TRUE)
## names(irr) = nms_lut$NEWNAME[match(names(irr), nms_lut$OLDNAME)]

## ======================================
## 3 - Area and production statists (data.gov.in)
## ======================================

## lookup table to convert to MapSPAM crop types
apy_mapspam_lut = read.csv("data/apy_mapspam_crop_lut.csv", header=TRUE)

## read area, production data, restrict to 2000-2011 and rename
## states and districts using lookup tables
apy =
    read.csv(file.path("data", "apy.csv")) %>%
    ## read.csv("data/rawdata/apy.csv") %>%
    filter(Crop_Year %in% c(2000:2011)) %>%
    mutate_each(funs(trimws), State_Name, District_Name, Season, Crop) %>%
    mutate_each(funs(replace(., .=="Telangana","Andhra Pradesh")), State_Name) %>%
    rename(State=State_Name,
           District=District_Name,
           Year=Crop_Year,
           APY_CROP=Crop) %>%
    rename_states(state_lut) %>%
    rename_districts(dist_lut) %>%
    left_join(apy_mapspam_lut, by=c("APY_CROP")) %>%
    filter(!is.na(MAPSPAM_CROP)) %>%
    group_by(State, District, Year, Season, MAPSPAM_CROP) %>%
    summarise_each(funs(sum(., na.rm=TRUE)), -APY_CROP) %>%
    left_join(gaul_lut, by=c("State","District")) %>%
    filter(!is.na(GAUL_CODE)) %>%
    group_by(State, GAUL_CODE, Year, Season, MAPSPAM_CROP) %>%
    summarise_each(funs(sum(., na.rm=TRUE)),
                   -District,
                   -GAUL_NAME) %>%
    ungroup() %>%
    gather(Element, Value, -(State:MAPSPAM_CROP)) %>%
    spread(Year, Value) %>%
    gather(Year, Value, -(State:Element)) %>%
    mutate(MAPSPAM_CROP = tolower(gsub(" ", "_", MAPSPAM_CROP))) %>%
    mutate(Season = tolower(gsub(" ", "_", Season))) %>%
    unite(MAPSPAM_CROP, MAPSPAM_CROP, Season, sep="-") %>%
    spread(MAPSPAM_CROP, Value) %>%
    left_join(unique(gaul_lut[c("GAUL_CODE","GAUL_NAME")]), by="GAUL_CODE") %>%
    dplyr::select(State,
                  GAUL_NAME,
                  GAUL_CODE,
                  Year,
                  Element,
                  everything()) %>%
    rename(District = GAUL_NAME) %>%
    arrange(State, District, Year)    

## xx =
##     apy %>%
##     gather(Crop, Value, -(State:Element)) %>%
##     separate(Crop, c("Crop", "Season"), sep="-") %>%
##     filter(Crop %in% "wheat") %>%
##     spread(Element, Value) %>%
##     mutate(Yield = Production / Area)

## max(xx$Yield, na.rm=TRUE) ## tons/ha

## xx =
##     xx %>%
##     filter(Year %in% 2005)

## sum(xx$Area, na.rm=TRUE)

## yy =
##     apy %>%
##     filter(Element %in% "Area") %>%
##     gather(Crop, Value, -(State:Element)) %>%
##     separate(Crop, c("Crop", "Season"), sep="-") %>%
##     ## spread(Element, Value) %>%
##     filter(Season %in% "rabi") %>%
##     filter(State %in% "Punjab") %>%
##     filter(District %in% "Moga") %>%
##     filter(Year %in% 2005) %>%
##     spread(Crop, Value) %>%
##     dplyr::select(-(State:Season))

## ======================================
## 4 - combine apy and irr data frames
## ======================================

apy_prod =
    apy %>%
    filter(Element %in% "Production") %>%
    dplyr::select(-Element) %>%
    gather(Crop, Value, -(State:Year)) %>%
    separate(Crop, c("Crop","Season"), sep="-") %>%
    group_by(State, District, GAUL_CODE, Year, Crop) %>%
    summarise_each(funs(sum(., na.rm=TRUE)), Value) %>% ## removes NA
    ungroup %>%
    group_by(State, District, GAUL_CODE, Crop) %>%
    summarise_each(funs(max(., na.rm=TRUE)), Value) %>%
    spread(Crop, Value) ## %>%
    ## filter(Year %in% 2005)

apy_area =
    apy %>%
    filter(Element %in% "Area") %>%
    dplyr::select(-Element) 

crop_nms = names(apy_prod)[4:ncol(apy_prod)]

## 12/07/2017 - I think this would be a sensible idea and perfectly
## justifiable considering that the main purpose of the dataset is
## to show crop change at the macro level

## for (i in 1:length(crop_nms)) {
##     nm = crop_nms[i]
##     pattern = paste0("^", nm, "-")
##     col_ix = grep(pattern, names(apy_area), value=FALSE)
##     prod = apy_prod[[nm]]
##     ordr = order(prod)
##     ix = which(cumsum(prod[ordr]) / sum(prod) < 0.001) %>% match(ordr) ## 0.001 is 0.01% (?)
##     sd = apy_prod[ix,c("State","District")] %>% arrange(State, District)
##     row_ix = match(interaction(apy_area$State, apy_area$District), interaction(sd$State, sd$District))
##     apy_area[,col_ix] = lapply(apy_area[,col_ix], function(x) replace(x, row_ix, 0))
## }

names(apy_area)[-(1:4)] %<>% paste0("apy_", .)

irr_area = irr

combined_data =
    bind_rows(apy_area[c("State","District","GAUL_CODE","Year")],
              irr_area[c("State","District","GAUL_CODE","Year")]) %>%
    unique %>%
    left_join(irr_area) %>%
    left_join(apy_area)

saveRDS(combined_data, "data/apy_indiastat_combined_data.rds")

## ======================================
## 5 - check data quality
## ======================================

ap_totals = apy_area %>%
    filter(State %in% "Andhra Pradesh") %>%
    gather(Crop, Value, -(State:Year)) %>%
    group_by(Crop) %>%
    summarise_each(funs(sum(., na.rm=TRUE)), Value)

ap_totals = ap_totals[order(ap_totals$Value),] %>% as.data.frame
    
## how to do this?
crop_totals =
    read.csv(file.path("data","apy.csv")) %>%
    filter(Crop_Year %in% c(2000:2011)) %>%
    mutate_each(funs(trimws), State_Name, District_Name, Season, Crop) %>%
    mutate_each(funs(replace(., .=="Telangana","Andhra Pradesh")), State_Name) %>%
    rename(State=State_Name,
           District=District_Name,
           Year=Crop_Year,
           APY_CROP=Crop) %>%
    rename_states(state_lut) %>%
    rename_districts(dist_lut) %>%
    dplyr::select(-Production) %>%
    group_by(APY_CROP) %>%
    summarise_each(funs(sum(., na.rm=TRUE)), Area)

crop_totals = crop_totals[order(crop_totals$Area),] %>% as.data.frame







## output state/district names from statistics/GAUL to create a lookup
## table with which we can associate polygons in GAUL with
## statistics (UNCOMMENT IF REQUIRED)

## administrative polygons
## dat =
##     as(adm2, "data.frame") %>%
##     dplyr::select(ADM0_CODE, ADM0_NAME, ADM1_CODE, ADM1_NAME, ADM2_CODE, ADM2_NAME) %>%
##     dplyr::arrange(ADM1_NAME, ADM2_NAME) %>%
##     unique %>%
##     write.csv("data/intermediate/g2008_2_India_db.csv", row.names=F)

## ## area, production, yield statistics
## apy =
##     read.csv("data/rawdata/apy.csv") %>%
##     filter(Crop_Year %in% c(2000:2011)) %>%
##     dplyr::select(State_Name, District_Name) %>%
##     mutate(State_Name = trimws(State_Name)) %>%
##     mutate_each(funs(replace(., .=="Telangana","Andhra Pradesh")), State_Name) %>%
##     dplyr::arrange(State_Name, District_Name) %>%
##     unique %>%
##     write.csv("data/intermediate/apy_adm2_nms.csv", row.names=F)

## apy =
##     apy %>%
##     ## mutate(Area_Unit="Ha") %>%
##     ## mutate(Production_Unit="Tonne") %>%
##     gather(Element, Value, Area, Production) %>%
##     arrange(State_Name, District_Name, Crop_Year, Season, Crop) %>%
##     mutate(Crop_Year=paste0("X", Crop_Year)) %>%
##     spread(Crop_Year, Value)

## x %>% dplyr::select(State, District) %>% unique %>% arrange(State, District) %>% write.csv("data/intermediate/indiastat_adm2_nms.csv", row.names=F)

## lut1 = read.csv("data/apy_gaul_lut.csv", header=TRUE)
## lut2 = read.csv("data/indiastat_gaul_lut.csv", header=TRUE)

## sia_nms = c("State", "District", "Year", "Canal-Government-NIA", "Canal-Private-NIA", "Canal-Total-NIA", "Tanks-NIA", "Tube Wells-NIA", "Other Wells-NIA", "Other Sources-NIA", "Total-NIA", "Canal-Government-GIA", "Canal-Private-GIA", "Canal-Total-GIA", "Tanks-GIA", "Tube Wells-GIA", "Other Wells-GIA", "Other Sources-GIA", "Total-GIA")

## cia_cereal_nms = c("State", "District", "Year", "Rice (Oryza Sativa)-Autumn", "Rice (Oryza Sativa)-Winter", "Rice (Oryza Sativa)-Summer", "Rice (Oryza Sativa)-Total", "Cholum or Jowar (Sorghum Vulgare)-Kharif", "Cholum or Jowar (Sorghum Vulgare)-Rabi", "Cholum or Jowar (Sorghum Vulgare)-Total", "Cumbu or Bajra (Pennisetumtyphoideum)", "Maize (Zea Mays", "Ragi or Marua (Elusine Coracana)", "Wheat (Triticumsativum)", "Barley (Hordeumvulgare)", "Other Cereals and Millets-Kharif", "Other Cereals and Millets-Rabi", "Other Cereals and Millets-Total", "Total Cereals and Millets")

## cia_food_nms = c("State", "District", "Year", "Gram (Cicerarietinum)", "Tur or Arhar (Cajanusindicus)", "Other Pulses (Excluding Gram and Tur or Arhar)-Kharif", "Other Pulses (Excluding Gram and Tur or Arhar)-Rabi", "Other Pulses (Excluding Gram and Tur or Arhar)-Total", "Total Pulses", "Total Food Grains", "Sugarcane (Saccharumofficinarum)", "Condiments and Spices", "Fruits and Vegetables Including root crops", "Other Food Crops", "Total Food Crops")

## cia_nonfood_nms =  c("State", "District", "Year", "Groundnut (Arachishypozes)", "Sesamum (Til or Jinjili) (Sesamum indicum)", "Rape & Mustard (Brassicasp)", "Linseed (Linumusitatissimum)", "Soyabean", "Sunflower", "Others", "Total", "Cotton (Gossyptumsp)", "Tobacco (Nicotianatobacum & N. Rustica)", "Fodder Crops", "Other Non-food Crops", "Total Non-food Crops", "Total (Under all Crops)")

